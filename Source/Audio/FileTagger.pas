{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    ------------------------------------------------------------------------
}

unit FileTagger;

interface

uses
  AddonAudioGenie,
  AudioGenie,
  Base64,
  Classes,
  Graphics,
  Math,
  SyncObjs,
  SysUtils,
  Windows;

type
  TTagData = class
  private
    FArtist, FTitle, FAlbum, FGenre, FComment, FTrackNumber: string;
    FPopulariMeterEmail: WideString;
    FPopulariMeterRating: SmallInt;
    FPopulariMeterCounter: LongInt;
  public
    function Copy: TTagData;

    property Artist: string read FArtist write FArtist;
    property Title: string read FTitle write FTitle;
    property Album: string read FAlbum write FAlbum;
    property Genre: string read FGenre write FGenre;
    property Comment: string read FComment write FComment;
    property TrackNumber: string read FTrackNumber write FTrackNumber;
  end;

  TFileTagger = class
  private
    FFilename: string;
    FAudioType: TAudioFormatID;
    FTag: TTagData;
  public
    constructor Create;
    destructor Destroy; override;

    function Read(Filename: string): Boolean;
    function Write(LCID: Cardinal; Filename: string): Boolean;

    property AudioType: TAudioFormatID read FAudioType;
    property Filename: string read FFilename;
    property Tag: TTagData read FTag;
  end;

implementation

uses
  AppData;

var
  FileTaggerLock: SyncObjs.TCriticalSection;

{ TFileTagger }

constructor TFileTagger.Create;
begin
  inherited;

  FTag := TTagData.Create;
end;

destructor TFileTagger.Destroy;
begin
  FTag.Free;

  inherited;
end;

function TFileTagger.Read(Filename: string): Boolean;
var
  AG: TAudioGenie3;
begin
  Result := False;
  FFilename := '';

  if not TAddonAudioGenie(AppGlobals.AddonManager.Find(TAddonAudioGenie)).FilesExtracted then
    Exit;

  FFilename := Filename;

  FileTaggerLock.Enter;
  try
    AG := TAudioGenie3.Create(AppGlobals.AddonManager.Find(TAddonAudioGenie).ModuleFilePath);

    try
      FAudioType := AG.AUDIOAnalyzeFileW(Filename);
      if FAudioType <> TAudioFormatID.UNKNOWN then
      begin
        FTag.FArtist := AG.AUDIOArtistW;
        FTag.FTitle := AG.AUDIOTitleW;
        FTag.FAlbum := AG.AUDIOAlbumW;
        FTag.FGenre := AG.AUDIOGenreW;
        FTag.FComment := AG.AUDIOCommentW;
        FTag.FTrackNumber := AG.AUDIOTrackW;

        case FAudioType of
          MPEG:
          begin
            FTag.FPopulariMeterEmail := AG.ID3V2GetPopularimeterEmailW(1);
            FTag.FPopulariMeterRating := AG.ID3V2GetPopularimeterRatingW(1);
            FTag.FPopulariMeterCounter := AG.ID3V2GetPopularimeterCounterW(1);
          end;
        end;

        Result := True;
      end;
    finally
      AG.Free;
    end;
  finally
    FileTaggerLock.Leave;
  end;
end;

function TFileTagger.Write(LCID: Cardinal; Filename: string): Boolean;
var
  LangCode: array[0..8] of AnsiChar;
  L: Integer;
  S: string;
  AG: TAudioGenie3;
  Ver: TOSVersionInfo;
begin
  Result := False;

  if not AppGlobals.AddonManager.Find(TAddonAudioGenie).FilesExtracted then
    Exit;

  FileTaggerLock.Enter;
  try
    AG := TAudioGenie3.Create(AppGlobals.AddonManager.Find(TAddonAudioGenie).ModuleFilePath);
    try
      if AG.AUDIOAnalyzeFileW(Filename) <> TAudioFormatID.UNKNOWN then
      begin
        AG.AUDIOArtistW := FTag.FArtist;
        AG.AUDIOTitleW := FTag.FTitle;
        AG.AUDIOAlbumW := FTag.FAlbum;
        AG.AUDIOGenreW := FTag.FGenre;
        AG.AUDIOCommentW := FTag.FComment;

        // Der Abschnitt hier fügt einen lokalisierten Kommentar hinzu, so dass der Windows-Explorer
        // ihn in der Eigenschaften-Seite anzeigt ($0067 geht ab Windows Vista).
        Ver.dwOSVersionInfoSize := SizeOf(Ver);
        if GetVersionEx(Ver) then
          if Ver.dwMajorVersion = 6 then
          begin
            ZeroMemory(@LangCode[0], 9);
            L := GetLocaleInfoA(LCID, $0067, @LangCode[0], 9);
            if L > 0 then
            begin
              S := LowerCase(LangCode);
              AG.ID3V2AddCommentW(S, '', FTag.FComment);
            end;
          end;

        AG.AUDIOTrackW := FTag.FTrackNumber;
        case FAudioType of
          MPEG:
            if (FTag.FPopulariMeterRating > 0) or (FTag.FPopulariMeterCounter > 0) then
              AG.ID3V2AddPopularimeterW(FTag.FPopulariMeterEmail, FTag.FPopulariMeterRating, FTag.FPopulariMeterCounter);
        end;

        if AG.AUDIOSaveChangesW then
          Result := True;
      end;
    finally
      AG.Free;
    end;
  finally
    FileTaggerLock.Leave;
  end;
end;

{ TTagData }

function TTagData.Copy: TTagData;
begin
  Result := TTagData.Create;
  Result.FArtist := FArtist;
  Result.FTitle := FTitle;
  Result.FAlbum := FAlbum;
  Result.FGenre := FGenre;
  Result.FComment := FComment;
  Result.FTrackNumber := FTrackNumber;
end;

initialization
  FileTaggerLock := SyncObjs.TCriticalSection.Create;

finalization
  FileTaggerLock.Free;

end.
