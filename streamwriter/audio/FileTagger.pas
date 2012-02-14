{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
  Windows, SysUtils, AudioGenie, AddonAudioGenie, SyncObjs;

type
  TFileTagger = class
  private
    FFilename: string;
    FArtist, FTitle, FAlbum, FComment, FTrackNumber: string;
  public
    constructor Create;
    destructor Destroy; override;

    function Read(Filename: string): Boolean;
    function Write(Filename: string): Boolean;

    property Filename: string read FFilename;
    property Artist: string read FArtist write FArtist;
    property Title: string read FTitle write FTitle;
    property Album: string read FAlbum write FAlbum;
    property Comment: string read FComment write FComment;
    property TrackNumber: string read FTrackNumber write FTrackNumber;
  end;

implementation

uses
  AppData;

var
  FileTaggerLock: TCriticalSection;

{ TFileTagger }

constructor TFileTagger.Create;
begin
  inherited;

end;

destructor TFileTagger.Destroy;
begin

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
  FArtist := '';
  FTitle := '';
  FAlbum := '';
  FComment := '';
  FTrackNumber := '';

  FileTaggerLock.Enter;
  try
    AG := TAudioGenie3.Create(TAddonAudioGenie(AppGlobals.AddonManager.Find(TAddonAudioGenie)).DLLPath);
    try
      if AG.AUDIOAnalyzeFileW(Filename) <> UNKNOWN then
      begin
        FArtist := AG.AUDIOArtistW;
        FTitle := AG.AUDIOTitleW;
        FAlbum := AG.AUDIOAlbumW;
        FComment := AG.AUDIOCommentW;
        FTrackNumber := AG.AUDIOTrackW;

        Result := True;
      end;
    finally
      AG.Free;
    end;
  finally
    FileTaggerLock.Leave;
  end;
end;

function TFileTagger.Write(Filename: string): Boolean;
var
  AG: TAudioGenie3;
begin
  Result := False;

  if not TAddonAudioGenie(AppGlobals.AddonManager.Find(TAddonAudioGenie)).FilesExtracted then
    Exit;

  FileTaggerLock.Enter;
  try
    AG := TAudioGenie3.Create(TAddonAudioGenie(AppGlobals.AddonManager.Find(TAddonAudioGenie)).DLLPath);
    try
      if AG.AUDIOAnalyzeFileW(Filename) <> UNKNOWN then
      begin
        AG.AUDIOArtistW := FArtist;
        AG.AUDIOTitleW := FTitle;
        AG.AUDIOAlbumW := FAlbum;
        AG.AUDIOCommentW := FComment;
        AG.AUDIOTrackW := FTrackNumber;

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

initialization
  FileTaggerLock := TCriticalSection.Create;

finalization
  FileTaggerLock.Free;

end.
