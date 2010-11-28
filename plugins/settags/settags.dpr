{
    ------------------------------------------------------------------------
    streamWriter settags plugin
    Copyright (c) 2010 Alexander Nottelmann

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
library settags;

uses
  Windows,
  SysUtils,
  Mp3FileUtils,
  PluginsShared in '..\..\streamwriter\PluginsShared.pas',
  LanguageObjects in '..\..\..\common\LanguageObjects.pas',
  Functions in '..\..\..\common\Functions.pas';

{$SetPEOptFlags $0140}

{$R .\res\language.res}

type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;

  TActResults = (arWin, arFail);
  TReadWrite = function(Keyname, Name, Value: PChar): Integer;

const
  AUTHOR = 'Graf Zwal';
  VERSION = 1;
  DEFAULT_ENABLED = True;

var
  Lang: string;
  Read: TReadWrite;
  Write: TReadWrite;

function GetVersion: Integer; stdcall;
begin
  Result := VERSION;
end;

function GetAuthor(Data: PChar; Len: Integer): Integer; stdcall;
begin
  Result := -1;
  if Len < Length(AUTHOR) * SizeOf(Char) then
    Exit;
  Move(AUTHOR[1], Data[0], Len);
  Result := Length(AUTHOR);
end;

function GetName(Data: PChar; Len: Integer): Integer; stdcall;
var
  s: string;
begin
  s := _('Set ID3 tags');
  Result := -1;
  if Len < Length(s) * SizeOf(Char) then
    Exit;
  Move(s[1], Data[0], Len);
  Result := Length(s);
end;

function GetHelp(Data: PChar; Len: Integer): Integer; stdcall;
var
  s: string;
begin
  s := _('This plugin writes ID3-tags to saved songs.');
  Result := -1;
  if Len < Length(s) * SizeOf(Char) then
    Exit;
  Move(s[1], Data[0], Len);
  Result := Length(s);
end;

function GetDefaultEnabled: Boolean; stdcall;
begin
  Result := DEFAULT_ENABLED;
end;

function Act(var Data: TPluginActData): Integer; stdcall;
var
  p: Integer;
  Artist, Title2: string;
  ID3V1: TID3v1Tag;
  ID3V2: TID3v2Tag;
begin
  Result := Integer(arFail);

  ID3V1 := TID3v1Tag.Create;
  ID3V2 := TID3v2Tag.Create;
  try
    try
      Artist := '';
      Title2 := '';

      p := Pos(' - ', Data.Title);
      if p > 0 then
      begin
        Artist := Copy(Data.Title, 1, p - 1);
        Title2 := Copy(Data.Title, p + 3, Length(Data.Title));
      end;

      if (Trim(Artist) <> '') and (Trim(Title2) <> '') then
      begin
        ID3V1.Artist := Artist;
        ID3V1.Title := Title2;
        ID3V2.Artist := Artist;
        ID3V2.Title := Title2;
      end else
      begin
        ID3V1.Title := Data.Title;
        ID3V2.Title := Data.Title;
      end;
      ID3V1.Track := IntToStr(Data.TrackNumber);
      ID3V2.Track := IntToStr(Data.TrackNumber);
      ID3V1.Album := Data.Station;
      ID3V2.Album := Data.Station;
      ID3V1.Comment := 'Recorded by streamWriter';
      ID3V2.Comment := 'Recorded by streamWriter';
      if (ID3V1.WriteToFile(Data.Filename) = MP3ERR_None) and (ID3V2.WriteToFile(Data.Filename) = MP3ERR_None) then
      begin
        Data.Filesize := GetFileSize(Data.Filename);
        Result := Integer(arWin);
      end;
    except
    end;
  finally
    ID3V1.Free;
    ID3V2.Free;
  end;
end;

function Configure(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;
begin
  Result := False;
end;

procedure Initialize(L: PChar; RF, WF: TReadWrite); stdcall;
begin
  Lang := L;
  Language.CurrentLanguage := LanguageList.FindLanguage(Lang);
  Read := RF;
  Write := WF;
end;

exports
  Initialize,
  GetVersion,
  GetAuthor,
  GetName,
  GetHelp,
  GetDefaultEnabled,
  Configure,
  Act;

end.

