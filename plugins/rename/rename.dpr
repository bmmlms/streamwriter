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
library rename;

uses
  Windows,
  Messages,
  SysUtils,
  TlHelp32,
  Mp3FileUtils,
  PluginsShared in '..\..\streamwriter\PluginsShared.pas',
  LanguageObjects in '..\..\..\common\LanguageObjects.pas',
  Functions in '..\..\..\common\Functions.pas';

type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;

  TActResults = (arWin, arFail);
  TReadWrite = function(Name, Value: PChar): Integer;

const
  AUTHOR = 'Graf Zwal';
  DEFAULT_ENABLED = False;

var
  Lang: string;
  Read: TReadWrite;
  Write: TReadWrite;

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
  s := _('Prefix filename with tracknumber');
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
  s := _('This plugin appends the tracknumber to filenames of saved songs, for example "Artist - Track.mp3" becomes "0012 - Artist - Track.mp3".');
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

function Act(Data: TPluginActData): Integer; stdcall;
var
  Filepath, Filename, Prefix: string;
begin
  Result := Integer(arFail);

  Filepath := ExtractFilePath(Data.Filename);
  Filepath := IncludeTrailingBackslash(Filepath);
  Filename := ExtractFileName(Data.Filename);
  Prefix := Format('%.*d', [4, Data.TrackNumber]) ;

  if RenameFile(Data.Filename, Filepath + Prefix + ' - ' + Filename) then
  begin
    StrPCopy(Data.Filename, Filepath + Prefix + ' - ' + Filename);
    Result := Integer(arWin);
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
  GetAuthor,
  GetName,
  GetHelp,
  GetDefaultEnabled,
  Configure,
  Act;

end.

