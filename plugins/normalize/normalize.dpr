{
    ------------------------------------------------------------------------
    streamWriter normalize plugin
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
library normalize;

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  TlHelp32,
  Mp3FileUtils,
  PluginsShared in '..\..\streamwriter\PluginsShared.pas',
  LanguageObjects in '..\..\..\common\LanguageObjects.pas',
  Functions in '..\..\..\common\Functions.pas';

{$R .\res\language.res}
{$R res\res.res}

type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;

  TActResults = (arWin, arFail);
  TReadWrite = function(Name, Value: PChar): Integer;

const
  AUTHOR = 'Graf Zwal';
  DEFAULT_ENABLED = True;

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
  s := _('Normalize');
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
  s := _('This plugin normalizes saved songs by using MP3Gain.');
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
  R: TResourceStream;
  Handle: Cardinal;
  ExitCode: DWord;
begin
  Result := Integer(arFail);

  if GetTempDir = '' then
    Exit;

  try
    try
      if not FileExists(GetTempDir + 'mp3gain.exe') then
      begin
        R := TResourceStream.Create(HInstance, 'MP3GAIN', RT_RCDATA);
        try
          R.SaveToFile(GetTempDir + 'mp3gain.exe');
        finally
          R.Free;
        end;
      end;

      RunProcess(Format('"%smp3gain.exe" "%s"', [GetTempDir, Data.Filename]), Handle, True);
      if Handle < High(Cardinal) then
      begin
        WaitForSingleObject(Handle, Infinite);
        GetExitCodeProcess(Handle, ExitCode);
        if ExitCode = 0 then
          Result := Integer(arWin);
      end;
    finally

    end;
  except
    on E: Exception do
      MsgBox(0, E.Message, '', 0);
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

