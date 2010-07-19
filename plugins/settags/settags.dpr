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
library settags;

uses
  Windows,
  Messages,
  SysUtils,
  TlHelp32,
  LanguageObjects in '..\..\..\common\LanguageObjects.pas',
  Functions in '..\..\..\common\Functions.pas';

type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;

  TPlayResults = (prOk, prError);
  TReadWrite = function(Name, Value: PChar): Integer;

const
  AUTHOR = 'Graf Zwal';

var
  Lang: string;
  Read: TReadWrite;
  Write: TReadWrite;

function GetAuthor(Data: PChar; Len: Integer): Integer; stdcall;
begin
  Result := -1;
  if Len < Length(AUTHOR) then
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
  if Len < Length(s) then
    Exit;
  Move(s[1], Data[0], Len);
  Result := Length(s);
end;

function Act(FileData: PMapBytes): Integer; stdcall;
var
  i, Offset: Integer;
  Count, Len: Word;

  Files: array of string;
begin
  Result := Integer(prError);

  Move(FileData^[0], Count, SizeOf(Count));
  SetLength(Files, Count);
  Offset := SizeOf(Count);
  for i := 0 to Count - 1 do
  begin
    Move(FileData^[Offset], Len, SizeOf(Len));
    Offset := Offset + SizeOf(Len);
    SetLength(Files[i], (Len div SizeOf(Char)) + SizeOf(Char));
    FillChar(Files[i][1], Length(Files[i]) * SizeOf(Char), #0);
    Move(FileData^[Offset], Files[i][1], Len);
    Offset := Offset + Len;
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
  Configure,
  Act;

end.

