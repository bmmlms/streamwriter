{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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

unit SWFunctions;

interface

uses
  Windows, SysUtils, AudioFunctions, Functions;

function GetAutoTuneInMinKbps(AudioType: TAudioTypes; Idx: Integer): Cardinal;
function FixPatternFilename(Filename: string): string;
function SecureSWURLToInsecure(URL: string): string;
function ConvertPattern(OldPattern: string): string;

implementation

function GetAutoTuneInMinKbps(AudioType: TAudioTypes; Idx: Integer): Cardinal;
begin
  Result := 0;
  case AudioType of
    atMPEG:
      begin
        case Idx of
          0: Result := 192;
          1: Result := 128;
        end;
      end;
    atAAC:
      begin
        case Idx of
          0: Result := 96;
          1: Result := 48;
        end;
      end;
  end;
end;

function FixPatternFilename(Filename: string): string;
var
  i: Integer;
begin
  Result := Filename;

  // Remove subsequent \
  i := 1;
  if Length(Result) > 0 then
    while True do
    begin
      if i = Length(Result) then
        Break;
      if Result[i] = '\' then
        if Result[i + 1] = '\' then
        begin
          Result := Copy(Result, 1, i) + Copy(Result, i + 2, Length(Result) - i);
          Continue;
        end;
      Inc(i);
    end;

  // Replace invalid characters for filenames
  Result := StringReplace(Result, '/', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, ':', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '*', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '"', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '?', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '<', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '>', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '|', ' ', [rfReplaceAll]);

  // Make sure there is no \ at the beginning/ending
  if Length(Result) > 0 then
    if Result[1] = '\' then
      Result := Copy(Result, 2, Length(Result) - 1);
  if Length(Result) > 0 then
    if Result[Length(Result)] = '\' then
      Result := Copy(Result, 1, Length(Result) - 1);
end;

function SecureSWURLToInsecure(URL: string): string;
var
  Res: TParseURLRes;
begin
  Result := URL;
  Res := ParseURL(URL);
  if Res.Success and Res.Secure and (Pos('streamwriter.', LowerCase(Res.Host)) > 0) then
    Result := 'http://' + Res.Host + ':80' + Res.Data;
end;

function ConvertPattern(OldPattern: string): string;
var
  i: Integer;
  C: Char;
  Patterns: string;
  Arr: TPatternReplaceArray;
begin
  Patterns := 'fatlusndi';
  SetLength(Arr, Length(Patterns));
  for i := 0 to Length(Patterns) - 1 do
  begin
    Arr[i].C := Patterns[i + 1];
    C := Arr[i].C[1];
    case C of
      'f':
        Arr[i].Replace := '%filename%';
      'a':
        Arr[i].Replace := '%artist%';
      't':
        Arr[i].Replace := '%title%';
      'l':
        Arr[i].Replace := '%album%';
      'u':
        Arr[i].Replace := '%streamtitle%';
      's':
        Arr[i].Replace := '%streamname%';
      'n':
        Arr[i].Replace := '%number%';
      'd':
        Arr[i].Replace := '%day%.%month%.%year';
      'i':
        Arr[i].Replace := '%hour%.%minute%.%second%';
    end;
  end;

  Result := PatternReplace(OldPattern, Arr);
end;

end.
