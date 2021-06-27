{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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
  Windows, SysUtils, AudioFunctions, Functions, RegExpr, Classes,
  Generics.Defaults, Generics.Collections, Constants, ComCtrls,
  LanguageObjects;

function GetAutoTuneInMinKbps(AudioType: TAudioTypes; Idx: Integer): Cardinal;
function FixPatternFilename(Filename: string): string;
function SecureSWURLToInsecure(URL: string): string;
function ConvertPattern(OldPattern: string): string;
function GetBestRegEx(Title: string; RegExps: TStringList): string;
function CheckRegExp(Handle: THandle; var RegExp: string; List: TListView; Item: TListItem): Boolean;

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
        Arr[i].Replace := '%day%.%month%.%year%';
      'i':
        Arr[i].Replace := '%hour%.%minute%.%second%';
    end;
  end;

  Result := PatternReplace(OldPattern, Arr);
end;

function GetBestRegEx(Title: string; RegExps: TStringList): string;
const
  BadChars: array[0..3] of string = (':', '-', '|', '*');
type
  TRegExData = record
    RegEx: string;
    BadWeight: Integer;
  end;
function GetBadWeight(Text: string): Integer;
var
  n: Integer;
begin
  Result := 0;

  Text := Trim(Text);
  if Length(Text) = 0 then
    Exit(3);

  for n := 0 to High(BadChars) do
    if Pos(BadChars[n], Text) > 0 then
      Result := Result + 2;

  if ContainsRegEx('(\d{2,})', Text) then
    Result := Result + 2;
end;
var
  i: Integer;
  R: TRegExpr;
  DefaultRegEx: string;
  RED: TRegExData;
  REDs: TList<TRegExData>;
begin
  Result := DefaultRegEx; // TODO: !?

  REDs := TList<TRegExData>.Create;
  try
    for i := 0 to RegExps.Count - 1 do
    begin
      RED.RegEx := RegExps[i];
      RED.BadWeight := 0;
      if RED.RegEx = DEFAULT_TITLE_REGEXP then
        RED.BadWeight := 1;

      R := TRegExpr.Create(RED.RegEx);
      R.ModifierI := True;
      try
        try
          if R.Exec(Title) then
          begin
            try
              if R.MatchFromName('a') <> '' then
                RED.BadWeight := RED.BadWeight + GetBadWeight(Trim(R.MatchFromName('a')))
              else
                RED.BadWeight := RED.BadWeight + 3;
            except end;

            try
              if R.MatchFromName('t') <> '' then
                RED.BadWeight := RED.BadWeight + GetBadWeight(Trim(R.MatchFromName('t')))
              else
                RED.BadWeight := RED.BadWeight + 3;
            except end;

            try
              if R.MatchFromName('l') <> '' then
              begin
                RED.BadWeight := RED.BadWeight - 6;
                RED.BadWeight := RED.BadWeight + GetBadWeight(Trim(R.MatchFromName('l')))
              end else
                RED.BadWeight := RED.BadWeight + 10;
            except end;
          end else
            RED.BadWeight := RED.BadWeight + 1000;

          REDs.Add(RED);
        except end;
      finally
        R.Free;
      end;
    end;

    {             // TODO:
    REDs.Sort(TComparer<TRegExData>.Construct(
      function (const L, R: TRegExData): integer
      begin
        Result := CmpInt(L.BadWeight, R.BadWeight);
      end
    ));
    }

    if REDs.Count > 0 then
      Result := REDs[0].RegEx;
  finally
    REDs.Free;
  end;
end;

function CheckRegExp(Handle: THandle; var RegExp: string; List: TListView; Item: TListItem): Boolean;
var
  i: Integer;
  RValid, ArtistFound, TitleFound: Boolean;
//  R: TPerlRegEx;
begin
  {           // TODO:
  Result := False;
  RegExp := Trim(RegExp);

  for i := 0 to List.Items.Count - 1 do
    if (LowerCase(RegExp) = LowerCase(Trim(List.Items[i].Caption))) and (List.Items[i] <> Item) then
    begin
      MsgBox(Handle, _('The specified regular expression is already on the list.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

  RValid := False;
  R := TPerlRegEx.Create;
  try
    R.RegEx := RegExp;
    try
      R.Compile;
      RValid := True;
    except end;
  finally
    R.Free;
  end;

  ArtistFound := (Pos('(?P<a>.*)', RegExp) > 0) or (Pos('(?P<a>.*?)', RegExp) > 0);
  TitleFound := (Pos('(?P<t>.*)', RegExp) > 0) or (Pos('(?P<t>.*?)', RegExp) > 0);

  if (RegExp = '') or (not RValid) or (not ArtistFound) or (not TitleFound) then
  begin
    MsgBox(Handle, _('Please supply a valid regular expression containing the groups (?P<a>.*)/(?P<a>.*?) and (?P<t>.*)/(?P<t>.*?).'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  Result := True;
  }
end;

end.
