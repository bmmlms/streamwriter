{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  AudioFunctions,
  Classes,
  ComCtrls,
  Constants,
  Functions,
  Generics.Collections,
  Generics.Defaults,
  Graphics,
  LanguageObjects,
  regexpr,
  SysUtils,
  Windows;

function GetAutoTuneInMinKbps(AudioType: TAudioTypes; Idx: Integer): Cardinal;
function FixPatternFilename(Filename: string): string;
function SecureSWURLToInsecure(URL: string): string;
function ConvertPattern(OldPattern: string): string;
function GetBestRegEx(Title: string; RegExps: TStringList): string;
function CheckRegExp(var RegExp: string; List: TListView; Item: TListItem): Boolean;
function GetGradientColor(const FromColor, ToColor: TColor; const Factor: Double): TColor;

implementation

type
  TRegExData = record
    RegEx: string;
    BadWeight: Integer;
  end;

function GetAutoTuneInMinKbps(AudioType: TAudioTypes; Idx: Integer): Cardinal;
begin
  Result := 0;
  case AudioType of
    atMPEG:
      case Idx of
        0: Result := 192;
        1: Result := 128;
      end;
    atAAC:
      case Idx of
        0: Result := 96;
        1: Result := 48;
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
  Res := TFunctions.ParseURL(URL);
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
  for i := 0 to High(Patterns) do
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

  Result := TFunctions.PatternReplace(OldPattern, Arr);
end;

function CompareRegExDatas(constref L, R: TRegExData): Integer;
begin
  Result := TFunctions.CmpInt(L.BadWeight, R.BadWeight);
end;

function GetBestRegEx(Title: string; RegExps: TStringList): string;
const
  BadChars: array[0..3] of string = (':', '-', '|', '*');

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

    if TFunctions.ContainsRegEx('(\d{2,})', Text) then
      Result := Result + 2;
  end;

var
  i: Integer;
  R: TRegExpr;
  RED: TRegExData;
  REDs: TList<TRegExData>;
begin
  Result := DEFAULT_TITLE_REGEXP;

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
            except
            end;

            try
              if R.MatchFromName('t') <> '' then
                RED.BadWeight := RED.BadWeight + GetBadWeight(Trim(R.MatchFromName('t')))
              else
                RED.BadWeight := RED.BadWeight + 3;
            except
            end;

            try
              if R.MatchFromName('l') <> '' then
              begin
                RED.BadWeight := RED.BadWeight - 6;
                RED.BadWeight := RED.BadWeight + GetBadWeight(Trim(R.MatchFromName('l')));
              end else
                RED.BadWeight := RED.BadWeight + 10;
            except
            end;
          end else
            RED.BadWeight := RED.BadWeight + 1000;

          REDs.Add(RED);
        except
        end;
      finally
        R.Free;
      end;
    end;

    REDs.Sort(TComparer<TRegExData>.Construct(@CompareRegExDatas));

    if REDs.Count > 0 then
      Result := REDs[0].RegEx;
  finally
    REDs.Free;
  end;
end;

function CheckRegExp(var RegExp: string; List: TListView; Item: TListItem): Boolean;
var
  i: Integer;
  RValid, ArtistFound, TitleFound: Boolean;
  R: TRegExpr;
begin
  Result := False;
  RegExp := Trim(RegExp);

  for i := 0 to List.Items.Count - 1 do
    if (LowerCase(RegExp) = LowerCase(Trim(List.Items[i].Caption))) and (List.Items[i] <> Item) then
    begin
      TFunctions.MsgBox(_('The specified regular expression is already on the list.'), _('Info'), MB_ICONINFORMATION);
      Exit;
    end;

  RValid := False;
  R := TRegExpr.Create(RegExp);
  try
    try
      R.Compile;
      RValid := True;
    except
    end;
  finally
    R.Free;
  end;

  ArtistFound := (Pos('(?P<a>.*)', RegExp) > 0) or (Pos('(?P<a>.*?)', RegExp) > 0);
  TitleFound := (Pos('(?P<t>.*)', RegExp) > 0) or (Pos('(?P<t>.*?)', RegExp) > 0);

  if (RegExp = '') or (not RValid) or (not ArtistFound) or (not TitleFound) then
  begin
    TFunctions.MsgBox(_('Please supply a valid regular expression containing the groups (?P<a>.*)/(?P<a>.*?) and (?P<t>.*)/(?P<t>.*?).'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  Result := True;
end;

function GetGradientColor(const FromColor, ToColor: TColor; const Factor: Double): TColor;
var
  FromRGB, ToRGB: Integer;
  R, G, B: Byte;
begin
  FromRGB := ColorToRGB(FromColor);
  ToRGB := ColorToRGB(ToColor);

  R := Red(FromRGB) + Trunc(Factor * (Red(ToRGB) - Red(FromRGB)));
  G := Green(FromRGB) + Trunc(Factor * (Green(ToRGB) - Green(FromRGB)));
  B := Blue(FromRGB) + Trunc(Factor * (Blue(ToRGB) - Blue(FromRGB)));

  Result := RGBToColor(R, G, B);
end;

end.
