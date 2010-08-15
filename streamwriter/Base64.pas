{
/**********************************************************\
|                                                          |
| The implementation of PHPRPC Protocol 3.0                |
|                                                          |
| Base64.pas                                               |
|                                                          |
| Release 3.0.0                                            |
| Copyright (c) 2005-2007 by Team-PHPRPC                   |
|                                                          |
| WebSite:  http://www.phprpc.org/                         |
|           http://www.phprpc.net/                         |
|           http://www.phprpc.com/                         |
|           http://sourceforge.net/projects/php-rpc/       |
|                                                          |
| Authors:  Ma Bingyao <andot@ujn.edu.cn>                  |
|                                                          |
| This file may be distributed and/or modified under the   |
| terms of the GNU Lesser General Public License (LGPL)    |
| version 3.0 as published by the Free Software Foundation |
| and appearing in the included file LICENSE.              |
|                                                          |
\**********************************************************/

/* Base64 library.
 *
 * Copyright (C) 2006-2007 Ma Bingyao <andot@ujn.edu.cn>
 * Version: 3.0.0
 * LastModified: Dec 10, 2007
 * This library is free.  You can redistribute it and/or modify it.
 */
}

unit Base64;

interface

uses
  SysUtils;

function EncodeU(const data:UnicodeString):AnsiString;
function Encode(const data:AnsiString):AnsiString;
function Decode(const data:AnsiString):AnsiString;

implementation

const

  base64EncodeChars: array[0..63] of AnsiChar = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/' );

  base64DecodeChars: array[#0..#127] of SmallInt = (
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1,
    -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1,
    -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1 );

function EncodeU(const data:UnicodeString):AnsiString;
var
  u: RawByteString;
begin
  u := UTF8Encode(data);
  Result := Encode(u);
end;

function Encode(const data:AnsiString):AnsiString;
var
  r, len, i, j, l : Longint;
  c : LongWord;
begin
  len := Length(data);
  r := len mod 3;
  dec(len, r);
  l := (len div 3) * 4;
  if (r > 0) then inc(l, 4);
  SetLength(result, l);
  i := 1;
  j := 1;
  while (i <= len) do begin
    c := ord(data[i]);
    inc(i);
    c := (c shl 8) or ord(data[i]);
    inc(i);
    c := (c shl 8) or ord(data[i]);
    inc(i);
    result[j] := base64EncodeChars[c shr 18];
    inc(j);
    result[j] := base64EncodeChars[(c shr 12) and $3f];
    inc(j);
    result[j] := base64EncodeChars[(c shr 6) and $3f];
    inc(j);
    result[j] := base64EncodeChars[c and $3f];
    inc(j);
  end;
  if (r = 1) then begin
    c := ord(data[i]);
    result[j] := base64EncodeChars[c shr 2];
    inc(j);
    result[j] := base64EncodeChars[(c and $03) shl 4];
    inc(j);
    result[j] := '=';
    inc(j);
    result[j] := '=';
  end
  else if (r = 2) then begin
    c := ord(data[i]);
    inc(i);
    c := (c shl 8) or ord(data[i]);
    result[j] := base64EncodeChars[c shr 10];
    inc(j);
    result[j] := base64EncodeChars[(c shr 4) and $3f];
    inc(j);
    result[j] := base64EncodeChars[(c and $0f) shl 2];
    inc(j);
    result[j] := '=';
  end;
end;

{$WARN COMPARING_SIGNED_UNSIGNED OFF}
{$WARN COMPARISON_FALSE OFF}
{$WARN COMPARISON_TRUE OFF}
function Decode(const data:AnsiString):AnsiString;
var
  r, len, i, j, l : Longint;
  b1, b2, b3, b4: Byte;
begin
  len := Length(data);
  if (len mod 4 > 0) then exit;
  r := 0;
  if (data[len - 1] = '=') then r := 1 else if (data[len] = '=') then r := 2;
  l := len;
  if (r > 0) then dec(l, 4);
  l := (l div 4) * 3;
  inc(l, r);
  SetLength(result, l);
  i := 1;
  j := 1;
  while (i <= len) do begin
    repeat
      b1 := base64DecodeChars[data[i]];
      inc(i);
    until ((i > len) or (b1 <> -1));
    if (b1 = -1) then break;

    repeat
      b2 := base64DecodeChars[data[i]];
      inc(i);
    until ((i > len) or (b2 <> -1));
    if (b2 = -1) then break;

    result[j] := ansichar((b1 shl 2) or ((b2 and $30) shr 4));
    inc(j);

    repeat
      if (data[i] = '=') then exit;
      b3 := base64DecodeChars[data[i]];
      inc(i);
    until ((i > len) or (b3 <> -1));
    if (b3 = -1) then break;

    result[j] := ansichar(((b2 and $0f) shl 4) or ((b3 and $3c) shr 2));
    inc(j);

    repeat
      if (data[i] = '=') then exit;
      b4 := base64DecodeChars[data[i]];
      inc(i);
    until ((i > len) or (b4 <> -1));
    if (b4 = -1) then break;
    result[j] := ansichar(((b3 and $03) shl 6) or b4);
    inc(j);
  end;
end;
{$WARN COMPARING_SIGNED_UNSIGNED ON}
{$WARN COMPARISON_FALSE ON}
{$WARN COMPARISON_TRUE ON}

end.
