{
    ------------------------------------------------------------------------
    streamWriter
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
unit AudioStream;

interface

uses
  Windows, SysUtils, StrUtils, Classes, ExtendedStream, MPEG, DynBASS;

type
  TPosRect = record
    A, B: UInt64;
  end;

  TPosArray = array of TPosRect;

  TAudioStreamFile = class(TFileStream)
  protected
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Int64; virtual; abstract;
    procedure SaveToFile(const Filename: string; From, Length: Int64);
  end;

  TMPEGStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(From: Int64; SearchBackwards: Boolean): Int64; override;
  end;

  TAACStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(From: Int64; SearchBackwards: Boolean): Int64; override;
  end;

  TAudioStreamMemory = class(TExtendedStream)
  private
  protected
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Int64; virtual; abstract;
    function GetPossibleTitle(ByteCount: UInt64): TPosRect; virtual; abstract;
  end;

  TMPEGStreamMemory = class(TAudioStreamMemory)
  private
    function FindSilence(From, Count: UInt64): TPosArray;
  public
    function GetFrame(From: Int64; SearchBackwards: Boolean): Int64; override;
    function GetPossibleTitle(ByteCount: UInt64): TPosRect; override;
  end;

  TAACStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(From: Int64; SearchBackwards: Boolean): Int64; override;
    function GetPossibleTitle(ByteCount: UInt64): TPosRect; override;
  end;

implementation

{ TAudioStream }

procedure TAudioStreamFile.SaveToFile(const Filename: string; From, Length: Int64);
var
  Stream: TFileStream;
  OldPos: Int64;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    OldPos := Position;
    Seek(From, soFromBeginning);
    Stream.CopyFrom(Self, Length);
    Seek(OldPos, soFromBeginning);
  finally
    Stream.Free;
  end;
end;

{ TMPEGStreamFile }

// TODO: Prüfen ob für GetFrame() bei searchbackwards immer ein FROM angegeben wird!!! wirds nämlich nicht!
function TMPEGStreamFile.GetFrame(From: Int64; SearchBackwards: Boolean): Int64;
var
  i, OldPos: Int64;
  Frame: FrameData;
  Len: Integer;
  Buf: array[0..3] of byte;
begin
  OldPos := Position;
  Result := -1;

  if SearchBackwards then
  begin
    i := From - 4;
    while i >= 0 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Len := GetFrameLength(Frame);

        if i + Len <= Size then
        begin
          Result := i + Len;
          Break;
        end;
      end;
      Dec(i);
    end;
  end else
  begin
    i := From;
    while i <= Size - 4 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Result := i;
        Break;
      end;
      Inc(i);
    end;
  end;
  Position := OldPos;
end;

{ TAACStreamFile }

function TAACStreamFile.GetFrame(From: Int64; SearchBackwards: Boolean): Int64;
begin
  if SearchBackwards then
    Result := Size - 1
  else
    Result := From;
end;

{ TMPEGStreamMemory }

function TMPEGStreamMemory.GetFrame(From: Int64; SearchBackwards: Boolean): Int64;
var
  i, OldPos: Int64;
  Frame: FrameData;
  Len: Integer;
  Buf: array[0..3] of byte;
begin
  OldPos := Position;
  Result := -1;
                      // TODO: SearchBAckwards muss anders behandelt werden, wegen SilenceSearch.
  if SearchBackwards then
  begin
    i := From - 4;
    while i >= 0 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Len := GetFrameLength(Frame);

        if i + Len <= Size then
        begin
          Result := i + Len;
          Break;
        end;
      end;
      Dec(i);
    end;
  end else
  begin
    i := From;
    while i <= Size - 4 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Result := i;
        Break;
      end;
      Inc(i);
    end;
  end;
  Position := OldPos;
end;

function TMPEGStreamMemory.FindSilence(From, Count: UInt64): TPosArray;
  procedure AddRes(A, B: UInt64; var Arr: TPosArray);
  begin
    SetLength(Arr, Length(Arr) + 1);
    Arr[High(Arr)].A := A;
    Arr[High(Arr)].B := B;
  end;
var
  Chan, Level: DWORD;
  P, SBegin, SEnd: Int64;
  S: TFileStream;
begin
  SBegin := 0;
  SEnd := 0;

  //SBegin := Size;

  SetLength(Result, 0);

  // TODO: Fehlerbehandlung?
  Chan := BASSStreamCreateFile(True, Pointer(Integer(Memory) + From), 0, Count - From, BASS_STREAM_DECODE);
  //Chan := BASSStreamCreateFile(True, Memory, 0, Count, BASS_STREAM_DECODE);

	while (BASSChannelIsActive(Chan) > 0) do
  begin
    //P := BASS_ChannelGetPosition(Chan, BASS_POS_BYTE or BASS_POS_DECODE) + From;
    P := BASSStreamGetFilePosition(Chan, BASS_FILEPOS_CURRENT) + From;

    //sec := BASS_ChannelBytes2Seconds(Chan, P);

    Level := BASSChannelGetLevel(Chan);  // TODO: guck mal in die doku zu getlevel :)

    // 3000000 war fürn file okay
    if Level < 3000000 then
    begin
      if SBegin = 0 then
        SBegin := P
      else
      begin

      end;
    end else
    begin
      if (SBegin > 0) and (SEnd = 0) then
      begin
        SEnd := P
      end;

      if (SBegin > 0) and (SEnd > 0) then
      begin
        AddRes(SBegin, SEnd, Result);

        SBegin := 0;
        SEnd := 0;
      end;

      SBegin := 0;
      SEnd := 0;
    end;
	end;

  BASSStreamFree(Chan);
end;

function TMPEGStreamMemory.GetPossibleTitle(ByteCount: UInt64): TPosRect;
var
  i: Integer;
  SilenceBegin, SilenceEnd: TPosArray;
begin
  Result.A := 0;
  Result.B := 0;

  SilenceBegin := FindSilence(0, ByteCount);
  SilenceEnd := FindSilence(Size - ByteCount, ByteCount);

  if Length(SilenceBegin) > 0 then
    Result.A := SilenceBegin[High(SilenceBegin)].B;

  //if Result.A > 0 then
  //begin
    if Length(SilenceEnd) > 0 then
      Result.B := SilenceEnd[Low(SilenceEnd)].A
    else
      Result.B := Size;
  //end;
end;

{ TAACStreamMemory }

function TAACStreamMemory.GetFrame(From: Int64; SearchBackwards: Boolean): Int64;
begin
  // TODO: ?! rückgabewerte passen???
  if SearchBackwards then
    Result := Size - 1
  else
    Result := From;
end;

function TAACStreamMemory.GetPossibleTitle(ByteCount: UInt64): TPosRect;
begin
  Result.A := 0;
  Result.B := 0;
end;

end.
