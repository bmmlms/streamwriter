{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
  Windows, SysUtils, StrUtils, Classes, ExtendedStream, MPEG, DynBass,
  Math;

type
  TPosRect = record
    A, B: Int64;
  end;

  TPosArray = array of TPosRect;

  TAudioStreamFile = class(TFileStream)
  public
    function GetFrame(F, T: Int64): TPosRect; virtual; abstract;
    procedure SaveToFile(const Filename: string; From, Length: Int64);
    function SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks, MinDuration: Int64): TPosRect;
  end;

  TMPEGStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  TAACStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  TOGGStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  TAudioStreamMemory = class(TExtendedStream)
  public
    function GetFrame(F, T: Int64): TPosRect; virtual; abstract;
    procedure SaveToFile(const Filename: string; From, Length: Int64);
    function SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks, MinDuration: Int64): TPosRect;
  end;

  TMPEGStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  TAACStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  TOGGStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

implementation

uses
  WaveData;

{ TAudioStreamFile }

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

function TAudioStreamFile.SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks, MinDuration: Int64): TPosRect;
var
  i, MaxLenIdx: Integer;
  WD, WD2: TWaveData;
  M1, M2: TExtendedStream;
  OldPos: Int64;
begin
  OldPos := Position;

  Result.A := -1;
  Result.B := -1;

  M1 := TExtendedStream.Create;
  M2 := TExtendedStream.Create;
  WD := TWaveData.Create;
  WD2 := TWaveData.Create;
  try
    try
      // Um die Hälfte zurückgehen, damit wir dann alles einlesen können
      StartPos := StartPos - LenStart div 2;
      EndPos := EndPos - LenEnd div 2;

      if StartPos < 0 then
        StartPos := 0;
      if StartPos + LenStart > Size then
        LenStart := Size - StartPos;

      if EndPos < 0 then
        EndPos := 0;
      if EndPos + LenEnd > Size then
        LenEnd := Size - EndPos;

      Seek(StartPos, soFromBeginning);
      M1.CopyFrom(Self, LenStart);

      Seek(EndPos, soFromBeginning);
      M2.CopyFrom(Self, LenEnd);

      WD.Load(M1);
      WD2.Load(M2);

      if MaxPeaks > -1 then
        MaxPeaks := Trunc((MaxPeaks / 100) * 6000);

      WD.AutoCut(False, MaxPeaks, MinDuration, 0, High(WD.WaveArray));
      WD2.AutoCut(True, MaxPeaks, MinDuration, 0, High(WD.WaveArray));

      if WD.Silence.Count > 0 then
      begin
        MaxLenIdx := 0;
        for i := 0 to WD.Silence.Count - 1 do
          if WD.Silence[i].CutEnd - WD.Silence[i].CutStart > WD.Silence[MaxLenIdx].CutEnd - WD.Silence[MaxLenIdx].CutStart then
            MaxLenIdx := i;
        Result.A := WD.WaveArray[WD.Silence[MaxLenIdx].CutEnd].Pos;
        Result.A := Round(Result.A * M1.Size / WD.Wavesize);
        Result.A := Result.A + StartPos;
      end;

      if WD2.Silence.Count > 0 then
      begin
        MaxLenIdx := 0;
        for i := 0 to WD2.Silence.Count - 1 do
          if WD2.Silence[i].CutEnd - WD2.Silence[i].CutStart > WD2.Silence[MaxLenIdx].CutEnd - WD2.Silence[MaxLenIdx].CutStart then
            MaxLenIdx := i;
        Result.B := WD2.WaveArray[WD2.Silence[MaxLenIdx].CutStart].Pos;
        Result.B := Round(Result.B * M2.Size / WD2.Wavesize);
        Result.B := Result.B + EndPos;
      end;
    except
      on E: Exception do
        raise Exception.Create('Error in SearchSilence(): ' + E.Message);
    end;
  finally
    M1.Free;
    M2.Free;
    WD.Free;
    WD2.Free;
  end;

  Seek(OldPos, soFromBeginning);
end;

{ TMPEGStreamFile }

function TMPEGStreamFile.GetFrame(F, T: Int64): TPosRect;
var
  i, OldPos: Int64;
  Frame: FrameData;
  FL, LastFrame: Integer;
  Buf: array[0..3] of byte;
begin
  Result.A := -1;
  Result.B := -1;

  LastFrame := -1;
  OldPos := Position;

  i := F;
  while i <= T - 4 do
  begin
    Position := i;
    Read(Buf, 4);

    if IsFrameHeader(Buf, 0) then
    begin
      DecodeHeader(Buf, Frame, 0);
      FL := GetFrameLength(Frame);

      if Result.A = -1 then
      begin
        // Wenn das der erste gefundene ist, prüfen, ob es wirklich ein Header ist,
        // indem wir schauen, ob nach der Länge wieder ein Header kommt.

        if Size < Position + FL * 2 then
          Exit;

        Seek(FL - 4, soFromCurrent);
        Read(Buf, 4);
        if IsFrameHeader(Buf, 0) then
        begin
          Result.A := i;
        end else
        begin
          Inc(i);
          Continue;
        end;
      end;

      Inc(i, FL);

      if (Result.B = -1) and (i + FL >= T) then
      begin
        if LastFrame > -1 then
          Result.B := LastFrame;
        Break;
      end;

      LastFrame := i;

      if (Result.A <> -1) and (Result.B <> -1) then
        Break;
    end else
      Inc(i);
  end;
  Position := OldPos;

  if Result.A = -1 then
    Result.A := F;
  if Result.B = -1 then
    Result.B := T;
end;

{ TAACStreamFile }

function TAACStreamFile.GetFrame(F, T: Int64): TPosRect;
begin
  Result.A := F;
  Result.B := T;
end;

{ TMPEGStreamMemory }

function TMPEGStreamMemory.GetFrame(F, T: Int64): TPosRect;
var
  i, OldPos: Int64;
  Frame: FrameData;
  FL, LastFrame: Integer;
  Buf: array[0..3] of byte;
begin
  Result.A := -1;
  Result.B := -1;

  LastFrame := -1;
  OldPos := Position;

  i := F;
  while i <= T - 4 do
  begin
    Position := i;
    Read(Buf, 4);

    if IsFrameHeader(Buf, 0) then
    begin
      DecodeHeader(Buf, Frame, 0);
      FL := GetFrameLength(Frame);

      if Result.A = -1 then
      begin
        // Wenn das der erste gefundene ist, prüfen, ob es wirklich ein Header ist,
        // indem wir schauen, ob nach der Länge wieder ein Header kommt.

        if Size < Position + FL * 2 then
          Exit;

        Seek(FL - 4, soFromCurrent);
        Read(Buf, 4);
        if IsFrameHeader(Buf, 0) then
        begin
          Result.A := i;
        end else
        begin
          Inc(i);
          Continue;
        end;
      end;

      Inc(i, FL);

      if (Result.B = -1) and (i + FL >= T) then
      begin
        if LastFrame > -1 then
          Result.B := LastFrame;
        Break;
      end;

      LastFrame := i;

      if (Result.A <> -1) and (Result.B <> -1) then
        Break;
    end else
      Inc(i);
  end;
  Position := OldPos;

  if Result.A = -1 then
    Result.A := F;
  if Result.B = -1 then
    Result.B := T;
end;

{ TAACStreamMemory }

function TAACStreamMemory.GetFrame(F, T: Int64): TPosRect;
begin
  Result.A := F;
  Result.B := T;
end;

{ TAudioStreamMemory }

procedure TAudioStreamMemory.SaveToFile(const Filename: string; From,
  Length: Int64);
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

function TAudioStreamMemory.SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks,
  MinDuration: Int64): TPosRect;
var
  i, MaxLenIdx: Integer;
  WD, WD2: TWaveData;
  M1, M2: TExtendedStream;
  OldPos: Int64;
begin
  OldPos := Position;

  Result.A := -1;
  Result.B := -1;

  M1 := TExtendedStream.Create;
  M2 := TExtendedStream.Create;
  WD := TWaveData.Create;
  WD2 := TWaveData.Create;
  try
    try
      // Um die Hälfte zurückgehen, damit wir dann alles einlesen können
      StartPos := StartPos - LenStart div 2;
      EndPos := EndPos - LenEnd div 2;

      if StartPos < 0 then
        StartPos := 0;
      if StartPos + LenStart > Size then
        LenStart := Size - StartPos;

      if EndPos < 0 then
        EndPos := 0;
      if EndPos + LenEnd > Size then
        LenEnd := Size - EndPos;

      Seek(StartPos, soFromBeginning);
      M1.CopyFrom(Self, LenStart);

      Seek(EndPos, soFromBeginning);
      M2.CopyFrom(Self, LenEnd);

      WD.Load(M1);
      WD2.Load(M2);

      if MaxPeaks > -1 then
        MaxPeaks := Trunc((MaxPeaks / 100) * 6000);

      WD.AutoCut(False, MaxPeaks, MinDuration, 0, High(WD.WaveArray));
      WD2.AutoCut(True, MaxPeaks, MinDuration, 0, High(WD.WaveArray));

      if WD.Silence.Count > 0 then
      begin
        MaxLenIdx := 0;
        for i := 0 to WD.Silence.Count - 1 do
          if WD.Silence[i].CutEnd - WD.Silence[i].CutStart > WD.Silence[MaxLenIdx].CutEnd - WD.Silence[MaxLenIdx].CutStart then
            MaxLenIdx := i;
        Result.A := WD.WaveArray[WD.Silence[MaxLenIdx].CutEnd].Pos;
        Result.A := Round(Result.A * M1.Size / WD.Wavesize);
        Result.A := Result.A + StartPos;
      end;

      if WD2.Silence.Count > 0 then
      begin
        MaxLenIdx := 0;
        for i := 0 to WD2.Silence.Count - 1 do
          if WD2.Silence[i].CutEnd - WD2.Silence[i].CutStart > WD2.Silence[MaxLenIdx].CutEnd - WD2.Silence[MaxLenIdx].CutStart then
            MaxLenIdx := i;
        Result.B := WD2.WaveArray[WD2.Silence[MaxLenIdx].CutStart].Pos;
        Result.B := Round(Result.B * M2.Size / WD2.Wavesize);
        Result.B := Result.B + EndPos;
      end;
    except
      on E: Exception do
        raise Exception.Create('Error in SearchSilence(): ' + E.Message);
    end;
  finally
    M1.Free;
    M2.Free;
    WD.Free;
    WD2.Free;
  end;

  Seek(OldPos, soFromBeginning);
end;

{ TOGGStreamFile }

function TOGGStreamFile.GetFrame(F, T: Int64): TPosRect;
begin
  Result.A := F;
  Result.B := T;
end;

{ TOGGStreamMemory }

function TOGGStreamMemory.GetFrame(F, T: Int64): TPosRect;
begin
  Result.A := F;
  Result.B := T;
end;

end.
