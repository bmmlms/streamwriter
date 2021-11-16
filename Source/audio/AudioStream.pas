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

{ This unit defines stream-classes where audio-data is written to.
  It contains classes for MPEG, AAC and OGG. }
unit AudioStream;

interface

uses
  Classes,
  DynBass,
  ExtendedStream,
  LanguageObjects,
  Math,
  MPEG,
  StrUtils,
  SysUtils,
  Windows;

type
  // Defines where data starts/ends
  TPosRect = record
    DataStart, DataEnd: Int64;
  end;

  // Abstract class for streams saved to disk
  TAudioStreamFile = class(TFileStream)
  public
    function GetFrame(F, T: Int64): TPosRect; virtual; abstract;
    procedure SaveToFile(const Filename: string; From, Length: Int64);
    function SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks, MinDuration: Int64): TPosRect;
  end;

  // Class for MPEG-streams saved to disk
  TMPEGStreamFile = class(TAudioStreamFile)
  public
    // Gets a frame in the MPEG-data to produce MPEG-compliant saved titles
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  // Class for AAC-streams saved to disk
  TAACStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  // Class for OGG-streams saved to disk
  TOGGStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  // Abstract class for streams saved to memory
  TAudioStreamMemory = class(TExtendedStream)
  public
    function GetFrame(F, T: Int64): TPosRect; virtual; abstract;
    procedure SaveToFile(const Filename: string; From, Length: Int64);
    function SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks, MinDuration: Int64): TPosRect;
  end;

  // Class for MPEG-streams saved to memory
  TMPEGStreamMemory = class(TAudioStreamMemory)
  public
    // Gets a frame in the MPEG-data to produce MPEG-compliant saved titles
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  // Class for AAC-streams saved to memory
  TAACStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(F, T: Int64): TPosRect; override;
  end;

  // Class for OGG-streams saved to memory
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
    Position := From;
    Stream.CopyFrom(Self, Length);
    Position := OldPos;
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

  Result.DataStart := -1;
  Result.DataEnd := -1;

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

      Position := StartPos;
      M1.CopyFrom(Self, LenStart);

      Position := EndPos;
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
        Result.DataStart := WD.WaveArray[WD.Silence[MaxLenIdx].CutEnd].Pos;
        Result.DataStart := Round(Result.DataStart * M1.Size / WD.Wavesize);
        Result.DataStart := Result.DataStart + StartPos;
      end;

      if WD2.Silence.Count > 0 then
      begin
        MaxLenIdx := 0;
        for i := 0 to WD2.Silence.Count - 1 do
          if WD2.Silence[i].CutEnd - WD2.Silence[i].CutStart > WD2.Silence[MaxLenIdx].CutEnd - WD2.Silence[MaxLenIdx].CutStart then
            MaxLenIdx := i;
        Result.DataEnd := WD2.WaveArray[WD2.Silence[MaxLenIdx].CutStart].Pos;
        Result.DataEnd := Round(Result.DataEnd * M2.Size / WD2.Wavesize);
        Result.DataEnd := Result.DataEnd + EndPos;
      end;
    except
      on E: Exception do
        raise Exception.Create(Format(_('SearchSilence() returned error: %s') + ' ', [_(E.Message)]));
    end;
  finally
    M1.Free;
    M2.Free;
    WD.Free;
    WD2.Free;
  end;

  Position := OldPos;
end;

{ TMPEGStreamFile }

function TMPEGStreamFile.GetFrame(F, T: Int64): TPosRect;
var
  i, OldPos, LastFrame: Int64;
  Frame: FrameData;
  FL: Integer;
  Buf: array[0..3] of byte;
begin
  Result.DataStart := -1;
  Result.DataEnd := -1;

  LastFrame := -1;
  OldPos := Position;

  if F < 0 then
    F := 0;
  if T > Size then
    T := Size;

  i := F;
  while i <= T - 4 do
  begin
    Position := i;
    Read(Buf, 4);

    if IsFrameHeader(Buf, 0) then
    begin
      DecodeHeader(Buf, Frame, 0);
      FL := GetFrameLength(Frame);

      if Result.DataStart = -1 then
      begin
        // Wenn das der erste gefundene ist, prüfen, ob es wirklich ein Header ist,
        // indem wir schauen, ob nach der Länge wieder ein Header kommt.

        if Size < Position + FL * 2 then
          Exit;

        Position := Position + FL - 4;
        Read(Buf, 4);
        if IsFrameHeader(Buf, 0) then
          Result.DataStart := i
        else
        begin
          Inc(i);
          Continue;
        end;
      end;

      Inc(i, FL);

      if (Result.DataEnd = -1) and (i + FL >= T) then
      begin
        if LastFrame > -1 then
          Result.DataEnd := LastFrame;
        Break;
      end;

      LastFrame := i;

      if (Result.DataStart <> -1) and (Result.DataEnd <> -1) then
        Break;
    end else
    begin
      Inc(i);

      if (Result.DataStart = -1) and (i - F > 4096) then // So lang darf ein Frame nie sein.
        raise Exception.Create('Error in audio data');
    end;
  end;
  Position := OldPos;

  if Result.DataStart = -1 then
    Result.DataStart := F;
  if Result.DataEnd = -1 then
    Result.DataEnd := T;
end;

{ TAACStreamFile }

function TAACStreamFile.GetFrame(F, T: Int64): TPosRect;
begin
  Result.DataStart := F;
  Result.DataEnd := T;
end;

{ TMPEGStreamMemory }

function TMPEGStreamMemory.GetFrame(F, T: Int64): TPosRect;
var
  i, OldPos, LastFrame: Int64;
  Frame: FrameData;
  FL: Integer;
  Buf: array[0..3] of byte;
begin
  Result.DataStart := -1;
  Result.DataEnd := -1;

  LastFrame := -1;
  OldPos := Position;

  if F < 0 then
    F := 0;
  if T > Size then
    T := Size;

  i := F;
  while i <= T - 4 do
  begin
    Position := i;
    Read(Buf, 4);

    if IsFrameHeader(Buf, 0) then
    begin
      DecodeHeader(Buf, Frame, 0);
      FL := GetFrameLength(Frame);

      if Result.DataStart = -1 then
      begin
        // Wenn das der erste gefundene ist, prüfen, ob es wirklich ein Header ist,
        // indem wir schauen, ob nach der Länge wieder ein Header kommt.

        if Size < Position + FL * 2 then
          Exit;

        Position := Position + FL - 4;
        Read(Buf, 4);
        if IsFrameHeader(Buf, 0) then
          Result.DataStart := i
        else
        begin
          Inc(i);
          Continue;
        end;
      end;

      Inc(i, FL);

      if (Result.DataEnd = -1) and (i + FL >= T) then
      begin
        if LastFrame > -1 then
          Result.DataEnd := LastFrame;
        Break;
      end;

      LastFrame := i;

      if (Result.DataStart <> -1) and (Result.DataEnd <> -1) then
        Break;
    end else
    begin
      Inc(i);

      if (Result.DataStart = -1) and (i - F > 4096) then // So lang darf ein Frame nie sein.
        raise Exception.Create('Error in audio data');
    end;
  end;
  Position := OldPos;

  if Result.DataStart = -1 then
    Result.DataStart := F;
  if Result.DataEnd = -1 then
    Result.DataEnd := T;
end;

{ TAACStreamMemory }

function TAACStreamMemory.GetFrame(F, T: Int64): TPosRect;
begin
  Result.DataStart := F;
  Result.DataEnd := T;
end;

{ TAudioStreamMemory }

procedure TAudioStreamMemory.SaveToFile(const Filename: string; From, Length: Int64);
var
  Stream: TFileStream;
  OldPos: Int64;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    OldPos := Position;
    Position := From;
    Stream.CopyFrom(Self, Length);
    Position := OldPos;
  finally
    Stream.Free;
  end;
end;

function TAudioStreamMemory.SearchSilence(StartPos, EndPos, LenStart, LenEnd, MaxPeaks, MinDuration: Int64): TPosRect;
var
  i, MaxLenIdx: Integer;
  WD, WD2: TWaveData;
  M1, M2: TExtendedStream;
  OldPos: Int64;
begin
  OldPos := Position;

  Result.DataStart := -1;
  Result.DataEnd := -1;

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

      Position := StartPos;
      M1.CopyFrom(Self, LenStart);

      Position := EndPos;
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
        Result.DataStart := WD.WaveArray[WD.Silence[MaxLenIdx].CutEnd].Pos;
        Result.DataStart := Round(Result.DataStart * M1.Size / WD.Wavesize);
        Result.DataStart := Result.DataStart + StartPos;
      end;

      if WD2.Silence.Count > 0 then
      begin
        MaxLenIdx := 0;
        for i := 0 to WD2.Silence.Count - 1 do
          if WD2.Silence[i].CutEnd - WD2.Silence[i].CutStart > WD2.Silence[MaxLenIdx].CutEnd - WD2.Silence[MaxLenIdx].CutStart then
            MaxLenIdx := i;
        Result.DataEnd := WD2.WaveArray[WD2.Silence[MaxLenIdx].CutStart].Pos;
        Result.DataEnd := Round(Result.DataEnd * M2.Size / WD2.Wavesize);
        Result.DataEnd := Result.DataEnd + EndPos;
      end;
    except
      on E: Exception do
        raise Exception.Create(Format(_('SearchSilence() returned error: %s') + ' ', [_(E.Message)]));
    end;
  finally
    M1.Free;
    M2.Free;
    WD.Free;
    WD2.Free;
  end;

  Position := OldPos;
end;

{ TOGGStreamFile }

function TOGGStreamFile.GetFrame(F, T: Int64): TPosRect;
begin
  Result.DataStart := F;
  Result.DataEnd := T;
end;

{ TOGGStreamMemory }

function TOGGStreamMemory.GetFrame(F, T: Int64): TPosRect;
begin
  Result.DataStart := F;
  Result.DataEnd := T;
end;

end.
