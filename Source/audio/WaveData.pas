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

{ This unit contains stuff to work with wave-data extracted from streams/files
  by using the BASS-library }
unit WaveData;

interface

uses
  SysUtils, Windows, Classes, DynBass, Math, Generics.Collections,
  Functions, AudioStream, FileConvertor;

type
  TMinSilence = record
    A, B: Int64;
    Peak: Cardinal;
  end;
  TMinSilenceArray = array of TMinSilence;

  TWaveEntry = record
    Pos, Len: Cardinal;
    Sec: Double;
    L, R: Word;
    Avg: Cardinal;
  end;
  TWaveEntryArray = array of TWaveEntry;

  TCutState = class
  public
    CutStart, CutEnd: Cardinal;

    constructor Create(F, T: Cardinal);
  end;

  TWaveData = class
  private
    FDecoder: Cardinal;
    FWaveArray: TWaveEntryArray;
    FSilence: TList<TCutState>;

    FFilename: string;
    FWavesize, FFilesize: Int64;
    FAudioStart, FAudioEnd: Cardinal;
    FCheckSum: Cardinal;
    FProgress: Cardinal;

    FOnProgress: TNotifyEvent;

    FZoomStart, FZoomEnd: Cardinal;

    procedure AnalyzeData;

    procedure FSetWaveArray(Value: TWaveEntryArray);

    function FGetZoomStart: Cardinal;
    function FGetZoomEnd: Cardinal;
    function FGetZoomSize: Cardinal;
    function FGetSecs: Double;
    procedure FSetZoomStart(StartPos: Cardinal);
    procedure FSetZoomEnd(EndPos: Cardinal);

    function FindLowestArea(SearchFirst: Boolean; FromEntry, ToEntry: Integer): TMinSilence;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(Stream: TMemoryStream); overload;
    procedure Load(Filename: string); overload;
    function Save(OutFile: string; StartPos, EndPos: Cardinal): Boolean;
    procedure AutoCut(SearchFirst: Boolean; MaxPeaks: Integer; MinDuration: Cardinal; FromEntry, ToEntry: Integer);
    procedure ClearSilence;
    function TimeBetween(F, T: Cardinal): Double;
    function IsInSilence(O: Cardinal): Boolean;

    property ZoomStart: Cardinal read FGetZoomStart write FSetZoomStart;
    property ZoomEnd: Cardinal read FGetZoomEnd write FSetZoomEnd;
    property ZoomSize: Cardinal read FGetZoomSize;
    property Secs: Double read FGetSecs;

    property WaveArray: TWaveEntryArray read FWaveArray write FSetWaveArray;
    property Silence: TList<TCutState> read FSilence;

    property Wavesize: Int64 read FWavesize;
    property Filesize: Int64 read FFilesize;
    property AudioStart: Cardinal read FAudioStart;
    property AudioEnd: Cardinal read FAudioEnd;
    property CheckSum: Cardinal read FCheckSum;
    property Progress: Cardinal read FProgress;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TWaveData }

procedure TWaveData.ClearSilence;
var
  i: Integer;
begin
  for i := 0 to FSilence.Count - 1 do
    FSilence[i].Free;
  FSilence.Clear;
end;

constructor TWaveData.Create;
begin
  FSilence := TList<TCutState>.Create;
end;

destructor TWaveData.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSilence.Count - 1 do
    FSilence[i].Free;
  FSilence.Free;
  inherited;
end;

procedure TWaveData.Load(Stream: TMemoryStream);
begin
  FDecoder := BASSStreamCreateFile(True, Stream.Memory, 0, Stream.Size, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN});

  if FDecoder = 0 then
  begin
    raise Exception.Create('Decoder could not be created');
  end;

  try
    AnalyzeData;
  finally
    BASSStreamFree(FDecoder);
  end;
end;

procedure TWaveData.Load(Filename: string);
begin
  FFilename := Filename;
  FFilesize := GetFileSize(Filename);
  if FFilesize = -1 then
  begin
    raise Exception.Create('Filesize could not be determined');
  end;

  FDecoder := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN});
  if FDecoder = 0 then
  begin
    raise Exception.Create('Decoder could not be created');
  end;

  try
    AnalyzeData;
  finally
    BASSStreamFree(FDecoder);
  end;
end;

function TWaveData.Save(OutFile: string; StartPos, EndPos: Cardinal): Boolean;
var
  C: TFileConvertor;
begin
  C := TFileConvertor.Create;
  try
    Result := C.Convert2WAV(FFilename, OutFile, nil, FWaveArray[StartPos].Pos, FWaveArray[EndPos].Pos);
  finally
    C.Free;
  end;
end;

procedure TWaveData.AnalyzeData;
var
  Level: DWORD;
  Position: QWORD;
  Counter, OldPercent: Cardinal;
  Len: Extended;
begin
  Counter := 0;
  OldPercent := 0;
  FCheckSum := 0;
  Position := 0;

  SetLength(FWaveArray, 1000);

  Len := BASSChannelGetLength(FDecoder, BASS_POS_BYTE);

  while BASSChannelIsActive(FDecoder) = BASS_ACTIVE_PLAYING do
  begin
    if Counter >= Cardinal(Length(FWaveArray)) then
    begin
      SetLength(FWaveArray, Length(FWaveArray) + 1000);
    end;

    Position := BASSChannelGetPosition(FDecoder, BASS_POS_BYTE);

    if Len > 0 then
    begin
      FProgress := Trunc((Position / Len) * 100);
      if FProgress <> OldPercent then
      begin
        if Assigned(FOnProgress) then
          FOnProgress(Self);
      end;
      OldPercent := FProgress;
    end;

    Level := BASSChannelGetLevel(FDecoder);
    FWaveArray[Counter].L := LOWORD(Level);
    FWaveArray[Counter].R := HIWORD(Level);
    FWaveArray[Counter].Avg := (LOWORD(Level) + HIWORD(Level)) div 2;
    FWaveArray[Counter].Pos := Position;

    FCheckSum := FCheckSum + FWaveArray[Counter].Avg;

    if Counter > 0 then
      FWaveArray[Counter - 1].Len := FWaveArray[Counter].Pos - FWaveArray[Counter - 1].Pos;
    FWaveArray[Counter].Sec := BASSChannelBytes2Seconds(FDecoder, Position);

    Inc(Counter);
  end;

  // Das ist wichtig - wenn man Stille hinzuf�gt, �ndert sich oben Avg nicht, Checksum bleibt gleich..
  FCheckSum := FCheckSum + Position;

  SetLength(FWaveArray, Counter);

  // Bei weniger als 2 = Crash sp�ter
  if Counter < 10 then
  begin
    raise Exception.Create('WaveArray is too short');
  end;

  FWaveArray[High(FWaveArray)].Len := BASSChannelGetLength(FDecoder, BASS_POS_BYTE) - FWaveArray[High(FWaveArray)].Pos;
  FWavesize := FWaveArray[High(FWaveArray)].Pos + FWaveArray[High(FWaveArray)].Len;

  FAudioStart := BASSStreamGetFilePosition(FDecoder, BASS_FILEPOS_START);
  FAudioEnd := BASSStreamGetFilePosition(FDecoder, BASS_FILEPOS_END);

  FZoomStart := 0;
  FZoomEnd := High(FWaveArray);
end;

procedure TWaveData.AutoCut(SearchFirst: Boolean; MaxPeaks: Integer; MinDuration: Cardinal; FromEntry, ToEntry: Integer);
var
  i: Integer;
  Avg: Cardinal;
  MinDurationD: Double;
  SilenceStart, SilenceEnd: Cardinal;
begin
  if MaxPeaks = -1 then
  begin
    FindLowestArea(SearchFirst, FromEntry, ToEntry);
  end else
  begin
    MinDurationD := MinDuration / 1000;
    MaxPeaks := Trunc((MaxPeaks / 100) * 6000);
    SilenceStart := 0;
    SilenceEnd := 0;
    for i := 0 to High(FWaveArray) do
    begin
      Avg := (FWaveArray[i].L + FWaveArray[i].R) div 2;

      if Avg < MaxPeaks then
      begin
        if SilenceStart = 0 then
        begin
          SilenceStart := i;
        end;
      end else
      begin
        if SilenceStart > 0 then
        begin
          SilenceEnd := i;

          if TimeBetween(SilenceStart, SilenceEnd) >= MinDurationD then
          begin
            FSilence.Add(TCutState.Create(SilenceStart, SilenceEnd));
          end else
          begin
          end;

          SilenceStart := 0;
          SilenceEnd := 0;
        end else
        begin

        end;
      end;
    end;

    if (SilenceStart > 0) and (SilenceEnd = 0) then
      FSilence.Add(TCutState.Create(SilenceStart, High(FWaveArray)));
  end;
end;

function TWaveData.FGetZoomStart: Cardinal;
begin
  Result := FZoomStart;
end;

function TWaveData.FindLowestArea(SearchFirst: Boolean; FromEntry, ToEntry: Integer): TMinSilence;
var
  i, n, Ret: Integer;
  MinSilence: TMinSilenceArray;
  MS: TMinSilence;
  EntryCount, MaxPeaks, Avg: Cardinal;
  CmpVal: Int64;
begin
  Result.A := -1;
  Result.B := -1;

  EntryCount := 0;
  for i := 0 to High(WaveArray) do
    if WaveArray[i].Sec * 1000 >= 100 then // 100ms absuchen
    begin
      EntryCount := i;
      Break;
    end;

  if EntryCount = 0 then
    Exit;

  MaxPeaks := High(Cardinal);
  SetLength(MinSilence, 0);
  for i := FromEntry to ToEntry - EntryCount do
  begin
    Avg := 0;
    for n := i to i + EntryCount do
      Avg := Avg + ((WaveArray[n].L + WaveArray[n].R) div 2);

    Avg := Avg div EntryCount;

    // Stille ist es nur, wenn unter 5000
    if Avg < 3000 then
    begin
      MS.A := i;
      MS.B := i + EntryCount;
      MS.Peak := Avg;
      SetLength(MinSilence, Length(MinSilence) + 1);
      MinSilence[High(MinSilence)] := MS;
    end;
  end;

  Ret := -1;

  // Die passende Stille aus dem Array suchen, je nachdem, ob wir von
  // vorne, oder von hinten Suchen sollen
  if SearchFirst then
  begin
    for i := 0 to High(MinSilence) do
      if MinSilence[i].Peak < MaxPeaks then
      begin
        Ret := i;
        Result.A := MinSilence[i].A;
        Result.B := MinSilence[i].B;
        MaxPeaks := MinSilence[i].Peak;
      end;
  end else
  begin
    for i := High(MinSilence) downto 0 do
      if MinSilence[i].Peak < MaxPeaks then
      begin
        Ret := i;
        Result.A := MinSilence[i].A;
        Result.B := MinSilence[i].B;
        MaxPeaks := MinSilence[i].Peak;
      end;
  end;

  // Jetzt die gefundene Stille noch breiter ziehen, wenn die Bereiche
  // davor/danach auch leise sind
  if Ret > -1 then
  begin
    for i := MinSilence[Ret].A downto FromEntry do
    begin
      CmpVal := MinSilence[Ret].Peak;
      if CmpVal < 1 then
        CmpVal := 200;
      CmpVal := Trunc(CmpVal + (CmpVal / 2));
      if FWaveArray[i].Avg > CmpVal then
      begin
        Result.A := i;
        Break;
      end;
    end;

    for i := MinSilence[Ret].B to ToEntry do
    begin
      CmpVal := MinSilence[Ret].Peak;
      if CmpVal < 1 then
        CmpVal := 200;
      CmpVal := Trunc(CmpVal + (CmpVal / 2));
      if FWaveArray[i].Avg > CmpVal then
      begin
        Result.B := i;
        Break;
      end;
    end;

    FSilence.Add(TCutState.Create(Result.A, Result.B));
  end;
end;

function TWaveData.FGetZoomEnd: Cardinal;
begin
  Result := FZoomEnd;
end;

function TWaveData.FGetZoomSize: Cardinal;
begin
  Result := ZoomEnd - ZoomStart;
end;

procedure TWaveData.FSetZoomStart(StartPos: Cardinal);
begin
  if StartPos <> ZoomStart then
    FZoomStart := StartPos;
end;

procedure TWaveData.FSetZoomEnd(EndPos: Cardinal);
begin
  if EndPos <> ZoomEnd then
    FZoomEnd := EndPos;
end;

function TWaveData.FGetSecs: Double;
begin
  Result := FWaveArray[High(FWaveArray)].Sec;
end;

procedure TWaveData.FSetWaveArray(Value: TWaveEntryArray);
begin
  FWaveArray := Value;
end;

function TWaveData.IsInSilence(O: Cardinal): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FSilence.Count - 1 do
  begin
    if (O >= FSilence[i].CutStart) and (O <= FSilence[i].CutEnd) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TWaveData.TimeBetween(F, T: Cardinal): Double;
begin
  Result := Max(FWaveArray[F].Sec, FWaveArray[T].Sec) - Min(FWaveArray[F].Sec, FWaveArray[T].Sec);
end;

{ TCutState }

constructor TCutState.Create(F, T: Cardinal);
begin
  inherited Create;

  CutStart := F;
  CutEnd := T;
end;

end.
