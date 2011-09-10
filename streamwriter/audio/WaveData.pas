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
unit WaveData;

interface

uses
  SysUtils, Windows, Classes, DynBass, Math, Generics.Collections,
  Functions, AudioStream;

type
  TWaveEntry = record
    Pos, Len: Cardinal;
    Sec: Double;
    L, R: Word;
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
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(Stream: TMemoryStream); overload;
    procedure Load(Filename: string); overload;
    function Save(Filename: string; StartPos, EndPos: Cardinal): Boolean;
    procedure AutoCut(MaxPeaks: Cardinal; MinDuration: Cardinal);
    function TimeBetween(F, T: Cardinal): Double;
    function IsInSilence(O: Cardinal): Boolean;

    property ZoomStart: Cardinal read FGetZoomStart write FSetZoomStart;
    property ZoomEnd: Cardinal read FGetZoomEnd write FSetZoomEnd;
    property ZoomSize: Cardinal read FGetZoomSize;
    property Secs: Double read FGetSecs;

    property WaveArray: TWaveEntryArray read FWaveArray write FSetWaveArray;
    //property CutStates: TList<TCutState> read FCutStates;
    property Silence: TList<TCutState> read FSilence;

    property Wavesize: Int64 read FWavesize;
    property Filesize: Int64 read FFilesize;
    property AudioStart: Cardinal read FAudioStart;
    property AudioEnd: Cardinal read FAudioEnd;
    property Progress: Cardinal read FProgress;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TWaveData }

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
  FDecoder := BASSStreamCreateFile(True, Stream.Memory, 0, Stream.Size, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN} {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});

  if FDecoder = 0 then
  begin
    raise Exception.Create('Error creating decoder');
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
  if FFilesize =  -1 then
  begin
    raise Exception.Create('Error getting filesize');
  end;

  FDecoder := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN} {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});
  if FDecoder = 0 then
  begin
    raise Exception.Create('Error creating decoder');
  end;

  try
    AnalyzeData;
  finally
    BASSStreamFree(FDecoder);
  end;
end;

function TWaveData.Save(Filename: string; StartPos, EndPos: Cardinal): Boolean;
var
  S, E: Cardinal;
  FS, StartTagBytes, EndTagBytes: Int64;
  FIn: TAudioStreamFile;
  FOut: TMemoryStream;
  P: TPosRect;
begin
  Result := False;

  try
    S := WaveArray[StartPos].Pos;
    E := WaveArray[EndPos].Pos + WaveArray[EndPos].Len;

    FS := Filesize - AudioStart - (Filesize - AudioEnd);
    StartTagBytes := FAudioStart;
    EndTagBytes := Filesize - FAudioEnd;

    StartPos := Round(S * (FS / Wavesize));
    EndPos := Round(E * (FS / Wavesize));

    StartPos := StartPos + AudioStart;
    EndPos := EndPos + AudioStart;

    FOut := TMemoryStream.Create;
    try
      if LowerCase(ExtractFileExt(Filename)) = '.mp3' then
        FIn := TMPEGStreamFile.Create(FFilename, fmOpenRead or fmShareDenyWrite)
      else
        FIn := TAACStreamFile.Create(FFilename, fmOpenRead or fmShareDenyWrite);
      try
        // Tags kopieren
        if StartTagBytes > 0 then
        begin
          FIn.Seek(0, soFromBeginning);
          FOut.CopyFrom(FIn, StartTagBytes);
        end;

        P := FIn.GetFrame(StartPos, EndPos);

        // Daten kopieren
        if (P.A > 0) and (P.B > 0) then
        begin
          FIn.Seek(P.A, soFromBeginning);
          FOut.CopyFrom(FIn, P.B - P.A);
        end else
        begin
          FIn.Seek(StartPos, soFromBeginning);
          FOut.CopyFrom(FIn, EndPos - StartPos);
        end;

        // Tags kopieren
        if EndTagBytes > 0 then
        begin
          FIn.Seek(FFilesize - EndTagBytes, soFromBeginning);
          FOut.CopyFrom(FIn, EndTagBytes);
        end;

        FreeAndNil(FIn);
        FOut.SaveToFile(Filename);
        Result := True;
      except
        FreeAndNil(FIn);
      end;
    finally
      FOut.Free;
    end;
  except
  end;
end;

procedure TWaveData.AnalyzeData;
var
  Level: DWord;
  Position: QWORD;
  Counter, OldPercent: Cardinal;
  Len: Extended;
begin
  Counter := 0;
  OldPercent := 0;

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
    FWaveArray[Counter].Pos := Position;
    if Counter > 0 then
      FWaveArray[Counter - 1].Len := FWaveArray[Counter].Pos - FWaveArray[Counter - 1].Pos;
    FWaveArray[Counter].Sec := BASSChannelBytes2Seconds(FDecoder, Position);

    Inc(Counter);
  end;

  SetLength(FWaveArray, Counter);

  // Bei weniger als 2 = Crash später
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

procedure TWaveData.AutoCut(MaxPeaks: Cardinal; MinDuration: Cardinal);
var
  i: Integer;
  Avg: Cardinal;
  MinDurationD: Double;
  SilenceStart, SilenceEnd: Cardinal;
begin
  for i := 0 to FSilence.Count - 1 do
    FSilence[i].Free;
  FSilence.Clear;

  MaxPeaks := Trunc((MaxPeaks / 100) * 6000);
  MinDurationD := MinDuration / 1000;

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

function TWaveData.FGetZoomStart: Cardinal;
begin
  Result := FZoomStart;
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
  // REMARK: Falsch wegen letztem element etc..
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
