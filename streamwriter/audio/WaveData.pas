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
unit WaveData;

interface

uses
  SysUtils, Windows, Classes, Functions, DynBASS, Math,
  Generics.Collections;

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
    FCutStates: TList<TCutState>;
    FSilence: TList<TCutState>;

    procedure FSetWaveArray(Value: TWaveEntryArray);

    //function FGetFileScanned: Boolean;
    function FGetCutStart: Cardinal;
    function FGetCutEnd: Cardinal;
    function FGetCutSize: Cardinal;
    function FGetSecs: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFile(Filename: string);
    procedure Cut(F, T: Cardinal);
    procedure AutoCut(MaxPeaks: Cardinal; MinDuration: Cardinal);
    function TimeBetween(F, T: Cardinal): Double;
    function IsInSilence(O: Cardinal): Boolean;

    //property FileScanned: Boolean read FGetFileScanned;
    property CutStart: Cardinal read FGetCutStart;
    property CutEnd: Cardinal read FGetCutEnd;
    property CutSize: Cardinal read FGetCutSize;
    property Secs: Double read FGetSecs;

    property WaveArray: TWaveEntryArray read FWaveArray write FSetWaveArray;
    property CutStates: TList<TCutState> read FCutStates;
    property Silence: TList<TCutState> read FSilence;
  end;

implementation

{ TWaveData }

constructor TWaveData.Create;
begin
  FCutStates := TList<TCutState>.Create;
  FSilence := TList<TCutState>.Create;
end;

destructor TWaveData.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCutStates.Count - 1 do
    FCutStates[i].Free;
  for i := 0 to FSilence.Count - 1 do
    FSilence[i].Free;
  FCutStates.Free;
  FSilence.Free;
  inherited;
end;

procedure TWaveData.LoadFile(Filename: string);
var
  i: Integer;
  Level: DWord;
  PeakL, PeakR: DWord;
  Position: QWORD;
  Counter, c2: Cardinal;
begin
  Counter := 0;

  SetLength(FWaveArray, 500);

  // TODO: Errorhandling...
  FDecoder := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN} {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});

  while BASSChannelIsActive(FDecoder) = BASS_ACTIVE_PLAYING do
  begin
    if Counter >= Length(FWaveArray) then
    begin
      SetLength(FWaveArray, Length(FWaveArray) + 500);
    end;

    Position := BASSChannelGetPosition(FDecoder, BASS_POS_BYTE);

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

  FWaveArray[High(FWaveArray)].Len := BASSChannelGetLength(FDecoder, BASS_POS_BYTE) - FWaveArray[High(FWaveArray)].Pos;

  BASSStreamFree(FDecoder);


  for i := 0 to FCutStates.Count - 1 do
    FCutStates[i].Free;
  FCutStates.Clear;
  FCutStates.Add(TCutState.Create(0, High(FWaveArray)));
end;

procedure TWaveData.Cut(F, T: Cardinal);
begin
  FCutStates.Add(TCutState.Create(F, T));
end;

procedure TWaveData.AutoCut(MaxPeaks: Cardinal; MinDuration: Cardinal);
var
  i: Integer;
  Avg: Cardinal;
  MinDurationD: Double;
  SilenceStart, SilenceEnd: Cardinal;
  CS: TCutState;
begin
  for i := 0 to FSilence.Count - 1 do
    FSilence[i].Free;
  FSilence.Clear;

  MinDurationD := MinDuration / 1000;

  SilenceStart := 0;
  SilenceEnd := 0;
  for i := 0 to High(FWaveArray) do
  begin
    if (FWaveArray[i].L = 0) or (FWaveArray[i].R = 0) then
      Continue;

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
end;

function TWaveData.FGetCutEnd: Cardinal;
begin
  Result := High(FWaveArray);
  if FCutStates.Count > 0 then
    Result := FCutStates[FCutStates.Count - 1].CutEnd;
end;

function TWaveData.FGetCutSize: Cardinal;
begin
  Result := CutEnd - CutStart;
end;

function TWaveData.FGetCutStart: Cardinal;
begin
  Result := 0;
  if FCutStates.Count > 0 then
    Result := FCutStates[FCutStates.Count - 1].CutStart;
end;

{
function TWaveData.FGetFileScanned: Boolean;
begin
  Result := FCutStates.Count > 0;
end;
}

function TWaveData.FGetSecs: Double;
var
  i: Cardinal;
begin
  // TODO: Weil das nur die Start-Ende - Sekunden sind ist das nicht die echte Länge.
  Result := FWaveArray[CutEnd].Sec - FWaveArray[CutStart].Sec;
end;

procedure TWaveData.FSetWaveArray(Value: TWaveEntryArray);
var
  i: Integer;
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
  Result := FWaveArray[T].Sec - FWaveArray[F].Sec; // TODO: falsch wegen letztem element etc..
end;

{ TCutState }

constructor TCutState.Create(F, T: Cardinal);
begin
  inherited Create;

  CutStart := F;
  CutEnd := T;
end;

end.