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

unit MonitorAnalyzer;

interface

uses
  Classes,
  DynBASS,
  ExtendedStream,
  Generics.Collections,
  HomeCommunication,
  SysUtils,
  Windows;

type
  TMonitorAnalyzer = class
  private
    FActive: Boolean;
    FStream: TExtendedStream;
    FTitleChanges: TList<UInt64>;
    FPlayer: Cardinal;
    FWaveDataStream: TExtendedStream;

    FOnAnalyzed: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Append(Source: TExtendedStream; Count: Integer);
    procedure TitleChanged;

    property Active: Boolean read FActive;

    property WaveDataStream: TExtendedStream read FWaveDataStream write FWaveDataStream;
    property OnAnalyzed: TNotifyEvent read FOnAnalyzed write FOnAnalyzed;
  end;

//procedure Test;

implementation

{ TMonitorAnalyzer }

procedure BASSClose(user: Pointer); stdcall;
begin

end;

function BASSLen(user: Pointer): QWORD; stdcall;
begin
  Result := 0;
end;

function BASSRead(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
var
  Mem: TExtendedStream;
  CopyLen: Cardinal;
begin
  Result := 0;

  Mem := TMonitorAnalyzer(user).FStream;

  if Mem.Size = 0 then
    Exit;

  CopyLen := Mem.Size;
  if Length < CopyLen then
    CopyLen := Length;

  CopyMemory(Buffer, Mem.Memory, CopyLen);
  Result := CopyLen;

  Mem.RemoveRange(0, CopyLen);
end;

function BASSSeek(offset: QWORD; user: Pointer): BOOL; stdcall;
begin
  Result := BOOL(0);
end;

constructor TMonitorAnalyzer.Create;
begin
  FActive := True;
  FStream := TExtendedStream.Create;
  FTitleChanges := TList<UInt64>.Create;
  FWaveDataStream := TExtendedStream.Create;
end;

destructor TMonitorAnalyzer.Destroy;
begin
  FStream.Free;
  FTitleChanges.Free;

  if FPlayer > 0 then
    BASSStreamFree(FPlayer);
  FWaveDataStream.Free;

  inherited;
end;

procedure TMonitorAnalyzer.Append(Source: TExtendedStream; Count: Integer);
var
  Funcs: BASS_FILEPROCS;
  BC: Cardinal;
  Level: DWORD;
begin
  if not FActive then
    Exit;

  FStream.Seek(0, soFromEnd);
  FStream.CopyFrom(Source, Count);

  if FStream.Size < 32768 then
    Exit;

  if FStream.Size > 1048576 then
  begin
    FActive := False;
    Exit;
  end;

  // Der Player darf erst hier erstellen. Es müssen beim erstellen Daten da sein, deshalb geht es
  // im Konstruktor nicht (es wird direkt BASSRead aufgerufen, wenn da nichts rauskommt ist ende!)
  if FPlayer = 0 then
  begin
    Funcs.Close := BASSClose;
    Funcs.length := BASSLen;
    Funcs.seek := BASSSeek;
    Funcs.Read := BASSRead;
    FPlayer := BASSStreamCreateFileUser(STREAMFILE_BUFFERPUSH, BASS_STREAM_DECODE, Funcs, Self);
  end;

  if FStream.Size = 0 then
    Exit;

  BC := BASSStreamPutFileData(FPlayer, FStream.Memory, FStream.Size);

  if (BC > 0) and (BC <> High(Cardinal)) then
  begin
    FStream.RemoveRange(0, BC);

    while BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING do
    begin
      Level := BASSChannelGetLevel(FPlayer);

      // Links
      FWaveDataStream.Write(LOWORD(Level));
      // Rechts
      FWaveDataStream.Write(HIWORD(Level));

      // Gleich sind wir am Ende. Also jetzt schon raus, um keine Infos über Daten unter 20ms Länge zu bekommen.
      if BASSStreamGetFilePosition(FPlayer, 5) < 1 then
        Break;
    end;
  end;
end;

procedure TMonitorAnalyzer.TitleChanged;
var
  i: Integer;
begin
  if not FActive then
    Exit;

  FTitleChanges.Add(FWaveDataStream.Size);

  if FTitleChanges.Count > 6 then
  begin
    FActive := False;

    for i := 0 to FTitleChanges.Count - 1 do
      FWaveDataStream.Write(Cardinal(FTitleChanges[i]));
    FWaveDataStream.Write(Byte(FTitleChanges.Count));

    if (FWaveDataStream.Size > 4096) and (Assigned(FOnAnalyzed)) then
      FOnAnalyzed(Self);
  end;
end;

{
procedure Test;
var
  MA: TMonitorAnalyzer;
  S: TExtendedStream;
begin

  MA := TMonitorAnalyzer.Create;
  S := TExtendedStream.Create;
  S.LoadFromFile('z:\x.mp3');
  S.Position := 0;

  while S.Position < S.Size do
  begin
    if S.Size - S.Position >= 1000 then
      MA.Append(S, 1000)
    else
    begin
      MA.Append(S, S.Size - S.Position);
      Break;
    end;
  end;

  MA.Free;
end;
}

end.
