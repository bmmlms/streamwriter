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

unit AudioFunctions;

interface

uses
  Windows, SysUtils, Math, Classes;

type
  TBandData = record
    Handle: DWORD;
    Gain: Single;
  end;

  TAudioInfo = record
  private
    procedure GetAudioInfo(Player: Cardinal; Size: Int64); overload;
    function IsVBR(Player: Cardinal): Boolean;
  public
    Length: Double;
    Bitrate: Integer;
    VBR: Boolean;
    BytesPerSec: Integer;
    BytesPerMSec: Double;
    Success: Boolean;

    procedure GetAudioInfo(MemoryStream: TMemoryStream); overload;
    procedure GetAudioInfo(Filename: string); overload;
  end;

  TBitrateInfo = record
    Pos: Int64;
    Time: Double;
  end;

  // Defines all possible types of audio-data - DO NOT change orders. Make sure atM4A is always the last one!
  // That is because the settings window and stream loading/saving uses this.
  // It generally makes sense not to alter these enums because they are used in binary streams..
  TAudioTypes = (atNone, atMPEG, atAAC, atOGG, atM4A);
  TBitrates = (brCBR, brVBR);
  TVBRQualities = (vqHigh, vqMedium, vqLow);

  TPlaylistTypes = (ptM3U, ptPLS, ptUnknown);

function RoundBitrate(Bitrate: Cardinal): Cardinal;
function GuessVBRQuality(Bitrate: Integer; AudioType: TAudioTypes): TVBRQualities;
function BuildTime(T: Double; MSecs: Boolean): string;
function FilenameToFormat(Filename: string): TAudioTypes;
function FormatToFiletype(Format: TAudioTypes): string;
function FormatToDesc(Format: TAudioTypes): string;
function BandToFreq(Idx: Integer): Integer;

implementation

uses
  DynBASS, Functions;

function RoundBitrate(Bitrate: Cardinal): Cardinal;
begin
  Result := Bitrate;

  if InRange(Bitrate, 30, 34) then
    Result := 32
  else if InRange(Bitrate, 62, 66) then
    Result := 64
  else if InRange(Bitrate, 94, 98) then
    Result := 96
  else if InRange(Bitrate, 126, 130) then
    Result := 128
  else if InRange(Bitrate, 158, 162) then
    Result := 160
  else if InRange(Bitrate, 190, 194) then
    Result := 192
  else if InRange(Bitrate, 222, 226) then
    Result := 224
  else if InRange(Bitrate, 254, 258) then
    Result := 256
  else if InRange(Bitrate, 318, 322) then
    Result := 320
  else if InRange(Bitrate, 382, 386) then
    Result := 384;
end;

function GuessVBRQuality(Bitrate: Integer; AudioType: TAudioTypes): TVBRQualities;
begin
  Result := vqMedium;
  case AudioType of
    atMPEG, atOGG:
      if Bitrate >= 210 then
        Result := vqHigh
      else if Bitrate >= 110 then
        Result := vqMedium
      else
        Result := vqLow;
    atAAC, atM4A:
      if Bitrate >= 150 then
        Result := vqHigh
      //else if Bitrate >= 128 then  // Das löst das Problem von "Rix". Ist vllt. nicht optimal, aber besser als nichts..
                                     // Am besten wäre es wohl, den Encoder präziser zu füttern was Bitraten angeht => Enum vergrößern..
                                     // Nicht 3 Stufen, sondern 6 oder so! Oder Bitrate durchschleifen und jeder Encoder denkt selber nach?
      //  Result := vqMedium
      else
        Result := vqMedium;
  end;
end;

function BuildTime(T: Double; MSecs: Boolean): string;
var
  Min, Sec, MSec: Word;
begin
  Min := Trunc(T / 60);
  T := T - Trunc(T / 60) * 60;
  Sec := Trunc(T);
  T := T - Trunc(T);
  MSec := (Trunc(T * 1000) div 10) * 10;
  if MSecs then
    Result := Format('%0.2d:%0.2d.%0.3d', [Min, Sec, MSec])
  else
    Result := Format('%0.2d:%0.2d', [Min, Sec])
end;

function FilenameToFormat(Filename: string): TAudioTypes;
begin
  Result := atNone;

  Filename := LowerCase(ExtractFileExt(Filename));

  if Filename = '.mp3' then
    Exit(atMPEG)
  else if Filename = '.aac' then
    Exit(atAAC)
  else if Filename = '.ogg' then
    Exit(atOGG)
  else if Filename = '.m4a' then
    Exit(atM4A);
end;

function FormatToFiletype(Format: TAudioTypes): string;
begin
  Result := '';
  case Format of
    atNone: ;
    atMPEG: Result := '.mp3';
    atAAC: Result := '.aac';
    atOGG: Result := '.ogg';
    atM4A: Result := '.m4a';
  end;
end;

function FormatToDesc(Format: TAudioTypes): string;
begin
  Result := '';
  case Format of
    atNone: ;
    atMPEG: Result := 'MP3';
    atAAC: Result := 'AAC';
    atOGG: Result := 'OGG';
  end;
end;

function BandToFreq(Idx: Integer): Integer;
begin
  case Idx of
    0: Result := 60;
    1: Result := 170;
    2: Result := 310;
    3: Result := 600;
    4: Result := 1000;
    5: Result := 3000;
    6: Result := 6000;
    7: Result := 12000;
    8: Result := 14000;
    9: Result := 16000;
    else
      raise Exception.Create('');
  end;
end;

procedure TAudioInfo.GetAudioInfo(MemoryStream: TMemoryStream);
var
  Player: Cardinal;
begin
  Success := False;
  if MemoryStream.Size = 0 then
    Exit;
  Player := BASSStreamCreateFile(True, MemoryStream.Memory, 0, MemoryStream.Size, BASS_STREAM_DECODE or BASS_STREAM_PRESCAN);

  if Player = 0 then
    Exit;

  try
    GetAudioInfo(Player, MemoryStream.Size);
  finally
    BASSStreamFree(Player);
  end;
end;

procedure TAudioInfo.GetAudioInfo(Filename: string);
var
  Player: Cardinal;
  Size: Int64;
begin
  Success := False;
  Size := GetFileSize(Filename);
  if Size = 0 then
    Exit;
  Player := BASSStreamCreateFile(False, PWideChar(UnicodeString(Filename)), 0, 0, BASS_STREAM_DECODE or BASS_UNICODE or BASS_STREAM_PRESCAN);

  try
    GetAudioInfo(Player, Size);
  finally
    BASSStreamFree(Player);
  end;
end;

procedure TAudioInfo.GetAudioInfo(Player: Cardinal; Size: Int64);
var
  Time: Double;
  BufLen: Int64;
begin
  BASSChannelSetPosition(Player, Size, BASS_POS_BYTE);
  Time := BASSChannelBytes2Seconds(Player, BASSChannelGetLength(Player, BASS_POS_BYTE));
  BufLen := BASSStreamGetFilePosition(Player, BASS_FILEPOS_END);
  if BufLen = -1 then
    Exit;

  Length := Time;
  Bitrate := Round(BufLen / ((125 * Time)) + 0.5);
  VBR := IsVBR(Player);
  if not VBR then
    Bitrate := RoundBitrate(Bitrate);

  BytesPerSec := Trunc((BufLen / (125 * Time) + 0.5) * 125);
  BytesPerMSec := ((BufLen / (125 * Time) + 0.5) * 125) / 1000;

  if BytesPerMSec > 0 then
    Success := True;
end;

function TAudioInfo.IsVBR(Player: Cardinal): Boolean;
var
  P, LastP, ElapsedP, LastElapsedP: QWORD;
  Counter: Integer;
begin
  Result := False;

  Counter := 0;
  LastP := 0;
  LastElapsedP := 0;
  BassChannelSetPosition(Player, 0, BASS_POS_BYTE);
  while BASSChannelIsActive(Player) = BASS_ACTIVE_PLAYING do
  begin
    BASSChannelGetLevel(Player);

    ElapsedP := BASSStreamGetFilePosition(Player, BASS_FILEPOS_CURRENT);

    if ElapsedP > 5000000 then
      Break;

    P := ElapsedP - LastElapsedP;
    if (P > 0) and (LastP > 0) and ((P > LastP + 1) or (P < LastP - 1)) then
      Inc(Counter);

    if Counter = 5 then
      Exit(True);

    LastP := P;
    LastElapsedP := ElapsedP;
  end;
end;

end.
