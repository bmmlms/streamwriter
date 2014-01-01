{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2014 Alexander Nottelmann

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

  TAudioFileInfo = record
    Length: Double;
    Bitrate: Integer;
    FileSize: Int64;
    VBR: Boolean;
    Success: Boolean;
  end;

  TBitrateInfo = record
    Pos: Int64;
    Time: Double;
  end;

  // Defines all possible types of audio-data - DO NOT change orders. Make sure atM4A is always the last one!
  // That is because the settings window and stream loading/saving uses this.
  // It generally makes sense not to alter these enums because they are used in binary streams..
  TAudioTypes = (atNone, atMPEG, atAAC, atOGG, atM4A);
  TBitRates = (brCBR, brVBR);
  TVBRQualities = (vqHigh, vqMedium, vqLow);

  TPlaylistTypes = (ptM3U, ptPLS, ptUnknown);

function RoundBitrate(Bitrate: Cardinal): Cardinal;
function GetFileInfo(Filename: string): TAudioFileInfo;
function GuessVBRQuality(BitRate: Integer; AudioType: TAudioTypes): TVBRQualities;
function BuildTime(T: Double; MSecs: Boolean): string;
function FiletypeToFormat(Filename: string): TAudioTypes;
function FormatToFiletype(Format: TAudioTypes): string;
function FormatToDesc(Format: TAudioTypes): string;
function BandToFreq(Idx: Integer): Integer;

implementation

uses
  DynBASS, Functions;

function RoundBitrate(Bitrate: Cardinal): Cardinal;
begin
  if Bitrate < 63 then
    Result := 32
  else if Bitrate < 95 then
    Result := 64
  else if Bitrate < 127 then
    Result := 96
  else if Bitrate < 159 then
    Result := 128
  else if Bitrate < 191 then
    Result := 160
  else if Bitrate < 223 then
    Result := 192
  else if Bitrate < 255 then
    Result := 224
  else if Bitrate < 319 then
    Result := 256
  else if Bitrate < 383 then
    Result := 320
  else
    Result := 384;
end;

function GetFileInfo(Filename: string): TAudioFileInfo;
var
  TempPlayer: Cardinal;
  Time: Double;
  BufLen: Int64;
  BytesPerSec: Integer;
  BitRate: Cardinal;
  FileSize: Int64;
  P, LastP, ElapsedP, LastElapsedP: QWORD;
  Counter: Integer;
begin
  Result.Length := 0;
  Result.Bitrate := 0;
  Result.VBR := False;
  Result.Success := False;

  TempPlayer := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE or BASS_STREAM_PRESCAN or BASS_UNICODE);

  if TempPlayer = 0 then
    Exit;

  try
    FileSize := GetFileSize(Filename);

    Result.FileSize := Filesize;

    BASSChannelSetPosition(TempPlayer, BASSStreamGetFilePosition(TempPlayer, BASS_FILEPOS_END), BASS_POS_BYTE);
    Time := BASSChannelBytes2Seconds(TempPlayer, BASSChannelGetLength(TempPlayer, BASS_POS_BYTE));
    BufLen := BASSStreamGetFilePosition(TempPlayer, BASS_FILEPOS_END);
    if BufLen = -1 then
      raise Exception.Create('');
    BytesPerSec := Trunc((BufLen / (125 * Time) + 0.5) * 125);
    BitRate := Trunc(BufLen / Floor(((125 * Time)) + 0.5));
    BitRate := RoundBitrate(BitRate);

    if BytesPerSec <= 10 then
      Exit;

    Result.Length := Time;
    Result.Bitrate := BitRate;
    Result.Success := True;

    Counter := 0;
    LastP := 0;
    LastElapsedP := 0;
    BassChannelSetPosition(TempPlayer, 0, BASS_POS_BYTE);
    while BASSChannelIsActive(TempPlayer) = BASS_ACTIVE_PLAYING do
    begin
      BASSChannelGetLevel(TempPlayer);

      ElapsedP := BASSStreamGetFilePosition(TempPlayer, BASS_FILEPOS_CURRENT);

      if ElapsedP > 5000000 then
        Break;

      P := ElapsedP - LastElapsedP;
      if (P > 0) and (LastP > 0) and ((P > LastP + 10) or (P < LastP - 10)) then
      begin
        Inc(Counter);
      end;

      if Counter = 5 then
      begin
        Result.VBR := True;
        Break;
      end;

      LastP := P;
      LastElapsedP := ElapsedP;
    end;
  finally
    BASSStreamFree(TempPlayer);
  end;
end;

function GuessVBRQuality(BitRate: Integer; AudioType: TAudioTypes): TVBRQualities;
begin
  Result := vqMedium;
  case AudioType of
    atMPEG, atOGG:
      if BitRate >= 210 then
        Result := vqHigh
      else if BitRate >= 110 then
        Result := vqMedium
      else
        Result := vqLow;
    atAAC, atM4A:
      if BitRate >= 150 then
        Result := vqHigh
      //else if BitRate >= 128 then  // Das löst das Problem von "Rix". Ist vllt. nicht optimal, aber besser als nichts..
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

function FiletypeToFormat(Filename: string): TAudioTypes;
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

end.
