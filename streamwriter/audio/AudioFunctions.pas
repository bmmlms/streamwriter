unit AudioFunctions;

interface

uses
  Windows, SysUtils, Math, Classes, DynBASS, Functions, TypeDefs;

function RoundBitrate(Bitrate: Cardinal): Cardinal;
function GetFileInfo(Filename: string): TAudioFileInfo;
function GuessVBRQuality(BitRate: Integer; AudioType: TAudioTypes): TVBRQualities;
function BuildTime(T: Double; MSecs: Boolean): string;

implementation

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
  i, n: Integer;
  TempPlayer: Cardinal;
  Time: Double;
  BufLen: Int64;
  FS: TFileStream;
  BytesPerSec: Integer;
  BitRate, LastBitRate: Cardinal;
  FileSize: Int64;
  Positions: array of Int64;
  P, LastP, ElapsedP, LastElapsedP: QWORD;
  StepSize: Int64;
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

    P := 0;
    LastP := 0;
    LastElapsedP := 0;
    BassChannelSetPosition(TempPlayer, 0, BASS_POS_BYTE);
    while BASSChannelIsActive(TempPlayer) = BASS_ACTIVE_PLAYING do
    begin
      BASSChannelGetLevel(TempPlayer);

      ElapsedP := BASSStreamGetFilePosition(TempPlayer, BASS_FILEPOS_CURRENT);

      P := ElapsedP - LastElapsedP;
      if (P > 0) and (LastP > 0) and ((P > LastP + 10) or (P < LastP - 10)) then
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
    atAAC, atM4A: Result := vqMedium; // TODO: !!!
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

end.
