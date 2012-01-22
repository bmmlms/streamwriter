unit AudioFunctions;

interface

uses
  Windows, SysUtils, Math, Classes, DynBASS, Functions;

type
  TAudioFileInfo = record
    Length: Double;
    Bitrate: Integer;
    Success: Boolean;
  end;

function RoundBitrate(Bitrate: Cardinal): Cardinal;
function GetFileInfo(Filename: string): TAudioFileInfo;

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
  TempPlayer: Cardinal;
  Time: Double;
  BufLen: Int64;
  FS: TFileStream;
  BytesPerSec: Integer;
  BitRate: Cardinal;
begin
  Result.Length := 0;
  Result.Bitrate := 0;
  Result.Success := False;

  TempPlayer := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE or BASS_STREAM_PRESCAN or BASS_UNICODE);

  if TempPlayer = 0 then
    Exit;

  try
    BASSChannelSetPosition(TempPlayer, GetFileSize(Filename), BASS_POS_BYTE);
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
  finally
    BASSStreamFree(TempPlayer);
  end;
end;

end.
