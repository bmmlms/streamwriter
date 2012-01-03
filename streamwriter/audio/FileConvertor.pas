unit FileConvertor;

interface

uses
  SysUtils, Windows, Classes, DynBASS, ExtendedStream;

const
  BE_CONFIG_MP3 = 0;
  BE_CONFIG_LAME = 256;

  BE_ERR_SUCCESSFUL: LongWord = 0;
  BE_ERR_INVALID_FORMAT: LongWord = 1;
  BE_ERR_INVALID_FORMAT_PARAMETERS: LongWord = 2;
  BE_ERR_NO_MORE_HANDLES: LongWord = 3;
  BE_ERR_INVALID_HANDLE: LongWord = 4;
  BE_ERR_BUFFER_TOO_SMALL: LongWord = 5;

  BE_MAX_HOMEPAGE = 256;

  BE_MP3_MODE_STEREO = 0;
  BE_MP3_MODE_JSTEREO = 1;
  BE_MP3_MODE_DUALCHANNEL = 2;
  BE_MP3_MODE_MONO = 3;

  MPEG1 = 1;
  MPEG2 = 0;

  NORMAL_QUALITY = 0;
  LOW_QUALITY = 1;
  HIGH_QUALITY = 2;
  VOICE_QUALITY = 3;
type
  // Stuff LAME makes use of
  PHBE_STREAM = ^THBE_STREAM;
  THBE_STREAM = LongWord;
  BE_ERR = LongWord;

  TMP3 = packed record
    dwSampleRate: LongWord;
    byMode: Byte;
    wBitRate: Word;
    bPrivate: LongWord;
    bCRC: LongWord;
    bCopyright: LongWord;
    bOriginal: LongWord;
  end;

  TLHV1 = packed record
    // STRUCTURE INFORMATION
    dwStructVersion: DWORD;
    dwStructSize: DWORD;

    // BASIC ENCODER SETTINGS
    dwSampleRate: DWORD;   // ALLOWED SAMPLERATE VALUES DEPENDS ON dwMPEGVersion
    dwReSampleRate: DWORD; // DOWNSAMPLERATE, 0=ENCODER DECIDES
    nMode: Integer;	      // BE_MP3_MODE_STEREO, BE_MP3_MODE_DUALCHANNEL, BE_MP3_MODE_MONO
    dwBitrate: DWORD;      // CBR bitrate, VBR min bitrate
    dwMaxBitrate: DWORD;   // CBR ignored, VBR Max bitrate
    nQuality: Integer;     // Quality setting (NORMAL,HIGH,LOW,VOICE)
    dwMpegVersion: DWORD;  // MPEG-1 OR MPEG-2
    dwPsyModel: DWORD;     // FUTURE USE, SET TO 0
    dwEmphasis: DWORD;     // FUTURE USE, SET TO 0

    // BIT STREAM SETTINGS
    bPrivate: LONGBOOL;    // Set Private Bit (TRUE/FALSE)
    bCRC: LONGBOOL;	      // Insert CRC (TRUE/FALSE)
    bCopyright: LONGBOOL;  // Set Copyright Bit (TRUE/FALSE)
    bOriginal: LONGBOOL;   // Set Original Bit (TRUE/FALSE)

    // VBR STUFF
    bWriteVBRHeader: LONGBOOL; // WRITE XING VBR HEADER (TRUE/FALSE)
    bEnableVBR: LONGBOOL;      // USE VBR ENCODING (TRUE/FALSE)
    nVBRQuality: Integer;      // VBR QUALITY 0..9 (best..worst)

    btReserved: array[0..255] of Byte; // FUTURE USE, SET TO 0
  end;

  type TAAC = packed record
    dwSampleRate: LongWord;
    byMode: Byte;
    wBitRate: Word;
    byEncodingMethod: Byte;
  end;

  TFormat = packed record
    case byte of
      1: (mp3: TMP3);
      2: (lhv1: TLHV1);
      3: (aac: TAAC);
    end;

  PBE_CONFIG = ^TBE_CONFIG;
  TBE_CONFIG = packed record
    dwConfig   : LongWord; // BE_CONFIG_MP3 or BE_CONFIG_LAME
    format     : TFormat;
  end;

  PBE_VERSION = ^TBE_VERSION;
  TBE_VERSION = record
    // BladeEnc DLL Version number
    byDLLMajorVersion : Byte;
    byDLLMinorVersion : Byte;

    // BladeEnc Engine Version Number
    byMajorVersion    : Byte;
    byMinorVersion    : Byte;

    // DLL Release date
    byDay             : Byte;
    byMonth           : Byte;
    wYear             : Word;

    // BladeEnc	Homepage URL
    zHomePage         : Array[0..BE_MAX_HOMEPAGE + 1] of Char;
  end;

  PLongWord = ^LongWord;
  PByte = ^Byte;

  LameInitStream = function (pbeConfig: PBE_CONFIG; dwSamples: PLongWord; dwBufferSize: PLongWord; phbeStream: PHBE_STREAM): BE_ERR; stdcall;
  LameEncodeChunk = function (hbeStream: THBE_STREAM; nSamples: LongWord; pSamples: PSmallInt; pOutput: PByte;pdwOutput: PLongWord): BE_ERR; stdcall;
  LameDeInitStream = function (hbeStream: THBE_STREAM; pOutput: PByte; pdwOutput: PLongWord): BE_ERR; stdcall;
  LameCloseStream  = function (hbeStream: THBE_STREAM): BE_ERR; stdcall;

  TFileConvertorProgressEvent = procedure(Sender: TObject; Percent: Integer) of object;

  TFileConvertor = class
  private
    FOnProgress: TFileConvertorProgressEvent;

    function Convert2WAV(FromFile, ToFile: string): Boolean;
    function ConvertWAV2MP3(FromFile, ToFile: string): Boolean;
  public
    function Convert(FromFile, ToFile: string): Boolean;

    property OnProgress: TFileConvertorProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TFileConvertor }

function TFileConvertor.Convert(FromFile, ToFile: string): Boolean;
var
  ExtFrom: string;
  ExtTo: string;
begin
  ExtFrom := LowerCase(ExtractFileExt(FromFile));
  ExtTo := LowerCase(ExtractFileExt(ToFile));

  if (ExtFrom = '.mp3') and (ExtTo = '.wav') then
    Result := Convert2WAV(FromFile, ToFile)
  else if (ExtFrom = '.wav') and (ExtTo = '.mp3') then
    Result := ConvertWAV2MP3(FromFile, ToFile);
end;

function TFileConvertor.Convert2WAV(FromFile, ToFile: string): Boolean;
var
  Channel: DWORD;
  Freq: Single;
  Buf: array[0..10000] of Byte;
  BytesRead: Integer;
  i: UInt64;
  Tmp: AnsiString;
  OutStream: TFileStream;
  Channels: Word;
  SamplesPerSec: DWORD;
  AvgBytesPerSec: DWORD;
  BlockAlign: Word;
  BitsPerSample: Word;
  ChanInfo: BASS_CHANNELINFO;
  PercentDone: Integer;
begin
  Channel := BASSStreamCreateFile(FALSE, PChar(FromFile), 0, 0, BASS_STREAM_DECODE {$IFDEF UNICODE}or BASS_UNICODE{$ENDIF});

  BASSChannelGetInfo(Channel, ChanInfo);
	Channels := ChanInfo.chans;

  if (ChanInfo.flags and 1 > 0) then
    BitsPerSample := 8
  else
    BitsPerSample := 16;

	BlockAlign := Channels * BitsPerSample div 8;
	BASSChannelGetAttribute(Channel, 1, Freq);
  SamplesPerSec := Trunc(Freq);
	AvgBytesPerSec := SamplesPerSec * BlockAlign;

  OutStream := TFileStream.Create(ToFile, fmCreate);
  try
    Tmp := 'RIFF';
    OutStream.Write(Tmp[1], Length(Tmp));
    Tmp := #0#0#0#0;
    OutStream.Write(Tmp[1], Length(Tmp));
    Tmp := 'WAVE';
    OutStream.Write(Tmp[1], Length(Tmp));
    Tmp := 'fmt ';
    OutStream.Write(Tmp[1], Length(Tmp));
    Tmp := #$10#0#0#0;
    OutStream.Write(Tmp[1], Length(Tmp));
    Tmp := #1#0;
    OutStream.Write(Tmp[1], Length(Tmp));

    if Channels = 1 then
       Tmp := #1#0
    else
       Tmp := #2#0;
    OutStream.Write(Tmp[1], Length(Tmp));

    OutStream.Write(SamplesPerSec, 2);

    Tmp := #0#0;
    OutStream.Write(Tmp[1], Length(Tmp));

    OutStream.Write(AvgBytesPerSec, 4);
    OutStream.Write(BlockAlign, 2);
    OutStream.Write(BitsPerSample, 2);

    Tmp := 'data';
    OutStream.Write(Tmp[1],length(Tmp));

    Tmp := #0#0#0#0;
    OutStream.Write(Tmp[1],length(Tmp));

    while (BASSChannelIsActive(Channel) > 0) do
    begin
      BytesRead := BASSChannelGetData(Channel, @Buf, 10000);
      OutStream.Write(buf, BytesRead);

      if Assigned(FOnProgress) then
      begin
        PercentDone := Trunc(100 * (BASSChannelGetPosition(Channel, BASS_POS_BYTE) / BASSChannelGetLength(Channel, BASS_POS_BYTE)));
        FOnProgress(Self, PercentDone);
      end;
    end;
    BASSStreamFree(Channel);

    i := OutStream.Size - 8;
    OutStream.Position := 4;
    OutStream.Write(i, 4);
    i := i - $24;
    OutStream.Position := 40;
    OutStream.Write(i, 4);
  finally
    OutStream.Free;
  end;
end;

function TFileConvertor.ConvertWAV2MP3(FromFile, ToFile: string): Boolean;
var
  DLLHandle: THandle;
  Encoder: string;
  InStream, OutStream: TFileStream;

  InitStream: LameInitStream;
  EncodeChunk: LameEncodeChunk;
  DeInitStream: LameDeInitStream;
  CloseStream: LameCloseStream;

  Done: LongWord;
  PercentDone: LongWord;
  PrevPercentDone: LongWord;

  Config: TBE_CONFIG;
  Samples, MP3Buffer: LongWord;

  HBEStream: THBE_STREAM;
  Err: BE_ERR;
  PMP3Buffer: PByte;
  Buffer: PSmallInt;
  Len, Write, ToRead, ToWrite: LongWord;
begin
  Done := 0;
  PercentDone := 0;
  PrevPercentDone := 0;

  Encoder := 'z:\lame-enc.dll';
  DLLHandle := LoadLibrary(PChar(Encoder));
  if DLLHandle <= 32 then
  begin
    raise Exception.Create('Could not load lame-enc.dll');
  end;

  try
    InitStream := GetProcAddress(DLLHandle,'beInitStream');
    EncodeChunk := GetProcAddress(DLLHandle,'beEncodeChunk');
    DeInitStream := GetProcAddress(DLLHandle,'beDeinitStream');
    CloseStream := GetProcAddress(DLLHandle,'beCloseStream');

    InStream := TFileStream.Create(FromFile, fmOpenRead);
    try
      OutStream := TFileStream.Create(ToFile, fmCreate);
      try
        GetMem(PMP3Buffer, MP3Buffer);
        GetMem(Buffer, Samples * SizeOf(SmallInt));

        Config.dwConfig := BE_CONFIG_LAME;

        Config.Format.lhv1.dwStructVersion := 1;
        Config.Format.lhv1.dwStructSize := SizeOf(Config);
        Config.Format.lhv1.dwSampleRate := 44100;
        Config.Format.lhv1.dwReSampleRate := 44100;
        Config.Format.lhv1.nMode := BE_MP3_MODE_STEREO;
        Config.Format.lhv1.dwBitrate := 256;
        Config.Format.lhv1.dwMaxBitrate := 256;
        Config.Format.lhv1.nQuality := HIGH_QUALITY;
        Config.Format.lhv1.dwMPegVersion := 1;
        Config.Format.lhv1.dwPsyModel := 0;
        Config.Format.lhv1.dwEmphasis := 0;
        Config.Format.lhv1.bPrivate := False;
        Config.Format.lhv1.bCRC := False;
        Config.Format.lhv1.bCopyright := True;
        Config.Format.lhv1.bOriginal := True;
        Config.Format.lhv1.bWriteVBRHeader := false;
        Config.Format.lhv1.bEnableVBR := false;
        Config.Format.lhv1.nVBRQuality := 0;

        Err := InitStream(@Config, @Samples, @MP3Buffer, @HBEStream);

        if (Err <> BE_ERR_SUCCESSFUL) then
        begin
          raise Exception.Create('Err <> BE_ERR_SUCCESSFUL');
        end;

        Len := InStream.Size;

        while (Done < Len) do
        begin
          if (Done + Samples * 2 < Len) then
          begin
            ToRead := Samples * 2;
          end else
          begin
            ToRead := Len - Done;
          end;

          InStream.ReadBuffer(Buffer^, ToRead);

          Err := EncodeChunk(HBEStream, ToRead div 2, Buffer, PMP3Buffer, @ToWrite);

          if (Err <> BE_ERR_SUCCESSFUL) then
          begin
            CloseStream(HBEStream);
            raise Exception.Create('Err <> BE_ERR_SUCCESSFUL');
          end;

          OutStream.WriteBuffer(PMP3Buffer^, ToWrite);

          Inc(Done, ToRead);

          if Assigned(FOnProgress) then
          begin
            PercentDone := Round(100.0 * Done / Len);
            FOnProgress(Self, PercentDone);
          end;
        end;

        Err := DeInitStream(HBEStream, PMP3Buffer, @Write);

        if (err <> BE_ERR_SUCCESSFUL) then
        begin
          CloseStream(HBEStream);
          raise Exception.Create('Err <> BE_ERR_SUCCESSFUL');
        end;

        if (Write <> 0) then
        begin
          OutStream.WriteBuffer(PMP3Buffer^, Write);
        end;

        CloseStream(HBEStream);
      finally
        OutStream.Free;

        FreeMem(PMP3Buffer);
        FreeMem(Buffer);
      end;
    finally
      InStream.Free;
    end;
  finally
    FreeLibrary(DLLHandle);
  end;
end;

end.
