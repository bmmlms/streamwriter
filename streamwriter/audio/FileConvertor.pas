unit FileConvertor;

interface

uses
  SysUtils, Windows, Classes, DynBASS, ExtendedStream, AddonLAME, AppData,
  AddonOGGEnc, AddonFAAC, Functions, PerlRegEx, TypeDefs;

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
    FCBRBitRate: Integer;
    FBitRateType: TBitRates;
    FVBRQuality: TVBRQualities;
    FOnProgress: TFileConvertorProgressEvent;

    procedure ReadCallbackMP3(Data: AnsiString);
    procedure ReadCallbackOGG(Data: AnsiString);
    procedure ReadCallbackAAC(Data: AnsiString);

    function ConvertWAV2MP3(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
    function ConvertWAV2OGG(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
    function ConvertWAV2AAC(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
    function ConvertWAV2M4A(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
  public
    function Convert(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
    function Convert2WAV(FromFile, ToFile: string; TerminateFlag: PBoolean = nil; F: Integer = 0; T: Integer = 0): Boolean;

    property CBRBitRate: Integer read FCBRBitRate write FCBRBitRate;
    property BitRateType: TBitRates read FBitRateType write FBitRateType;
    property VBRQuality: TVBRQualities read FVBRQuality write FVBRQuality;
    property OnProgress: TFileConvertorProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  PostProcess, PostProcessMP4Box;

{ TFileConvertor }

function TFileConvertor.Convert(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
var
  ExtFrom: string;
  ExtTo: string;
begin
  Result := False;

  ExtFrom := LowerCase(ExtractFileExt(FromFile));
  ExtTo := LowerCase(ExtractFileExt(ToFile));

  if ExtTo = '.wav' then
    Result := Convert2WAV(FromFile, ToFile, TerminateFlag)
  else if (ExtFrom = '.wav') and (ExtTo = '.mp3') then
    Result := ConvertWAV2MP3(FromFile, ToFile, TerminateFlag)
  else if (ExtFrom = '.wav') and (ExtTo = '.ogg') then
    Result := ConvertWAV2OGG(FromFile, ToFile, TerminateFlag)
  else if (ExtFrom = '.wav') and (ExtTo = '.aac') then
    Result := ConvertWAV2AAC(FromFile, ToFile, TerminateFlag)
  else if (ExtFrom = '.wav') and (ExtTo = '.m4a') then
    Result := ConvertWAV2M4A(FromFile, ToFile, TerminateFlag);
end;

function TFileConvertor.Convert2WAV(FromFile, ToFile: string; TerminateFlag: PBoolean = nil; F: Integer = 0; T: Integer = 0): Boolean;
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
  Result := False;

  Channel := BASSStreamCreateFile(False, PChar(FromFile), 0, 0, BASS_STREAM_DECODE {$IFDEF UNICODE}or BASS_UNICODE{$ENDIF});

  if Channel = 0 then
    Exit;

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
    OutStream.Write(Tmp[1], Length(Tmp));

    Tmp := #0#0#0#0;
    OutStream.Write(Tmp[1], Length(Tmp));

    if F > 0 then
      BASSChannelSetPosition(Channel, F, BASS_POS_BYTE);

    while (BASSChannelIsActive(Channel) > 0) do
    begin
      BytesRead := BASSChannelGetData(Channel, @Buf, 10000);
      OutStream.Write(buf, BytesRead);

      if Assigned(FOnProgress) then
      begin
        PercentDone := Trunc(100 * (BASSChannelGetPosition(Channel, BASS_POS_BYTE) / BASSChannelGetLength(Channel, BASS_POS_BYTE)));
        FOnProgress(Self, PercentDone);
      end;

      if T > 0 then
        if BASSChannelGetPosition(Channel, BASS_POS_BYTE) > T then
          Break;

      if (TerminateFlag <> nil) and (TerminateFlag^) then
        Break;
    end;
    BASSStreamFree(Channel);

    i := OutStream.Size - 8;
    OutStream.Position := 4;
    OutStream.Write(i, 4);
    i := i - $24;
    OutStream.Position := 40;
    OutStream.Write(i, 4);

    Result := True;
  finally
    OutStream.Free;
  end;

  if Result and (TerminateFlag <> nil) and (TerminateFlag^) then
  begin
    DeleteFile(PChar(ToFile));
    Result := False;
  end
end;

function TFileConvertor.ConvertWAV2AAC(FromFile, ToFile: string;
  TerminateFlag: PBoolean): Boolean;
var
  CmdLine, ToFileTemp, Opts: string;
  Output: AnsiString;
  Addon: TAddonFAAC;
  EC: Cardinal;
begin
  Addon := AppGlobals.AddonManager.Find(TAddonFAAC) as TAddonFAAC;

  if not Addon.FilesExtracted then
    Exit(False);

  ToFileTemp := RemoveFileExt(ToFile) + '_convert.aac';

  case FBitRateType of
    brCBR:
      begin
        Opts := '-b ' + IntToStr(FCBRBitRate);
      end;
    brVBR:
      begin
        case FVBRQuality of
          vqHigh: Opts := '-q 150';
          vqMedium: Opts := '-q 100';
          vqLow: Opts := '-q 50';
        end;
      end;
  end;

  CmdLine := Addon.EXEPath + ' ' + Opts + ' -o "' + ToFileTemp + '" "' + FromFile + '"';
  if RunProcess(CmdLine, ExtractFilePath(Addon.EXEPath), 300000, Output, EC, TerminateFlag, ReadCallbackAAC) = 2 then
  begin
    Result := False;
  end else
  begin
    Result := FileExists(ToFileTemp);
  end;

  if Result then
    Result := MoveFileEx(PChar(ToFileTemp), PChar(ToFile), MOVEFILE_REPLACE_EXISTING);
end;

function TFileConvertor.ConvertWAV2M4A(FromFile, ToFile: string;
  TerminateFlag: PBoolean): Boolean;
var
  ToFileTemp, ToFileTemp2: string;
begin
  ToFileTemp := RemoveFileExt(ToFile) + '_temp.aac';
  ToFileTemp2 := '';

  Result := ConvertWAV2AAC(FromFile, ToFileTemp, TerminateFlag);

  if Result then
  begin
    ToFileTemp2 := RemoveFileExt(ToFile) + '_convert.m4a';
    Result := TPostProcessMP4Box(AppGlobals.StreamSettings.PostProcessors.Find(TPostProcessMP4Box)).MP4BoxMux(ToFileTemp, ToFileTemp2, TerminateFlag) = arWin;

    if Result then
      Result := MoveFileEx(PChar(ToFileTemp2), PChar(ToFile), MOVEFILE_REPLACE_EXISTING);
  end;

  DeleteFile(PChar(ToFileTemp));
  if ToFileTemp2 <> '' then
    DeleteFile(PChar(ToFileTemp2));
end;

function TFileConvertor.ConvertWAV2MP3(FromFile, ToFile: string; TerminateFlag: PBoolean = nil): Boolean;
var
  Addon: TAddonLAME;

  CmdLine, Opts, ToFileTemp: string;
  Output: AnsiString;
  EC: Cardinal;
begin
  Result := False;

  Addon := AppGlobals.AddonManager.Find(TAddonLAME) as TAddonLAME;

  if not Addon.FilesExtracted then
    Exit;

  ToFileTemp := RemoveFileExt(ToFile) + '_convert.mp3';

  case FBitRateType of
    brCBR:
      begin
        Opts := '-b ' + IntToStr(FCBRBitRate);
      end;
    brVBR:
      begin
        case FVBRQuality of
          vqHigh: Opts := '-V 0';
          vqMedium: Opts := '-V 4';
          vqLow: Opts := '-V 8';
        end;
      end;
  end;

  CmdLine := Addon.EXEPath + ' ' + Opts + ' "' + FromFile + '" "' + ToFileTemp + '"';
  if RunProcess(CmdLine, ExtractFilePath(Addon.EXEPath), 300000, Output, EC, TerminateFlag, ReadCallbackMP3) = 2 then
  begin
    Result := False;
  end else
  begin
    Result := FileExists(ToFileTemp);
  end;

  if Result then
    Result := MoveFileEx(PChar(ToFileTemp), PChar(ToFile), MOVEFILE_REPLACE_EXISTING);
end;

function TFileConvertor.ConvertWAV2OGG(FromFile, ToFile: string;
  TerminateFlag: PBoolean): Boolean;
var
  CmdLine, ToFileTemp, Opts: string;
  Output: AnsiString;
  Addon: TAddonOGGEnc;
  EC: Cardinal;
begin
  Addon := AppGlobals.AddonManager.Find(TAddonOGGEnc) as TAddonOGGEnc;

  if not Addon.FilesExtracted then
    Exit(False);

  ToFileTemp := RemoveFileExt(ToFile) + '_convert.ogg';

  case FBitRateType of
    brCBR:
      begin
        Opts := '-m ' + IntToStr(FCBRBitRate) + ' -M ' + IntToStr(FCBRBitRate);
      end;
    brVBR:
      begin
        case FVBRQuality of
          vqHigh: Opts := '-m 224 -M 320';
          vqMedium: Opts := '-m 128 -M 192';
          vqLow: Opts := '-m 32 -M 96';
        end;
      end;
  end;

  CmdLine := Addon.EXEPath + ' ' + Opts + ' "' + FromFile + '" -o "' + ToFileTemp + '"';
  if RunProcess(CmdLine, ExtractFilePath(Addon.EXEPath), 300000, Output, EC, TerminateFlag, ReadCallbackOGG) = 2 then
  begin
    Result := False;
  end else
  begin
    Result := FileExists(ToFileTemp);
  end;

  if Result then
    Result := MoveFileEx(PChar(ToFileTemp), PChar(ToFile), MOVEFILE_REPLACE_EXISTING);
end;

procedure TFileConvertor.ReadCallbackAAC(Data: AnsiString);
var
  R: TPerlRegEx;
  M: string;
begin
  if not Assigned(FOnProgress) then
   Exit;

  R := TPerlRegEx.Create;
  try
    R.Subject := Data;
    R.RegEx := '(\d+)%\)';
    try
      if R.Match then
      begin
        repeat
          M := R.MatchedText;
          M := Copy(M, 1, Length(M) - 2);
          FOnProgress(Self, StrToInt(M));
        until not R.MatchAgain;
      end;
    except
    end;
  finally
    R.Free;
  end;
end;

procedure TFileConvertor.ReadCallbackMP3(Data: AnsiString);
var
  R: TPerlRegEx;
  M: string;
begin
  if not Assigned(FOnProgress) then
   Exit;

  R := TPerlRegEx.Create;
  try
    R.Subject := Data;
    R.RegEx := '(\d+)%\)';
    try
      if R.Match then
      begin
        repeat
          M := R.MatchedText;
          M := Copy(M, 1, Length(M) - 2);
          FOnProgress(Self, Trunc(StrToFloat(M)));
        until not R.MatchAgain;
      end;
    except
    end;
  finally
    R.Free;
  end;
end;

procedure TFileConvertor.ReadCallbackOGG(Data: AnsiString);
var
  R: TPerlRegEx;
  M: string;
begin
  if not Assigned(FOnProgress) then
   Exit;

  R := TPerlRegEx.Create;
  try
    R.Subject := Data;
    R.RegEx := '(\d+)[.,](\d+)%';
    try
      if R.Match then
      begin
        repeat
          M := R.MatchedText;
          M := Copy(M, 1, Length(M) - 1);
          FOnProgress(Self, Trunc(StrToFloat(M)));
        until not R.MatchAgain;
      end;
    except
    end;
  finally
    R.Free;
  end;
end;

end.
