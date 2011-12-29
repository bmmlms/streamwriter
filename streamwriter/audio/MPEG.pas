{ This unit contains stuff to handle MPEG-files }
unit MPEG;

interface

const
  { Table for bit rates }
  MPEG_BIT_RATE: array [0..3, 0..3, 0..15] of Word =
    (
    { For MPEG 2.5 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { Reserved }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
    { For MPEG 2 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256, 0)),
    { For MPEG 1 }
    ((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 0),
    (0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384, 0),
    (0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448, 0))
    );

  { Sample rate codes }
  MPEG_SAMPLE_RATE_LEVEL_3 = 0;                                     { Level 3 }
  MPEG_SAMPLE_RATE_LEVEL_2 = 1;                                     { Level 2 }
  MPEG_SAMPLE_RATE_LEVEL_1 = 2;                                     { Level 1 }
  MPEG_SAMPLE_RATE_UNKNOWN = 3;                               { Unknown value }

  { Table for sample rates }
  MPEG_SAMPLE_RATE: array [0..3, 0..3] of Word =
    (
    (11025, 12000, 8000, 0),                                   { For MPEG 2.5 }
    (0, 0, 0, 0),                                                  { Reserved }
    (22050, 24000, 16000, 0),                                    { For MPEG 2 }
    (44100, 48000, 32000, 0));                                   { For MPEG 1 }

  { MPEG version codes }
  MPEG_VERSION_2_5 = 0;                                            { MPEG 2.5 }
  MPEG_VERSION_UNKNOWN = 1;                                 { Unknown version }
  MPEG_VERSION_2 = 2;                                                { MPEG 2 }
  MPEG_VERSION_1 = 3;                                                { MPEG 1 }

  { MPEG version names }
  MPEG_VERSION: array [0..3] of string =
    ('MPEG 2.5', 'MPEG ?', 'MPEG 2', 'MPEG 1');

  { MPEG layer codes }
  MPEG_LAYER_UNKNOWN = 0;                                     { Unknown layer }
  MPEG_LAYER_III = 1;                                             { Layer III }
  MPEG_LAYER_II = 2;                                               { Layer II }
  MPEG_LAYER_I = 3;                                                 { Layer I }

type
  FrameData = record
    Found: Boolean;                                     { True if frame found }
    Position: Integer;                           { Frame position in the file }
    Size: Word;                                          { Frame size (bytes) }
    Xing: Boolean;                                     { True if Xing encoder }
    Data: array [1..4] of Byte;                 { The whole frame header data }
    VersionID: Byte;                                        { MPEG version ID }
    LayerID: Byte;                                            { MPEG layer ID }
    ProtectionBit: Boolean;                        { True if protected by CRC }
    BitRateID: Word;                                            { Bit rate ID }
    SampleRateID: Word;                                      { Sample rate ID }
    PaddingBit: Boolean;                               { True if frame padded }
    PrivateBit: Boolean;                                  { Extra information }
    ModeID: Byte;                                           { Channel mode ID }
    ModeExtensionID: Byte;             { Mode extension ID (for Joint Stereo) }
    CopyrightBit: Boolean;                        { True if audio copyrighted }
    OriginalBit: Boolean;                            { True if original media }
    EmphasisID: Byte;                                           { Emphasis ID }
  end;

function GetCoefficient(const Frame: FrameData): Byte;
function GetBitRate(const Frame: FrameData): Word;
function GetSampleRate(const Frame: FrameData): Word;
function GetPadding(const Frame: FrameData): Byte;
function IsFrameHeader(const HeaderData: array of Byte; Offset: Integer): Boolean;
procedure DecodeHeader(const x: array of Byte; var Frame: FrameData; Offset: Integer);
function GetFrameLength(const Frame: FrameData): Word;

implementation

function GetCoefficient(const Frame: FrameData): Byte;
begin
  { Get frame size coefficient }
  if Frame.VersionID = MPEG_VERSION_1 then
    if Frame.LayerID = MPEG_LAYER_I then Result := 48
    else Result := 144
  else
    if Frame.LayerID = MPEG_LAYER_I then Result := 24
    else if Frame.LayerID = MPEG_LAYER_II then Result := 144
    else Result := 72;
end;

function GetBitRate(const Frame: FrameData): Word;
begin
  { Get bit rate }
  Result := MPEG_BIT_RATE[Frame.VersionID, Frame.LayerID, Frame.BitRateID];
end;

{ ------------------------------------------------------------------------------ }

function GetSampleRate(const Frame: FrameData): Word;
begin
  { Get sample rate }
  Result := MPEG_SAMPLE_RATE[Frame.VersionID, Frame.SampleRateID];
end;

{ ------------------------------------------------------------------------------ }

function GetPadding(const Frame: FrameData): Byte;
begin
  { Get frame padding }
  if Frame.PaddingBit then
    if Frame.LayerID = MPEG_LAYER_I then Result := 4
    else Result := 1
  else Result := 0;
end;

function IsFrameHeader(const HeaderData: array of Byte; Offset: Integer): Boolean;
begin
  { Check for valid frame header }
  if ((HeaderData[0 + Offset] and $FF) <> $FF) or
    ((HeaderData[1 + Offset] and $E0) <> $E0) or
    (((HeaderData[1 + Offset] shr 3) and 3) = 1) or
    (((HeaderData[1 + Offset] shr 1) and 3) = 0) or
    ((HeaderData[2 + Offset] and $F0) = $F0) or
    ((HeaderData[2 + Offset] and $F0) = 0) or
    (((HeaderData[2 + Offset] shr 2) and 3) = 3) or
    ((HeaderData[3 + Offset] and 3) = 2) then
    Result := false
  else
    Result := true;
end;

procedure DecodeHeader(const x: array of Byte; var Frame: FrameData; Offset: Integer);
var
  HeaderData: array[0..3] of byte;
begin
  HeaderData[0] := x[Offset];
  HeaderData[1] := x[Offset+1];
  HeaderData[2] := x[Offset+2];
  HeaderData[3] := x[Offset+3];

  { Decode frame header data }
  Move(HeaderData, Frame.Data, SizeOf(Frame.Data));

  Frame.VersionID := (HeaderData[1 + Offset] shr 3) and 3;
  Frame.LayerID := (HeaderData[1 + Offset] shr 1) and 3;
  Frame.ProtectionBit := (HeaderData[1 + Offset] and 1) <> 1;
  Frame.BitRateID := HeaderData[2 + Offset] shr 4;
  Frame.SampleRateID := (HeaderData[2 + Offset] shr 2) and 3;
  Frame.PaddingBit := ((HeaderData[2 + Offset] shr 1) and 1) = 1;
  Frame.PrivateBit := (HeaderData[2 + Offset] and 1) = 1;
  Frame.ModeID := (HeaderData[3 + Offset] shr 6) and 3;
  Frame.ModeExtensionID := (HeaderData[3 + Offset] shr 4) and 3;
  Frame.CopyrightBit := ((HeaderData[3 + Offset] shr 3) and 1) = 1;
  Frame.OriginalBit := ((HeaderData[3 + Offset] shr 2) and 1) = 1;
  Frame.EmphasisID := HeaderData[3 + Offset] and 3;
end;

function GetFrameLength(const Frame: FrameData): Word;
var
  Coefficient, BitRate, SampleRate, Padding: Word;
begin
  { Calculate MPEG frame length }
  Coefficient := GetCoefficient(Frame);
  BitRate := GetBitRate(Frame);
  SampleRate := GetSampleRate(Frame);
  Padding := GetPadding(Frame);
  Result := Trunc(Coefficient * BitRate * 1000 / SampleRate) + Padding;
end;

end.
