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
unit CutView;

interface

uses
  SysUtils, Windows, Classes, Controls, StdCtrls, ExtCtrls,
  Graphics, DynBASS, Forms, Math, Generics.Collections, GUIFunctions,
  LanguageObjects;

type
  TPeakEvent = procedure(P, AI, L, R: Integer) of object;

  TWavBufArray = array of SmallInt;

  TWaveEntry = record
    Pos, Len: Cardinal;
    Sec: Double;
    L, R: Word;
  end;
  TWaveEntryArray = array of TWaveEntry;

  TCutState = class
  public
    CutStart, CutEnd: Cardinal;
    CutArrayStart, CutArrayEnd: Cardinal;
  end;

  TScanThread = class(TThread)
  private
    FDecoder: Cardinal;
    FPaintWidth: Cardinal;

    FWaveArray: TWaveEntryArray;

    FBytePos, FArrayPos, FScanL, FScanR: Integer;

    FOnScan: TPeakEvent;
    FOnEndScan: TNotifyEvent;

    procedure SyncScan;
    procedure SyncEndScan;
  protected
    procedure Execute; override;
  public
    constructor Create(Decoder, PaintWidth: Cardinal);
    destructor Destroy; override;

    property OnScan: TPeakEvent read FOnScan write FOnScan;
    property OnEndScan: TNotifyEvent read FOnEndScan write FOnEndScan;
  end;

  TCutView = class(TPaintBox)
  private
    FBuf: TBitmap;
    FTimer: TTimer;
    FStartLine, FEndLine, FPlayLine: Cardinal;

    FPeakColor, FPeakEndColor, FStartColor, FEndColor, FPlayColor: TColor;

    FScanThread: TScanThread;

    FBytesParsed: Int64;
    FArrayIndexParsed: Int64;

    FWaveArray: TWaveEntryArray;

    FAudioStart, FAudioEnd: Cardinal;

    FSourceBytes: Cardinal;
    FDone: Boolean;
    FBytes: Cardinal;
    FSecs: Double;

    FDecoder: Cardinal;
    FPlayer: Cardinal;
    FSync: Cardinal;
    FFilename: string;

    FCutStates: TList<TCutState>;

    function BuildTime(T: Double): string;

    procedure BuildBuffer;

    procedure TimerTimer(Sender: TObject);

    function GetBytes(X: Integer): Int64;

    procedure SetLine(IsStart: Boolean; X: Integer);

    function HTML2Color(const HTML: String): Integer;

    procedure AddCut(A, B, C, D: Cardinal);

    procedure ThreadScan(P, AI, L, R: Integer);
    procedure ThreadEndScan(Sender: TObject);
    procedure ThreadTerminate(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(Filename: string);
    procedure Cut;
    procedure Undo;
    procedure Play;
    procedure Stop;

    procedure Save;
    procedure SaveAs;
    function TranslatePosition(Position: Cardinal): Cardinal;

    property Bytes: Cardinal read FBytes;
    property StartLine: Cardinal read FStartLine;
    property EndLine: Cardinal read FEndLine;

    property AudioStart: Cardinal read FAudioStart;
    property AudioEnd: Cardinal read FAudioEnd;
  end;

  procedure LoopSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;

implementation

{ TScanThread }

constructor TScanThread.Create(Decoder, PaintWidth: Cardinal);
begin
  inherited Create(True);
  FDecoder := Decoder;
  FPaintWidth := PaintWidth;
end;

destructor TScanThread.Destroy;
begin

  inherited;
end;

procedure TScanThread.Execute;
var
  level : DWord;
  peak : array[0..1] of DWORD;
  Position : QWORD;
  Counter : integer;
  bpp: integer;
  last: Cardinal;
  v: double;
  c, lbuf, rbuf, added: cardinal;
  tmp: Cardinal;
begin
  peak[0] := 0;
  peak[1] := 0;
  counter := 0;
  last := 0;
  lbuf := 0;
  rbuf := 0;
  added := 0;
  c := 0;
  tmp := 0;

  SetLength(FWaveArray, 500);

  while BASSChannelIsActive(Fdecoder) = BASS_ACTIVE_PLAYING do
  begin
    if Terminated then
      Exit;

    Position := BASSChannelGetPosition(FDecoder, BASS_POS_BYTE);

    level := BASSChannelGetLevel(Fdecoder); // scan peaks
    if (peak[0]<LOWORD(level)) then
      peak[0]:=LOWORD(level); // set left peak
		if (peak[1]<HIWORD(level)) then
      peak[1]:=HIWORD(level); // set right peak

    if Counter >= Length(FWaveArray) then
    begin
      SetLength(FWaveArray, Length(FWaveArray) + 500);
    end;

    FWaveArray[Counter].Pos := Position;
    if Counter > 0 then
      FWaveArray[Counter - 1].Len := FWaveArray[Counter].Pos - FWaveArray[Counter - 1].Pos;
    FWaveArray[Counter].Sec := BASSChannelBytes2Seconds(FDecoder, Position);
    FWaveArray[Counter].L := Peak[0];
    FWaveArray[Counter].R := Peak[1];

    FBytePos := Position;
    FArrayPos := Counter;
    if Position > 0 then
      if Counter mod 100 = 0 then
        Synchronize(SyncScan);

    Inc(Counter);

    peak[0] := 0;
    peak[1] := 0;
  end;

  if Terminated then
    Exit;

  SetLength(FWaveArray, Counter);

  FWaveArray[High(FWaveArray)].Len := BASSChannelGetLength(FDecoder, BASS_POS_BYTE) - FWaveArray[High(FWaveArray)].Pos;

  BASSStreamFree(FDecoder);

  Synchronize(SyncEndScan);
end;

procedure TScanThread.SyncEndScan;
begin
  if Assigned(FOnEndScan) then
    FOnEndScan(Self);
end;

procedure TScanThread.SyncScan;
begin
  if Assigned(FOnScan) then
    FOnScan(FBytePos, FArrayPos, FScanL, FScanR);
end;

{ TPeakView }

function TCutView.HTML2Color(const HTML: String): Integer;
var
  Offset: Integer;
begin
    // check for leading '#'
    if Copy(HTML, 1, 1) = '#' then
      Offset := 1
    else
      Offset := 0;
    // convert hexa-decimal values to RGB
    Result :=
      Integer(StrToInt('$' + Copy(HTML, Offset + 1, 2))) +
      Integer(StrToInt('$' + Copy(HTML, Offset + 3, 2))) shl 8 +
      Integer(StrToInt('$' + Copy(HTML, Offset + 5, 2))) shl 16;
end;

procedure TCutView.AddCut(A, B, C, D: Cardinal);
var
  CS: TCutState;
begin
  CS := TCutState.Create;
  CS.CutStart := A;
  CS.CutEnd := B;
  CS.CutArrayStart := C;
  CS.CutArrayEnd := D;
  FCutStates.Add(CS);
end;

procedure TCutView.BuildBuffer;
  procedure DrawLine(Bytes, ByteCount: Cardinal; Color: TColor);
  var
    L: Cardinal;
  begin
    L := Trunc((Bytes / ByteCount) * FBuf.Width);

    FBuf.Canvas.Pen.Color := Color;
    FBuf.Canvas.MoveTo(L, 0);
    FBuf.Canvas.LineTo(L, FBuf.Height);

    FBuf.Canvas.Font.Color := clWhite;
    FBuf.Canvas.Brush.Color := clBlack;

    SetBkMode(FBuf.Canvas.Handle, TRANSPARENT);
  end;
  procedure DrawLineText(Bytes, ByteCount, X: Cardinal; Secs: Double);
  var
    L, TW: Cardinal;
    T: Double;
    SecText: string;
  begin
    L := Trunc((Bytes / ByteCount) * FBuf.Width);
    SecText := BuildTime(Secs);
    TW := GetTextWidth(SecText, Canvas.Font);
    if FBuf.Width < L + 4 + TW then
      FBuf.Canvas.TextOut(L - 4 - TW, X, SecText)
    else
      FBuf.Canvas.TextOut(L + 4, X, SecText);
  end;
var
  i, v: Integer;
  v2: Double;
  Last: Cardinal;
  LBuf, RBuf: Cardinal;
  Added: Cardinal;
  HT: Cardinal;
  PP: Integer;
  L: Integer;
  LastCut: TCutState;
  Bytes, ArrayFrom, ByteCount, ArrayTo, CutBytes: Cardinal;
  LineSecBegin, LineSecEnd: Double;
  SecText: string;
begin
  if FCutStates = nil then
    Exit;
  LastCut := nil;

  FBuf.Canvas.Brush.Color := clBlack;
  FBuf.Canvas.FillRect(Rect(0, 0, FBuf.Width, FBuf.Height));

  if Length(FWaveArray) = 0 then
    Exit;

  LBuf := 0;
  RBuf := 0;
  Added := 0;
  Last := 0;

  LineSecBegin := 0;
  LineSecEnd := 0;

  if FCutStates.Count > 0 then
    LastCut := FCutStates[FCutStates.Count - 1];

  // Wieviel Bytes müssen wir malen?
  if LastCut = nil then
  begin
    Bytes := FBytesParsed;
    ByteCount := FBytes;
    ArrayFrom := 0;
    ArrayTo := FArrayIndexParsed;
  end else
  begin
    Bytes := LastCut.CutEnd - LastCut.CutStart;
    if Bytes = 0 then
      Bytes := FBytes
    else
      Bytes := Bytes + FWaveArray[LastCut.CutArrayEnd].Len;
    ByteCount := Bytes;
    ArrayFrom := LastCut.CutArrayStart;
    ArrayTo := LastCut.CutArrayEnd;
    if ArrayTo = 0 then
      ArrayTo := Length(FWaveArray) - 1;
  end;

  // Maximale Zeichenbreite auf Puffer ausrechnen
  v2 := (Bytes / ByteCount) * FBuf.Width;

  for i := ArrayFrom to ArrayTo do
  begin
    v := trunc(((i - ArrayFrom) / (ArrayTo - ArrayFrom)) * v2);

    if v = Last then
    begin
      LBuf := LBuf + FWaveArray[i].L;
      RBuf := RBuf + FWaveArray[i].R;
      Added := Added + 1;
      Continue;
    end else
    begin
      if Added > 0 then
      begin
        LBuf := LBuf div Added;
        RBuf := RBuf div Added;
      end else
      begin
        LBuf := FWaveArray[i].L;
        RBuf := FWaveArray[i].R;
      end;
    end;

    if LineSecBegin = 0 then
      if FWaveArray[i].Pos - FWaveArray[ArrayFrom].Pos > FStartLine then
        LineSecBegin := FWaveArray[i].Sec;
    if LineSecEnd = 0 then
      if FWaveArray[i].Pos - FWaveArray[ArrayFrom].Pos > FEndLine then
        LineSecEnd := FWaveArray[i].Sec;

    ht := FBuf.Height div 2;

    FBuf.Canvas.Pen.Color := FPeakColor;
    FBuf.Canvas.MoveTo(v, ht - 1);
    FBuf.Canvas.LineTo(v, ht - 1 - Trunc((LBuf / 32768) * ht));
    FBuf.Canvas.Pixels[v, ht -1 - Trunc((LBuf / 32768) * ht)] := FPeakEndColor;

    FBuf.Canvas.Pen.Color := FPeakColor;
    FBuf.Canvas.MoveTo(v, ht + 1);
    FBuf.Canvas.LineTo(v, ht + 1 + Trunc((RBuf / 32768) * ht));
    FBuf.Canvas.Pixels[v, ht + 1 + Trunc((RBuf / 32768) * ht)] := FPeakEndColor;

    RBuf := 0;
    LBuf := 0;
    Added := 0;

    Last := v;
  end;

  if (LastCut <> nil) then
  begin
    DrawLine(FStartLine, ByteCount, FStartColor);
    DrawLine(FEndLine, ByteCount, FEndColor);

    if FPlayer > 0 then
    begin
      DrawLine(BASSChannelGetPosition(FPlayer, BASS_POS_BYTE) - LastCut.CutStart, ByteCount, FPlayColor);
    end;

    if LineSecEnd = 0 then
      LineSecEnd := FWaveArray[High(FWaveArray)].Sec;

    DrawLineText(FStartLine, ByteCount, 16, LineSecBegin);
    DrawLineText(FEndLine, ByteCount, 28, LineSecEnd);
  end;

  FBuf.Canvas.Font.Color := clWhite;
  FBuf.Canvas.TextOut(4, 4, BuildTime(FSecs));
end;

function TCutView.BuildTime(T: Double): string;
var
  Hour, Min, Sec, MSec: Word;
begin
  Min := Trunc(T / 60);
  T := T - Trunc(T / 60) * 60;
  Sec := Trunc(T);
  T := T - Trunc(T);
  MSec := Trunc(T * 1000);

  Result := Format('%0.2d:%0.2d.%0.3d', [Min, Sec, MSec]) + ' ' + _('minutes');
end;

constructor TCutView.Create(AOwner: TComponent);
begin
  inherited;

  FDone := False;

  FPeakColor := HTML2Color('3b477e');
  FPeakEndColor := HTML2Color('424e83');
  FStartColor := HTML2Color('ece52b');
  FEndColor := HTML2Color('218030');
  FPlayColor := HTML2Color('c33131');

  FBytesParsed := -1;
  FArrayIndexParsed := -1;

  SetLength(FWaveArray, 0);

  if FBuf = nil then
    FBuf := TBitmap.Create;

  FCutStates := TList<TCutState>.Create;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerTimer;
end;

procedure TCutView.Cut;
var
  i: Integer;
  CF, CT: Int64;
  WA: TWaveEntryArray;
  LastCut: TCutState;
  MaxBytes, P: Cardinal;
begin
  CF := -1;
  CT := -1;

  if FCutStates.Count < 1 then
    Exit;

  LastCut := FCutStates[FCutStates.Count - 1];

  for i := 0 to Length(FWaveArray) - 1 do
  begin
    if FWaveArray[i].Pos > LastCut.CutStart + FStartLine then
    begin
      if i > 0 then
        CF := i - 1
      else
        CF := i;
      Break;
    end;
  end;

  if CF > -1 then
  begin
    for i := CF to Length(FWaveArray) - 1 do
    begin
      if FWaveArray[i].Pos > LastCut.CutStart + FEndLine then
      begin
        CT := i - 1;
        Break;
      end;
    end;
  end;

  if FCutStates.Count > 1 then
  begin
    MaxBytes := FCutStates[FCutStates.Count - 1].CutEnd - FCutStates[FCutStates.Count - 1].CutStart;
  end else
    MaxBytes := FBytes;

  if ((CF > -1) and (CT > -1)) and (CF < CT) and ((FWaveArray[CF].Pos > 0) or (FWaveArray[CT].Pos < MaxBytes)) then
  begin
    FStartLine := 0;
    FEndLine := FWaveArray[CT].Pos - FWaveArray[CF].Pos;

    AddCut(FWaveArray[CF].Pos, FWaveArray[CT].Pos, CF, CT);
    FSecs := FWaveArray[CT].Sec - FWaveArray[CF].Sec;

    {
    LastCut := FCutStates[FCutStates.Count - 1];
    if FPlayer > 0 then
    begin
      P := BASSChannelGetPosition(FPlayer, BASS_POS_BYTE);
      if (P < FWaveArray[CF].Pos) or
         (P > FWaveArray[CT].Pos) then
      begin
        BASSChannelStop(FPlayer);
      end;
    end;
    }

    BuildBuffer;
    Paint;
  end;
end;

destructor TCutView.Destroy;
var
  i: Integer;
begin
  if FScanThread <> nil then
  begin
    FScanThread.Terminate;
    while FScanThread <> nil do
    begin
      Application.ProcessMessages;
    end;
  end;

  for i := 0 to FCutStates.Count - 1 do
    FCutStates[i].Free;

  if FPlayer > 0 then
    BASSStreamFree(FPlayer);

  FCutStates.Free;
  FBuf.Free;
  inherited;
end;

function TCutView.GetBytes(X: Integer): Int64;
var
  D: Double;
  LastCut: TCutState;
  F, T: Cardinal;
begin
  LastCut := FCutStates[FCutStates.Count - 1];

  if LastCut.CutEnd = 0 then
  begin
    F := 0;
    T := FBytes;
  end else
  begin
    F := LastCut.CutStart;
    T := LastCut.CutEnd;
  end;

  D := (X / FBuf.Width) * (T - F);
  Result := Trunc(D);
end;

procedure TCutView.LoadFile(Filename: string);
var
  x: TFileStream;
begin
  FDone := False;

  FFilename := Filename;

  x := TFileStream.Create(Filename, fmOpenRead);
  FSourceBytes := x.Size;
  x.Free;

  FDecoder := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN} {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});
  // TODO: Fehlerbehandlung? BASSFree() auf den Decoder?

  FStartLine := 0;
  FEndLine := 0;

  FAudioStart := BASSStreamGetFilePosition(FDecoder, BASS_FILEPOS_START);
  FAudioEnd := BASSStreamGetFilePosition(FDecoder, BASS_FILEPOS_END);
  FBytes := BASSChannelGetLength(FDecoder, BASS_POS_BYTE);

  FSecs := BASSChannelBytes2Seconds(FDecoder, FBytes);

  FScanThread := TScanThread.Create(FDecoder, FBuf.Width);
  FScanThread.FreeOnTerminate := True;
  FScanThread.OnTerminate := ThreadTerminate;
  FScanThread.OnScan := ThreadScan;
  FScanThread.OnEndScan := ThreadEndScan;

  FScanThread.Resume;
end;

procedure TCutView.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if not FDone then
    Exit;

  case Button of
    mbLeft:
      begin
        SetLine(True, X);
      end;
    mbRight:
      begin
        SetLine(False, X);
      end;
  end;
end;

procedure TCutView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if not FDone then
    Exit;

  if ssLeft in Shift then
  begin
    SetLine(True, X);
  end else if ssRight in Shift then
  begin
    SetLine(False, X);
  end;
end;

procedure TCutView.Paint;
begin
  inherited;

  Canvas.Draw(0, 0, FBuf);
end;

procedure TCutView.Play;
var
  LastCut: TCutState;
begin
  if FCutStates.Count = 0 then
    Exit;

  if not (FStartLine < FEndLine) then
    Exit;

  LastCut := FCutStates[FCutStates.Count - 1];

  if FPlayer > 0 then
    BASSChannelStop(FPlayer)
  else
    // TODO: Fehlerbehandlung für die BASS funktionen gibt es faktisch nicht, nirgendwo. FAiL.
    FPlayer := BASSStreamCreateFile(False, PChar(FFilename), 0, 0, {$IFDEF UNICODE}BASS_UNICODE{$ENDIF});

  BASSChannelSetPosition(FPlayer, LastCut.CutStart + FStartLine, BASS_POS_BYTE);
  FPlayLine := FStartLine;
  BASSChannelPlay(FPlayer, False);
  if FSync > 0 then
    BASSChannelRemoveSync(FPlayer, FSync);
  FSync := BASSChannelSetSync(FPlayer, BASS_SYNC_POS or BASS_SYNC_MIXTIME, LastCut.CutStart + FEndLine, LoopSyncProc, Self);
end;

procedure TCutView.Stop;
begin
  if FPlayer > 0 then
    BASSChannelStop(FPlayer);
end;

procedure TCutView.Save;
var
  SaveFrom, SaveLen: Cardinal;
  PlayFrom: Cardinal;
  FIn: TMemoryStream;
  FOut: TFileStream;
  LastCut: TCutState;
  i: Integer;
  F, T: Cardinal;
  WA: TWaveEntryArray;
begin
  if FCutStates.Count = 0 then
    Exit;

  LastCut := FCutStates[FCutStates.Count - 1];

  PlayFrom := 0;
  SaveFrom := TranslatePosition(FCutStates[FCutStates.Count - 1].CutStart);
  SaveLen := TranslatePosition(FCutStates[FCutStates.Count - 1].CutEnd) - TranslatePosition(FCutStates[FCutStates.Count - 1].CutStart);

  if SaveLen < 10 then
    Exit;

  // TODO: Beim cutten sind die frames nicht komplett oder? was ist mit den headern wegen tags etc?
  if FPlayer > 0 then
  begin
    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
    begin
      PlayFrom := BASSChannelGetPosition(FPlayer, BASS_POS_BYTE);// - TranslatePosition(FCutStates[FCutStates.Count - 1].CutStart);
      BASSStreamFree(FPlayer);
    end;
  end;

  FIn := TMemoryStream.Create;
  try
    FIn.LoadFromFile(FFilename);
    FIn.Seek(SaveFrom, soFromBeginning);


    try
      FOut := TFileStream.Create('z:\xxx.mp3', fmCreate);
      FOut.CopyFrom(FIn, SaveLen);
    finally
      FOut.Free;
    end;
  finally
    FIn.Free;
  end;

  F := 0;
  T := 0;
  for i := 0 to Length(FWaveArray) - 1 do
  begin
    if (F = 0) and (FWaveArray[i].Pos > FCutStates[FCutStates.Count - 1].CutStart) then
      F := i;
    if FWaveArray[i].Pos > FCutStates[FCutStates.Count - 1].CutEnd then
      T := i;
    if (F > 0) and (T > 0) then
      Break;
  end;

  for i := 0 to FCutStates.Count - 1 do
    FCutStates[i].Free;
  FCutStates.Clear;

  SetLength(WA, T - F);
  for i := F to T - 1 do
  begin
    WA[i - F] := FWaveArray[i];
    WA[i - F].Pos := FWaveArray[i].Pos - FWaveArray[F].Pos; // FCutStates[FCutStates.Count - 1].CutStart;
    WA[i - F].Sec := FWaveArray[i].Sec - FWaveArray[F].Sec;
  end;
  FWaveArray := WA;
  AddCut(0, 0, 0, 0);
  FBytes := FWaveArray[High(FWaveArray)].Pos + FWaveArray[High(FWaveArray)].Len;
  FSecs := FWaveArray[High(FWaveArray)].Sec;

  if PlayFrom > 0 then
  begin
    FPlayer := BASSStreamCreateFile(False, PChar(FFilename), 0, 0, {$IFDEF UNICODE}BASS_UNICODE{$ENDIF});

    BASSChannelSetPosition(FPlayer, PlayFrom, BASS_POS_BYTE);
    BASSChannelPlay(FPlayer, False);
    if FSync > 0 then
      BASSChannelRemoveSync(FPlayer, FSync);
    FSync := BASSChannelSetSync(FPlayer, BASS_SYNC_POS or BASS_SYNC_MIXTIME, LastCut.CutStart + FEndLine, LoopSyncProc, Self);
  end;

  BuildBuffer;
  Paint;
end;

procedure TCutView.SaveAs;
begin

end;

procedure TCutView.Resize;
begin
  inherited;

  if FBuf <> nil then
    FBuf.Free;
  FBuf := TBitmap.Create;
  FBuf.Height := ClientHeight;
  FBuf.Width := ClientWidth;

  BuildBuffer;
end;

procedure TCutView.SetLine(IsStart: Boolean; X: Integer);
var
  B: Int64;
  P: Cardinal;
  LastCut: TCutState;
begin
  if FCutStates.Count = 0 then
    Exit;

  LastCut := FCutStates[FCutStates.Count - 1];

  B := GetBytes(X);

  if IsStart then
  begin
    if B < 0 then
      B := 0;
    if B > FBytes then
      B := FBytes - 1;
    FStartLine := B;
    if (FEndLine <= FStartLine) and (FEndLine > 0) then
      FEndLine := B;
  end else
  begin
    if B <= 0 then
      B := 0;
    if B > FBytes then
      B := FBytes;
    FEndLine := B;
    if FStartLine >= FEndLine then
      FStartLine := B;
  end;

  if FPlayer > 0 then
  begin
    p := BASSChannelGetPosition(FPlayer, BASS_POS_BYTE);
    if (LastCut.CutStart + FStartLine > P) or
       (LastCut.CutStart + FEndLine < P) then
    begin
      BASSChannelStop(FPlayer);
    end else
    begin
      if not IsStart then
      begin
        if FPlayer > 0 then
        begin
          if FSync > 0 then
            BASSChannelRemoveSync(FPlayer, FSync);
          FSync := BASSChannelSetSync(FPlayer, BASS_SYNC_POS or BASS_SYNC_MIXTIME, LastCut.CutStart + FEndLine, LoopSyncProc, Self);
        end;
      end;
    end;
  end;

  BuildBuffer;
  Paint;
end;

procedure TCutView.ThreadEndScan(Sender: TObject);
begin
  FBytesParsed := FScanThread.FBytePos;
  FArrayIndexParsed := FScanThread.FArrayPos;

  FEndLine := FBytesParsed;

  FWaveArray := FScanThread.FWaveArray;

  AddCut(0, FBytes, 0, High(FWaveArray));
  //BuildBuffer;

  FDone := True;
  //Paint;
end;

procedure TCutView.ThreadScan(P, AI, L, R: Integer);
begin
  // TOOD: asdf.
  FWaveArray := FScanThread.FWaveArray;

  FBytesParsed := P;
  FArrayIndexParsed := AI;

  BuildBuffer;
  Paint;
end;

procedure TCutView.ThreadTerminate(Sender: TObject);
begin
  FScanThread := nil;
end;

procedure TCutView.TimerTimer(Sender: TObject);
var
  P: Cardinal;
  LastCut: TCutState;
begin
  if (FPlayer > 0) and (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING) then
  begin
    {
    if FCutStates.Count > 1 then
    begin
      LastCut := FCutStates[FCutStates.Count - 1];
      P := BASSChannelGetPosition(FPlayer, BASS_POS_BYTE);
      if (P < LastCut.CutStart + FStartLine) or
         (P > LastCut.CutStart + FEndLine) then
      begin
        BASSChannelStop(FPlayer);
      end;
    end;
    }
    BuildBuffer;
    Paint;
  end;
end;

function TCutView.TranslatePosition(Position: Cardinal): Cardinal;
var
  i: Integer;
begin
  Result := Trunc((Position / FBytes) * FSourceBytes);
end;

procedure TCutView.Undo;
var
  CuttedFrom, CuttedTo, LastSize: Cardinal;
begin
  if FCutStates.Count > 1 then
  begin
    CuttedFrom := FCutStates[FCutStates.Count - 1].CutStart;
    CuttedTo := FCutStates[FCutStates.Count - 1].CutEnd;
    if CuttedTo = 0 then
    begin
      CuttedFrom := 0;
      CuttedTo := FBytes;
    end;
    LastSize := FCutStates[FCutStates.Count - 2].CutEnd - FCutStates[FCutStates.Count - 2].CutStart;

    FCutStates[FCutStates.Count - 1].Free;
    FCutStates.Delete(FCutStates.Count - 1);

    FStartLine := Trunc((CuttedFrom / LastSize) * (FCutStates[FCutStates.Count - 1].CutEnd - FCutStates[FCutStates.Count - 1].CutStart));
    FEndLine := Trunc((CuttedTo / LastSize) * (FCutStates[FCutStates.Count - 1].CutEnd - FCutStates[FCutStates.Count - 1].CutStart));
    FSecs := FWaveArray[FCutStates[FCutStates.Count - 1].CutArrayEnd].Sec - FWaveArray[FCutStates[FCutStates.Count - 1].CutArrayEnd].Sec;

    BuildBuffer;
    Paint;
  end;
end;

procedure LoopSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  PV: TCutView;
begin
  PV := TCutView(user);
  BASSChannelStop(PV.FPlayer);
end;

end.
