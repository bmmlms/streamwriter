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
  SysUtils, Windows, Classes, Controls, StdCtrls, ExtCtrls, Functions,
  Graphics, DynBASS, Forms, Math, Generics.Collections, GUIFunctions,
  LanguageObjects, WaveData, Messages;

type
  TPeakEvent = procedure(P, AI, L, R: Integer) of object;

  TWavBufArray = array of SmallInt;

  TCutView = class;

  TScanThread = class(TThread)
  private
    FFilename: string;
    FWaveData: TWaveData;

    FOnScan: TPeakEvent;
    FOnEndScan: TNotifyEvent;

    procedure SyncScan;
    procedure SyncEndScan;
  protected
    procedure Execute; override;
  public
    constructor Create(Filename: string);
    destructor Destroy; override;

    property OnScan: TPeakEvent read FOnScan write FOnScan;
    property OnEndScan: TNotifyEvent read FOnEndScan write FOnEndScan;
  end;

  TCutPaintBox = class(TPaintBox)
  private
    FBuf: TBitmap;
    FCutView: TCutView;
    FTimer: TTimer;
    FPlayingIndex: Cardinal;

    FPeakColor, FPeakEndColor, FStartColor, FEndColor, FPlayColor: TColor;
    FStartLine, FEndLine, FPlayLine: Cardinal;

    procedure BuildBuffer;
    procedure SetLine(IsStart: Boolean; X: Integer);
    function PixelsToArray(X: Integer): Integer;
    function GetPlayerPos: Cardinal;

    procedure TimerTimer(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCutView = class(TPanel)
  private
    FScanThread: TScanThread;
    FPB: TCutPaintBox;
    FWaveData: TWaveData;

    //FDecoder: Cardinal;
    FPlayer: Cardinal;
    FSync: Cardinal;
    FFilename: string;

    procedure ThreadScan(P, AI, L, R: Integer);
    procedure ThreadEndScan(Sender: TObject);
    procedure ThreadTerminate(Sender: TObject);

    procedure MsgRefresh(var Msg: TMessage); message WM_USER + 123;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(Filename: string);

    procedure Cut;
    procedure Undo;
    procedure Save;
    procedure SaveAs;
    procedure Play;
    procedure Stop;
    procedure AutoCut(MaxPeaks: Cardinal; MinDuration: Cardinal);
  end;

  procedure LoopSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;

implementation

{ TScanThread }

constructor TScanThread.Create(Filename: string);
begin
  inherited Create(True);
  FFilename := Filename;
  FWaveData := TWaveData.Create;
end;

destructor TScanThread.Destroy;
begin
  //FWaveData.Free;
  inherited;
end;

procedure TScanThread.Execute;
var
  i: Integer;
  Level: DWord;
  PeakL, PeakR: DWord;
  Position: QWORD;
  Counter, c2: Cardinal;
begin
  FWaveData.LoadFile(FFilename);
  Synchronize(SyncEndScan);
end;

procedure TScanThread.SyncEndScan;
begin
  if Assigned(FOnEndScan) then
    FOnEndScan(Self);
end;

procedure TScanThread.SyncScan;
begin
  {
  if Assigned(FOnScan) then
    FOnScan(FBytePos, FArrayPos, FScanL, FScanR);
  }
end;

{ TCutView }

constructor TCutView.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FPB := TCutPaintBox.Create(Self);
  FPB.Parent := Self;
  FPB.Align := alClient;

  //FWaveData := TWaveData.Create;
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

  FWaveData.Free;

  if FPlayer > 0 then
    BASSStreamFree(FPlayer);

  inherited;
end;

procedure TCutView.LoadFile(Filename: string);
var
  x: TFileStream;
begin
  if FScanThread <> nil then
    Exit;

  FFilename := Filename;

  //x := TFileStream.Create(Filename, fmOpenRead);
  //FSourceBytes := x.Size;
  //x.Free;

  //FDecoder := BASSStreamCreateFile(False, PChar(Filename), 0, 0, BASS_STREAM_DECODE {or BASS_STREAM_PRESCAN} {$IFDEF UNICODE} or BASS_UNICODE{$ENDIF});
  // TODO: Fehlerbehandlung? BASSFree() auf den Decoder?

  {
  FStartLine := 0;
  FEndLine := 0;

  FAudioStart := BASSStreamGetFilePosition(FDecoder, BASS_FILEPOS_START);
  FAudioEnd := BASSStreamGetFilePosition(FDecoder, BASS_FILEPOS_END);
  FBytes := BASSChannelGetLength(FDecoder, BASS_POS_BYTE);

  FSecs := BASSChannelBytes2Seconds(FDecoder, FBytes);
  }

  FScanThread := TScanThread.Create(Filename);
  FScanThread.FreeOnTerminate := True;
  FScanThread.OnTerminate := ThreadTerminate;
  FScanThread.OnScan := ThreadScan;
  FScanThread.OnEndScan := ThreadEndScan;

  FPB.BuildBuffer;
  FPB.Paint;

  FScanThread.Resume;
end;

procedure TCutView.MsgRefresh(var Msg: TMessage);
begin
  FPB.BuildBuffer;
  FPB.Paint;
end;

procedure TCutView.Cut;
begin
  if FWaveData = nil then
    Exit;

  if FPB.FEndLine - FPB.FStartLine <= 0 then
    Exit;

  if (FWaveData.CutStart = FPB.FStartLine) and (FWaveData.CutEnd = FPB.FEndLine) then
    Exit;

  FWaveData.Cut(FPB.FStartLine, FPB.FEndLine);

  FPB.BuildBuffer;
  FPB.Paint;
end;

procedure TCutView.Undo;
begin
  if (FWaveData = nil) or (FWaveData.CutStates.Count = 1) then
    Exit;

  FWaveData.CutStates[FWaveData.CutStates.Count - 1].Free;
  FWaveData.CutStates.Delete(FWaveData.CutStates.Count - 1);

  FPB.BuildBuffer;
  FPB.Paint;
end;

procedure TCutView.Save;
begin

end;

procedure TCutView.SaveAs;
begin

end;

procedure TCutView.Play;
begin
  if FWaveData = nil then
    Exit;

  if not (FPB.FStartLine < FPB.FEndLine) then
    Exit;

  if FWaveData.TimeBetween(FPB.FStartLine, FPB.FEndLine) < 1 then
    Exit;

  FPB.FPlayingIndex := 0;

  if FPlayer = 0 then
    // TODO: Fehlerbehandlung für die BASS funktionen gibt es faktisch nicht, nirgendwo. FAiL.
    FPlayer := BASSStreamCreateFile(False, PChar(FFilename), 0, 0, {$IFDEF UNICODE}BASS_UNICODE{$ENDIF});

  BASSChannelSetPosition(FPlayer, FWaveData.WaveArray[FPB.FStartLine].Pos, BASS_POS_BYTE);
  BASSChannelPlay(FPlayer, False);
  if FSync > 0 then
    BASSChannelRemoveSync(FPlayer, FSync);
  FSync := BASSChannelSetSync(FPlayer, BASS_SYNC_POS or BASS_SYNC_MIXTIME, FWaveData.WaveArray[FPB.FEndLine].Pos, LoopSyncProc, Self);
end;

procedure TCutView.Stop;
begin
  if FPlayer > 0 then
    BASSChannelStop(FPlayer);
  FPB.BuildBuffer;
  FPB.Paint;
end;

procedure TCutView.AutoCut(MaxPeaks: Cardinal; MinDuration: Cardinal);
begin
  if FWaveData = nil then
    Exit;

  FWaveData.AutoCut(MaxPeaks, MinDuration);

  FPB.BuildBuffer;
  FPB.Repaint;
end;

procedure TCutView.ThreadEndScan(Sender: TObject);
begin
  // TODO: Evtl nix machen, wenn das array leer oder sehr sehr klein ist oder so.

  if FWaveData <> nil then
    FWaveData.Free;
  FWaveData := FScanThread.FWaveData;


  FPB.FStartLine := 0;
  FPB.FEndLine := High(FWaveData.WaveArray);

  FPB.BuildBuffer;
  FPB.Repaint;
end;

procedure TCutView.ThreadScan(P, AI, L, R: Integer);
begin

end;

procedure TCutView.ThreadTerminate(Sender: TObject);
begin
  FScanThread := nil;
end;

procedure LoopSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  PV: TCutView;
begin
  PV := TCutView(user);
  BASSChannelStop(PV.FPlayer);
  BASSChannelRemoveSync(Channel, Handle);

  PostMessage(PV.Handle, WM_USER + 123, 0, 0);
end;

{ TCutPaintBox }

procedure TCutPaintBox.BuildBuffer;
  procedure DrawLine(ArrayIdx: Cardinal; Color: TColor);
  var
    L: Cardinal;
  begin
    L := Trunc(((ArrayIdx - FCutView.FWaveData.CutStart) / FCutView.FWaveData.CutSize) * FBuf.Width);

    FBuf.Canvas.Pen.Color := Color;
    FBuf.Canvas.MoveTo(L, 0);
    FBuf.Canvas.LineTo(L, FBuf.Height);

    FBuf.Canvas.Brush.Color := clBlack;
  end;
  function BuildTime(T: Double): string;
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
  procedure DrawLineText(ArrayIdx, X: Cardinal);
  var
    L: Cardinal;
    TS: TSize;
    SecText: string;
  begin
    L := Trunc(((ArrayIdx - FCutView.FWaveData.CutStart) / FCutView.FWaveData.CutSize) * FBuf.Width);
    SecText := BuildTime(FCutView.FWaveData.WaveArray[ArrayIdx].Sec - FCutView.FWaveData.WaveArray[FCutView.FWaveData.CutStart].Sec);
    FBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FBuf.Canvas.Handle, TRANSPARENT);
    TS := GetTextSize(SecText, Canvas.Font);
    if FBuf.Width < L + 4 + TS.cx then
      FBuf.Canvas.TextOut(L - 4 - TS.cx, X, SecText)
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
  TS: TSize;
  PP: Integer;
  L: Integer;
  Txt: string;
  Bytes, ArrayFrom, ByteCount, ArrayTo, CutBytes: Cardinal;
  PeakColor: TColor;
  L1, L2: Cardinal;
  CS, CE: Cardinal;
begin
  FBuf.Canvas.Brush.Color := clBlack;
  FBuf.Canvas.FillRect(Rect(0, 0, FBuf.Width, FBuf.Height));

  if (ClientHeight < 2) or (ClientWidth < 2) then
    Exit;

  if (FCutView.FWaveData = nil) or (FCutView.FScanThread <> nil) then
  begin
    Txt := _('Loading file...');
    TS := GetTextSize(Txt, Canvas.Font);
    FBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FBuf.Canvas.Handle, TRANSPARENT);
    FBuf.Canvas.TextOut(FBuf.Width div 2 - TS.cx div 2, FBuf.Height div 2 - TS.cy, Txt);
  end;

  if (FCutView = nil) or (FCutView.FWaveData = nil) then
    Exit;

  if Length(FCutView.FWaveData.WaveArray) = 0 then
    Exit;


  ht := FBuf.Height div 2;

  for i := 0 to FCutView.FWaveData.Silence.Count - 1 do
  begin
    CS := FCutView.FWaveData.Silence[i].CutStart;
    CE := FCutView.FWaveData.Silence[i].CutEnd;

    if (CS < FCutView.FWaveData.CutStart) and (CE < FCutView.FWaveData.CutStart) then
      Continue;
    if CS > FCutView.FWaveData.CutEnd then
      Continue;

    if CS < FCutView.FWaveData.CutStart then
      CS := FCutView.FWaveData.CutStart;
    if CE > FCutView.FWaveData.CutEnd then
      CE := FCutView.FWaveData.CutEnd;

    L1 := Floor(((CS - FCutView.FWaveData.CutStart) / FCutView.FWaveData.CutSize) * FBuf.Width);
    L2 := Ceil(((CE - FCutView.FWaveData.CutStart) / FCutView.FWaveData.CutSize) * FBuf.Width);

    if L2 - L1 <= 2 then
      Inc(L2);

    FBuf.Canvas.Brush.Color := clGray;
    FBuf.Canvas.FillRect(Rect(L1, 0, L2, ht - 1));
    FBuf.Canvas.FillRect(Rect(L1, ht + 1, L2, FBuf.Height));
  end;



  LBuf := 0;
  RBuf := 0;
  Added := 0;
  Last := 0;

  ArrayFrom := FCutView.FWaveData.CutStart;
  ArrayTo := FCutView.FWaveData.CutEnd;

  v2 := (1 / 1) * FBuf.Width;

  for i := ArrayFrom to ArrayTo do
  begin
    v := trunc(((i - ArrayFrom) / (ArrayTo - ArrayFrom)) * v2);

    if v = Last then
    begin
      LBuf := LBuf + FCutView.FWaveData.WaveArray[i].L;
      RBuf := RBuf + FCutView.FWaveData.WaveArray[i].R;
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
        LBuf := FCutView.FWaveData.WaveArray[i].L;
        RBuf := FCutView.FWaveData.WaveArray[i].R;
      end;
    end;

    FBuf.Canvas.Pen.Color := FPeakColor;

    FBuf.Canvas.MoveTo(v, ht - 1);
    FBuf.Canvas.LineTo(v, ht - 1 - Trunc((LBuf / 33000) * ht));
    FBuf.Canvas.Pixels[v, ht -1 - Trunc((LBuf / 33000) * ht)] := FPeakEndColor;

    FBuf.Canvas.MoveTo(v, ht + 1);
    FBuf.Canvas.LineTo(v, ht + 1 + Trunc((RBuf / 33000) * ht));
    FBuf.Canvas.Pixels[v, ht + 1 + Trunc((RBuf / 33000) * ht)] := FPeakEndColor;

    RBuf := 0;
    LBuf := 0;
    Added := 0;

    Last := v;
  end;

  DrawLine(FStartLine, FStartColor);
  DrawLine(FEndLine, FEndColor);

  DrawLineText(FStartLine, 16);
  DrawLineText(FEndLine, 28);

  if BASSChannelIsActive(FCutView.FPlayer) = BASS_ACTIVE_PLAYING then
  begin
    DrawLine(FPlayingIndex, FPlayColor);
    DrawLineText(FPlayingIndex, 40);
  end;

  {
  for i := 0 to FCutView.FWaveData.Silence.Count - 1 do
  begin
    if (FCutView.FWaveData.CutStart <= FCutView.FWaveData.Silence[i].CutStart) and
       (FCutView.FWaveData.CutEnd >= FCutView.FWaveData.Silence[i].CutEnd) then
    begin
      DrawLine(FCutView.FWaveData.Silence[i].CutStart, clPurple);
      DrawLine(FCutView.FWaveData.Silence[i].CutEnd, clPurple);
    end;
  end;
  }

  FBuf.Canvas.Font.Color := clWhite;
  FBuf.Canvas.TextOut(4, 4, BuildTime(FCutView.FWaveData.Secs));
end;

constructor TCutPaintBox.Create(AOwner: TComponent);
begin
  inherited;

  FPeakColor := HTML2Color('3b477e');
  FPeakEndColor := HTML2Color('424e83');
  FStartColor := HTML2Color('ece52b');
  FEndColor := HTML2Color('218030');
  FPlayColor := HTML2Color('c33131');

  FCutView := TCutView(AOwner);
  if FBuf = nil then
    FBuf := TBitmap.Create;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerTimer;
end;

destructor TCutPaintBox.Destroy;
begin
  FBuf.Free;
  inherited;
end;

procedure TCutPaintBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FCutView.FWaveData = nil then
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

procedure TCutPaintBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FCutView.FWaveData = nil then
    Exit;

  if ssLeft in Shift then
  begin
    SetLine(True, X);
  end else if ssRight in Shift then
  begin
    SetLine(False, X);
  end;
end;

procedure TCutPaintBox.Paint;
begin
  inherited;

  Canvas.Draw(0, 0, FBuf);
end;

procedure TCutPaintBox.Resize;
begin
  inherited;

  if FBuf = nil then
    Exit;

  FBuf.Width := ClientWidth;
  FBuf.Height := ClientHeight;

  BuildBuffer;
end;

function TCutPaintBox.PixelsToArray(X: Integer): Integer;
begin
  Result := FCutView.FWaveData.CutStart + Ceil((X / FBuf.Width) * (FCutView.FWaveData.CutSize));

  if Result > FCutView.FWaveData.CutEnd then
    Result := FCutView.FWaveData.CutEnd;
  if Result < FCutView.FWaveData.CutStart then
    Result := FCutView.FWaveData.CutStart;
end;

function TCutPaintBox.GetPlayerPos: Cardinal;
var
  i: Integer;
  SearchFrom, BytePos: Cardinal;
begin
  Result := 0;
  BytePos := BASSChannelGetPosition(FCutView.FPlayer, BASS_POS_BYTE);

  SearchFrom := 0;
  if BytePos > FCutView.FWaveData.WaveArray[FPlayingIndex].Pos then
  begin
    SearchFrom := FPlayingIndex;
  end;

  for i := SearchFrom to FCutView.FWaveData.CutEnd do
  begin
    if FCutView.FWaveData.WaveArray[i].Pos >= BytePos then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TCutPaintBox.SetLine(IsStart: Boolean; X: Integer);
var
  P, ArrayPos: Cardinal;
begin
  ArrayPos := PixelsToArray(X);

  if ArrayPos < FCutView.FWaveData.CutStart then
    ArrayPos := FCutView.FWaveData.CutStart;
  if ArrayPos > FCutView.FWaveData.CutEnd then
    ArrayPos := FCutView.FWaveData.CutEnd;

  if IsStart then
  begin
    // TODO: Dieses - 1 und unten Idx auf 1 setzen könnte bei leeren mp3s failen. crashed wenn man ganz doll reinzoomed/cuttet und beide linien nach maximal links schieben will.
    FStartLine := ArrayPos;
    if FStartLine >= FCutView.FWaveData.CutEnd then
      FStartLine := FCutView.FWaveData.CutEnd - 1;
    if FEndLine <= FStartLine then
      FEndLine := FStartLine + 1;
  end else
  begin
    FEndLine := ArrayPos;
    if FEndLine <= FCutView.FWaveData.CutStart then
      FEndLine := FCutView.FWaveData.CutStart + 1;
    if FStartLine >= FEndLine then
      FStartLine := FEndLine - 1;
  end;

  if FCutView.FPlayer > 0 then
  begin
    P := BASSChannelGetPosition(FCutView.FPlayer, BASS_POS_BYTE);
    if (FCutView.FWaveData.WaveArray[FStartLine].Pos > P) or // + FCutView.FWaveData.FWaveArray[FStartLine].Pos > P) or
       (FCutView.FWaveData.WaveArray[FEndLine].Pos < P) then // + FCutView.FWaveData.FWaveArray[FEndLine].Pos < P) then
    begin
      BASSChannelStop(FCutView.FPlayer);
    end else
    begin
      if not IsStart then
      begin
        if FCutView.FSync > 0 then
          BASSChannelRemoveSync(FCutView.FPlayer, FCutView.FSync);
        FCutView.FSync := BASSChannelSetSync(FCutView.FPlayer, BASS_SYNC_POS or BASS_SYNC_MIXTIME, FCutView.FWaveData.WaveArray[FEndLine].Pos, LoopSyncProc, FCutView);
      end;
    end;
  end;

  BuildBuffer;
  Paint;
end;

procedure TCutPaintBox.TimerTimer(Sender: TObject);
begin
  if (FCutView.FPlayer > 0) and (BASSChannelIsActive(FCutView.FPlayer) = BASS_ACTIVE_PLAYING) then
  begin
    FPlayingIndex := GetPlayerPos;
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

end.
