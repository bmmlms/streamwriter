{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

    Portions created by Ralf Kruse

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
  LanguageObjects, WaveData, Messages, ComCtrls, AppData, Player,
  PlayerManager, Plugins, SoX, DownloadAddons, ConfigureSoX, Logging,
  MsgDlg, DragDrop, DropTarget, DropComboTarget, Mp3FileUtils,
  MessageBus, AppMessages;

type
  TPeakEvent = procedure(P, AI, L, R: Integer) of object;
  TWavBufArray = array of SmallInt;
  TCutStates = (csReady, csLoading, csWorking, csError);

  TCutView = class;

  TProcessThread = class(TThread)
  private
    FCommandLine: string;
    FWorkingDir: string;
    FFilePath: string;
    FTempFile: string;
    FProcessOutput: AnsiString;

    FOnSuccess: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure SyncSuccess;
    procedure SyncError;
  protected
    procedure Execute; override;
  public
    constructor Create(CommandLine, WorkingDir, FilePath, TempFile: string);
    destructor Destroy; override;

    property TempFile: string read FTempFile;
    property ProcessOutput: AnsiString read FProcessOutput;

    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  TScanThread = class(TThread)
  private
    FFilename: string;
    FWaveData: TWaveData;

    FOnScanProgress: TNotifyEvent;
    FOnEndScan: TNotifyEvent;
    FOnScanError: TNotifyEvent;

    procedure SyncScanProgress;
    procedure SyncEndScan;
    procedure SyncScanError;

    procedure WaveDataScanProgress(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(Filename: string);
    destructor Destroy; override;

    property OnScanProgress: TNotifyEvent read FOnScanProgress write FOnScanProgress;
    property OnEndScan: TNotifyEvent read FOnEndScan write FOnEndScan;
    property OnScanError: TNotifyEvent read FOnScanError write FOnScanError;
  end;

  TMouseMode = (mmDown, mmMove, mmUp);
  TControlMode = (cmNone, cmView, cmScroll);

  PMouseButton = ^TMouseButton;

  TCutPaintBox = class(TCustomControl)
  const
    ScrollbarHeight = 8;
    MinimumDisplayedSampleCount = 30;
  private
    FWaveBuf: TBitmap;
    FDrawBuf: TBitmap;

    FCutView: TCutView;
    FTimer: TTimer;
    FPlayingIndex: Cardinal;
    FPlayerPaused: Boolean;

    FPeakColor, FPeakEndColor, FStartColor, FEndColor, FPlayColor, FZoomOuterColor, FZoomInnerColor: TColor;
    FStartLine, FEndLine, FPlayLine, FZoomStartLine, FZoomEndLine, FEffectStartLine, FEffectEndLine: Cardinal;
    FDoZoom: Boolean;

    FMouseOldX, FMouseOldY, FMouseMoveStartX: integer;
    FControlMode: TControlMode;

    procedure BuildBuffer;
    procedure BuildDrawBuffer;

    procedure SetLine(X: Integer; Button: TMouseButton; Mode: TMouseMode);
    procedure HandleScrollBar(X: Integer; Y: Integer; Button: PMouseButton;
      Mode: TMouseMode);
    function GetControlMode(Y: Integer): TControlMode;
    function PixelsToArray(X: Integer): Cardinal;
    function GetPlayerPos: Cardinal;

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

    procedure TimerTimer(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TUndoStep = class
  private
    FFilename: string;
    FNumber: Integer;
    FStartLine: Cardinal;
    FEndLine: Cardinal;
    FEffectStartLine: Cardinal;
    FEffectEndLine: Cardinal;
    FPlayLine: Cardinal;
  public
    constructor Create(Filename: string; Number: Integer;
      StartLine, EndLine, EffectStartLine, EffectEndLine, PlayLine: Cardinal);

    property Filename: string read FFilename;
    property Number: Integer read FNumber;
    property StartLine: Cardinal read FStartLine;
    property EndLine: Cardinal read FEndLine;
    property EffectStartLine: Cardinal read FEffectStartLine;
    property EffectEndLine: Cardinal read FEffectEndLine;
    property PlayLine: Cardinal read FPlayLine;
  end;

  TUndoList = class(TList<TUndoStep>);

  TLineMode = (lmEdit, lmPlay, lmEffectsMarker);

  TCutFileEvent = procedure(Sender: TObject; Filename: string) of object;

  TCutView = class(TPanel)
  private
    FScanThread: TScanThread;
    FProcessThread: TProcessThread;
    FPB: TCutPaintBox;
    FWaveData: TWaveData;
    FState: TCutStates;
    FLineMode: TLineMode;
    FProgressBarLoad: TProgressBar;
    FWasSaved: Boolean;
    FUndoList: TUndoList;
    FID: Integer;
    FUndoStep: TUndoStep;

    FPlayer: TPlayer;
    FFilename: string;
    FFilesize: UInt64;

    FDropTarget: TDropComboTarget;

    FOnStateChanged: TNotifyEvent;
    FOnCutFile: TCutFileEvent;

    function GetSoX: TSoXPlugin;
    procedure StartProcessing(CmdLine: string);
    function AddUndo: Boolean;

    procedure ThreadScanProgress(Sender: TObject);
    procedure ThreadEndScan(Sender: TObject);
    procedure ThreadScanError(Sender: TObject);
    procedure ThreadTerminate(Sender: TObject);

    procedure ProcessThreadSuccess(Sender: TObject);
    procedure ProcessThreadError(Sender: TObject);
    procedure ProcessThreadTerminate(Sender: TObject);

    procedure PlayerEndReached(Sender: TObject);
    procedure PlayerPlay(Sender: TObject);
    procedure PlayerPause(Sender: TObject);
    procedure PlayerStop(Sender: TObject);

    procedure MsgRefresh(var Msg: TMessage); message WM_USER + 1234;

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);

    procedure MessageReceived(Msg: TMessageBase);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(Filename: string);

    function CheckSoX: Boolean;

    procedure Cut;
    procedure Undo;
    function Save(StartPos, EndPos: Cardinal): Boolean;
    procedure SaveAs;
    procedure Play;
    procedure Stop;
    procedure AutoCut(MaxPeaks: Integer; MinDuration: Cardinal);
    function ApplyFade(Fadein: Boolean): Boolean;
    procedure ApplyFadein;
    procedure ApplyFadeout;
    procedure ApplyEffects;
    procedure ZoomIn;
    procedure ZoomOut;

    function CanCut: Boolean;
    function CanUndo: Boolean;
    function CanPlay: Boolean;
    function CanStop: Boolean;
    function CanAutoCut: Boolean;
    function CanSetLine: Boolean;
    function CanZoomIn: Boolean;
    function CanZoomOut: Boolean;
    function CanEffectsMarker: Boolean;
    function CanApplyFadeIn: Boolean;
    function CanApplyFadeOut: Boolean;
    function CanApplyEffects: Boolean;

    property Player: TPlayer read FPlayer;
    property LineMode: TLineMode read FLineMode write FLineMode;
    property Filesize: UInt64 read FFilesize;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
    property OnCutFile: TCutFileEvent read FOnCutFile write FOnCutFile;
  end;

implementation

uses
  CutTab;

{ TScanThread }

constructor TScanThread.Create(Filename: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FFilename := Filename;
  FWaveData := TWaveData.Create;
  FWaveData.OnProgress := WaveDataScanProgress;
end;

destructor TScanThread.Destroy;
begin

  inherited;
end;

procedure TScanThread.Execute;
begin
  try
    FWaveData.Load(FFilename);
    Synchronize(SyncEndScan);
  except
    FreeAndNil(FWaveData);
    Synchronize(SyncScanError);
  end;

  if Terminated then
    FWaveData.Free;
end;

procedure TScanThread.SyncEndScan;
begin
  if Terminated then
    Exit;
  if Assigned(FOnEndScan) then
    FOnEndScan(Self);
end;

procedure TScanThread.SyncScanError;
begin
  if Terminated then
    Exit;
  if Assigned(FOnScanError) then
    FOnScanError(Self);
end;

procedure TScanThread.SyncScanProgress;
begin
  if Terminated then
    Exit;
  if Assigned(FOnScanProgress) then
    FOnScanProgress(Self);
end;

procedure TScanThread.WaveDataScanProgress(Sender: TObject);
begin
  Synchronize(SyncScanProgress);
end;

{ TCutView }

constructor TCutView.Create(AOwner: TComponent);
begin
  inherited;

  FID := Random(1000);
  BevelOuter := bvNone;

  FPB := TCutPaintBox.Create(Self);
  FPB.Parent := Self;
  FPB.Align := alClient;

  FLineMode := lmPlay;

  FProgressBarLoad := TProgressBar.Create(Self);
  FProgressBarLoad.Max := 100;
  FProgressBarLoad.Parent := Self;
  FProgressBarLoad.Visible := False;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfFile];
  FDropTarget.Register(Self);
  FDropTarget.OnDrop := DropTargetDrop;

  FUndoList := TUndoList.Create;

  MsgBus.AddSubscriber(MessageReceived);
end;

destructor TCutView.Destroy;
var
  i: Integer;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  if FScanThread <> nil then
  begin
    FScanThread.OnTerminate := nil;
    FScanThread.OnScanProgress := nil;
    FScanThread.OnEndScan := nil;
    FScanThread.OnScanError := nil;
    FScanThread.Terminate;
  end;

  if FProcessThread <> nil then
  begin
    FProcessThread.OnTerminate := nil;
    FProcessThread.OnSuccess := nil;
    FProcessThread.OnError := nil;
    FProcessThread.Terminate;
  end;

  if FWaveData <> nil then
    FreeAndNil(FWaveData);

  if FPlayer <> nil then
  begin
    // Das hier gibt gerne Exceptions. Aber nur, wenn ein CutView auf ist, und mann das Programm beendet.
    // Schieß mich tot... das hier hilft, keine Ahnung woher der Fehler kommt.
    try
      FreeAndNil(FPlayer);
    except end;
  end;

  for i := 0 to FUndoList.Count - 1 do
  begin
    DeleteFile(PChar(FUndoList[i].Filename));
    FUndoList[i].Free;
  end;
  FUndoList.Free;

  inherited;
end;

procedure TCutView.DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
var
  i: Integer;
begin
  for i := 0 to FDropTarget.Files.Count - 1 do
  begin
    // REMARK: Bei neuen Formaten brauche ich dafür vielleicht ein Array!
    if (LowerCase(ExtractFileExt(FDropTarget.Files[i])) = '.mp3') or
       (LowerCase(ExtractFileExt(FDropTarget.Files[i])) = '.aac') then
    begin
      if Assigned(FOnCutFile) then
        FOnCutFile(TCutTab(Self.Parent), FDropTarget.Files[i]);
    end;
  end;
end;

function TCutView.GetSoX: TSoXPlugin;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to AppGlobals.PluginManager.Plugins.Count - 1 do
    if AppGlobals.PluginManager.Plugins[i] is TSoXPlugin then
    begin
      Result := TSoXPlugin(AppGlobals.PluginManager.Plugins[i]);
      Break;
    end;
end;

procedure TCutView.LoadFile(Filename: string);
begin
  if (FScanThread <> nil) or (FProcessThread <> nil) then
    Exit;

  FFilename := Filename;

  if FPlayer <> nil then
  begin
    FreeAndNil(FPlayer);
  end;
  FPlayer := TPlayer.Create;
  FPlayer.OnEndReached := PlayerEndReached;
  FPlayer.OnPlay := PlayerPlay;
  FPlayer.OnPause := PlayerPause;
  FPlayer.OnStop := PlayerStop;
  Players.AddPlayer(FPlayer);

  try
    if Bass.DeviceAvailable then
      FPlayer.Filename := FFilename;
  except
    ThreadScanError(Self);
    Exit;
  end;

  FProgressBarLoad.Visible := True;

  FState := csLoading;
  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;

  FScanThread := TScanThread.Create(Filename);
  FScanThread.OnTerminate := ThreadTerminate;
  FScanThread.OnEndScan := ThreadEndScan;
  FScanThread.OnScanError := ThreadScanError;
  FScanThread.OnScanProgress := ThreadScanProgress;

  FPB.FEffectStartLine := 0;
  FPB.FEffectEndLine := 0;

  FScanThread.Resume;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.MessageReceived(Msg: TMessageBase);
var
  M: TFileModifyMsg;
begin
  {
  if Msg is TFileModifyMsg then
  begin
    if LowerCase(FPlayer.Filename) = LowerCase(TFileModifyMsg(Msg).Filename) then
      FPlayer.Stop(False);
  end;
  }
end;

procedure TCutView.MsgRefresh(var Msg: TMessage);
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);

  FPB.FPlayLine := 0;

  FPB.BuildDrawBuffer;
  FPB.Paint;
end;

procedure TCutView.Cut;
begin
  if not CanCut then
    Exit;

  if not AddUndo then
    Exit;

  if not Save(FPB.FStartLine, FPB.FEndLine) then
    Exit;

  if FPlayer <> nil then
  begin
    FPlayer.Stop(False);
    FPB.FPlayLine := 0;
  end;

  FPB.FZoomStartLine := High(Cardinal);
  FPB.FZoomEndLine := High(Cardinal);
  FPB.FDoZoom := False;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Paint;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.Undo;
var
  UndoStep: TUndoStep;
begin
  if not CanUndo then
    Exit;

  UndoStep := FUndoList[FUndoList.Count - 1];

  if CopyFile(PChar(UndoStep.Filename), PChar(FFilename), False) then
  begin
    FWaveData.Free;
    FWaveData := nil;

    FWasSaved := True;
    LoadFile(FFilename);

    DeleteFile(PChar(UndoStep.Filename));

    FPB.BuildBuffer;
    FPB.BuildDrawBuffer;
    FPB.Paint;

    FUndoStep := UndoStep;
    FUndoList.Remove(UndoStep);

    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
  end else
  begin
    MsgBox(GetParentForm(Self).Handle, _('The file could not be replaced by the saved undo file that contains the last version of the file.'), _('Error'), MB_ICONERROR);
  end;
end;

procedure TCutView.ZoomIn;
var
  Swap: Cardinal;
begin
  if not CanZoomIn then
    Exit;

  if FPB.FEffectStartLine > FPB.FEffectEndLine then
  begin
    Swap := FPB.FEffectStartLine;
    FPB.FEffectStartLine := FPB.FEffectEndLine;
    FPB.FEffectEndLine := Swap;
  end;

  FPB.FZoomStartLine := FPB.FEffectStartLine;
  FPB.FZoomEndLine := FPB.FEffectEndLine;
  FPB.FDoZoom := True;

  //FPB.FEffectStartLine := High(Cardinal);
  //FPB.FEffectEndLine := High(Cardinal);

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Paint;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ZoomOut;
begin
  if not CanZoomOut then
    Exit;

  FPB.FZoomStartLine := 0;
  FPB.FZoomEndLine := High(FWaveData.WaveArray);
  FPB.FDoZoom := True;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Paint;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

function TCutView.Save(StartPos, EndPos: Cardinal): Boolean;
begin
  Result := False;

  if FPlayer <> nil then
  begin
    FreeAndNil(FPlayer);
  end;

  try
    MsgBus.SendMessage(TFileModifyMsg.Create(FFilename));

    if FWaveData.Save(FFilename, StartPos, EndPos) then
    begin
      FreeAndNil(FWaveData);
      FWasSaved := True;
      LoadFile(FFilename);

      Result := True;
    end else
    begin
      MsgBox(GetParentForm(Self).Handle, _('The file could not be saved.'#13#10'Please make sure the file is not in use by another application.'), _('Info'), MB_ICONINFORMATION);
    end;
  finally
    // Wenn Result = True wird FPlayer in LoadFile() neu erstellt
    if not Result then
    begin
      FPlayer := TPlayer.Create;
      try
        FPlayer.Filename := FFilename;

        FPlayer.OnEndReached := PlayerEndReached;
        FPlayer.OnPlay := PlayerPlay;
        FPlayer.OnPause := PlayerPause;
        FPlayer.OnStop := PlayerStop;
        Players.AddPlayer(FPlayer);
      except
        ThreadScanError(Self);
      end;
    end;
  end;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.SaveAs;
begin
  raise Exception.Create('');
end;

procedure TCutView.Play;
begin
  if not CanPlay then
    Exit;

  FPB.FPlayingIndex := 0;

  if FPlayer = nil then
  begin
    Exit;
  end;

  FPlayer.Volume := AppGlobals.PlayerVolume;
  FPlayer.PositionByte := FWaveData.WaveArray[FPB.FPlayLine].Pos;

  FPlayer.Play;

  FPlayer.PosToReach := FWaveData.WaveArray[High(FWaveData.WaveArray)].Pos;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerEndReached(Sender: TObject);
begin
  FPlayer.Stop(False);
  FPlayer.PositionByte := 0;

  PostMessage(Handle, WM_USER + 1234, 0, 0);
end;

procedure TCutView.PlayerPause(Sender: TObject);
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerPlay(Sender: TObject);
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerStop(Sender: TObject);
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ProcessThreadError(Sender: TObject);
var
  Msg: string;
begin
  FWasSaved := False;
  Msg := FProcessThread.ProcessOutput;
  FProcessThread := nil;
  MsgBox(GetParentForm(Self).Handle, Format(_('An error occured while processing the file:'#13#10'%s'), [Msg]) , _('Error'), MB_ICONERROR);

  LoadFile(FFilename);
end;

procedure TCutView.ProcessThreadSuccess(Sender: TObject);
begin
  FProcessThread := nil;
  FState := csReady;
  FWasSaved := True;
  LoadFile(FFilename);

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ProcessThreadTerminate(Sender: TObject);
begin

end;

procedure TCutView.Resize;
begin
  inherited;

  FProgressBarLoad.Width := Min(350, ClientWidth - 50);
  FProgressBarLoad.Height := 24;
  FProgressBarLoad.Left := ClientWidth div 2 - FProgressBarLoad.Width div 2;
  FProgressBarLoad.Top := ClientHeight div 2 - FProgressBarLoad.Height div 2 + 20;
end;

procedure TCutView.StartProcessing(CmdLine: string);
var
  TempFile: string;
begin
  if FPlayer <> nil then
  begin
    FreeAndNil(FPlayer);
  end;

  if FWaveData <> nil then
    FreeAndNil(FWaveData);

  TempFile := RemoveFileExt(FFilename) + '_soxconvert' + ExtractFileExt(FFilename);
  CmdLine := StringReplace(CmdLine, '[[TEMPFILE]]', TempFile, [rfReplaceAll]);

  FWasSaved := True;
  FState := csWorking;
  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Paint;

  FProcessThread := TProcessThread.Create(CmdLine, ExtractFilePath(GetSoX.SoXExe), FFilename, TempFile);
  FProcessThread.OnSuccess := ProcessThreadSuccess;
  FProcessThread.OnError := ProcessThreadError;
  FProcessThread.OnTerminate := ProcessThreadTerminate;
  FProcessThread.Resume;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.Stop;
begin
  if not CanStop then
    Exit;

  if FPlayer <> nil then
  begin
    FPlayer.Pause;
  end;

  FPB.BuildDrawBuffer;
  FPB.Paint;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

function TCutView.ApplyFade(Fadein: Boolean): Boolean;
var
  CmdLine: string;
  FadeTo, FadeStart: Cardinal;
begin
  Result := False;

  if not CheckSoX then
    Exit;

  CmdLine := '"' + GetSoX.SoXExe + '" --norm "' + FFilename + '" ' + '"[[TEMPFILE]]" ';

  if Fadein then
  begin
    FadeTo := Max(FPB.FEffectStartLine, FPB.FEffectEndLine);

    CmdLine := CmdLine + 'fade p ' + IntToStr(Round(FWaveData.WaveArray[FadeTo].Sec))
  end else
  begin
    FadeStart := Min(FPB.FEffectStartLine, FPB.FEffectEndLine);

    CmdLine := CmdLine + 'fade p 0 ' + IntToStr(Round(FWaveData.WaveArray[High(FWaveData.WaveArray)].Sec)) + ' ' +
      IntToStr(Round(FWaveData.WaveArray[High(FWaveData.WaveArray)].Sec - FWaveData.WaveArray[FadeStart].Sec))
  end;

  if not AddUndo then
    Exit;

  StartProcessing(CmdLine);
end;

procedure TCutView.ApplyFadein;
begin
  if not CanApplyFadeIn then
    Exit;

  ApplyFade(True);
end;

procedure TCutView.ApplyFadeout;
begin
  if not CanApplyFadeOut then
    Exit;

  ApplyFade(False);
end;

function TCutView.AddUndo: Boolean;
var
  Number: Integer;
  FN, Dest: string;
begin
  Result := False;

  if FUndoList.Count > 0 then
  begin
    Number := FUndoList[FUndoList.Count - 1].Number + 1;
  end else
    Number := 1;

  FN := ExtractFileName(FFilename);
  Dest := AppGlobals.TempDir + 'UNDO_' + Copy(FN, 1, Length(FN) - Length(ExtractFileExt(FN))) + '_' + IntToStr(FID) + '_' + IntToStr(Number) + (ExtractFileExt(FN));
  if CopyFile(PChar(FFilename), PChar(Dest), False) then
  begin
    FUndoList.Add(TUndoStep.Create(Dest, Number, FPB.FStartLine, FPB.FEndLine,
      FPB.FEffectStartLine, FPB.FEffectEndLine, FPB.FPlayLine));
    Result := True;
  end;

  if not Result then
  begin
    if TfrmMsgDlg.ShowMsg(GetParentForm(Self), _('The temporary file for making the undo functionality work could not be created.'#13#10 +
                                                 'Do you want to continue with undo disabled?'), 9, btOKCancel) <> mtCancel
    then
      Result := True;
  end;
end;

procedure TCutView.ApplyEffects;
var
  CmdLine: string;
  F: TfrmConfigureSoX;
begin
  if not CheckSoX then
    Exit;

  F := TfrmConfigureSox.Create(Self, GetSox, False, False, False, 5, 5, False, False, 3, 3);
  try
    F.ShowModal;

    if F.SaveData then
    begin
      CmdLine := '';

      if F.Normalize then
        CmdLine := CmdLine + 'gain -b -n';

      if F.FadeoutStart and F.FadeoutEnd then
        CmdLine := CmdLine + ' fade p ' + IntToStr(F.FadeoutStartLength) + ' ' + IntToStr(Round(FWaveData.Secs)) + ' ' + IntToStr(F.FadeoutEndLength)
      else if F.FadeoutStart then
        CmdLine := CmdLine + ' fade p ' + IntToStr(F.FadeoutStartLength)
      else if F.FadeoutEnd then
        CmdLine := CmdLine + ' fade p 0 ' + IntToStr(Round(FWaveData.Secs)) + ' ' + IntToStr(F.FadeoutEndLength);

      if F.SilenceStart and F.SilenceEnd then
        CmdLine := CmdLine + ' pad ' + IntToStr(F.SilenceStartLength) + ' ' + IntToStr(F.SilenceEndLength)
      else if F.SilenceStart then
        CmdLine := CmdLine + ' pad ' + IntToStr(F.SilenceStartLength)
      else if F.SilenceEnd then
        CmdLine := CmdLine + ' pad 0 ' + IntToStr(F.SilenceEndLength);

      if CmdLine <> '' then
      begin
        if not AddUndo then
          Exit;

        CmdLine := '"' + GetSoX.SoXExe + '" --norm "' + FFilename + '" ' + '"[[TEMPFILE]]" ' + CmdLine;

        StartProcessing(CmdLine);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCutView.AutoCut(MaxPeaks: Integer; MinDuration: Cardinal);
var
  i, n, MaxLenIdx, ArrIdx: Integer;
  WD, WD2: TWaveData;
  OldPos: Int64;
  Avg, Avg2: Cardinal;
  MinSilence: TMinSilenceArray;
  MS: TMinSilence;
  EntryCount: Cardinal;
  A, B: Int64;
begin
  if FWaveData = nil then
    Exit;

  FWaveData.AutoCut(-1, MinDuration);

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;
end;

function TCutView.CanCut: Boolean;
begin
  Result := (FWaveData <> nil) and (FPB.FEndLine - FPB.FStartLine > 0) and
    (FWaveData.TimeBetween(FPB.FStartLine, FPB.FEndLine) >= 0.5) and
    ((FPB.FStartLine > 0) or (FPB.FEndLine < High(FWaveData.WaveArray)));
end;

function TCutView.CanEffectsMarker: Boolean;
begin
  Result := (FWaveData <> nil) and (LowerCase(ExtractFileExt(FFilename)) = '.mp3');
end;

function TCutView.CanUndo: Boolean;
begin
  Result := (FWaveData <> nil) and (FUndoList.Count > 0);
end;

function TCutView.CanPlay: Boolean;
begin
  Result := (FPlayer <> nil) and (FWaveData <> nil) and (not FPlayer.Playing);
end;

function TCutView.CanStop: Boolean;
begin
  Result := (FPlayer <> nil) and FPlayer.Playing;
end;

function TCutView.CanApplyFadeIn: Boolean;
var
  Tolerance: Cardinal;
begin
  if FWaveData <> nil then
    Tolerance := Trunc((FWaveData.ZoomSize div FPB.ClientWidth) * 3.5) + 4;
  Result := (FWaveData <> nil) and
            (LowerCase(ExtractFileExt(FFilename)) = '.mp3') and
            (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5) and
            ((FPB.FEffectStartLine <= Tolerance) or (FPB.FEffectEndLine <= Tolerance));
end;

function TCutView.CanApplyFadeOut: Boolean;
var
  Tolerance: Cardinal;
begin
  if FWaveData <> nil then
    Tolerance := Trunc((FWaveData.ZoomSize div FPB.ClientWidth) * 3.5) + 4;
  Result := (FWaveData <> nil) and
            (LowerCase(ExtractFileExt(FFilename)) = '.mp3') and
            (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5) and
            ((FPB.FEffectStartLine >= Length(FWaveData.WaveArray) - Tolerance) or (FPB.FEffectEndLine >= Length(FWaveData.WaveArray) - Tolerance));
end;

function TCutView.CanApplyEffects: Boolean;
begin
  Result := (FWaveData <> nil) and (LowerCase(ExtractFileExt(FFilename)) = '.mp3');
end;

function TCutView.CanAutoCut: Boolean;
begin
  Result := FWaveData <> nil;
end;

function TCutView.CanSetLine: Boolean;
begin
  Result := FWaveData <> nil;
end;

function TCutView.CanZoomIn: Boolean;
begin
  Result := (FWaveData <> nil) and (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5);
end;

function TCutView.CanZoomOut: Boolean;
begin
  Result := (FWaveData <> nil) and ((FWaveData.ZoomStart <> High(Cardinal)) or (FWaveData.ZoomEnd <> High(FWaveData.WaveArray))) and
    ((FWaveData.ZoomStart <> 0) or (FWaveData.ZoomEnd <> High(FWaveData.WaveArray)));
end;

function TCutView.CheckSoX: Boolean;
var
  Res: Integer;
  DA: TfrmDownloadAddons;
  CS: TfrmConfigureSoX;
  Plugin: TSoXPlugin;
begin
  Result := False;

  Plugin := GetSox;
  if Plugin = nil then
    Exit;

  if not Plugin.ReadyForActivate then
  begin
    Res := MsgBox(GetParentForm(Self).Handle, _('This function cannot be used because needed files have not been downloaded.'#13#10'Do you want to download these files now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1);
    if Res = IDYES  then
    begin
      if not Plugin.ShowInitMessage(Handle) then
        Exit;

      DA := TfrmDownloadAddons.Create(Self, Plugin);
      try
        DA.ShowModal;

        if not DA.Downloaded then
        begin
          if DA.Error then
            MsgBox(GetParentForm(Self).Handle, _('An error occured while downloading the file.'), _('Error'), MB_ICONEXCLAMATION);
          Exit;
        end;
      finally
        DA.Free;
      end;
    end else if Res = IDNO then
    begin
      Exit;
    end;

    // Nochmal initialisieren. Evtl. wurde eben erst die .dll heruntergeladen, dann extrahiert .Initialize() jetzt
    Plugin.Initialize;
  end;

  if Plugin.ReadyForActivate and (not Plugin.ReadyForUse) then
  begin
    CS := TfrmConfigureSoX.Create(GetParentForm(Self), Plugin);
    try
      CS.ShowModal;

      Plugin.Initialize;

      if not Plugin.ReadyForUse then
        Exit;
    finally
      CS.Free;
    end;
  end;

  if not Plugin.ReadyForUse then
  begin
    MsgBox(GetParentForm(Self).Handle, _('The plugin is not ready for use. This might happen when it''s files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);
    Exit;
  end else
    Result := True;
end;

procedure TCutView.ThreadEndScan(Sender: TObject);
begin
  FProgressBarLoad.Visible := False;

  if FWaveData <> nil then
    FWaveData.Free;
  FWaveData := FScanThread.FWaveData;

  if FUndoStep <> nil then
  begin
    FPB.FStartLine := FUndoStep.StartLine;
    FPB.FEndLine := FUndoStep.EndLine;
    FPB.FEffectStartLine := FUndoStep.EffectStartLine;
    FPB.FEffectEndLine := FUndoStep.EffectEndLine;
    FPB.FPlayLine := FUndoStep.PlayLine;
  end else
  begin
    FPB.FStartLine := 0;
    FPB.FEndLine := High(FWaveData.WaveArray);
    FPB.FPlayLine := 0;
  end;

  FScanThread := nil;

  FState := csReady;

  if FWasSaved then
  begin
    if Assigned(TCutTab(Owner).OnSaved) then
      TCutTab(Owner).OnSaved(Owner, FWaveData.Filesize, Trunc(FWaveData.Secs));
  end;
  FWasSaved := False;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);

  FPB.Resize;
  FPB.Paint;
  FPB.Repaint;
end;

procedure TCutView.ThreadScanError(Sender: TObject);
begin
  FProgressBarLoad.Visible := False;
  FWasSaved := False;
  FState := csError;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;
end;

procedure TCutView.ThreadScanProgress(Sender: TObject);
begin
  if FScanThread.FWaveData.Progress < 100 then
    FProgressBarLoad.Position := FScanThread.FWaveData.Progress + 1;
  FProgressBarLoad.Position := FScanThread.FWaveData.Progress;
end;

procedure TCutView.ThreadTerminate(Sender: TObject);
begin
  FWasSaved := False;
  if FProgressBarLoad.Visible then
    FProgressBarLoad.Visible := False;
  FScanThread := nil;
  if FUndoStep <> nil then
    FreeAndNil(FUndoStep);
end;

{ TCutPaintBox }

procedure TCutPaintBox.BuildBuffer;
  procedure DrawTransparentBox(StartIdx: Cardinal; EndIdx: Cardinal; LineColor: TColor; FillColor: TColor);
  var
    RectStart, RectEnd: Int64;
    OriginalMode: TPenMode;
  begin
    // Die Konvertierung nach Int64 ist wichtig. Sonst gibt es Darstellungsfehler:
    // Wenn man im Zoom-Modus was markiert und nach rechts scrollt, über den Anfang des markierten,
    // wird alles als ausgewählt angezeigt. Darum bloß nicht entfernen :)
    RectStart := Trunc(((Int64(StartIdx) - Int64(FCutView.FWaveData.ZoomStart)) / Int64(FCutView.FWaveData.ZoomSize)) * FWaveBuf.Width);
    RectEnd := Trunc(((Int64(EndIdx) - Int64(FCutView.FWaveData.ZoomStart)) / Int64(FCutView.FWaveData.ZoomSize)) * FWaveBuf.Width);

    with FWaveBuf.Canvas do
    begin
      Pen.Color := LineColor;
      originalMode := Pen.Mode;
      Pen.Mode := TPenMode.pmNotXor;
      Brush.Color := FillColor;
      Rectangle(rectStart, 0, rectEnd, FWaveBuf.Height - ScrollbarHeight - 2);
      Pen.Mode := originalMode;
    end;
  end;
  procedure DrawScrollBar(Color: TColor);
  var StartX, StartY, EndX: Integer;
      y: Cardinal;
  begin
    with FWaveBuf.Canvas do
    begin
      //Draw Outline
      Pen.Color := Color;
      StartY := Height - 2 - ScrollbarHeight;
      MoveTo(1, StartY);
      LineTo(Width - 2, StartY);
      LineTo(Width - 2, Height - 2);
      LineTo(1, Height - 2);
      LineTo(1, StartY);

      //Draw Bar
      StartX := Trunc((FCutView.FWaveData.ZoomStart * (FWaveBuf.Width - 6)) / High(FCutView.FWaveData.WaveArray)) + 3;
      EndX := Trunc((FCutView.FWaveData.ZoomEnd * (FWaveBuf.Width - 6)) / High(FCutView.FWaveData.WaveArray)) + 3;
      if StartX = EndX then
        EndX := StartX + 1;
      for y := 0 to ScrollbarHeight - 4 do
      begin
        MoveTo(StartX, StartY + y + 2);
        LineTo(EndX, StartY + y + 2);
      end;
    end;
  end;
var
  i, v, vnext: Integer;
  v2: Double;
  Last: Integer;
  LBuf, RBuf: Cardinal;
  Added: Cardinal;
  HT: Cardinal;
  TS: TSize;
  Txt: string;
  ArrayFrom, ArrayTo: Cardinal;
  L1, L2: Cardinal;
  CS, CE: Cardinal;
begin
  FWaveBuf.Canvas.Brush.Color := clBlack;
  FWaveBuf.Canvas.FillRect(Rect(0, 0, FWaveBuf.Width, FWaveBuf.Height));

  if (ClientHeight < 2) or (ClientWidth < 2) then
    Exit;

  if FCutView.FState = csError then
  begin
    Txt := _('Error loading file');
    TS := GetTextSize(Txt, Canvas.Font);
    FWaveBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FWaveBuf.Canvas.Handle, TRANSPARENT);
    FWaveBuf.Canvas.TextOut(FWaveBuf.Width div 2 - TS.cx div 2, FWaveBuf.Height div 2 - TS.cy, Txt);
    Exit;
  end;

  if FCutView.FState = csWorking then
  begin
    Txt := _('Working...');
    TS := GetTextSize(Txt, Canvas.Font);
    FWaveBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FWaveBuf.Canvas.Handle, TRANSPARENT);
    FWaveBuf.Canvas.TextOut(FWaveBuf.Width div 2 - TS.cx div 2, FWaveBuf.Height div 2 - TS.cy, Txt);
    Exit;
  end;

  if (FCutView.FWaveData = nil) or (FCutView.FState = csLoading) then
  begin
    Txt := _('Loading file...');
    TS := GetTextSize(Txt, Canvas.Font);
    FWaveBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FWaveBuf.Canvas.Handle, TRANSPARENT);
    FWaveBuf.Canvas.TextOut(FWaveBuf.Width div 2 - TS.cx div 2, FWaveBuf.Height div 2 - TS.cy, Txt);
    Exit;
  end;

  if (FCutView = nil) or (FCutView.FWaveData = nil) then
    Exit;

  if Length(FCutView.FWaveData.WaveArray) = 0 then
    Exit;

  ht := (FWaveBuf.Height div 2) - ScrollbarHeight - 1;

  LBuf := 0;
  RBuf := 0;
  Added := 0;
  Last := 0;

  if FDoZoom then
  begin
    if FZoomStartLine = High(Cardinal) then
    begin
      FCutView.FWaveData.ZoomStart := 0;
      FCutView.FWaveData.ZoomEnd := High(FCutView.FWaveData.WaveArray);
    end else
    begin
      FCutView.FWaveData.ZoomStart := FZoomStartLine;
      FCutView.FWaveData.ZoomEnd := FZoomEndLine;
      FZoomStartLine := High(Cardinal);
      FZoomEndLine := High(Cardinal);
    end;
    FDoZoom := False;
  end;

  for i := 0 to FCutView.FWaveData.Silence.Count - 1 do
  begin
    CS := FCutView.FWaveData.Silence[i].CutStart;
    CE := FCutView.FWaveData.Silence[i].CutEnd;

    L1 := Floor(((CS - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FWaveBuf.Width);
    L2 := Ceil(((CE - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FWaveBuf.Width);

    if L2 - L1 <= 2 then
      Inc(L2);

    FWaveBuf.Canvas.Brush.Color := clGray;
    FWaveBuf.Canvas.FillRect(Rect(L1, 0, L2, ht - 1));
    FWaveBuf.Canvas.FillRect(Rect(L1, ht + 1, L2, FWaveBuf.Height - ScrollbarHeight - 3));
  end;

  ArrayFrom := FCutView.FWaveData.ZoomStart;
  ArrayTo := FCutView.FWaveData.ZoomEnd;

  v2 := (1 / 1) * FWaveBuf.Width;

  for i := ArrayFrom to ArrayTo do
  begin
    v := Integer(Trunc(((i - Int64(ArrayFrom)) / (ArrayTo - ArrayFrom)) * v2));
    vnext := Integer(Trunc(((i - Int64(ArrayFrom) + 1) / (ArrayTo - ArrayFrom)) * v2));

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

    FWaveBuf.Canvas.Pen.Color := FPeakColor;
    FWaveBuf.Canvas.Brush.Color := FPeakColor;
    if abs(vnext - v) <= 2 then
    begin
      FWaveBuf.Canvas.MoveTo(v, ht - 1);
      FWaveBuf.Canvas.LineTo(v, ht - 1 - Trunc((LBuf / 33000) * ht));
      FWaveBuf.Canvas.Pixels[v, ht - 1 - Trunc((LBuf / 33000) * ht)] := FPeakEndColor;
      FWaveBuf.Canvas.MoveTo(v, ht + 1);
      FWaveBuf.Canvas.LineTo(v, ht + 1 + Trunc((RBuf / 33000) * ht));
      FWaveBuf.Canvas.Pixels[v, ht + 1 + Trunc((RBuf / 33000) * ht)] := FPeakEndColor;
    end else
    begin
      FWaveBuf.Canvas.FillRect(Rect(v, ht, vnext - 1, ht - Trunc((LBuf / 33000) * ht)));
      FWaveBuf.Canvas.FillRect(Rect(v, ht + 1, vnext - 1, ht + 1 + Trunc((LBuf / 33000) * ht)));
    end;

    RBuf := 0;
    LBuf := 0;
    Added := 0;

    Last := v;
  end;

  FWaveBuf.Canvas.Pen.Color := FZoomOuterColor;
  FWaveBuf.Canvas.MoveTo(0, ht);
  FWaveBuf.Canvas.LineTo(FWaveBuf.Width, ht);

  DrawTransparentBox(FZoomStartLine, FZoomEndLine, FZoomOuterColor, FZoomInnerColor);

  DrawTransparentBox(FEffectStartLine, FEffectEndLine, clRed, clRed);

  DrawScrollBar(FZoomOuterColor);
end;

procedure TCutPaintBox.BuildDrawBuffer;
  procedure DrawLine(ArrayIdx: Cardinal; Color: TColor);
  var
    L: Cardinal;
  begin
    L := Trunc(((ArrayIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FDrawBuf.Width);

    FDrawBuf.Canvas.Pen.Color := Color;
    FDrawBuf.Canvas.MoveTo(L, 0);
    FDrawBuf.Canvas.LineTo(L, FDrawBuf.Height - ScrollbarHeight - 2);

    FDrawBuf.Canvas.Brush.Color := clBlack;
  end;
  function BuildTime(T: Double): string;
  var
    Min, Sec, MSec: Word;
  begin
    Min := Trunc(T / 60);
    T := T - Trunc(T / 60) * 60;
    Sec := Trunc(T);
    T := T - Trunc(T);
    MSec := (Trunc(T * 1000) div 10) * 10;
    Result := Format('%0.2d:%0.2d.%0.3d', [Min, Sec, MSec]) + ' ' + _('minutes');
  end;
  procedure DrawLineText(ArrayIdx, X: Cardinal);
  var
    L: Integer;
    TS: TSize;
    SecText: string;
  begin
    L := Trunc(((ArrayIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FDrawBuf.Width);
    SecText := BuildTime(FCutView.FWaveData.WaveArray[ArrayIdx].Sec);
    FDrawBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FDrawBuf.Canvas.Handle, TRANSPARENT);
    TS := GetTextSize(SecText, Canvas.Font);
    if FDrawBuf.Width < L + 4 + TS.cx then
      FDrawBuf.Canvas.TextOut(L - 4 - TS.cx, X, SecText)
    else
      FDrawBuf.Canvas.TextOut(L + 4, X, SecText);
  end;
begin
  FDrawBuf.Free;
  FDrawBuf := TBitmap.Create;
  FDrawBuf.Width := FWaveBuf.Width;
  FDrawBuf.Height := FWaveBuf.Height;

  FDrawBuf.Canvas.Draw(0, 0, FWaveBuf);

  if (FCutView.FWaveData <> nil) and (FCutView.FState <> csWorking) then
  begin
    DrawLine(FStartLine, FStartColor);
    DrawLine(FEndLine, FEndColor);

    DrawLineText(FStartLine, 16);
    DrawLineText(FEndLine, 28);

    DrawLine(FPlayLine, FPlayColor);
    DrawLineText(FPlayLine, 40);

    FDrawBuf.Canvas.Font.Color := clWhite;
    FDrawBuf.Canvas.TextOut(4, 4, BuildTime(FCutView.FWaveData.Secs) + ' - ' + RemoveFileExt(ExtractFileName(FCutView.FFilename)));
  end;
end;

constructor TCutPaintBox.Create(AOwner: TComponent);
begin
  inherited;

  FPeakColor := HTML2Color('3b477e');
  FPeakEndColor := HTML2Color('424e83');
  FStartColor := HTML2Color('ece52b');
  FEndColor := HTML2Color('218030');
  FPlayColor := HTML2Color('c33131');
  FZoomOuterColor :=  HTML2Color('748cf7');
  FZoomInnerColor := HTML2Color('4d5ea5');

  FControlMode := cmNone;

  FCutView := TCutView(AOwner);
  if FWaveBuf = nil then
    FWaveBuf := TBitmap.Create;
  if FDrawBuf = nil then
    FDrawBuf := TBitmap.Create;

  FZoomStartLine := High(Cardinal);
  FZoomEndLine := High(Cardinal);

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.OnTimer := TimerTimer;
end;

destructor TCutPaintBox.Destroy;
begin
  FWaveBuf.Free;
  FDrawBuf.Free;
  FTimer.Enabled := False;
  inherited;
end;

procedure TCutPaintBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FCutView.FWaveData = nil then
    Exit;

  if Button = mbLeft then
    FMouseMoveStartX := X;

  SetCaptureControl(Self);

  FControlMode := GetControlMode(Y);

  case FControlMode of
    cmView:
      if (Button = mbLeft) or (Button = mbRight) then
        SetLine(X, Button, mmDown);
    cmScroll:
      if Button = mbLeft then
        HandleScrollBar(X, Y, @Button, mmDown);
  end;
end;

function TCutPaintBox.GetControlMode(Y: Integer): TControlMode;
begin
  if Y >= Height - ScrollbarHeight - 2 then
    Result := cmScroll
  else
    Result := cmView;
end;

procedure TCutPaintBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Button: PMouseButton;
  ButtonData: TMouseButton;
begin
  inherited;

  if FCutView.FWaveData = nil then
    Exit;

  if (X <> FMouseOldX) or (Y <> FMouseOldY) then
  begin
    if ssLeft in Shift then
    begin
      ButtonData := mbLeft;
      Button := @ButtonData;
    end
    else if ssRight in Shift then
    begin
      ButtonData := mbRight;
      Button := @ButtonData;
    end
    else
      Button := nil;

    case FControlMode of
      cmView:
        if (ssLeft in Shift) or (ssRight in Shift) then
          SetLine(X, Button^, mmMove);
      cmScroll:
        if ssLeft in Shift then
          HandleScrollBar(X, Y, Button, mmMove);
    end;

    FMouseOldX := X;
    FMouseOldY := Y;
  end;
end;

procedure TCutPaintBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if FControlMode = cmView then
    SetLine(X, Button, mmUp);

  ReleaseCapture;
  FControlMode := cmNone;
end;

procedure TCutPaintBox.Paint;
begin
  inherited;

  Canvas.Draw(0, 0, FDrawBuf);
end;

procedure TCutPaintBox.Resize;
begin
  inherited;

  if FWaveBuf = nil then
    Exit;

  FWaveBuf.Free;
  FWaveBuf := TBitmap.Create;
  FWaveBuf.Width := ClientWidth;
  FWaveBuf.Height := ClientHeight;

  BuildBuffer;
  BuildDrawBuffer;
end;

function TCutPaintBox.PixelsToArray(X: Integer): Cardinal;
begin
  // Wichtig weil Integer und Cardinal.
  if X < 0 then
    X := 0;
  if X > ClientWidth then
    X := ClientWidth;

  Result := FCutView.FWaveData.ZoomStart + Cardinal(Ceil((X / FWaveBuf.Width) * FCutView.FWaveData.ZoomSize));

  if Result > FCutView.FWaveData.ZoomEnd then
    Result := FCutView.FWaveData.ZoomEnd;
  if Result < FCutView.FWaveData.ZoomStart then
    Result := FCutView.FWaveData.ZoomStart;
end;

function TCutPaintBox.GetPlayerPos: Cardinal;
var
  i: Integer;
  SearchFrom, BytePos: Cardinal;
begin
  Result := 0;
  BytePos := FCutView.FPlayer.PositionByte;

  SearchFrom := 0;
  if BytePos > FCutView.FWaveData.WaveArray[FPlayingIndex].Pos then
  begin
    SearchFrom := FPlayingIndex;
  end;

  for i := SearchFrom to High(FCutView.FWaveData.WaveArray) - 1 do
  begin
    if FCutView.FWaveData.WaveArray[i].Pos >= BytePos then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TCutPaintBox.SetLine(X: Integer; Button: TMouseButton; Mode: TMouseMode);
var
  ArrayPos: Cardinal;
begin
  ArrayPos := PixelsToArray(X);

  if ArrayPos < FCutView.FWaveData.ZoomStart then
    ArrayPos := FCutView.FWaveData.ZoomStart;
  if ArrayPos > FCutView.FWaveData.ZoomEnd then
    ArrayPos := FCutView.FWaveData.ZoomEnd;

  case FCutView.FLineMode of
    lmEdit:
      if (Button = mbLeft) and (Mode <> mmUp) then
      begin
        FStartLine := ArrayPos;
        if FStartLine >= FCutView.FWaveData.ZoomEnd then
          FStartLine := FCutView.FWaveData.ZoomEnd - 1;
        if FEndLine <= FStartLine then
          FEndLine := FStartLine + 1;
      end else if (Button = mbRight) and (Mode <> mmUp) then
      begin
        FEndLine := ArrayPos;
        if FEndLine <= FCutView.FWaveData.ZoomStart then
          FEndLine := FCutView.FWaveData.ZoomStart + 1;
        if FStartLine >= FEndLine then
          FStartLine := FEndLine - 1;
      end;
    lmPlay:
      begin
        if (Button = mbLeft) and (Mode <> mmUp) then
        begin
          if FCutView.FPlayer <> nil then
          begin
            if FCutView.FPlayer.Playing then
            begin
              FPlayerPaused := True;
              FCutView.FPlayer.Pause;
            end;
            FCutView.FPlayer.PositionByte := FCutView.FWaveData.WaveArray[ArrayPos].Pos;
          end;
          FPlayLine := ArrayPos;
        end;

        if (Button = mbLeft) and (Mode = mmUp) then
          if (FCutView.FPlayer <> nil) and FPlayerPaused then
          begin
            FCutView.FPlayer.Play;
            FPlayerPaused := False;
          end;
      end;
    lmEffectsMarker:
      begin
        if (Button = mbLeft) and (Mode <> mmUp) then
        begin
          if (FEffectStartLine = High(Cardinal)) or (Mode = mmDown) then
          begin
            FEffectStartLine := ArrayPos;
            FEffectEndLine := ArrayPos;
          end else
          begin
            FEffectEndLine := ArrayPos;
          end;
        end;

        BuildBuffer;
      end;
  end;

  if Assigned(FCutView.FOnStateChanged) then
    FCutView.FOnStateChanged(FCutView);

  BuildDrawBuffer;
  Paint;
end;

procedure TCutPaintBox.HandleScrollBar(X: Integer; Y: Integer; Button: PMouseButton;
  Mode: TMouseMode);
var
  StartX, EndX, DiffX: Integer;
begin
  DiffX := Trunc(((X - FMouseMoveStartX) * High(FCutView.FWaveData.WaveArray)) / (FWaveBuf.Width - 6));
  StartX := FCutView.FWaveData.ZoomStart + Cardinal(DiffX);
  EndX := FCutView.FWaveData.ZoomEnd + Cardinal(DiffX);
  if StartX < 0 then
  begin
    StartX := 0;
    EndX := FCutView.FWaveData.ZoomSize;
  end;
  if EndX > High(FCutView.FWaveData.WaveArray) then
  begin
    EndX := High(FCutView.FWaveData.WaveArray);
    StartX := EndX - FCutView.FWaveData.ZoomSize;
  end;
  FZoomStartLine := StartX;
  FZoomEndLine := EndX;
  FMouseMoveStartX := X;
  FDoZoom := true;
  BuildBuffer;
  BuildDrawBuffer;
  Paint;
end;

procedure TCutPaintBox.TimerTimer(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;

  if (FCutView.FWaveData <> nil) and (FCutView.FPlayer <> nil) and FCutView.FPlayer.Playing and (not FCutView.FPlayer.Paused) then
  begin
    FPlayingIndex := GetPlayerPos;
    FPlayLine := FPlayingIndex;

    BuildDrawBuffer;
    Paint;
  end;
end;

procedure TCutPaintBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

{ TProcessThread }

constructor TProcessThread.Create(CommandLine, WorkingDir, FilePath, TempFile: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FCommandLine := CommandLine;
  FWorkingDir := WorkingDir;
  FFilePath := FilePath;
  FTempFile := TempFile;
end;

destructor TProcessThread.Destroy;
begin

  inherited;
end;

procedure TProcessThread.Execute;
var
  Res: Integer;
  LoopStarted: Cardinal;
  FS: TFileStream;
  Failed: Boolean;
  EC: DWORD;

  ID3V1: TID3v1Tag;
  ID3V2: TID3v2Tag;
begin
  inherited;

  ID3V1 := TID3v1Tag.Create;
  ID3V2 := TID3v2Tag.Create;
  try
    ID3V1.ReadFromFile(FFilePath);
    ID3V2.ReadFromFile(FFilePath);

    Res := RunProcess(FCommandLine, FWorkingDir, 120000, FProcessOutput, EC, @Self.Terminated);

    ID3V1.WriteToFile(FTempFile);
    ID3V2.WriteToFile(FTempFile);
  finally
    ID3V1.Free;
    ID3V2.Free;
  end;


  if Terminated then
  begin
    DeleteFile(PChar(TempFile));
    Exit;
  end;

  Failed := True;

  if FileExists(TempFile) and (Res = 0) and (EC = 0) then
  begin
    LoopStarted := GetTickCount;
    while Failed do
    begin
      try
        FS := TFileStream.Create(TempFile, fmOpenRead or fmShareExclusive);
        try
          Failed := False;
          Break;
        finally
          FS.Free;
        end;
      except
        Sleep(50);
        if GetTickCount > LoopStarted + 5000 then
        begin
          Break;
        end;
      end;
    end;

    //if not Failed then
    //  if not DeleteFile(PChar(FFilePath)) then
    //    Failed := True;

    // Hier ist MoveFileEx, damit der DirWatcher keine Benachrichtigung bekommt.
    if not Failed then
      if not MoveFileEx(PChar(TempFile), PChar(FFilePath), MOVEFILE_REPLACE_EXISTING) then
        Failed := True;
  end;

  DeleteFile(PChar(TempFile));

  if Failed then
    SyncError
  else
    SyncSuccess;
end;

procedure TProcessThread.SyncError;
begin
  if Terminated then
    Exit;
  if Assigned(FOnSuccess) then
    FOnError(Self);
end;

procedure TProcessThread.SyncSuccess;
begin
  if Terminated then
    Exit;
  if Assigned(FOnError) then
    FOnSuccess(Self);
end;

{ TUndoStep }

constructor TUndoStep.Create(Filename: string; Number: Integer;
  StartLine, EndLine, EffectStartLine, EffectEndLine, PlayLine: Cardinal);
begin
  FFilename := Filename;
  FNumber := Number;
  FStartLine := StartLine;
  FEndLine := EndLine;
  FEffectStartLine := EffectStartLine;
  FEffectEndLine := EffectEndLine;
  FPlayLine := PlayLine;
end;

end.



