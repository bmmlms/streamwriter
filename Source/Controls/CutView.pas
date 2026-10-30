{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2026 Alexander Nottelmann

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

{ This unit contains every control the TCutTab displays }
unit CutView;

interface

uses
  AddonSoX,
  AppData,
  AppMessages,
  AudioFunctions,
  Classes,
  ComCtrls,
  ConfigureSoX,
  Constants,
  Controls,
  DataManager,
  DragDrop,
  DropComboTarget,
  DropTarget,
  DynBASS,
  ExtCtrls,
  FileConvertor,
  FileTagger,
  Forms,
  Functions,
  Generics.Collections,
  Graphics,
  GraphUtil,
  LanguageObjects,
  LazUTF8,
  Logging,
  Math,
  MessageBus,
  MStringFunctions,
  Player,
  PlayerManager,
  PostProcess,
  PostProcessConvert,
  PostProcessSoX,
  SWFunctions,
  SysUtils,
  WaveData,
  Windows;

type
  TCutStates = (csReady, csLoading, csWorking, csDecoding, csEncoding, csConvertorError,
    csLoadError, csCutError);

  TCutView = class;

  TProcessThread = class(TThread)
  private
    FCommandLine: string;
    FWorkingDir: string;
    FFilePath: string;
    FTempFile: string;
    FProcessOutput: AnsiString;
    FProgressBuffer: AnsiString;
    FProgress: Integer;

    FOnProgress: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure SyncProgress;
    procedure SyncSuccess;
    procedure SyncError;

    procedure ReadCallbackSoX(Data: AnsiString);
  protected
    procedure Execute; override;
  public
    constructor Create(CommandLine, WorkingDir, FilePath, TempFile: string);

    property TempFile: string read FTempFile;
    property ProcessOutput: AnsiString read FProcessOutput;
    property Progress: Integer read FProgress;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;

  TScanThread = class(TThread)
  private
    FFilename: string;
    FWaveData: TWaveData;
    FWaveDataOwned: Boolean;
    FLastProgress: Integer;

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
    function TakeWaveData: TWaveData;

    property WaveData: TWaveData read FWaveData;
    property OnScanProgress: TNotifyEvent read FOnScanProgress write FOnScanProgress;
    property OnEndScan: TNotifyEvent read FOnEndScan write FOnEndScan;
    property OnScanError: TNotifyEvent read FOnScanError write FOnScanError;
  end;

  TSaveCutThread = class(TThread)
  private
    FInFilename, FOutFilename: string;
    FS, FE: Int64;
    FProgress: Integer;
    FLastProgress: Integer;

    FOnSaveProgress: TNotifyEvent;
    FOnEndSave: TNotifyEvent;
    FOnSaveError: TNotifyEvent;

    procedure SyncSaveCutProgress;
    procedure SyncSaveCutEnd;
    procedure SyncSaveCutError;

    procedure FileConvertorProgress(Sender: TObject; Percent: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(InFilename, OutFilename: string; S, E: Int64);

    property OutFilename: string read FOutFilename;
    property Progress: Integer read FProgress;

    property OnSaveProgress: TNotifyEvent read FOnSaveProgress write FOnSaveProgress;
    property OnEndSave: TNotifyEvent read FOnEndSave write FOnEndSave;
    property OnSaveError: TNotifyEvent read FOnSaveError write FOnSaveError;
  end;

  TMouseMode = (mmDown, mmMove, mmUp);
  TControlMode = (cmNone, cmView, cmScroll);

  TCutPaintBox = class(TCustomControl)
  const
    MinimumDisplayedSampleCount = 30;
  private
    FWaveBuf: Graphics.TBitmap;
    FDrawBuf: Graphics.TBitmap;

    FCutView: TCutView;
    FTimer: TTimer;
    FPlayingIndex: Cardinal;
    FPlayerPaused: Boolean;

    FBackgroundColor, FPeakColor, FPeakEndColor, FStartColor, FEndColor, FPlayColor: TColor;
    FStartLine, FEndLine, FPlayLine, FZoomStartLine, FZoomEndLine, FEffectStartLine, FEffectEndLine: Integer;
    FDoZoom: Boolean;

    FMouseOldX, FMouseOldY, FMouseMoveStartX: Integer;
    FControlMode: TControlMode;
    FScrollbarHeight: Integer;
    FSpacing: Integer;
    FEdge: Integer;

    procedure BuildBuffer;
    procedure BuildDrawBuffer;

    function ClampIndex(Value: Integer): Integer;
    procedure ClampZoomRange(var StartPos, EndPos: Integer);
    procedure SetLine(X: Integer; Button: TMouseButton; Mode: TMouseMode);
    procedure HandleScrollBar(X: Integer; Y: Integer; Mode: TMouseMode);
    function GetControlMode(Y: Integer): TControlMode;
    function PixelsToArray(X: Integer): Integer;
    function GetPlayerPos: Integer;
    procedure ZoomAtCursor(X, Y: Integer; Delta: Integer);

    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;

    procedure TimerTimer(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TUndoStep = class
  private
    FFilename: string;
    FStartLine: Integer;
    FEndLine: Integer;
    FEffectStartLine: Integer;
    FEffectEndLine: Integer;
    FPlayLine: Integer;
  public
    constructor Create(Filename: string; StartLine, EndLine, EffectStartLine, EffectEndLine, PlayLine: Integer);

    property Filename: string read FFilename;
    property StartLine: Integer read FStartLine;
    property EndLine: Integer read FEndLine;
    property EffectStartLine: Integer read FEffectStartLine;
    property EffectEndLine: Integer read FEffectEndLine;
    property PlayLine: Integer read FPlayLine;
  end;

  TUndoList = class(TObjectList<TUndoStep>);

  TLineMode = (lmEdit, lmPlay, lmEffectsMarker);

  TCutFileEvent = procedure(Sender: TObject; Filename: string) of object;

  TCutView = class(TPanel)
  private
    FScanThread: TScanThread;
    FProcessThread: TProcessThread;
    FSaveCutThread: TSaveCutThread;
    FPB: TCutPaintBox;
    FWaveData: TWaveData;
    FState: TCutStates;
    FLineMode: TLineMode;
    FProgressBarLoad: TProgressBar;
    FUndoList: TUndoList;
    FUndoStep: TUndoStep;
    FLastCheckSum: Cardinal;
    FSaving: Boolean;
    FShuttingDown: Boolean;
    FProcessSucceeded: Boolean;
    FSaveCutSucceeded: Boolean;
    FConvertorFileInfo: TAudioInfo;

    FPlayer: TPlayer;
    FOriginalFilename: string;
    FWorkingFilename: string;

    FBitrateType: TBitrates;
    FQuality: TVBRQualities;
    FBitrate: Cardinal;

    FDropTarget: TDropComboTarget;

    FOnStateChanged: TNotifyEvent;
    FOnCutFile: TCutFileEvent;

    FFileTagger: TFileTagger;
    FFileConvertorThread: TPostProcessConvertThread;

    procedure StartProcessing(CmdLine: string);
    procedure AddUndo;
    procedure ClearUndoList(DeleteFiles: Boolean);
    procedure ShutdownThreads;
    procedure ThreadCleanupAsync(Data: PtrInt);
    procedure FileConvertorFinishAsync(Data: PtrInt);
    procedure FileConvertorSavedAsync(Data: PtrInt);
    procedure SaveCutThreadEndSaveAsync(Data: PtrInt);
    procedure SaveCutThreadErrorAsync(Data: PtrInt);
    procedure StateChangedAsync(Data: PtrInt);
    procedure UpdateProgressBar(Percent: Integer);
    procedure SetLoadError;
    function HasWaveData: Boolean;

    procedure ThreadScanProgress(Sender: TObject);
    procedure ThreadEndScan(Sender: TObject);
    procedure ThreadScanError(Sender: TObject);
    procedure ThreadTerminate(Sender: TObject);

    procedure ProcessThreadSuccess(Sender: TObject);
    procedure ProcessThreadError(Sender: TObject);
    procedure ProcessThreadProgress(Sender: TObject);
    procedure ProcessThreadTerminate(Sender: TObject);

    procedure SaveCutThreadProgress(Sender: TObject);
    procedure SaveCutThreadEndSave(Sender: TObject);
    procedure SaveCutThreadError(Sender: TObject);
    procedure SaveCutThreadTerminate(Sender: TObject);

    procedure PlayerEndReached(Sender: TObject);
    procedure PlayerEndReachedAsync(Data: PtrInt);
    procedure PlayerPlay(Sender: TObject);
    procedure PlayerPause(Sender: TObject);
    procedure PlayerStop(Sender: TObject);

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);

    procedure MessageReceived(Msg: TMessageBase);

    function CreateConvertor: Boolean;

    procedure FileConvertorProgress(Sender: TObject);
    procedure FileConvertorFinish(Sender: TObject);
    procedure FileConvertorError(Sender: TObject);
    procedure FileConvertorTerminate(Sender: TObject);

    function GetUndoFilename: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(Filename: string; IsConverted, LoadTrackData: Boolean); overload;
    procedure LoadFile(Track: TTrackInfo); overload;

    function CheckSoX: Boolean;

    procedure SetPosition(Start: Boolean);

    procedure Save;
    procedure Cut;
    procedure Undo;
    procedure Play;
    procedure Stop;
    procedure AutoCut(MaxPeaks: Integer; MinDuration: Cardinal);
    procedure ApplyFade;
    procedure ApplyEffects;
    procedure ZoomIn;
    procedure ZoomOut;

    function CanSave: Boolean;
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

    property LastCheckSum: Cardinal read FLastCheckSum;
    property WaveData: TWaveData read FWaveData;
    property UndoList: TUndoList read FUndoList;
    property Player: TPlayer read FPlayer;
    property LineMode: TLineMode read FLineMode write FLineMode;
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
  FreeOnTerminate := False;
  FFilename := Filename;
  FWaveData := TWaveData.Create;
  FWaveDataOwned := True;
  FLastProgress := -1;
  FWaveData.OnProgress := WaveDataScanProgress;
end;

destructor TScanThread.Destroy;
begin
  if FWaveDataOwned then
    FreeAndNil(FWaveData);
  inherited;
end;

function TScanThread.TakeWaveData: TWaveData;
begin
  Result := FWaveData;
  if Result <> nil then
    Result.OnProgress := nil;
  FWaveData := nil;
  FWaveDataOwned := False;
end;

procedure TScanThread.Execute;
begin
  try
    FWaveData.Load(FFilename);
    Synchronize(SyncEndScan);
  except
    FreeAndNil(FWaveData);
    FWaveDataOwned := False;
    Synchronize(SyncScanError);
  end;
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
  if FWaveData = nil then
    Exit;
  if FWaveData.Progress = FLastProgress then
    Exit;

  FLastProgress := FWaveData.Progress;
  Synchronize(SyncScanProgress);
end;

{ TCutView }

constructor TCutView.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FPB := TCutPaintBox.Create(Self);
  FPB.Parent := Self;
  FPB.Align := alClient;

  FLineMode := lmPlay;

  FProgressBarLoad := TProgressBar.Create(Self);
  FProgressBarLoad.Max := 100;
  FProgressBarLoad.Width := Scale96ToFont(300);
  FProgressBarLoad.Height := Scale96ToFont(PROGRESSBAR_HEIGHT);
  FProgressBarLoad.Parent := Self;
  FProgressBarLoad.AnchorHorizontalCenterTo(Self);
  FProgressBarLoad.AnchorVerticalCenterTo(Self);
  FProgressBarLoad.Visible := False;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfFile];
  FDropTarget.Register(Self);
  FDropTarget.OnDrop := DropTargetDrop;

  FUndoList := TUndoList.Create(True);
  FFileTagger := TFileTagger.Create;

  MsgBus.AddSubscriber(MessageReceived);
end;

function TCutView.CreateConvertor: Boolean;
begin
  Result := False;
  if FShuttingDown or (FFileConvertorThread <> nil) then
    Exit;

  FFileConvertorThread := TPostProcessConvertThread.Create(nil, AppGlobals.Data.StreamSettings.PostProcessors[0]);
  FFileConvertorThread.FreeOnTerminate := False;
  FFileConvertorThread.OnProgress := FileConvertorProgress;
  FFileConvertorThread.OnFinish := FileConvertorFinish;
  FFileConvertorThread.OnError := FileConvertorError;
  FFileConvertorThread.OnTerminate := FileConvertorTerminate;
  Result := True;
end;

destructor TCutView.Destroy;
begin
  FShuttingDown := True;
  FDropTarget.OnDrop := nil;
  // The DropTarget library lacks FreeNotification, so unregister while this control is intact.
  FreeAndNil(FDropTarget);
  MsgBus.RemoveSubscriber(MessageReceived);

  ShutdownThreads;
  FFileTagger.Free;

  if FWaveData <> nil then
    FreeAndNil(FWaveData);

  if FPlayer <> nil then
    FreeAndNil(FPlayer);

  if FUndoStep <> nil then
  begin
    SysUtils.DeleteFile(FUndoStep.Filename);
    FreeAndNil(FUndoStep);
  end;
  ClearUndoList(True);
  FUndoList.Free;
  SysUtils.DeleteFile(FWorkingFilename);

  inherited;
end;

procedure TCutView.ShutdownThreads;
var
  FileConvertorThread: TPostProcessConvertThread;
  ScanThread: TScanThread;
  ProcessThread: TProcessThread;
  SaveCutThread: TSaveCutThread;
  ProcessTempFile, SaveCutTempFile: string;
begin
  Application.RemoveAsyncCalls(Self);

  FileConvertorThread := FFileConvertorThread;
  ScanThread := FScanThread;
  ProcessThread := FProcessThread;
  SaveCutThread := FSaveCutThread;
  ProcessTempFile := '';
  SaveCutTempFile := '';

  if ProcessThread <> nil then
    ProcessTempFile := ProcessThread.TempFile;
  if SaveCutThread <> nil then
    SaveCutTempFile := SaveCutThread.OutFilename;

  if FileConvertorThread <> nil then
  begin
    FileConvertorThread.OnProgress := nil;
    FileConvertorThread.OnFinish := nil;
    FileConvertorThread.OnError := nil;
    FileConvertorThread.OnTerminate := nil;
    FileConvertorThread.Terminate;
  end;

  if ScanThread <> nil then
  begin
    ScanThread.OnScanProgress := nil;
    ScanThread.OnEndScan := nil;
    ScanThread.OnScanError := nil;
    ScanThread.OnTerminate := nil;
    ScanThread.Terminate;
  end;

  if ProcessThread <> nil then
  begin
    ProcessThread.OnSuccess := nil;
    ProcessThread.OnError := nil;
    ProcessThread.OnTerminate := nil;
    ProcessThread.Terminate;
  end;

  if SaveCutThread <> nil then
  begin
    SaveCutThread.OnSaveProgress := nil;
    SaveCutThread.OnEndSave := nil;
    SaveCutThread.OnSaveError := nil;
    SaveCutThread.OnTerminate := nil;
    SaveCutThread.Terminate;
  end;

  if FileConvertorThread <> nil then
  begin
    FileConvertorThread.WaitFor;
    FileConvertorThread.Free;
  end;
  if ScanThread <> nil then
  begin
    ScanThread.WaitFor;
    ScanThread.Free;
  end;
  if ProcessThread <> nil then
  begin
    ProcessThread.WaitFor;
    ProcessThread.Free;
  end;
  if SaveCutThread <> nil then
  begin
    SaveCutThread.WaitFor;
    SaveCutThread.Free;
  end;

  if ProcessTempFile <> '' then
    SysUtils.DeleteFile(ProcessTempFile);
  if SaveCutTempFile <> '' then
    SysUtils.DeleteFile(SaveCutTempFile);

  FFileConvertorThread := nil;
  FScanThread := nil;
  FProcessThread := nil;
  FSaveCutThread := nil;
end;

procedure TCutView.DropTargetDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  i: Integer;
begin
  if FShuttingDown or (FDropTarget = nil) or not Assigned(FOnCutFile) or not (Parent is TCutTab) then
    Exit;

  for i := 0 to FDropTarget.Files.Count - 1 do
    if FilenameToFormat(FDropTarget.Files[i]) <> atNone then
      FOnCutFile(TCutTab(Parent), FDropTarget.Files[i]);
end;

procedure TCutView.FileConvertorError(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  FState := csConvertorError;

  FProgressBarLoad.Visible := False;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;
end;

procedure TCutView.FileConvertorFinish(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  FConvertorFileInfo := TPostProcessConvertThread(Sender).FileInfo;
  Application.QueueAsyncCall(Self.FileConvertorFinishAsync, 0);
end;

procedure TCutView.FileConvertorFinishAsync(Data: PtrInt);
begin
  if FShuttingDown then
    Exit;

  if FState = csEncoding then
  begin
    FFileTagger.Write(Language.CurrentLanguage.LCID, FOriginalFilename);

    ClearUndoList(True);

    if FConvertorFileInfo.Success then
      Application.QueueAsyncCall(Self.FileConvertorSavedAsync, 0);
  end;

  LoadFile(FWorkingFilename, True, False);
end;

procedure TCutView.FileConvertorSavedAsync(Data: PtrInt);
begin
  if FShuttingDown then
    Exit;

  if Assigned(TCutTab(Owner).OnSaved) then
    TCutTab(Owner).OnSaved(Owner, FConvertorFileInfo);
end;

procedure TCutView.FileConvertorProgress(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  UpdateProgressBar(TPostProcessConvertThread(Sender).Progress);
end;

procedure TCutView.FileConvertorTerminate(Sender: TObject);
begin
  if not FShuttingDown then
    Application.QueueAsyncCall(Self.ThreadCleanupAsync, PtrInt(Sender));
end;

procedure TCutView.ThreadCleanupAsync(Data: PtrInt);
var
  Thread: TThread;
  IsFileConvertor, IsScan, IsProcess, IsSaveCut: Boolean;
  SaveCutOutputFilename: string;
begin
  if FShuttingDown then
    Exit;

  Thread := TThread(Pointer(Data));
  if Thread = nil then
    Exit;

  Thread.WaitFor;

  IsFileConvertor := Thread = FFileConvertorThread;
  IsScan := Thread = FScanThread;
  IsProcess := Thread = FProcessThread;
  IsSaveCut := Thread = FSaveCutThread;
  SaveCutOutputFilename := '';
  if IsSaveCut and not FSaveCutSucceeded then
    SaveCutOutputFilename := TSaveCutThread(Thread).OutFilename;

  if IsFileConvertor then
    FFileConvertorThread := nil;
  if IsScan then
  begin
    FScanThread := nil;
    if not Application.Terminated then
    begin
      FProgressBarLoad.Visible := False;
      if FUndoStep <> nil then
        FreeAndNil(FUndoStep);
    end;
  end;
  if IsProcess then
    FProcessThread := nil;
  if IsSaveCut then
  begin
    FSaveCutThread := nil;
    if FScanThread = nil then
      FProgressBarLoad.Visible := False;
  end;

  Thread.Free;

  if SaveCutOutputFilename <> '' then
    SysUtils.DeleteFile(SaveCutOutputFilename);

  if IsProcess and not FShuttingDown then
    if FProcessSucceeded then
    begin
      FState := csReady;
      LoadFile(FWorkingFilename, True, False);
    end else
    begin
      TFunctions.MsgBox(_('An error occured while processing the file.'), _('Error'), MB_ICONERROR);
      LoadFile(FWorkingFilename, True, False);
    end;
end;

function TCutView.GetUndoFilename: string;
begin
  repeat
    Result := ConcatPaths([AppGlobals.TempDir, 'UNDO_%s.wav'.Format([TGUID.NewGuid.ToString(True)])]);
  until not FileExists(Result);
end;

procedure TCutView.LoadFile(Track: TTrackInfo);
begin
  if Track.VBR then
  begin
    FBitrateType := brVBR;
    FQuality := GuessVBRQuality(Track.Bitrate, FilenameToFormat(Track.Filename));
  end else
  begin
    FBitrateType := brCBR;
    FBitrate := Track.Bitrate;
  end;

  LoadFile(Track.Filename, False, False);
end;

procedure TCutView.LoadFile(Filename: string; IsConverted, LoadTrackData: Boolean);
var
  Info: TAudioInfo;
begin
  if FShuttingDown or (FScanThread <> nil) or (FProcessThread <> nil) or
    (not IsConverted and (FFileConvertorThread <> nil)) then
    Exit;

  if IsConverted then
  begin
    if FPlayer <> nil then
      FreeAndNil(FPlayer);

    FPlayer := TPlayer.Create;
    FPlayer.ShowTitle := False;
    FPlayer.OnEndReached := PlayerEndReached;
    FPlayer.OnPlay := PlayerPlay;
    FPlayer.OnPause := PlayerPause;
    FPlayer.OnStop := PlayerStop;
    Players.AddPlayer(FPlayer);

    if not Bass.DeviceAvailable then
    begin
      FreeAndNil(FPlayer);
      SetLoadError;
      Exit;
    end;

    try
      FPlayer.Filename := Filename;
    except
      FreeAndNil(FPlayer);
      SetLoadError;
      Exit;
    end;

    FProgressBarLoad.Position := 0;
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
    FScanThread.Start;

    FPB.FEffectStartLine := 0;
    FPB.FEffectEndLine := 0;

    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
  end else
  begin
    if LoadTrackData then
    begin
      Info.GetAudioInfo(Filename);
      if Info.Success then
      begin
        if Info.VBR then
        begin
          FBitrateType := brVBR;
          FQuality := GuessVBRQuality(Info.Bitrate, FilenameToFormat(Filename));
        end else
        begin
          FBitrateType := brCBR;
          FBitrate := Info.Bitrate;
        end;
      end else
      begin
          SetLoadError;
          Exit;
        end;
    end;

    if not CreateConvertor then
      Exit;

    FOriginalFilename := Filename;
    FWorkingFilename := GetUndoFilename;

    FState := csDecoding;

    FProgressBarLoad.Position := 0;
    FProgressBarLoad.Visible := True;

    FFileConvertorThread.Convert(FOriginalFilename, FWorkingFilename, nil);
    FFileConvertorThread.Start;

    FPB.BuildBuffer;
    FPB.BuildDrawBuffer;
    FPB.Repaint;
  end;
end;

procedure TCutView.MessageReceived(Msg: TMessageBase);
begin
  if Msg is TFileModifyMsg then
    if Assigned(FPlayer) and (UTF8CompareText(FPlayer.Filename, TFileModifyMsg(Msg).Filename) = 0) then
      FPlayer.Stop(False);
end;

procedure TCutView.Cut;
var
  S, E: Int64;
begin
  if not CanCut then
    Exit;

  AddUndo;

  S := FWaveData.WaveArray[FPB.FStartLine].Pos;
  E := FWaveData.WaveArray[FPB.FEndLine].Pos;

  FreeAndNil(FPlayer);
  FreeAndNil(FWaveData);

  FProgressBarLoad.Position := 0;
  FProgressBarLoad.Visible := True;

  FState := csWorking;
  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  FSaveCutThread := TSaveCutThread.Create(FWorkingFilename, GetUndoFilename, S, E);
  FSaveCutSucceeded := False;
  FSaveCutThread.OnSaveProgress := SaveCutThreadProgress;
  FSaveCutThread.OnEndSave := SaveCutThreadEndSave;
  FSaveCutThread.OnSaveError := SaveCutThreadError;
  FSaveCutThread.OnTerminate := SaveCutThreadTerminate;

  FSaveCutThread.Start;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.Undo;
var
  UndoStep: TUndoStep;
begin
  if not CanUndo then
    Exit;

  FreeAndNil(FPlayer);

  UndoStep := FUndoList[FUndoList.Count - 1];

  if FileExists(FWorkingFilename) then
    SysUtils.DeleteFile(FWorkingFilename);

  if not RenameFile(UndoStep.Filename, FWorkingFilename) then
  begin
    TFunctions.MsgBox(_('The file could not be replaced by the saved undo file that contains the last version of the file.'), _('Error'), MB_ICONERROR);
    Exit;
  end;

  FWaveData.Free;
  FWaveData := nil;

  LoadFile(FWorkingFilename, True, False);

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  if FUndoStep <> nil then
  begin
    SysUtils.DeleteFile(FUndoStep.Filename);
    FreeAndNil(FUndoStep);
  end;
  FUndoStep := FUndoList.Extract(UndoStep);

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ZoomIn;
var
  Swap: Integer;
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

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

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
  FPB.Invalidate;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.Save;
var
  EncoderSettings: TEncoderSettings;
begin
  if FShuttingDown or (FFileConvertorThread <> nil) or not CanSave then
    Exit;

  FSaving := True;

  if FPlayer <> nil then
    FreeAndNil(FPlayer);

  FreeAndNil(FWaveData);

  // Wiedergabe ggf. anhalten
  MsgBus.SendMessage(TFileModifyMsg.Create(FOriginalFilename));

  FFileTagger.Read(FOriginalFilename);

  if not CreateConvertor then
    Exit;

  EncoderSettings := TEncoderSettings.Create(FilenameToFormat(FOriginalFilename), FBitrateType, FQuality);
  EncoderSettings.CBRBitrate := FBitrate;
  try
    FFileConvertorThread.Convert(FWorkingFilename, FOriginalFilename, EncoderSettings);
  finally
    EncoderSettings.Free;
  end;
  FFileConvertorThread.Start;

  FState := csEncoding;
  FProgressBarLoad.Position := 0;
  FProgressBarLoad.Visible := True;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.SaveCutThreadEndSave(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  FSaveCutSucceeded := True;
  FWorkingFilename := TSaveCutThread(Sender).OutFilename;
  Application.QueueAsyncCall(Self.SaveCutThreadEndSaveAsync, 0);
end;

procedure TCutView.SaveCutThreadEndSaveAsync(Data: PtrInt);
begin
  if FShuttingDown then
    Exit;

  LoadFile(FWorkingFilename, True, False);
end;

procedure TCutView.SaveCutThreadError(Sender: TObject);
begin
  if not FShuttingDown then
  begin
    FSaveCutSucceeded := False;
    Application.QueueAsyncCall(Self.SaveCutThreadErrorAsync, 0);
  end;
end;

procedure TCutView.SaveCutThreadErrorAsync(Data: PtrInt);
var
  DriveLetter: string;
begin
  if FShuttingDown then
    Exit;
  FSaving := False;

  FProgressBarLoad.Visible := False;
  FState := csCutError;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;

  DriveLetter := '';
  if UTF8LengthFast(AppGlobals.TempDir) > 3 then
  begin
    DriveLetter := Copy(AppGlobals.TempDir, 1, 2);
    TFunctions.MsgBox(Format(_('The temporary cut-file could not be saved. Make sure there is enough free diskspace on drive %s.'), [DriveLetter]), _('Error'), MB_ICONERROR);
  end;

  if Assigned(FOnStateChanged) then
    Application.QueueAsyncCall(Self.StateChangedAsync, 0);
end;

procedure TCutView.StateChangedAsync(Data: PtrInt);
begin
  if not FShuttingDown and Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.UpdateProgressBar(Percent: Integer);
begin
  if FProgressBarLoad = nil then
    Exit;

  if Percent < FProgressBarLoad.Min then
    Percent := FProgressBarLoad.Min
  else if Percent > FProgressBarLoad.Max then
    Percent := FProgressBarLoad.Max;

  FProgressBarLoad.Position := Percent;
end;

function TCutView.HasWaveData: Boolean;
begin
  Result := False;
  if FWaveData = nil then
    Exit;
  Result := Length(FWaveData.WaveArray) > 0;
end;

procedure TCutView.SaveCutThreadProgress(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  UpdateProgressBar(TSaveCutThread(Sender).Progress);
end;

procedure TCutView.SaveCutThreadTerminate(Sender: TObject);
begin
  if not FShuttingDown then
    Application.QueueAsyncCall(Self.ThreadCleanupAsync, PtrInt(Sender));
end;

procedure TCutView.SetPosition(Start: Boolean);
var
  Paused: Boolean;
begin
  if not HasWaveData then
    Exit;
  if Length(FWaveData.WaveArray) < 2 then
    Exit;

  if FWaveData.ZoomSize < Length(FWaveData.WaveArray) - 1 then
  begin
    if Start then
    begin
      FPB.FZoomStartLine := 0;
      FPB.FZoomEndLine := FWaveData.ZoomSize;
    end else
    begin
      FPB.FZoomStartLine := Length(FWaveData.WaveArray) - FWaveData.ZoomSize - 1;
      FPB.FZoomEndLine := Length(FWaveData.WaveArray) - 1;
    end;
    FPB.FDoZoom := True;

    FPB.BuildBuffer;
  end;

  if Start then
  begin
    Paused := False;
    if Assigned(FPlayer) then
    begin
      Paused := FPlayer.Playing;
      FPlayer.Pause;
      FPlayer.PositionByte := FWaveData.WaveArray[0].Pos;
    end;

    FPB.FPlayLine := 0;

    if Paused then
      FPlayer.Play;
  end else
  begin
    Stop;
    if Assigned(FPlayer) then
      FPlayer.PositionByte := FWaveData.WaveArray[Length(FWaveData.WaveArray) - 1].Pos;
    FPB.FPlayLine := Length(FWaveData.WaveArray) - 1;
  end;

  FPB.BuildDrawBuffer;
  FPB.Invalidate;
end;

procedure TCutView.Play;
begin
  if not CanPlay then
    Exit;

  FPB.FPlayingIndex := 0;

  if FPlayer = nil then
    Exit;

  FPlayer.Volume := Players.Volume;
  FPlayer.PositionByte := FWaveData.WaveArray[FPB.FPlayLine].Pos;

  FPlayer.Play;

  FPlayer.PosToReach := FWaveData.WaveArray[High(FWaveData.WaveArray)].Pos;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerEndReached(Sender: TObject);
begin
  if FShuttingDown or (FPlayer = nil) or (Sender <> FPlayer) then
    Exit;

  FPB.FTimer.Enabled := False;
  FPlayer.Stop(False);
  FPlayer.PositionByte := 0;

  Application.QueueAsyncCall(Self.PlayerEndReachedAsync, 0);
end;

procedure TCutView.PlayerEndReachedAsync(Data: PtrInt);
begin
  if FShuttingDown then
    Exit;

  FPB.FPlayLine := 0;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerPause(Sender: TObject);
begin
  if FShuttingDown then
    Exit;
  FPB.FTimer.Enabled := False;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerPlay(Sender: TObject);
begin
  if FShuttingDown then
    Exit;
  FPB.FTimer.Enabled := True;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.PlayerStop(Sender: TObject);
begin
  if FShuttingDown then
    Exit;
  FPB.FTimer.Enabled := False;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ProcessThreadError(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  FProcessSucceeded := False;
end;

procedure TCutView.ProcessThreadSuccess(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  FWorkingFilename := TProcessThread(Sender).TempFile;
  FProcessSucceeded := True;
end;

procedure TCutView.ProcessThreadProgress(Sender: TObject);
begin
  if FShuttingDown then
    Exit;

  UpdateProgressBar(TProcessThread(Sender).Progress);
end;

procedure TCutView.ProcessThreadTerminate(Sender: TObject);
begin
  if not FShuttingDown then
    Application.QueueAsyncCall(Self.ThreadCleanupAsync, PtrInt(Sender));
end;

procedure TCutView.StartProcessing(CmdLine: string);
var
  TempFile: string;
begin
  AddUndo;
  FProcessSucceeded := False;

  if FPlayer <> nil then
    FreeAndNil(FPlayer);

  if FWaveData <> nil then
    FreeAndNil(FWaveData);

  TempFile := GetUndoFilename;

  CmdLine := StringReplace(CmdLine, '[[TEMPFILE]]', TempFile, [rfReplaceAll]);

  FState := csWorking;
  FProgressBarLoad.Position := 0;
  FProgressBarLoad.Visible := True;
  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  FProcessThread := TProcessThread.Create(CmdLine, ExtractFilePath(AppGlobals.AddonManager.Find(TAddonSoX).ModuleFilePath), FWorkingFilename, TempFile);
  FProcessThread.OnProgress := ProcessThreadProgress;
  FProcessThread.OnSuccess := ProcessThreadSuccess;
  FProcessThread.OnError := ProcessThreadError;
  FProcessThread.OnTerminate := ProcessThreadTerminate;
  FProcessThread.Start;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.Stop;
begin
  if not CanStop then
    Exit;

  if FPlayer <> nil then
    FPlayer.Pause;

  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ApplyFade;
var
  CmdLine: string;
  FadeTo, FadeStart: Integer;
begin
  if not HasWaveData then
    Exit;
  if Length(FWaveData.WaveArray) < 2 then
    Exit;
  if not CheckSoX then
    Exit;

  CmdLine := '"' + AppGlobals.AddonManager.Find(TAddonSoX).ModuleFilePath + '" --show-progress "' + FWorkingFilename + '" ' + '"[[TEMPFILE]]" ';

  if CanApplyFadeIn then
  begin
    FadeTo := Max(FPB.FEffectStartLine, FPB.FEffectEndLine);

    CmdLine := CmdLine + 'fade p ' + IntToStr(Round(FWaveData.WaveArray[FadeTo].Sec));

    StartProcessing(CmdLine);
  end else if CanApplyFadeOut then
  begin
    FadeStart := Min(FPB.FEffectStartLine, FPB.FEffectEndLine);

    CmdLine := CmdLine + 'fade p 0 ' + IntToStr(Round(FWaveData.WaveArray[High(FWaveData.WaveArray)].Sec)) + ' ' + IntToStr(Round(FWaveData.WaveArray[High(FWaveData.WaveArray)].Sec - FWaveData.WaveArray[FadeStart].Sec));

    StartProcessing(CmdLine);
  end;
end;

procedure TCutView.AddUndo;
begin
  FUndoList.Add(TUndoStep.Create(FWorkingFilename, FPB.FStartLine, FPB.FEndLine, FPB.FEffectStartLine, FPB.FEffectEndLine, FPB.FPlayLine));
end;

procedure TCutView.ClearUndoList(DeleteFiles: Boolean);
var
  i: Integer;
begin
  if FUndoList = nil then
    Exit;

  if DeleteFiles then
    for i := 0 to FUndoList.Count - 1 do
      SysUtils.DeleteFile(FUndoList[i].Filename);

  FUndoList.Clear;
end;

procedure TCutView.ApplyEffects;
var
  CmdLine: string;
  F: TfrmConfigureSoX;
begin
  if not HasWaveData then
    Exit;
  if Length(FWaveData.WaveArray) < 2 then
    Exit;
  if not CheckSoX then
    Exit;

  F := TfrmConfigureSox.Create(Self, (AppGlobals.Data.StreamSettings.PostProcessors.Find(TPostProcessSoX) as TPostProcessSoX), AppGlobals.ApplyEffectNormalize, False, False,
    AppGlobals.ApplyEffectFadeoutStart, AppGlobals.ApplyEffectFadeoutEnd, False, False, AppGlobals.ApplyEffectSilenceStart, AppGlobals.ApplyEffectSilenceEnd, Trunc(FWaveData.WaveArray[High(FWaveData.WaveArray)].Sec));
  try
    F.ShowModal;

    if F.SaveData then
    begin
      AppGlobals.ApplyEffectFadeoutStart := F.FadeoutStartLength;
      AppGlobals.ApplyEffectFadeoutEnd := F.FadeoutEndLength;
      AppGlobals.ApplyEffectSilenceStart := F.SilenceStartLength;
      AppGlobals.ApplyEffectSilenceEnd := F.SilenceEndLength;
      AppGlobals.ApplyEffectNormalize := F.Normalize;

      CmdLine := '';

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

      if (CmdLine <> '') or F.Normalize then
      begin
        if F.Normalize then
          CmdLine := '"' + AppGlobals.AddonManager.Find(TAddonSoX).ModuleFilePath + '" --show-progress --norm "' + FWorkingFilename + '" ' + '"[[TEMPFILE]]" ' + CmdLine
        else
          CmdLine := '"' + AppGlobals.AddonManager.Find(TAddonSoX).ModuleFilePath + '" --show-progress "' + FWorkingFilename + '" ' + '"[[TEMPFILE]]" ' + CmdLine;

        StartProcessing(CmdLine);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCutView.AutoCut(MaxPeaks: Integer; MinDuration: Cardinal);
begin
  if not HasWaveData then
    Exit;
  if Length(FWaveData.WaveArray) < 2 then
    Exit;

  FWaveData.ClearSilence;
  FWaveData.AutoCut(False, MaxPeaks, MinDuration, 0, High(FWaveData.WaveArray) div 2);
  FWaveData.AutoCut(True, MaxPeaks, MinDuration, High(FWaveData.WaveArray) div 2, High(FWaveData.WaveArray));

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;
end;

function TCutView.CanCut: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  if Length(FWaveData.WaveArray) < 2 then
    Exit;

  Result := (FPB.FEndLine - FPB.FStartLine > 0) and
    (FWaveData.TimeBetween(FPB.FStartLine, FPB.FEndLine) >= 0.5) and
    ((FPB.FStartLine > 0) or (FPB.FEndLine < High(FWaveData.WaveArray)));
end;

function TCutView.CanEffectsMarker: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := Length(FWaveData.WaveArray) > 1;
end;

function TCutView.CanUndo: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := (Length(FWaveData.WaveArray) > 1) and (FUndoList.Count > 0);
end;

function TCutView.CanPlay: Boolean;
begin
  Result := False;
  if (FPlayer = nil) or not HasWaveData then
    Exit;
  Result := (Length(FWaveData.WaveArray) > 1) and (not FPlayer.Playing);
end;

function TCutView.CanStop: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  if Length(FWaveData.WaveArray) < 2 then
    Exit;
  Result := (FPlayer <> nil) and FPlayer.Playing;
end;

function TCutView.CanApplyFadeIn: Boolean;
var
  Tolerance: Integer;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  if (Length(FWaveData.WaveArray) < 2) or (FPB.ClientWidth <= 0) then
    Exit;

  Tolerance := Trunc((FWaveData.ZoomSize div FPB.ClientWidth) * 3.5) + 4;
  Result := (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5) and ((FPB.FEffectStartLine <= Tolerance) or (FPB.FEffectEndLine <= Tolerance));
end;

function TCutView.CanApplyFadeOut: Boolean;
var
  Tolerance: Integer;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  if (Length(FWaveData.WaveArray) < 2) or (FPB.ClientWidth <= 0) then
    Exit;

  Tolerance := Trunc((FWaveData.ZoomSize div FPB.ClientWidth) * 3.5) + 4;
  Result := (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5) and ((FPB.FEffectStartLine >= Length(FWaveData.WaveArray) - Tolerance) or (FPB.FEffectEndLine >= Length(FWaveData.WaveArray) - Tolerance));
end;

function TCutView.CanApplyEffects: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := Length(FWaveData.WaveArray) > 1;
end;

function TCutView.CanAutoCut: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := Length(FWaveData.WaveArray) > 1;
end;

function TCutView.CanSave: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := (Length(FWaveData.WaveArray) > 1) and (FLastCheckSum <> FWaveData.CheckSum);
end;

function TCutView.CanSetLine: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := Length(FWaveData.WaveArray) > 1;
end;

function TCutView.CanZoomIn: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := (Length(FWaveData.WaveArray) > 1) and
    (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5);
end;

function TCutView.CanZoomOut: Boolean;
begin
  Result := False;
  if not HasWaveData then
    Exit;
  Result := (Length(FWaveData.WaveArray) > 1) and
    ((FWaveData.ZoomStart <> 0) or (FWaveData.ZoomEnd <> High(FWaveData.WaveArray)));
end;

function TCutView.CheckSoX: Boolean;
var
  PostProcessor: TPostProcessSoX;
begin
  PostProcessor := AppGlobals.Data.StreamSettings.PostProcessors.Find(TPostProcessSoX) as TPostProcessSoX;

  if not PostProcessor.DependenciesMet then
    Result := AppGlobals.PostProcessManager.EnablePostProcess(GetParentForm(Self), True, PostProcessor)
  else
    Result := True;
end;

procedure TCutView.ThreadEndScan(Sender: TObject);
begin
  if FShuttingDown then
    Exit;
  FProgressBarLoad.Visible := False;

  if FWaveData <> nil then
    FreeAndNil(FWaveData);
  FWaveData := TScanThread(Sender).TakeWaveData;
  if not HasWaveData then
  begin
    SetLoadError;
    Exit;
  end;

  if (FSaving) or (FUndoList.Count = 0) then
    FLastCheckSum := FWaveData.CheckSum;
  FSaving := False;

  if FUndoStep <> nil then
  begin
    FPB.FStartLine := FPB.ClampIndex(FUndoStep.StartLine);
    FPB.FEndLine := FPB.ClampIndex(FUndoStep.EndLine);
    if FUndoStep.EffectStartLine < 0 then
      FPB.FEffectStartLine := -1
    else
      FPB.FEffectStartLine := FPB.ClampIndex(FUndoStep.EffectStartLine);
    if FUndoStep.EffectEndLine < 0 then
      FPB.FEffectEndLine := -1
    else
      FPB.FEffectEndLine := FPB.ClampIndex(FUndoStep.EffectEndLine);
    FPB.FPlayLine := FPB.ClampIndex(FUndoStep.PlayLine);
  end else
  begin
    FPB.FStartLine := 0;
    FPB.FEndLine := High(FWaveData.WaveArray);
    FPB.FEffectStartLine := -1;
    FPB.FEffectEndLine := -1;
    FPB.FPlayLine := 0;
  end;

  if Length(FWaveData.WaveArray) > 1 then
  begin
    if FPB.FStartLine >= FPB.FEndLine then
    begin
      FPB.FEndLine := Min(High(FWaveData.WaveArray), FPB.FStartLine + 1);
      if FPB.FStartLine >= FPB.FEndLine then
        FPB.FStartLine := Max(0, FPB.FEndLine - 1);
    end;
  end else
  begin
    FPB.FStartLine := 0;
    FPB.FEndLine := 0;
  end;
  FPB.FPlayingIndex := Cardinal(FPB.ClampIndex(FPB.FPlayLine));

  FState := csReady;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Invalidate;

  if Assigned(FOnStateChanged) then
    Application.QueueAsyncCall(Self.StateChangedAsync, 0);
end;

procedure TCutView.ThreadScanError(Sender: TObject);
begin
  SetLoadError;
end;

procedure TCutView.ThreadScanProgress(Sender: TObject);
begin
  if FShuttingDown or (TScanThread(Sender).WaveData = nil) then
    Exit;

  UpdateProgressBar(TScanThread(Sender).WaveData.Progress);
end;

procedure TCutView.ThreadTerminate(Sender: TObject);
begin
  if not FShuttingDown then
    Application.QueueAsyncCall(Self.ThreadCleanupAsync, PtrInt(Sender));
end;

procedure TCutView.SetLoadError;
begin
  if FShuttingDown then
    Exit;

  FSaving := False;
  FProgressBarLoad.Visible := False;
  FState := csLoadError;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;
end;

{ TCutPaintBox }

function TCutPaintBox.ClampIndex(Value: Integer): Integer;
var
  MaxIndex: Integer;
begin
  Result := 0;
  if FCutView = nil then
    Exit;
  if not FCutView.HasWaveData then
    Exit;

  MaxIndex := High(FCutView.FWaveData.WaveArray);
  if Value < 0 then
    Result := 0
  else if Value > MaxIndex then
    Result := MaxIndex
  else
    Result := Value;
end;

procedure TCutPaintBox.ClampZoomRange(var StartPos, EndPos: Integer);
var
  MaxIndex, MinimumRange, Swap: Integer;
begin
  if FCutView = nil then
  begin
    StartPos := 0;
    EndPos := -1;
    Exit;
  end;
  if not FCutView.HasWaveData then
  begin
    StartPos := 0;
    EndPos := -1;
    Exit;
  end;

  MaxIndex := High(FCutView.FWaveData.WaveArray);
  StartPos := ClampIndex(StartPos);
  EndPos := ClampIndex(EndPos);
  if StartPos > EndPos then
  begin
    Swap := StartPos;
    StartPos := EndPos;
    EndPos := Swap;
  end;

  MinimumRange := Min(MinimumDisplayedSampleCount, MaxIndex);
  if EndPos - StartPos < MinimumRange then
  begin
    EndPos := Min(MaxIndex, StartPos + MinimumRange);
    StartPos := Max(0, EndPos - MinimumRange);
  end;
end;

procedure TCutPaintBox.BuildBuffer;

  procedure DrawPeakEnd(X, Y: Integer);
  begin
    if (X < 0) or (X >= FWaveBuf.Width) or (Y < 0) or (Y >= FWaveBuf.Height) then
      Exit;

    FWaveBuf.Canvas.Brush.Color := FPeakEndColor;
    FWaveBuf.Canvas.FillRect(Classes.Rect(X, Y, X + 1, Y + 1));
  end;

  procedure DrawTransparentBox(StartIdx: Integer; EndIdx: Integer; BrushColor: TColor; PenColor: TColor = clNone);
  var
    RectStart, RectEnd: Integer;
    OriginalMode: TPenMode;
  begin
    if PenColor = clNone then
      PenColor := BrushColor;

    RectStart := Trunc(((StartIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FWaveBuf.Width);
    RectEnd := Trunc(((EndIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FWaveBuf.Width);

    with FWaveBuf.Canvas do
    begin
      Pen.Color := PenColor;
      OriginalMode := Pen.Mode;
      Pen.Mode := TPenMode.pmXor;
      Brush.Color := BrushColor;
      Rectangle(RectStart, 0, RectEnd, FWaveBuf.Height - FScrollbarHeight - FEdge);
      Pen.Mode := OriginalMode;
    end;
  end;

  procedure DrawScrollBar(Color: TColor);
  var
    StartX, StartY, EndX, MaxIndex: Integer;
  begin
    MaxIndex := Max(1, High(FCutView.FWaveData.WaveArray));
    with FWaveBuf.Canvas do
    begin
      // Draw Outline
      Pen.Color := Color;
      StartY := ClientHeight - 2 - FScrollbarHeight;
      MoveTo(1, StartY);
      LineTo(Width - 2, StartY);
      LineTo(Width - 2, ClientHeight - 2);
      LineTo(1, ClientHeight - 2);
      LineTo(1, StartY);

      // Draw Bar
      StartX := Trunc((Int64(FCutView.FWaveData.ZoomStart) * (FWaveBuf.Width - 6)) / MaxIndex) + 3;
      EndX := Trunc((Int64(FCutView.FWaveData.ZoomEnd) * (FWaveBuf.Width - 6)) / MaxIndex) + 3;
      if StartX = EndX then
        EndX := StartX + 1;

      Brush.Color := Color;
      FillRect(StartX, StartY + 2, EndX, StartY + FScrollbarHeight - 1);
    end;
  end;

var
  i, V, VNext, ZoomRangeSize: Integer;
  V2: Double;
  Last: Integer;
  LBuf, RBuf: Cardinal;
  Added: Cardinal;
  HT: Cardinal;
  ArrayFrom, ArrayTo: Integer;
  L1, L2: Integer;
  CS, CE: Integer;
  TextWrite: string = '';
begin
  if (FWaveBuf = nil) or (FWaveBuf.Width <= 6) or (FWaveBuf.Height < 2) then
    Exit;
  if FCutView = nil then
    Exit;

  FWaveBuf.Canvas.Brush.Color := FBackgroundColor;
  FWaveBuf.Canvas.FillRect(Classes.Rect(0, 0, FWaveBuf.Width, FWaveBuf.Height));

  if (ClientHeight < 2) or (ClientWidth < 2) then
    Exit;

  case FCutView.FState of
    csLoadError:
      TextWrite := _('Error loading file');
    csCutError:
      TextWrite := _('Error cutting file');
    csConvertorError:
      TextWrite := _('Error decoding/encoding file');
    csWorking:
      TextWrite := _('Working...');
    csDecoding:
      TextWrite := _('Decoding...');
    csEncoding:
      TextWrite := _('Encoding...');
    csLoading:
      TextWrite := _('Loading...');
  end;

  if TextWrite <> '' then
  begin
    FWaveBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FWaveBuf.Canvas.Handle, TRANSPARENT);
    FWaveBuf.Canvas.TextOut(FWaveBuf.Width div 2 - FWaveBuf.Canvas.GetTextWidth(TextWrite) div 2, FWaveBuf.Height div 2 - FWaveBuf.Canvas.GetTextHeight(TextWrite) - Scale96ToFont(PROGRESSBAR_HEIGHT) div 2 - FSpacing, TextWrite);
    Exit;
  end;

  if not FCutView.HasWaveData then
    Exit;

  HT := (FWaveBuf.Height div 2) - FScrollbarHeight - 1;

  LBuf := 0;
  RBuf := 0;
  Added := 0;
  Last := -1;

  if FDoZoom then
  begin
    if FZoomStartLine <= -1 then
    begin
      FZoomStartLine := 0;
      FZoomEndLine := High(FCutView.FWaveData.WaveArray);
    end;

    ClampZoomRange(FZoomStartLine, FZoomEndLine);
    FCutView.FWaveData.ZoomStart := FZoomStartLine;
    FCutView.FWaveData.ZoomEnd := FZoomEndLine;
    FZoomStartLine := -1;
    FZoomEndLine := -1;
    FDoZoom := False;
  end;

  ArrayFrom := FCutView.FWaveData.ZoomStart;
  ArrayTo := FCutView.FWaveData.ZoomEnd;
  ClampZoomRange(ArrayFrom, ArrayTo);
  FCutView.FWaveData.ZoomStart := ArrayFrom;
  FCutView.FWaveData.ZoomEnd := ArrayTo;
  ZoomRangeSize := Max(1, ArrayTo - ArrayFrom);

  for i := 0 to FCutView.FWaveData.Silence.Count - 1 do
  begin
    CS := FCutView.FWaveData.Silence[i].CutStart;
    CE := FCutView.FWaveData.Silence[i].CutEnd;

    L1 := Floor(((CS - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FWaveBuf.Width);
    L2 := Ceil(((CE - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FWaveBuf.Width);

    if L2 - L1 <= 2 then
      Inc(L2);

    FWaveBuf.Canvas.Brush.Color := clGray;
    FWaveBuf.Canvas.FillRect(Classes.Rect(L1, 0, L2, HT - 1));
    FWaveBuf.Canvas.FillRect(Classes.Rect(L1, HT + 1, L2, FWaveBuf.Height - FScrollbarHeight - FEdge));
  end;

  V2 := (1 / 1) * FWaveBuf.Width;

  for i := ArrayFrom to ArrayTo do
  begin
    V := Integer(Trunc(((i - ArrayFrom) / ZoomRangeSize) * V2));
    VNext := Integer(Trunc(((i - ArrayFrom + 1) / ZoomRangeSize) * V2));

    if V = Last then
    begin
      LBuf := LBuf + FCutView.FWaveData.WaveArray[i].L;
      RBuf := RBuf + FCutView.FWaveData.WaveArray[i].R;
      Added := Added + 1;
      Continue;
    end else if Added > 0 then
    begin
      LBuf := LBuf div Added;
      RBuf := RBuf div Added;
    end else
    begin
      LBuf := FCutView.FWaveData.WaveArray[i].L;
      RBuf := FCutView.FWaveData.WaveArray[i].R;
    end;

    FWaveBuf.Canvas.Pen.Color := FPeakColor;
    FWaveBuf.Canvas.Brush.Color := FPeakColor;
    if abs(VNext - V) <= 2 then
    begin
      FWaveBuf.Canvas.MoveTo(V, HT - 1);
      FWaveBuf.Canvas.LineTo(V, HT - 1 - Trunc((LBuf / 33000) * HT));
      DrawPeakEnd(V, HT - 1 - Trunc((LBuf / 33000) * HT));
      FWaveBuf.Canvas.MoveTo(V, HT + 1);
      FWaveBuf.Canvas.LineTo(V, HT + 1 + Trunc((RBuf / 33000) * HT));
      DrawPeakEnd(V, HT + 1 + Trunc((RBuf / 33000) * HT));
    end else
    begin
      FWaveBuf.Canvas.FillRect(Classes.Rect(V, HT, VNext - 1, HT - Trunc((LBuf / 33000) * HT)));
      FWaveBuf.Canvas.FillRect(Classes.Rect(V, HT + 1, VNext - 1, HT + 1 + Trunc((RBuf / 33000) * HT)));
    end;

    RBuf := 0;
    LBuf := 0;
    Added := 0;

    Last := V;
  end;

  if Added > 0 then
  begin
    LBuf := LBuf div Added;
    RBuf := RBuf div Added;

    FWaveBuf.Canvas.Pen.Color := FPeakColor;
    FWaveBuf.Canvas.Brush.Color := FPeakColor;

    FWaveBuf.Canvas.MoveTo(Last, HT - 1);
    FWaveBuf.Canvas.LineTo(Last, HT - 1 - Trunc((LBuf / 33000) * HT));
    DrawPeakEnd(Last, HT - 1 - Trunc((LBuf / 33000) * HT));
    FWaveBuf.Canvas.MoveTo(Last, HT + 1);
    FWaveBuf.Canvas.LineTo(Last, HT + 1 + Trunc((RBuf / 33000) * HT));
    DrawPeakEnd(Last, HT + 1 + Trunc((RBuf / 33000) * HT));
  end;

  FWaveBuf.Canvas.Pen.Color := ColorAdjustLuma(FPeakColor, -20, False);
  FWaveBuf.Canvas.MoveTo(0, HT);
  FWaveBuf.Canvas.LineTo(FWaveBuf.Width, HT);

  DrawTransparentBox(FEffectStartLine, FEffectEndLine, ColorAdjustLuma(FBackgroundColor, -200, False));

  DrawScrollBar(FPeakColor);
end;

procedure TCutPaintBox.BuildDrawBuffer;

  procedure DrawLine(ArrayIdx: Integer; Color: TColor);
  var
    L: Integer;
  begin
    ArrayIdx := ClampIndex(ArrayIdx);
    L := Trunc(((ArrayIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FDrawBuf.Width);

    FDrawBuf.Canvas.Pen.Color := Color;
    FDrawBuf.Canvas.Line(L, FEdge, L, FDrawBuf.Height - FScrollbarHeight - FEdge * 2);
  end;

  procedure DrawLineText(ArrayIdx, Y: Integer);
  var
    L, TextWidth: Integer;
    SecText: string;
  begin
    ArrayIdx := ClampIndex(ArrayIdx);
    L := Trunc(((ArrayIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FDrawBuf.Width);
    SecText := BuildTime(FCutView.FWaveData.WaveArray[ArrayIdx].Sec, True);
    TextWidth := FDrawBuf.Canvas.GetTextWidth(SecText);
    if FDrawBuf.Width < L + FSpacing + TextWidth then
      FDrawBuf.Canvas.TextOut(L - FSpacing - TextWidth, Y, SecText)
    else
      FDrawBuf.Canvas.TextOut(L + FSpacing, Y, SecText);
  end;

begin
  if (FWaveBuf = nil) or (FDrawBuf = nil) then
    Exit;

  FDrawBuf.SetSize(FWaveBuf.Width, FWaveBuf.Height);

  FDrawBuf.Canvas.Draw(0, 0, FWaveBuf);

  if FCutView = nil then
    Exit;
  if not FCutView.HasWaveData then
    Exit;
  if FCutView.FState in [csWorking, csDecoding, csEncoding, csConvertorError] then
    Exit;

  DrawLine(FStartLine, FStartColor);
  DrawLine(FEndLine, FEndColor);
  DrawLine(FPlayLine, FPlayColor);

  FDrawBuf.Canvas.Font.Color := clWhite;
  SetBkMode(FDrawBuf.Canvas.Handle, TRANSPARENT);

  FDrawBuf.Canvas.TextOut(FSpacing, FSpacing, BuildTime(FCutView.FWaveData.Secs, True) + ' - ' + TFunctions.RemoveFileExt(ExtractFileName(FCutView.FOriginalFilename)));

  DrawLineText(FPlayLine, FSpacing * 2 + FDrawBuf.Canvas.GetTextHeight('Ay'));
  DrawLineText(FStartLine, FSpacing * 3 + FDrawBuf.Canvas.GetTextHeight('Ay') * 2);
  DrawLineText(FEndLine, FSpacing * 4 + FDrawBuf.Canvas.GetTextHeight('Ay') * 3);
end;

constructor TCutPaintBox.Create(AOwner: TComponent);
begin
  inherited;

  FBackgroundColor := clBlack;
  FPeakColor := AppGlobals.AccentColor;
  FPeakEndColor := GetGradientColor(FPeakColor, FBackgroundColor, 0.5);
  FStartColor := TFunctions.HTML2Color('ece52b');
  FEndColor := TFunctions.HTML2Color('218030');
  FPlayColor := TFunctions.HTML2Color('c33131');

  FControlMode := cmNone;
  TabStop := True;
  FScrollbarHeight := GetSystemMetrics(SM_CYHSCROLL);
  FSpacing := Scale96ToFont(4);
  FEdge := GetSystemMetrics(SM_CXEDGE);

  FCutView := TCutView(AOwner);
  if FWaveBuf = nil then
    FWaveBuf := Graphics.TBitmap.Create;
  if FDrawBuf = nil then
    FDrawBuf := Graphics.TBitmap.Create;

  FZoomStartLine := -1;
  FZoomEndLine := -1;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerTimer;
end;

destructor TCutPaintBox.Destroy;
begin
  FTimer.Enabled := False;
  FWaveBuf.Free;
  FDrawBuf.Free;
  inherited;
end;

procedure TCutPaintBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  Self.SetFocus;

  if FCutView = nil then
    Exit;
  if not FCutView.CanSetLine then
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
        HandleScrollBar(X, Y, mmDown);
  end;
end;

function TCutPaintBox.GetControlMode(Y: Integer): TControlMode;
begin
  if Y >= Height - FScrollbarHeight - FEdge then
    Result := cmScroll
  else
    Result := cmView;
end;

procedure TCutPaintBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Button: TMouseButton;
begin
  inherited;

  if FCutView = nil then
    Exit;
  if not FCutView.CanSetLine then
    Exit;

  if (X <> FMouseOldX) or (Y <> FMouseOldY) then
  begin
    Button := mbLeft;
    if ssLeft in Shift then
      Button := mbLeft
    else if ssRight in Shift then
      Button := mbRight;

    case FControlMode of
      cmView:
        if (ssLeft in Shift) or (ssRight in Shift) then
          SetLine(X, Button, mmMove);
      cmScroll:
        if ssLeft in Shift then
          HandleScrollBar(X, Y, mmMove);
    end;

    FMouseOldX := X;
    FMouseOldY := Y;
  end;
end;

procedure TCutPaintBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FControlMode = cmView then
    SetLine(X, Button, mmUp);

  SetCaptureControl(nil);
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

  FWaveBuf.SetSize(ClientWidth, ClientHeight);

  BuildBuffer;
  BuildDrawBuffer;
end;

function TCutPaintBox.PixelsToArray(X: Integer): Integer;
begin
  Result := 0;
  if FCutView = nil then
    Exit;
  if not FCutView.HasWaveData then
    Exit;
  if FWaveBuf = nil then
    Exit;
  if FWaveBuf.Width <= 0 then
    Exit;

  Result := FCutView.FWaveData.ZoomStart + Ceil((X / FWaveBuf.Width) * FCutView.FWaveData.ZoomSize);
  Result := ClampIndex(Result);
  Result := Max(FCutView.FWaveData.ZoomStart, Min(Result, FCutView.FWaveData.ZoomEnd));
end;

function TCutPaintBox.GetPlayerPos: Integer;
var
  i: Integer;
  SearchFrom, BytePos: Int64;
begin
  Result := -1;
  if FCutView = nil then
    Exit;
  if (FCutView.FPlayer = nil) or not FCutView.HasWaveData then
    Exit;

  if FPlayingIndex > Cardinal(High(FCutView.FWaveData.WaveArray)) then
    FPlayingIndex := Cardinal(High(FCutView.FWaveData.WaveArray));

  BytePos := FCutView.FPlayer.PositionByte;

  SearchFrom := 0;
  if BytePos > FCutView.FWaveData.WaveArray[Integer(FPlayingIndex)].Pos then
    SearchFrom := FPlayingIndex;

  for i := SearchFrom to High(FCutView.FWaveData.WaveArray) - 1 do
    if FCutView.FWaveData.WaveArray[i].Pos >= BytePos then
    begin
      Result := i;
      Break;
    end;

  if Result < 0 then
    Result := High(FCutView.FWaveData.WaveArray);
end;

procedure TCutPaintBox.SetLine(X: Integer; Button: TMouseButton; Mode: TMouseMode);
var
  ArrayPos: Integer;
begin
  if FCutView = nil then
    Exit;
  if not FCutView.HasWaveData then
    Exit;
  if Length(FCutView.FWaveData.WaveArray) < 2 then
    Exit;

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
          FStartLine := Max(FCutView.FWaveData.ZoomStart, FCutView.FWaveData.ZoomEnd - 1);
        if FEndLine <= FStartLine then
          FEndLine := Min(High(FCutView.FWaveData.WaveArray), FStartLine + 1);
        if FEndLine <= FStartLine then
          FStartLine := Max(0, FEndLine - 1);
      end else if (Button = mbRight) and (Mode <> mmUp) then
      begin
        FEndLine := ArrayPos;
        if FEndLine <= FCutView.FWaveData.ZoomStart then
          FEndLine := Min(High(FCutView.FWaveData.WaveArray), FCutView.FWaveData.ZoomStart + 1);
        if FStartLine >= FEndLine then
          FStartLine := Max(0, FEndLine - 1);
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
        if (FEffectStartLine <= -1) or (Mode = mmDown) then
        begin
          FEffectStartLine := ArrayPos;
          FEffectEndLine := ArrayPos;
        end else
          FEffectEndLine := ArrayPos;

      BuildBuffer;
    end;
  end;

  FStartLine := ClampIndex(FStartLine);
  FEndLine := ClampIndex(FEndLine);
  FPlayLine := ClampIndex(FPlayLine);
  if FEffectStartLine >= 0 then
    FEffectStartLine := ClampIndex(FEffectStartLine);
  if FEffectEndLine >= 0 then
    FEffectEndLine := ClampIndex(FEffectEndLine);

  BuildDrawBuffer;
  Invalidate;

  if Assigned(FCutView.FOnStateChanged) then
    Application.QueueAsyncCall(FCutView.StateChangedAsync, 0);
end;

procedure TCutPaintBox.HandleScrollBar(X: Integer; Y: Integer; Mode: TMouseMode);
var
  StartX, EndX, DiffX: Integer;
begin
  if FCutView = nil then
    Exit;
  if not FCutView.HasWaveData then
    Exit;
  if Length(FCutView.FWaveData.WaveArray) < 2 then
    Exit;
  if (FWaveBuf = nil) or (FWaveBuf.Width <= 6) then
    Exit;

  DiffX := Trunc((Int64(X - FMouseMoveStartX) * High(FCutView.FWaveData.WaveArray)) / (FWaveBuf.Width - 6));
  StartX := FCutView.FWaveData.ZoomStart + DiffX;
  EndX := FCutView.FWaveData.ZoomEnd + DiffX;
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
  ClampZoomRange(StartX, EndX);
  FZoomStartLine := StartX;
  FZoomEndLine := EndX;
  FMouseMoveStartX := X;
  FDoZoom := True;
  BuildBuffer;
  BuildDrawBuffer;
  Invalidate;
end;

procedure TCutPaintBox.TimerTimer(Sender: TObject);
var
  PlayingPos: Integer;
begin
  if csDestroying in ComponentState then
    Exit;
  if FCutView = nil then
    Exit;
  if FCutView.FShuttingDown then
    Exit;
  if not FCutView.HasWaveData then
    Exit;
  if Length(FCutView.FWaveData.WaveArray) < 2 then
    Exit;
  if FCutView.FPlayer = nil then
    Exit;

  if FCutView.FPlayer.Playing and (not FCutView.FPlayer.Paused) then
  begin
    PlayingPos := GetPlayerPos;
    if PlayingPos < 0 then
      Exit;

    FPlayingIndex := Cardinal(PlayingPos);
    FPlayLine := FPlayingIndex;

    BuildDrawBuffer;
    Invalidate;
  end;
end;

procedure TCutPaintBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCutPaintBox.WMMouseWheel(var Msg: TWMMouseWheel);
var
  MousePos: TPoint;
begin
  MousePos := Mouse.CursorPos;
  MousePos := ScreenToClient(MousePos);

  ZoomAtCursor(MousePos.X, MousePos.Y, Msg.WheelDelta);
end;

procedure TCutPaintBox.ZoomAtCursor(X, Y: Integer; Delta: Integer);
var
  StartPos, EndPos, Range, NewRange, NewStart, NewEnd, CursorPos, MaxIndex: Integer;
begin
  if FCutView = nil then
    Exit;
  if not FCutView.HasWaveData then
    Exit;
  if Length(FCutView.FWaveData.WaveArray) < 2 then
    Exit;
  if (FWaveBuf = nil) or (FWaveBuf.Width <= 6) then
    Exit;
  if GetControlMode(Y) <> cmView then
    Exit;

  StartPos := FCutView.FWaveData.ZoomStart;
  EndPos := FCutView.FWaveData.ZoomEnd;
  Range := Max(1, EndPos - StartPos);
  MaxIndex := High(FCutView.FWaveData.WaveArray);

  CursorPos := PixelsToArray(X);

  if Delta > 0 then
  begin
    // Zoom in - nothing to do if the current range is already minimal
    if Range <= MinimumDisplayedSampleCount then
      Exit;

    NewRange := (Range + 1) div 2;
    // Anchor the new window at the cursor. The multiplication keeps the
    // calculation exact, so a zoom out undoes a zoom in without rounding drift.
    NewStart := CursorPos - Trunc((Int64(CursorPos - StartPos) * NewRange) / Range);
  end
  else
  begin
    // Zoom out - nothing to do if the whole file is displayed already
    if (StartPos = 0) and (EndPos = High(FCutView.FWaveData.WaveArray)) then
      Exit;

    NewRange := Range * 2;
    if NewRange >= MaxIndex then
    begin
      NewStart := 0;
      NewEnd := MaxIndex;
    end
    else
    begin
      NewStart := CursorPos - Trunc((Int64(CursorPos - StartPos) * NewRange) / Range);
      NewEnd := NewStart + NewRange;
    end;
  end;

  if Delta > 0 then
    NewEnd := NewStart + NewRange;

  if NewStart < 0 then
    NewStart := 0;
  NewEnd := NewStart + NewRange;
  if NewEnd > MaxIndex then
  begin
    NewEnd := MaxIndex;
    NewStart := Max(0, NewEnd - NewRange);
  end;

  ClampZoomRange(NewStart, NewEnd);

  FZoomStartLine := NewStart;
  FZoomEndLine := NewEnd;
  FDoZoom := True;

  BuildBuffer;
  BuildDrawBuffer;
  Invalidate;

  if Assigned(FCutView.FOnStateChanged) then
    FCutView.FOnStateChanged(FCutView);
end;

{ TProcessThread }

constructor TProcessThread.Create(CommandLine, WorkingDir, FilePath, TempFile: string);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FCommandLine := CommandLine;
  FWorkingDir := WorkingDir;
  FFilePath := FilePath;
  FTempFile := TempFile;
end;

procedure TProcessThread.Execute;
var
  LoopStarted: Int64;
  FS: TFileStream;
  Failed: Boolean;
  EC: DWORD;
begin
  Failed := True;

  case TFunctions.RunProcess(FCommandLine, FWorkingDir, 300000, FProcessOutput, EC, @Self.Terminated, True, ReadCallbackSoX) of
    rpWin:
      if FileExists(TempFile) and (EC = 0) then
      begin
        LoopStarted := GetTickCount64;
        while Failed do
        begin
          if Terminated then
            Break;
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
            if GetTickCount64 > LoopStarted + 5000 then
              Break;
          end;
        end;
      end;
    rpFail, rpTerminated, rpTimeout: ;
  end;

  if Failed then
    SysUtils.DeleteFile(TempFile);

  if Failed then
    Synchronize(SyncError)
  else
    Synchronize(SyncSuccess);
end;

procedure TProcessThread.ReadCallbackSoX(Data: AnsiString);
var
  PercentPos, NumberStart, Index, ProgressValue: Integer;
begin
  FProgressBuffer := FProgressBuffer + Data;
  PercentPos := Pos('%', FProgressBuffer);
  while PercentPos > 0 do
  begin
    NumberStart := PercentPos - 1;
    while (NumberStart > 0) and (FProgressBuffer[NumberStart] in ['0'..'9', '.', ',']) do
      Dec(NumberStart);

    ProgressValue := 0;
    Index := NumberStart + 1;
    while (Index < PercentPos) and (FProgressBuffer[Index] in ['0'..'9']) do
    begin
      ProgressValue := Min(100, ProgressValue * 10 + Ord(FProgressBuffer[Index]) - Ord('0'));
      Inc(Index);
    end;

    if Index > NumberStart + 1 then
    begin
      if ProgressValue <> FProgress then
      begin
        FProgress := ProgressValue;
        Synchronize(SyncProgress);
      end;
    end;

    Delete(FProgressBuffer, 1, PercentPos);
    PercentPos := Pos('%', FProgressBuffer);
  end;

  if Length(FProgressBuffer) > 256 then
    Delete(FProgressBuffer, 1, Length(FProgressBuffer) - 256);
end;

procedure TProcessThread.SyncProgress;
begin
  if Terminated then
    Exit;
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

procedure TProcessThread.SyncError;
begin
  if Terminated then
    Exit;
  if Assigned(FOnError) then
    FOnError(Self);
end;

procedure TProcessThread.SyncSuccess;
begin
  if Terminated then
    Exit;
  if Assigned(FOnSuccess) then
    FOnSuccess(Self);
end;

{ TUndoStep }

constructor TUndoStep.Create(Filename: string; StartLine, EndLine, EffectStartLine, EffectEndLine, PlayLine: Integer);
begin
  FFilename := Filename;
  FStartLine := StartLine;
  FEndLine := EndLine;
  FEffectStartLine := EffectStartLine;
  FEffectEndLine := EffectEndLine;
  FPlayLine := PlayLine;
end;

{ TSaveCutThread }

constructor TSaveCutThread.Create(InFilename, OutFilename: string; S, E: Int64);
begin
  inherited Create(True);

  FreeOnTerminate := False;

  FInFilename := InFilename;
  FOutFilename := OutFilename;
  FS := S;
  FE := E;
  FLastProgress := -1;
end;

procedure TSaveCutThread.Execute;
var
  C: TFileConvertor;
begin
  C := TFileConvertor.Create;
  C.OnProgress := FileConvertorProgress;
  try
    if C.Convert2WAV(FInFilename, FOutFilename, @Terminated, FS, FE) then
      Synchronize(SyncSaveCutEnd)
    else
      Synchronize(SyncSaveCutError);
  finally
    C.Free;
  end;
end;

procedure TSaveCutThread.FileConvertorProgress(Sender: TObject; Percent: Integer);
begin
  if FLastProgress <> Percent then
  begin
    FLastProgress := Percent;
    FProgress := Percent;
    Synchronize(SyncSaveCutProgress);
  end;
end;

procedure TSaveCutThread.SyncSaveCutEnd;
begin
  if Terminated then
    Exit;
  if Assigned(FOnEndSave) then
    FOnEndSave(Self);
end;

procedure TSaveCutThread.SyncSaveCutError;
begin
  if Terminated then
    Exit;
  if Assigned(FOnSaveError) then
    FOnSaveError(Self);
end;

procedure TSaveCutThread.SyncSaveCutProgress;
begin
  if Terminated then
    Exit;
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self);
end;

end.
