{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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
  SysUtils, Windows, Classes, Controls, StdCtrls, ExtCtrls, Functions,
  Graphics, DynBASS, Forms, Math, Generics.Collections, GUIFunctions,
  LanguageObjects, WaveData, Messages, ComCtrls, AppData, Player,
  PlayerManager, PostProcess, PostProcessSoX, DownloadAddons, ConfigureSoX,
  MsgDlg, DragDrop, DropTarget, DropComboTarget, AudioFunctions,
  MessageBus, AppMessages, AddonSoX, PostProcessConvert, FileTagger,
  DataManager, PerlRegEx, Logging, FileConvertor;

type
  TPeakEvent = procedure(P, AI, L, R: Integer) of object;
  TWavBufArray = array of SmallInt;
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

    FOnSuccess: TNotifyEvent;
    FOnError: TNotifyEvent;

    procedure SyncSuccess;
    procedure SyncError;

    procedure ReadCallbackSoX(Data: AnsiString);
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

  TSaveCutThread = class(TThread)
  private
    FInFilename, FOutFilename: string;
    FS, FE: Cardinal;
    FProgress: Integer;

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
    constructor Create(InFilename, OutFilename: string; S, E: Cardinal);
    destructor Destroy; override;

    property OutFilename: string read FOutFilename;
    property Progress: Integer read FProgress;

    property OnSaveProgress: TNotifyEvent read FOnSaveProgress write FOnSaveProgress;
    property OnEndSave: TNotifyEvent read FOnEndSave write FOnEndSave;
    property OnSaveError: TNotifyEvent read FOnSaveError write FOnSaveError;
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
    FStartLine: Cardinal;
    FEndLine: Cardinal;
    FEffectStartLine: Cardinal;
    FEffectEndLine: Cardinal;
    FPlayLine: Cardinal;
  public
    constructor Create(Filename: string; StartLine, EndLine, EffectStartLine, EffectEndLine, PlayLine: Cardinal);

    property Filename: string read FFilename;
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
    FSaveCutThread: TSaveCutThread;
    FPB: TCutPaintBox;
    FWaveData: TWaveData;
    FState: TCutStates;
    FLineMode: TLineMode;
    FProgressBarLoad: TProgressBar;
    FUndoList: TUndoList;
    FID: Integer;
    FUndoStep: TUndoStep;
    FLastCheckSum: Cardinal;
    FSaving: Boolean;

    FPlayer: TPlayer;
    FOriginalFilename: string;
    FWorkingFilename: string;
    FFilesize: UInt64;
    FSaveSecs: UInt64;

    FBitRateType: TBitRates;
    FQuality: TVBRQualities;
    FBitRate: Cardinal;

    FDropTarget: TDropComboTarget;

    FOnStateChanged: TNotifyEvent;
    FOnCutFile: TCutFileEvent;

    FFileTagger: TFileTagger;
    FFileConvertorThread: TPostProcessConvertThread;

    procedure StartProcessing(CmdLine: string);
    procedure AddUndo;

    procedure ThreadScanProgress(Sender: TObject);
    procedure ThreadEndScan(Sender: TObject);
    procedure ThreadScanError(Sender: TObject);
    procedure ThreadTerminate(Sender: TObject);

    procedure ProcessThreadSuccess(Sender: TObject);
    procedure ProcessThreadError(Sender: TObject);
    procedure ProcessThreadTerminate(Sender: TObject);

    procedure SaveCutThreadProgress(Sender: TObject);
    procedure SaveCutThreadEndSave(Sender: TObject);
    procedure SaveCutThreadError(Sender: TObject);
    procedure SaveCutThreadTerminate(Sender: TObject);

    procedure PlayerEndReached(Sender: TObject);
    procedure PlayerPlay(Sender: TObject);
    procedure PlayerPause(Sender: TObject);
    procedure PlayerStop(Sender: TObject);

    procedure MsgRefresh(var Msg: TMessage); message WM_USER + 1234;

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer);

    procedure MessageReceived(Msg: TMessageBase);

    procedure CreateConvertor;

    procedure FileConvertorProgress(Sender: TObject);
    procedure FileConvertorFinish(Sender: TObject);
    procedure FileConvertorError(Sender: TObject);
    procedure FileConvertorTerminate(Sender: TObject);

    function GetUndoFilename: string;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFile(Filename: string; IsConverted, LoadTrackData: Boolean); overload;
    procedure LoadFile(Track: TTrackInfo); overload;

    function CheckSoX: Boolean;

    procedure Save;
    procedure Cut;
    procedure Undo;
    procedure Play;
    procedure Stop;
    procedure AutoCut(MaxPeaks: Integer; MinDuration: Cardinal);
    function ApplyFade(Fadein: Boolean): Boolean;
    procedure ApplyFadein;
    procedure ApplyFadeout;
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
  FFileTagger := TFileTagger.Create;

  MsgBus.AddSubscriber(MessageReceived);
end;

procedure TCutView.CreateConvertor;
begin
  FFileConvertorThread := TPostProcessConvertThread.Create(nil, AppGlobals.Data.StreamSettings.PostProcessors[0]);
  FFileConvertorThread.OnProgress := FileConvertorProgress;
  FFileConvertorThread.OnFinish := FileConvertorFinish;
  FFileConvertorThread.OnError := FileConvertorError;
  FFileConvertorThread.OnTerminate := FileConvertorTerminate;
end;

destructor TCutView.Destroy;
var
  i: Integer;
begin
  FFileTagger.Free;

  if FFileConvertorThread <> nil then
  begin
    FFileConvertorThread.OnProgress := nil;
    FFileConvertorThread.OnFinish := nil;
    FFileConvertorThread.OnError := nil;
    FFileConvertorThread.Terminate;
  end;

  MsgBus.RemoveSubscriber(MessageReceived);

  if FScanThread <> nil then
  begin
    FScanThread.OnScanProgress := nil;
    FScanThread.OnEndScan := nil;
    FScanThread.OnScanError := nil;
    FScanThread.Terminate;
  end;

  if FProcessThread <> nil then
  begin
    FProcessThread.OnSuccess := nil;
    FProcessThread.OnError := nil;
    FProcessThread.Terminate;
  end;

  if FSaveCutThread <> nil then
  begin
    FSaveCutThread.OnSaveProgress := nil;
    FSaveCutThread.OnEndSave := nil;
    FSaveCutThread.OnSaveError := nil;
    FSaveCutThread.Terminate;
  end;

  while (FFileConvertorThread <> nil) or (FScanThread <> nil) or (FProcessThread <> nil) do
  begin
    Sleep(100);
    Application.ProcessMessages;
  end;

  if FWaveData <> nil then
    FreeAndNil(FWaveData);

  if FPlayer <> nil then
  begin
    FreeAndNil(FPlayer);
  end;

  for i := 0 to FUndoList.Count - 1 do
  begin
    DeleteFile(PChar(FUndoList[i].Filename));
    FUndoList[i].Free;
  end;
  FUndoList.Free;
  DeleteFile(PChar(FWorkingFilename));

  inherited;
end;

procedure TCutView.DropTargetDrop(Sender: TObject; ShiftState: TShiftState;
  APoint: TPoint; var Effect: Integer);
var
  i: Integer;
begin
  for i := 0 to FDropTarget.Files.Count - 1 do
  begin
    if FiletypeToFormat(LowerCase(ExtractFileExt(FDropTarget.Files[i]))) <> atNone then
    begin
      if Assigned(FOnCutFile) then
        FOnCutFile(TCutTab(Self.Parent), FDropTarget.Files[i]);
    end;
  end;
end;

procedure TCutView.FileConvertorError(Sender: TObject);
begin
  FState := csConvertorError;

  FProgressBarLoad.Visible := False;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;
end;

procedure TCutView.FileConvertorFinish(Sender: TObject);
var
  i: Integer;
begin
  if FState = csEncoding then
  begin
    FFileTagger.Write(Language.CurrentLanguage.LCID, FOriginalFilename);

    for i := 0 to FUndoList.Count - 1 do
    begin
      DeleteFile(PChar(FUndoList[i].Filename));
      FUndoList[i].Free;
    end;
    FUndoList.Clear;

    if Assigned(TCutTab(Owner).OnSaved) then
      if FFileConvertorThread.FileInfo.Success then
        TCutTab(Owner).OnSaved(Owner, FFileConvertorThread.FileInfo);
  end;

  LoadFile(FWorkingFilename, True, False);
end;

procedure TCutView.FileConvertorProgress(Sender: TObject);
begin
  if FFileConvertorThread.Progress < 100 then
    FProgressBarLoad.Position := FFileConvertorThread.Progress + 1;
  FProgressBarLoad.Position := FFileConvertorThread.Progress;
end;

procedure TCutView.FileConvertorTerminate(Sender: TObject);
begin
  FFileConvertorThread := nil;
end;

function TCutView.GetUndoFilename: string;
begin
  repeat
    Result := AppGlobals.TempDir + 'UNDO_' + IntToStr(GetTickCount) + '_' + RemoveFileExt(ExtractFileName(FOriginalFilename)) + '_cut.wav';
  until not FileExists(Result);
end;

procedure TCutView.LoadFile(Track: TTrackInfo);
begin
  if Track.VBR then
  begin
    FBitRateType := brVBR;
    FQuality := GuessVBRQuality(Track.BitRate, FiletypeToFormat(Track.Filename));
  end else
  begin
    FBitRateType := brCBR;
    FBitRate := Track.BitRate;
  end;

  LoadFile(Track.Filename, False, False);
end;

procedure TCutView.LoadFile(Filename: string; IsConverted, LoadTrackData: Boolean);
var
  Info: TAudioFileInfo;
begin
  if (FScanThread <> nil) or (FProcessThread <> nil) then
    Exit;

  if IsConverted then
  begin
    if FPlayer <> nil then
    begin
      FreeAndNil(FPlayer);
    end;
    FPlayer := TPlayer.Create;
    FPlayer.ShowTitle := False;
    FPlayer.OnEndReached := PlayerEndReached;
    FPlayer.OnPlay := PlayerPlay;
    FPlayer.OnPause := PlayerPause;
    FPlayer.OnStop := PlayerStop;
    Players.AddPlayer(FPlayer);

    try
      if Bass.DeviceAvailable then
        FPlayer.Filename := Filename;
    except
      ThreadScanError(Self);
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

    FPB.FEffectStartLine := 0;
    FPB.FEffectEndLine := 0;

    FScanThread.Resume;

    if Assigned(FOnStateChanged) then
      FOnStateChanged(Self);
  end else
  begin
    FOriginalFilename := Filename;
    FWorkingFilename := GetUndoFilename;

    FState := csLoading;

    FState := csDecoding;

    CreateConvertor;

    FProgressBarLoad.Position := 0;
    FProgressBarLoad.Visible := True;

    if LoadTrackData then
    begin
      Info := GetFileInfo(Filename);

      if Info.VBR then
      begin
        FBitRateType := brVBR;
        FQuality := GuessVBRQuality(Info.BitRate, FiletypeToFormat(Filename));
      end else
      begin
        FBitRateType := brCBR;
        FBitRate := Info.BitRate;
      end;
    end;

    FFileConvertorThread.Convert(FOriginalFilename, FWorkingFilename, nil);
    FFileConvertorThread.Resume;

    FPB.BuildBuffer;
    FPB.BuildDrawBuffer;
    FPB.Repaint;
  end;
end;

procedure TCutView.MessageReceived(Msg: TMessageBase);
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
var
  S, E: Cardinal;
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
  FPB.Paint;

  FSaveCutThread := TSaveCutThread.Create(FWorkingFilename, GetUndoFilename, S, E);
  FSaveCutThread.OnSaveProgress := SaveCutThreadProgress;
  FSaveCutThread.OnEndSave := SaveCutThreadEndSave;
  FSaveCutThread.OnSaveError := SaveCutThreadError;
  FSaveCutThread.OnTerminate := SaveCutThreadTerminate;

  FSaveCutThread.Resume;

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

  if MoveFileEx(PChar(UndoStep.Filename), PChar(FWorkingFilename), MOVEFILE_REPLACE_EXISTING) then
  begin
    FWaveData.Free;
    FWaveData := nil;

    LoadFile(FWorkingFilename, True, False);

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

procedure TCutView.Save;
var
  EncoderSettings: TEncoderSettings;
begin
  FSaving := True;

  if FPlayer <> nil then
    FreeAndNil(FPlayer);

  FSaveSecs := Trunc(FWaveData.Secs);
  FreeAndNil(FWaveData);

  // Wiedergabe ggf. anhalten
  MsgBus.SendMessage(TFileModifyMsg.Create(FOriginalFilename));

  FFileTagger.Read(FOriginalFilename);

  CreateConvertor;

  EncoderSettings := TEncoderSettings.Create(FiletypeToFormat(FOriginalFilename), FBitRateType, FQuality);
  EncoderSettings.CBRBitrate := FBitRate;
  try
    FFileConvertorThread.Convert(FWorkingFilename, FOriginalFilename, EncoderSettings);
  finally
    EncoderSettings.Free;
  end;
  FFileConvertorThread.Resume;

  FState := csEncoding;
  FProgressBarLoad.Position := 0;
  FProgressBarLoad.Visible := True;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Paint;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.SaveCutThreadEndSave(Sender: TObject);
begin
  LoadFile(FSaveCutThread.FOutFilename, True, False);

  FWorkingFilename := FSaveCutThread.FOutFilename;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.SaveCutThreadError(Sender: TObject);
var
  DriveLetter: string;
begin
  FSaving := False;

  FProgressBarLoad.Visible := False;
  FState := csCutError;

  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Repaint;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);

  DriveLetter := '';
  if Length(AppGlobals.TempDir) > 3 then
  begin
    DriveLetter := Copy(AppGlobals.TempDir, 0, 2);
    MsgBox(GetParentForm(Self).Handle, Format(_('The temporary cut-file could not be saved. Make sure there is enough free diskspace on drive %s.'), [DriveLetter]), _('Error'), MB_ICONERROR)
  end;
end;

procedure TCutView.SaveCutThreadProgress(Sender: TObject);
begin
  if FSaveCutThread.Progress < 100 then
    FProgressBarLoad.Position := FSaveCutThread.Progress + 1;
  FProgressBarLoad.Position := FSaveCutThread.Progress;
end;

procedure TCutView.SaveCutThreadTerminate(Sender: TObject);
begin
  FProgressBarLoad.Visible := False;

  FSaveCutThread := nil;
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

  FPlayer.Volume := Players.Volume;
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
  Msg := FProcessThread.ProcessOutput;
  FProcessThread := nil;
  MsgBox(GetParentForm(Self).Handle, _('An error occured while processing the file.'), _('Error'), MB_ICONERROR);

  LoadFile(FWorkingFilename, True, False);
end;

procedure TCutView.ProcessThreadSuccess(Sender: TObject);
begin
  FWorkingFilename := TProcessThread(Sender).FTempFile;

  FProcessThread := nil;
  FState := csReady;

  LoadFile(FWorkingFilename, True, False);

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCutView.ProcessThreadTerminate(Sender: TObject);
begin
  FProcessThread := nil;
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
  AddUndo;

  if FPlayer <> nil then
  begin
    FreeAndNil(FPlayer);
  end;

  if FWaveData <> nil then
    FreeAndNil(FWaveData);

  TempFile := GetUndoFilename;

  CmdLine := StringReplace(CmdLine, '[[TEMPFILE]]', TempFile, [rfReplaceAll]);

  FState := csWorking;
  FPB.BuildBuffer;
  FPB.BuildDrawBuffer;
  FPB.Paint;

  FProcessThread := TProcessThread.Create(CmdLine,
    IncludeTrailingBackslash(ExtractFilePath((AppGlobals.AddonManager.Find(TAddonSoX) as TAddonSoX).EXEPath)),
    FWorkingFilename, TempFile);
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

  CmdLine := '"' + (AppGlobals.AddonManager.Find(TAddonSoX) as TAddonSoX).EXEPath + '" --show-progress "' + FWorkingFilename + '" ' + '"[[TEMPFILE]]" ';

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

procedure TCutView.AddUndo;
begin
  FUndoList.Add(TUndoStep.Create(FWorkingFilename, FPB.FStartLine, FPB.FEndLine,
    FPB.FEffectStartLine, FPB.FEffectEndLine, FPB.FPlayLine));
end;

procedure TCutView.ApplyEffects;
var
  CmdLine: string;
  F: TfrmConfigureSoX;
begin
  if not CheckSoX then
    Exit;

  F := TfrmConfigureSox.Create(Self, (AppGlobals.Data.StreamSettings.PostProcessors.Find(TPostProcessSoX) as TPostProcessSoX), False, False, False, 5, 5, False, False, 3, 3,
    Trunc(FWaveData.WaveArray[High(FWaveData.WaveArray)].Sec));
  try
    F.ShowModal;

    if F.SaveData then
    begin
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
          CmdLine := '"' + (AppGlobals.AddonManager.Find(TAddonSoX) as TAddonSoX).EXEPath + '" --show-progress --norm "' + FWorkingFilename + '" ' + '"[[TEMPFILE]]" ' + CmdLine
        else
          CmdLine := '"' + (AppGlobals.AddonManager.Find(TAddonSoX) as TAddonSoX).EXEPath + '" --show-progress "' + FWorkingFilename + '" ' + '"[[TEMPFILE]]" ' + CmdLine;

        StartProcessing(CmdLine);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCutView.AutoCut(MaxPeaks: Integer; MinDuration: Cardinal);
begin
  if FWaveData = nil then
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
  Result := (FWaveData <> nil) and (FPB.FEndLine - FPB.FStartLine > 0) and
    (FWaveData.TimeBetween(FPB.FStartLine, FPB.FEndLine) >= 0.5) and
    ((FPB.FStartLine > 0) or (FPB.FEndLine < High(FWaveData.WaveArray)));
end;

function TCutView.CanEffectsMarker: Boolean;
begin
  Result := (FWaveData <> nil);
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
  if FWaveData = nil then
    Exit(False);

  Tolerance := Trunc((FWaveData.ZoomSize div FPB.ClientWidth) * 3.5) + 4;
  Result := (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5) and
            ((FPB.FEffectStartLine <= Tolerance) or (FPB.FEffectEndLine <= Tolerance));
end;

function TCutView.CanApplyFadeOut: Boolean;
var
  Tolerance: Cardinal;
begin
  if FWaveData = nil then
    Exit(False);

  Tolerance := Trunc((FWaveData.ZoomSize div FPB.ClientWidth) * 3.5) + 4;
  Result := (FWaveData.TimeBetween(FPB.FEffectStartLine, FPB.FEffectEndLine) >= 0.5) and
            ((FPB.FEffectStartLine >= Length(FWaveData.WaveArray) - Tolerance) or (FPB.FEffectEndLine >= Length(FWaveData.WaveArray) - Tolerance));
end;

function TCutView.CanApplyEffects: Boolean;
begin
  Result := (FWaveData <> nil);
end;

function TCutView.CanAutoCut: Boolean;
begin
  Result := FWaveData <> nil;
end;

function TCutView.CanSave: Boolean;
begin
  Result := (FWaveData <> nil) and (FLastCheckSum <> FWaveData.CheckSum);
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
  FProgressBarLoad.Visible := False;

  if FWaveData <> nil then
    FWaveData.Free;
  FWaveData := FScanThread.FWaveData;

  if (FSaving) or (FUndoList.Count = 0) then
    FLastCheckSum := FWaveData.CheckSum;
  FSaving := False;

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

  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);

  FPB.Resize;
  FPB.Paint;
  FPB.Repaint;
end;

procedure TCutView.ThreadScanError(Sender: TObject);
begin
  FSaving := False;

  FProgressBarLoad.Visible := False;
  FState := csLoadError;

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
  FScanThread := nil;

  if not Application.Terminated then
  begin
    if FProgressBarLoad.Visible then
      FProgressBarLoad.Visible := False;

    if FUndoStep <> nil then
      FreeAndNil(FUndoStep);
  end;
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
  ArrayFrom, ArrayTo: Cardinal;
  L1, L2: Cardinal;
  CS, CE: Cardinal;

  TextWrite: string;
begin
  TextWrite := '';

  FWaveBuf.Canvas.Brush.Color := clBlack;
  FWaveBuf.Canvas.FillRect(Rect(0, 0, FWaveBuf.Width, FWaveBuf.Height));

  if (ClientHeight < 2) or (ClientWidth < 2) then
    Exit;

  TextWrite := '';
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
    TS := GetTextSize(TextWrite, Canvas.Font);
    FWaveBuf.Canvas.Font.Color := clWhite;
    SetBkMode(FWaveBuf.Canvas.Handle, TRANSPARENT);
    FWaveBuf.Canvas.TextOut(FWaveBuf.Width div 2 - TS.cx div 2, FWaveBuf.Height div 2 - TS.cy, TextWrite);
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
  procedure DrawLineText(ArrayIdx, X: Cardinal);
  var
    L: Integer;
    TS: TSize;
    SecText: string;
  begin
    L := Trunc(((ArrayIdx - FCutView.FWaveData.ZoomStart) / FCutView.FWaveData.ZoomSize) * FDrawBuf.Width);
    SecText := BuildTime(FCutView.FWaveData.WaveArray[ArrayIdx].Sec, True);
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

  if (FCutView.FWaveData <> nil) and (not (FCutView.FState in [csWorking, csDecoding, csEncoding, csConvertorError])) then
  begin
    DrawLine(FStartLine, FStartColor);
    DrawLine(FEndLine, FEndColor);

    DrawLineText(FStartLine, 16);
    DrawLineText(FEndLine, 28);

    DrawLine(FPlayLine, FPlayColor);
    DrawLineText(FPlayLine, 40);

    FDrawBuf.Canvas.Font.Color := clWhite;

    FDrawBuf.Canvas.TextOut(4, 4, BuildTime(FCutView.FWaveData.Secs, True) + ' - ' + RemoveFileExt(ExtractFileName(FCutView.FOriginalFilename)));
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
  LoopStarted: Cardinal;
  FS: TFileStream;
  Failed: Boolean;
  EC: DWORD;
begin
  inherited;

  Failed := True;

  case RunProcess(FCommandLine, FWorkingDir, 300000, FProcessOutput, EC, @Self.Terminated, True, ReadCallbackSoX) of
    rpWin:
      if FileExists(TempFile) and (EC = 0) then
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
      end;
    rpFail, rpTerminated, rpTimeout:;
  end;

  if Failed then
    DeleteFile(PChar(TempFile));

  if Failed then
    SyncError
  else
    SyncSuccess;
end;

procedure TProcessThread.ReadCallbackSoX(Data: AnsiString);
begin

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

constructor TUndoStep.Create(Filename: string; StartLine, EndLine, EffectStartLine, EffectEndLine, PlayLine: Cardinal);
begin
  FFilename := Filename;
  FStartLine := StartLine;
  FEndLine := EndLine;
  FEffectStartLine := EffectStartLine;
  FEffectEndLine := EffectEndLine;
  FPlayLine := PlayLine;
end;

{ TSaveCutThread }

constructor TSaveCutThread.Create(InFilename, OutFilename: string; S, E: Cardinal);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FInFilename := InFilename;
  FOutFilename := OutFilename;
  FS := S;
  FE := E;
end;

destructor TSaveCutThread.Destroy;
begin

  inherited;
end;

procedure TSaveCutThread.Execute;
var
  C: TFileConvertor;
begin
  inherited;

  C := TFileConvertor.Create;
  C.OnProgress := FileConvertorProgress;
  try
    if C.Convert2WAV(FInFilename, FOutFilename, @Terminated, FS, FE) then
    begin
      Synchronize(SyncSaveCutEnd);
    end else
    begin
      Synchronize(SyncSaveCutError);
    end;
  finally
    C.Free;
  end;
end;

procedure TSaveCutThread.FileConvertorProgress(Sender: TObject;
  Percent: Integer);
begin
  FProgress := Percent;
  Synchronize(SyncSaveCutProgress);
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



