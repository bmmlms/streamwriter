{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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

{ This unit contains the TabControl to display cutting of recorded files }
unit CutTab;

interface

uses
  AppData,
  AppMessages,
  AudioFunctions,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  CutTabSearchSilence,
  CutToolBar,
  CutView,
  DataManager,
  DynBass,
  ExtCtrls,
  Forms,
  Functions,
  Images,
  LanguageObjects,
  Logging,
  MControls,
  MessageBus,
  MVolumePanel,
  PlayerManager,
  SharedControls,
  SharedData,
  SysUtils,
  Tabs,
  Windows;

type
  TFileSavedEvent = procedure(Sender: TObject; AudioInfo: TAudioInfo) of object;

  { TCutTab }

  TCutTab = class(TMainTabSheet)
  private
    FToolbarPanel: TPanel;
    FToolBar: TCutToolBar;
    FVolume: TMVolumePanel;
    FCutView: TCutView;
    FFilename: string;
    FTrack: TTrackInfo;

    FOnSaved: TFileSavedEvent;
    FOnPlayStarted: TNotifyEvent;

    procedure UpdateButtons;

    procedure SaveClick(Sender: TObject);
    procedure PosClick(Sender: TObject);
    procedure AutoCutClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure ApplyFadeinClick(Sender: TObject);
    procedure ApplyFadeoutClick(Sender: TObject);
    procedure ApplyEffectsClick(Sender: TObject);
    procedure ZoomInClick(Sender: TObject);
    procedure ZoomOutClick(Sender: TObject);

    procedure CutViewStateChanged(Sender: TObject);
    procedure VolumeTrackbarChange(Sender: TObject);
    function VolumeGetVolumeBeforeMute(Sender: TObject): Integer;

    procedure MessageReceived(Msg: TMessageBase);
  protected
    procedure ControlsAligned; override;
  public
    constructor Create(AOwner: TComponent; Track: TTrackInfo; Filename: string = ''); reintroduce;
    destructor Destroy; override;

    procedure PausePlay;

    function CanClose: Boolean; override;

    function ProcessShortCut(Msg: TWMKey): Boolean; override;

    property Filename: string read FFilename;

    property CutView: TCutView read FCutView;
    property OnSaved: TFileSavedEvent read FOnSaved write FOnSaved;
    property OnPlayStarted: TNotifyEvent read FOnPlayStarted write FOnPlayStarted;
  end;

implementation

{ TCutTab }

function TCutTab.CanClose: Boolean;
begin
  Result := inherited;

  if FCutView.WaveData <> nil then
    if FCutView.LastCheckSum <> FCutView.WaveData.CheckSum then
      if TFunctions.MsgBox(Format(_('The file "%s" has not been saved. Do you really want to close the editor?'), [ExtractFileName(FFilename)]), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON2) = IDYES then
        Result := True
      else
        Result := False;
end;

constructor TCutTab.Create(AOwner: TComponent; Track: TTrackInfo; Filename: string = '');
begin
  inherited Create(AOwner);

  MsgBus.AddSubscriber(MessageReceived);

  FTrack := Track;
  if Track <> nil then
    FFilename := Track.Filename
  else
    FFilename := Filename;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Parent := Self;
  FToolbarPanel.Align := alTop;
  FToolbarPanel.BevelOuter := bvNone;

  FToolBar := TCutToolBar.Create(Self);
  FToolBar.Parent := FToolbarPanel;
  FToolBar.Align := alClient;
  FToolBar.Images := modSharedData.imgImages;
  FToolbar.Save.OnClick := SaveClick;
  FToolBar.PosEdit.OnClick := PosClick;
  FToolBar.PosPlay.OnClick := PosClick;
  FToolBar.ZoomIn.OnClick := ZoomInClick;
  FToolBar.ZoomOut.OnClick := ZoomOutClick;
  FToolBar.PosEffectsMarker.OnClick := PosClick;
  FToolBar.AutoCut.OnClick := AutoCutClick;
  FToolBar.Cut.OnClick := CutClick;
  FToolBar.Undo.OnClick := UndoClick;
  FToolBar.ApplyFadein.OnClick := ApplyFadeinClick;
  FToolBar.ApplyFadeout.OnClick := ApplyFadeoutClick;
  FToolBar.ApplyEffects.OnClick := ApplyEffectsClick;
  FToolBar.Play.OnClick := PlayClick;
  FToolBar.Stop.OnClick := StopClick;

  FVolume := TMVolumePanel.Create(Self);
  FVolume.Parent := FToolbarPanel;
  FVolume.Align := alRight;
  FVolume.Images := modSharedData.imgImages;
  FVolume.ImageIndexMute := TImages.SOUND_MUTE;
  FVolume.ImageIndexSound := TImages.SOUND;
  FVolume.ImageIndexSoundLow := TImages.SOUND_LOW;
  FVolume.Enabled := Bass.DeviceAvailable;
  FVolume.Volume := Players.Volume;
  FVolume.OnVolumeChange := VolumeTrackbarChange;
  FVolume.OnGetVolumeBeforeMute := VolumeGetVolumeBeforeMute;

  FCutView := TCutView.Create(Self);
  FCutView.Parent := Self;
  FCutView.Align := alClient;
  FCutView.OnStateChanged := CutViewStateChanged;

  ImageIndex := TImages.CUT;
  ShowCloseButton := True;

  if FTrack <> nil then
  begin
    Caption := ExtractFileName(StringReplace(FTrack.Filename, '&', '&&', [rfReplaceAll]));
    FCutView.LoadFile(FTrack);
  end else
  begin
    Caption := ExtractFileName(StringReplace(Filename, '&', '&&', [rfReplaceAll]));
    FCutView.LoadFile(Filename, False, True);
  end;

  UpdateButtons;
  Language.Translate(Self);
end;

procedure TCutTab.UpdateButtons;
begin
  FToolBar.Save.Enabled := FCutView.CanSave;
  FToolBar.PosEdit.Enabled := FCutView.CanSetLine;
  FToolBar.PosPlay.Enabled := FCutView.CanSetLine;
  FToolBar.AutoCut.Enabled := FCutView.CanAutoCut;
  FToolBar.PosPlay.Enabled := FCutView.CanSetLine;
  FToolBar.Cut.Enabled := FCutView.CanCut;
  FToolBar.ZoomIn.Enabled := FCutView.CanZoomIn;
  FToolBar.ZoomOut.Enabled := FCutView.CanZoomOut;
  FToolBar.PosEffectsMarker.Enabled := FCutView.CanEffectsMarker;
  FToolBar.Undo.Enabled := FCutView.CanUndo;
  FToolBar.ApplyFadein.Enabled := FCutView.CanApplyFadeIn;
  FToolBar.ApplyFadeout.Enabled := FCutView.CanApplyFadeOut;
  FToolBar.ApplyEffects.Enabled := FCutView.CanApplyEffects;
  FToolBar.Play.Enabled := FCutView.CanPlay and Bass.DeviceAvailable;
  FToolBar.Stop.Enabled := FCutView.CanStop and Bass.DeviceAvailable;

  FToolBar.PosEdit.Down := False;
  FToolBar.PosPlay.Down := False;
  FToolBar.PosEffectsMarker.Down := False;

  case FCutView.LineMode of
    lmEdit:
      FToolBar.PosEdit.Down := True;
    lmPlay:
      FToolBar.PosPlay.Down := True;
    lmEffectsMarker:
      FToolBar.PosEffectsMarker.Down := True;
  end;
end;

function TCutTab.VolumeGetVolumeBeforeMute(Sender: TObject): Integer;
begin
  Result := Players.VolumeBeforeMute;
end;

procedure TCutTab.VolumeTrackbarChange(Sender: TObject);
begin
  Players.Volume := FVolume.Volume;
  if FVolume.VolumeBeforeDrag > -1 then
    Players.VolumeBeforeMute := FVolume.VolumeBeforeDrag;
end;

procedure TCutTab.ZoomInClick(Sender: TObject);
begin
  FCutView.ZoomIn;
end;

procedure TCutTab.ZoomOutClick(Sender: TObject);
begin
  FCutView.ZoomOut;
end;

procedure TCutTab.PosClick(Sender: TObject);
begin
  if TToolButton(Sender).Down then
    Exit;

  FToolBar.PosEdit.Down := False;
  FToolBar.PosPlay.Down := False;
  FToolBar.PosEffectsMarker.Down := False;

  if Sender = FToolBar.PosEdit then
  begin
    FCutView.LineMode := lmEdit;
    FToolBar.PosEdit.Down := True;
  end else if Sender = FToolBar.PosPlay then
  begin
    FCutView.LineMode := lmPlay;
    FToolBar.PosPlay.Down := True;
  end else if Sender = FToolBar.PosEffectsMarker then
  begin
    FCutView.LineMode := lmEffectsMarker;
    FToolBar.PosEffectsMarker.Down := True;
  end;
end;

function TCutTab.ProcessShortCut(Msg: TWMKey): Boolean;
var
  Button: TToolButton;
begin
  Result := False;
  Button := nil;

  if (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_MENU) = 0) then
  begin
    if Msg.CharCode = Ord('S') then
      Button := FToolBar.Save;
    if Msg.CharCode = Ord('Z') then
      Button := FToolBar.Undo;
  end else
  begin
    if (Msg.CharCode = VK_SPACE) and (FCutView.Player <> nil) then
      if FCutView.Player.Playing then
        Button := FToolBar.Stop
      else
        Button := FToolBar.Play;

    if Msg.CharCode = VK_HOME then
    begin
      FCutView.SetPosition(True);
      Result := True;
    end;

    if Msg.CharCode = VK_END then
    begin
      FCutView.SetPosition(False);
      Result := True;
    end;

    if Msg.CharCode = Ord('S') then
      Button := FToolBar.PosEffectsMarker;

    if (Msg.CharCode = VK_ADD) or (Msg.CharCode = VK_OEM_PLUS) then
      Button := FToolBar.ZoomIn;

    if (Msg.CharCode = VK_SUBTRACT) or (Msg.CharCode = VK_OEM_MINUS) then
      Button := FToolBar.ZoomOut;

    if Msg.CharCode = Ord('P') then
      Button := FToolBar.PosPlay;

    if Msg.CharCode = Ord('C') then
      Button := FToolBar.PosEdit;

    if Msg.CharCode = Ord('F') then
      FCutView.ApplyFade;
  end;

  if Button <> nil then
  begin
    if Button.Enabled then
      Button.Click;
    Result := True;
  end;
end;

procedure TCutTab.ApplyFadeinClick(Sender: TObject);
begin
  FCutView.ApplyFade;
end;

procedure TCutTab.ApplyFadeoutClick(Sender: TObject);
begin
  FCutView.ApplyFade;
end;

procedure TCutTab.ApplyEffectsClick(Sender: TObject);
begin
  FCutView.ApplyEffects;
end;

procedure TCutTab.AutoCutClick(Sender: TObject);
var
  F: TfrmCutTabSearchSilence;
begin
  F := TfrmCutTabSearchSilence.Create(Self);
  try
    F.ShowModal;

    if F.Okay then
      FCutView.AutoCut(F.SilenceLevel, F.SilenceLength);
  finally
    F.Free;
  end;
end;

procedure TCutTab.CutClick(Sender: TObject);
begin
  FCutView.Cut;
end;

procedure TCutTab.UndoClick(Sender: TObject);
begin
  FCutView.Undo;
end;

procedure TCutTab.PausePlay;
begin
  FCutView.Stop;
end;

procedure TCutTab.PlayClick(Sender: TObject);
begin
  FCutView.Play;

  if Assigned(FOnPlayStarted) then
    FOnPlayStarted(Self);
end;

procedure TCutTab.StopClick(Sender: TObject);
begin
  FCutView.Stop;
end;

procedure TCutTab.CutViewStateChanged(Sender: TObject);
begin
  UpdateButtons;
end;

destructor TCutTab.Destroy;
begin
  MsgBus.RemoveSubscriber(MessageReceived);

  inherited;
end;

procedure TCutTab.MessageReceived(Msg: TMessageBase);
var
  VolMsg: TVolumeChangedMsg;
begin
  if Msg is TVolumeChangedMsg then
  begin
    VolMsg := TVolumeChangedMsg(Msg);

    if VolMsg.Volume <> FVolume.Volume then
      FVolume.Volume := TVolumeChangedMsg(Msg).Volume;
  end;
end;

procedure TCutTab.ControlsAligned;
begin
  inherited ControlsAligned;

  FToolbarPanel.ClientHeight := FToolbar.Height;
end;

procedure TCutTab.SaveClick(Sender: TObject);
begin
  FCutView.Save;
end;

end.
