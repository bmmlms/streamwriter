{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit CutTab;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  MControls, LanguageObjects, Tabs, CutView, Functions, AppData, SharedControls;

type
  TCutToolBar = class(TToolBar)
  private
    FSave: TToolButton;
    FSep3: TToolButton;
    FPosZoom: TToolButton;
    FPosEdit: TToolButton;
    FPosPlay: TToolButton;
    FSep1: TToolButton;
    FAutoCut: TToolButton;
    FCut: TToolButton;
    FUndo: TToolButton;
    FSep2: TToolButton;
    FPlay: TToolButton;
    FStop: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Setup;
  end;

  TCutTab = class(TMainTabSheet)
  private
    FToolbarPanel: TPanel;
    FToolBar: TCutToolBar;
    FVolume: TVolumePanel;
    FCutView: TCutView;
    FFilename: string;

    FOnSaved: TNotifyEvent;
    FOnVolumeChanged: TSeekChangeEvent;

    procedure UpdateButtons;

    procedure SaveClick(Sender: TObject);
    procedure PosClick(Sender: TObject);
    procedure AutoCutClick(Sender: TObject);
    procedure CutClick(Sender: TObject);
    procedure UndoClick(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure StopClick(Sender: TObject);

    procedure CutViewStateChanged(Sender: TObject);
    procedure VolumeTrackbarChange(Sender: TObject);

    procedure FSetVolume(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure Setup(Filename: string; ToolBarImages: TImageList);

    property Filename: string read FFilename;
    property Volume: Integer write FSetVolume;

    property OnSaved: TNotifyEvent read FOnSaved write FOnSaved;
    property OnVolumeChanged: TSeekChangeEvent read FOnVolumeChanged write FOnVolumeChanged;
  end;

implementation

{ TCutTab }

constructor TCutTab.Create(AOwner: TComponent);
begin
  inherited;

  ImageIndex := 17;
end;

procedure TCutTab.UpdateButtons;
begin
  FToolBar.FSave.Enabled := FCutView.CanSave;
  FToolBar.FPosEdit.Enabled := FCutView.CanSetLine;
  FToolBar.FPosPlay.Enabled := FCutView.CanSetLine;
  FToolBar.FAutoCut.Enabled := FCutView.CanAutoCut;
  FToolBar.FPosPlay.Enabled := FCutView.CanZoom;
  FToolBar.FCut.Enabled := FCutView.CanCut;
  FToolbar.FPosZoom.Enabled := FCutView.CanZoom;
  FToolBar.FUndo.Enabled := FCutView.CanUndo;
  FToolBar.FPlay.Enabled := FCutView.CanPlay;
  FToolBar.FStop.Enabled := FCutView.CanStop;

  // Das muss so, sonst klappt das .Down := True nicht, wenn sie
  // vorher Disabled waren, vor dem Enable da oben...
  FToolBar.FPosEdit.Down := False;
  FToolBar.FPosPlay.Down := False;
  FToolBar.FPosZoom.Down := False;

  case FCutView.LineMode of
    lmEdit:
      FToolBar.FPosEdit.Down := True;
    lmPlay:
      FToolBar.FPosPlay.Down := True;
    lmZoom:
      FToolBar.FPosZoom.Down := True;
  end;
end;

procedure TCutTab.VolumeTrackbarChange(Sender: TObject);
begin
  //AppGlobals.CutVolume := FVolume.Volume;
  FCutView.Volume := FVolume.Volume;

  if Assigned(FOnVolumeChanged) then
    FOnVolumeChanged(Self, FVolume.Volume);
end;

procedure TCutTab.SaveClick(Sender: TObject);
begin
  if FCutView.Save then
    if Assigned(FOnSaved) then
      FOnSaved(Self);
end;

procedure TCutTab.PosClick(Sender: TObject);
begin
  if TToolButton(Sender).Down then
    Exit;

  FToolBar.FPosEdit.Down := False;
  FToolBar.FPosPlay.Down := False;
  FToolBar.FPosZoom.Down := False;

  if Sender = FToolBar.FPosEdit then
  begin
    FCutView.LineMode := lmEdit;
    FToolBar.FPosEdit.Down := True;
  end;
  if Sender = FToolBar.FPosPlay then
  begin
    FCutView.LineMode := lmPlay;
    FToolBar.FPosPlay.Down := True;
  end;
  if Sender = FToolBar.FPosZoom then
  begin
    FCutView.LineMode := lmZoom;
    FToolBar.FPosZoom.Down := True;
  end;
end;

procedure TCutTab.AutoCutClick(Sender: TObject);
begin
  FCutView.AutoCut(AppGlobals.StreamSettings.SilenceLevel, AppGlobals.StreamSettings.SilenceLength);
end;

procedure TCutTab.CutClick(Sender: TObject);
begin
  FCutView.Cut;
end;

procedure TCutTab.UndoClick(Sender: TObject);
begin
  FCutView.Undo;
end;

procedure TCutTab.PlayClick(Sender: TObject);
begin
  FCutView.Play;
end;

procedure TCutTab.StopClick(Sender: TObject);
begin
  FCutView.Stop;
end;

procedure TCutTab.CutViewStateChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TCutTab.FSetVolume(Value: Integer);
begin
  FVolume.NotifyOnMove := False;
  FVolume.Volume := Value;
  FVolume.NotifyOnMove := True;

  FCutView.Volume := FVolume.Volume;
end;

procedure TCutTab.Setup(Filename: string; ToolBarImages: TImageList);
begin
  MaxWidth := 120;
  Caption := ExtractFileName(Filename);
  FFilename := Filename;

  FToolbarPanel := TPanel.Create(Self);
  FToolbarPanel.Align := alTop;
  FToolbarPanel.BevelOuter := bvNone;
  FToolbarPanel.Parent := Self;
  FToolbarPanel.ClientHeight := 24;

  FToolBar := TCutToolBar.Create(Self);
  FToolBar.Parent := FToolbarPanel;
  FToolBar.Images := ToolBarImages;
  FToolBar.Align := alLeft;
  FToolBar.Width := Self.ClientWidth - 130;
  FToolBar.Height := 24;
  FToolBar.Setup;

  FToolBar.FSave.OnClick := SaveClick;
  FToolBar.FPosEdit.OnClick := PosClick;
  FToolBar.FPosPlay.OnClick := PosClick;
  FToolBar.FPosZoom.OnClick := PosClick;
  FToolBar.FAutoCut.OnClick := AutoCutClick;
  FToolBar.FCut.OnClick := CutClick;
  FToolBar.FUndo.OnClick := UndoClick;
  FToolBar.FPlay.OnClick := PlayClick;
  FToolBar.FStop.OnClick := StopClick;

  FVolume := TVolumePanel.Create(Self);
  FVolume.Parent := FToolbarPanel;
  FVolume.Align := alRight;
  FVolume.Setup;
  FVolume.Width := 140;
  FVolume.Volume := AppGlobals.PlayerVolume;
  FVolume.OnVolumeChange := VolumeTrackbarChange;

  FCutView := TCutView.Create(Self);
  FCutView.Parent := Self;
  FCutView.Align := alClient;
  FCutView.Volume := FVolume.Volume;
  FCutView.OnStateChanged := CutViewStateChanged;

  UpdateButtons;
  FCutView.LoadFile(Filename);
end;

{ TCutToolbar }

constructor TCutToolBar.Create(AOwner: TComponent);
begin
  inherited;

  ShowHint := True;
  Transparent := True;
end;

procedure TCutToolBar.Setup;
begin
  FStop := TToolButton.Create(Self);
  FStop.Parent := Self;
  FStop.Hint := _('Stop');
  FStop.ImageIndex := 1;

  FPlay := TToolButton.Create(Self);
  FPlay.Parent := Self;
  FPlay.Hint := _('Play');
  FPlay.ImageIndex := 33;

  FSep2 := TToolButton.Create(Self);
  FSep2.Parent := Self;
  FSep2.Style := tbsSeparator;
  FSep2.Width := 8;

  FUndo := TToolButton.Create(Self);
  FUndo.Parent := Self;
  FUndo.Hint := _('Undo');
  FUndo.ImageIndex := 18;

  FCut := TToolButton.Create(Self);
  FCut.Parent := Self;
  FCut.Hint := _('Cut');
  FCut.ImageIndex := 17;

  FAutoCut := TToolButton.Create(Self);
  FAutoCut.Parent := Self;
  FAutoCut.Hint := _('Show silence according to configured settings');
  FAutoCut.ImageIndex := 19;

  FSep1 := TToolButton.Create(Self);
  FSep1.Parent := Self;
  FSep1.Style := tbsSeparator;
  FSep1.Width := 8;

  FPosZoom := TToolButton.Create(Self);
  FPosZoom.Parent := Self;
  FPosZoom.Hint := _('Zoom in (left mousebutton selects area, right mousebutton zooms back)');
  FPosZoom.ImageIndex := 48;

  FPosEdit := TToolButton.Create(Self);
  FPosEdit.Parent := Self;
  FPosEdit.Hint := _('Set cutpositions (left mousebutton sets start, right button sets end)');
  FPosEdit.ImageIndex := 37;

  FPosPlay := TToolButton.Create(Self);
  FPosPlay.Parent := Self;
  FPosPlay.Hint := _('Set playposition');
  FPosPlay.ImageIndex := 27;

  FSep3 := TToolButton.Create(Self);
  FSep3.Parent := Self;
  FSep3.Style := tbsSeparator;
  FSep3.Width := 8;

  FSave := TToolButton.Create(Self);
  FSave.Parent := Self;
  FSave.Hint := _('Save');
  FSave.ImageIndex := 14;
end;

end.


