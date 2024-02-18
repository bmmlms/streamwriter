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

unit Wizard;

interface

uses
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  ImgList,
  LanguageObjects,
  LCLType,
  Logging,
  MLabeledEdit,
  ShlObj,
  StdCtrls,
  SysUtils,
  WizardBase;

type

  { TfrmWizard }

  TfrmWizard = class(TfrmWizardBase)
    chkMonitorMode: TCheckBox;
    Label20: TLabel;
    Label4: TLabel;
    pnlDir: TPanel;
    pnlMisc: TPanel;
    chkLimit: TCheckBox;
    txtDir: TMLabeledEditButton;
    txtMaxSpeed: TMLabeledSpinEdit;
    pnlSelectMode: TPanel;
    optModeEasy: TRadioButton;
    optModeAdvanced: TRadioButton;
    txtMonitorCount: TMLabeledSpinEdit;
    procedure cmdBrowseClick(Sender: TObject);
    procedure chkLimitClick(Sender: TObject);
    procedure chkMonitorModeClick(Sender: TObject);
    procedure Label20Click(Sender: TObject);
  protected
    procedure RegisterSteps; override;
    procedure Finish; override;
    function IsValid(Step: TStep): Boolean; override;
    procedure InitStep(Step: TStep); override;
  end;

implementation

{$R *.lfm}

procedure TfrmWizard.Finish;
begin
  AppGlobals.Dir := txtDir.Control.Text;
  AppGlobals.DirAuto := txtDir.Control.Text;
  AppGlobals.LimitSpeed := chkLimit.Checked;
  AppGlobals.MaxSpeed := txtMaxSpeed.Control.Value;
  AppGlobals.MonitorMode := chkMonitorMode.Checked;
  AppGlobals.MonitorCount := txtMonitorCount.Control.Value;

  inherited;
end;

procedure TfrmWizard.InitStep(Step: TStep);
var
  s: string;
begin
  inherited;

  if Step.Panel = pnlDir then
  begin
    if (AppGlobals.Dir <> '') and DirectoryExists(AppGlobals.Dir) then
      txtDir.Control.Text := AppGlobals.Dir
    else
    begin
      s := TFunctions.GetShellFolder(CSIDL_MYMUSIC);
      if (Trim(s) <> '') and (txtDir.Control.Text = '') then
      begin
        s := ConcatPaths([s, 'streamWriter']);
        txtDir.Control.Text := s;
      end;
    end;
  end else if Step.Panel = pnlMisc then
  begin
    chkLimit.Checked := AppGlobals.LimitSpeed;
    txtMaxSpeed.Control.Value := AppGlobals.MaxSpeed;
    chkMonitorMode.Checked := AppGlobals.MonitorMode;
    txtMonitorCount.Control.Value := AppGlobals.MonitorCount;

    chkLimitClick(chkLimit);
    chkMonitorModeClick(chkMonitorMode);
  end else if Step.Panel = pnlSelectMode then
  ;
end;

function TfrmWizard.IsValid(Step: TStep): Boolean;
begin
  Result := inherited;

  if not Result then
    Exit;

  if Step.Panel = pnlDir then
  begin
    try
      ForceDirectories(txtDir.Control.Text);
    except
    end;

    if not DirectoryExists(txtDir.Control.Text) then
    begin
      TFunctions.MsgBox(_('The selected folder does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
      Result := False;
    end;
  end;
end;

procedure TfrmWizard.Label20Click(Sender: TObject);
begin
  chkMonitorMode.Checked := not chkMonitorMode.Checked;
end;

procedure TfrmWizard.RegisterSteps;
begin
  inherited;

  FStepList.Add(TStep.Create('Select folder', pnlDir));
  FStepList[FStepList.Count - 1].Description := 'Please select a folder where recorded songs will be saved.';
  FStepList.Add(TStep.Create('Miscellaneous settings', pnlMisc));
  FStepList[FStepList.Count - 1].Description := 'Miscellaneous settings can be configured here.';
  //FStepList.Add(TStepDir.Create('Select mode', pnlSelectMode));
  //FStepList[FStepList.Count - 1].Description := 'Please specify whether you want to use easy or advanced mode. Advanced mode contains some features not needed by most people - if you want to record songs automatically using a wishlist, easy mode provides everything you need.';
end;

procedure TfrmWizard.chkLimitClick(Sender: TObject);
begin
  txtMaxSpeed.Enabled := chkLimit.Checked;
end;

procedure TfrmWizard.chkMonitorModeClick(Sender: TObject);
begin
  txtMonitorCount.Enabled := chkMonitorMode.Checked;
end;

procedure TfrmWizard.cmdBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  if not TFunctions.BrowseDialog(Self, _('Select folder for saved songs'), Dir) then
    Exit;

  if DirectoryExists(Dir) then
    txtDir.Control.Text := Dir
  else
    TFunctions.MsgBox(_('The selected folder does not exist. Please choose another one.'), _('Info'), MB_ICONINFORMATION);
end;

end.
