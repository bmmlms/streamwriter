{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls, ShellAPI, ShlObj, AppData,
  ImgList, LanguageObjects, Functions, WizardBase, GUIFunctions, Logging,
  PngSpeedButton, SharedData;

type
  TStepDir = class(TStep)

  end;

  TfrmWizard = class(TfrmWizardBase)
    pnlDir: TPanel;
    cmdBrowse: TPngSpeedButton;
    txtDir: TLabeledEdit;
    pnlMisc: TPanel;
    chkLimit: TCheckBox;
    txtMaxSpeed: TLabeledEdit;
    Label2: TLabel;
    chkMonitorMode: TCheckBox;
    txtMonitorCount: TLabeledEdit;
    Label20: TLabel;
    procedure cmdBrowseClick(Sender: TObject);
    procedure chkLimitClick(Sender: TObject);
    procedure chkMonitorModeClick(Sender: TObject);
  protected
    procedure RegisterSteps; override;
    procedure Finish; override;
    function IsValid(Step: TStep): Boolean; override;
    procedure InitStep(Step: TStep); override;
  published
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

procedure TfrmWizard.Finish;
begin
  AppGlobals.Dir := txtDir.Text;
  AppGlobals.DirAuto := txtDir.Text;
  AppGlobals.LimitSpeed := chkLimit.Checked;
  if StrToIntDef(txtMaxSpeed.Text, -1) > 0 then
    AppGlobals.MaxSpeed := StrToInt(txtMaxSpeed.Text);
  AppGlobals.MonitorMode := chkMonitorMode.Checked;
  if StrToIntDef(txtMonitorCount.Text, -1) > 0 then
    AppGlobals.MonitorCount := StrToInt(txtMonitorCount.Text);
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
      txtDir.Text := IncludeTrailingBackslash(AppGlobals.Dir)
    else
    begin
      s := GetShellFolder(CSIDL_MYMUSIC);
      if (Trim(s) <> '') and (txtDir.Text = '') then
      begin
        s := IncludeTrailingPathDelimiter(s) + 'streamWriter\';
        txtDir.Text := s;
      end;
    end;
  end else
  begin
    chkLimit.Checked := AppGlobals.LimitSpeed;
    if AppGlobals.MaxSpeed > 0 then
      txtMaxSpeed.Text := IntToStr(AppGlobals.MaxSpeed);
    chkMonitorMode.Checked := AppGlobals.MonitorMode;
    if AppGlobals.MonitorCount > 0 then
      txtMonitorCount.Text := IntToStr(AppGlobals.MonitorCount);
  end;
end;

function TfrmWizard.IsValid(Step: TStep): Boolean;
begin
  Result := inherited;

  if not Result then
    Exit;

  if Step.Panel = pnlDir then
  begin
    try
      ForceDirectories(txtDir.Text);
    except
    end;

    if not DirectoryExists(txtDir.Text) then
    begin
      MsgBox(Handle, _('The selected folder does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
      Result := False;
    end;
  end else if Step.Panel = pnlMisc then
  begin
    if chkLimit.Checked then
      if StrToIntDef(txtMaxSpeed.Text, -1) <= 0 then
      begin
        MsgBox(Handle, _('Please enter the maximum bandwidth in KB/s available to streamWriter.'), _('Info'), MB_ICONINFORMATION);
        Result := False;
      end;
    if chkMonitorMode.Checked then
      if StrToIntDef(txtMonitorCount.Text, -1) <= 0 then
      begin
        MsgBox(Handle, _('Please enter the maximum number of streams to monitor.'), _('Info'), MB_ICONINFORMATION);
        Result := False;
      end;
  end;
end;

procedure TfrmWizard.RegisterSteps;
begin
  inherited;
  FStepList.Add(TStepDir.Create('Select folder', pnlDir));
  FStepList[FStepList.Count - 1].Description := 'Please select a folder where recorded songs will be saved.';
  FStepList.Add(TStepDir.Create('Miscellaneous settings', pnlMisc));
  FStepList[FStepList.Count - 1].Description := 'Miscellaneous settings can be configured here.';
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
  Dir: String;
begin
  Dir := BrowseDialog(Handle, _('Select folder for saved songs:'), BIF_RETURNONLYFSDIRS);
  if DirectoryExists(Dir) then
  begin
    Dir := IncludeTrailingBackslash(Dir);
    txtDir.Text := Dir;
  end;
end;

constructor TfrmWizard.Create(AOwner: TComponent);
begin
  inherited;

  cmdBrowse.PngImage := modSharedData.imgImages.PngImages[85].PngImage;
end;

end.
