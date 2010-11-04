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
unit Wizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Buttons, StdCtrls, ShellAPI, ShlObj, AppData,
  ImgList, LanguageObjects, Functions, WizardBase, GUIFunctions;

type
  TStepIceServer = class(TStep)

  end;

  TStepDir = class(TStep)

  end;

  TfrmWizard = class(TfrmWizardBase)
    pnlICEServer: TPanel;
    pnlDir: TPanel;
    cmdBrowse: TSpeedButton;
    txtDir: TLabeledEdit;
    lblDir: TLabel;
    Label6: TLabel;
    chkRelay: TCheckBox;
    chkSubmitStreams: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure cmdBrowseClick(Sender: TObject);
  protected
    procedure RegisterSteps; override;
    procedure Finish; override;
    function IsValid(Step: TStep): Boolean; override;
    procedure InitStep(Step: TStep); override;
  published

  end;

implementation

{$R *.dfm}

procedure TfrmWizard.Finish;
begin
  AppGlobals.Dir := txtDir.Text;
  //AppGlobals.Relay := chkRelay.Checked;
  AppGlobals.SubmitStreams := chkSubmitStreams.Checked;
  inherited;
end;

procedure TfrmWizard.InitStep(Step: TStep);
var
  s: string;
begin
  inherited;

  if Step.Panel = pnlICEServer then
  begin
    //chkRelay.Checked := AppGlobals.Relay;
    chkSubmitStreams.Checked := AppGlobals.SubmitStreams;
  end;
  if Step.Panel = pnlDir then
  begin
    if (AppGlobals.Dir <> '') and (not DirectoryExists(AppGlobals.Dir)) then
      txtDir.Text := IncludeTrailingBackslash(AppGlobals.Dir)
    else
    begin
      s := GetShellFolder(CSIDL_MYMUSIC);
      if (Trim(s) <> '') then
      begin
        s := IncludeTrailingPathDelimiter(s) + 'streamWriter\';
        txtDir.Text := s;
      end;
    end;
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

    if DirectoryExists(txtDir.Text) then
    begin
      AppGlobals.Dir := txtDir.Text;
    end else
    begin
      MsgBox(Handle, _('The selected folder does not exist.'#13#10'Please select another folder.'), _('Info'), MB_ICONINFORMATION);
      Result := False;
    end;
  end;
end;

procedure TfrmWizard.RegisterSteps;
begin
  inherited;
  FStepList.Add(TStepIceServer.Create('Settings', pnlICEServer));
  FStepList.Add(TStepDir.Create('Select folder', pnlDir));
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

end.
