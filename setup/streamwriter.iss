[Files]
Source: ..\bin\streamwriter.exe; DestDir: {app}; Flags: ignoreversion
Source: .\InnoCallback.dll; DestDir: {tmp}; Flags: dontcopy
Source: .\helper\bin\sw_setup_helper.dll; DestDir: {tmp}; Flags: dontcopy

[Icons]
Name: {group}\streamWriter; Filename: {app}\streamwriter.exe; IconFilename: {app}\streamwriter.exe; IconIndex: 0

[Languages]
Name: Deutsch; MessagesFile: compiler:Languages\German.isl
Name: English; MessagesFile: compiler:Default.isl

[CustomMessages]
Deutsch.Launch=streamWriter starten
Deutsch.Running=streamWriter läuft gerade und muss beendet werden, damit die Installation fortgesetzt werden kann.\nDie Installation wird automatisch fortgesetzt, wenn streamWriter beendet wurde.
Deutsch.Running2=streamWriter wird gerade beendet...
Deutsch.Running3=streamWriter läuft gerade
Deutsch.Running4=streamWriter muss beendet werden, damit fortgesetzt werden kann
Deutsch.Running5=Es scheint, dass streamWriter sich nicht ordnungsgemäß beenden kann. Bitte lade die neueste Setup-Version von streamwriter.org herunter und führe das Update manuell durch.
Deutsch.ExitApp=streamWriter beenden
Deutsch.PleaseRestart=Das Update wurde abgeschlossen. Bitte starte streamWriter neu.

English.Launch=Launch streamWriter
English.Running=streamWriter is currently running and needs to be closed to continue setup.\nSetup will continue automatically when streamWriter was closed.
English.Running2=streamWriter is exiting...
English.Running3=streamWriter is running
English.Running4=streamWriter needs to be closed to continue
English.Running5=It seems that streamWriter cannot close correctly. Please download the newest setup-file from streamwriter.org and update manually.
English.ExitApp=Close streamWriter
English.PleaseRestart=The update was installed successfully. Please restart streamWriter.

[Setup]
OutputBaseFilename=streamwriter_setup
InternalCompressLevel=ultra
SolidCompression=true
Encryption=false
AppName=streamWriter
AppVerName=streamWriter
PrivilegesRequired=admin
DefaultDirName={pf32}\streamWriter
AllowRootDirectory=true
AllowUNCPath=false
DefaultGroupName=streamWriter
AlwaysUsePersonalGroup=false
ShowLanguageDialog=yes
LanguageDetectionMethod=none
AlwaysShowComponentsList=false
WizardImageFile=compiler:wizmodernimage-is.bmp
WizardSmallImageFile=wizmodernsmallimage-is.bmp
Compression=lzma/ultra
LicenseFile=license.txt
VersionInfoVersion=4.3.0.2

[Run]
Filename: {app}\streamwriter.exe; WorkingDir: {app}; Flags: waituntilidle postinstall skipifsilent; Description: "{cm:Launch}"

[Code]
type TTimerProc = procedure(h:longword; msg:longword; idevent:longword; dwTime:longword);

function WrapTimerProc(callback:TTimerProc; paramcount:integer):longword; external 'wrapcallback@files:innocallback.dll stdcall';
function AppRunning(AppName: PAnsiChar): Boolean; external 'AppRunning@files:sw_setup_helper.dll stdcall';
function AppWndHandle(AppName: PAnsiChar): Cardinal; external 'AppWndHandle@files:sw_setup_helper.dll stdcall';

procedure PostQuitMessage; external 'PostQuitMessage@user32.dll stdcall';
function SetTimer(hWnd: longword; nIDEvent, uElapse: longword; lpTimerFunc: longword): longword; external 'SetTimer@user32.dll stdcall';
function KillTimer(hWnd, nIDEvent: LongWord): LongWord; external 'KillTimer@user32 stdcall';
function GetTickCount: LongWord; external 'GetTickCount@kernel32 stdcall';

var
  PID: Integer;
  Text: TNewStaticText;
  Button: TNewButton;
const
  AppName = 'streamWriter';

function TranslateNewline(Text: String): String;
begin
  Result := Text;
  StringChangeEx(Result, '\n', #13#10, True);
end;

function PosEx(Needle, Haystack: String; Index: Integer): Integer;
begin
  Result := Pos(Needle, AnsiLowerCase(Copy(Haystack, Index + 1, Length(Haystack))));
end;

procedure ButtonOnClick(Sender: TObject);
begin
  PostMessage(AppWndHandle(AppName), 16, 0, 0);
end;

procedure CreatePages;
var
  Page: TWizardPage;
begin
  Page := CreateCustomPage(wpReady, ExpandConstant('{cm:Running3}'), ExpandConstant('{cm:Running4}'));

  PID := Page.ID;

  Text := TNewStaticText.Create(Page);
  Text.WordWrap := True;
  Text.Width := Page.SurfaceWidth;
  Text.Parent := Page.Surface;

  Button := TNewButton.Create(Page);
  Button.Caption := ExpandConstant('{cm:ExitApp}');
  Button.Width := 130;
  Button.OnClick := @ButtonOnClick;
  Button.Parent := Page.Surface; 
end;

procedure UpdateControls;
begin
  if AppRunning(AppName) and (AppWndHandle(AppName) = 0) then
  begin
    Button.Enabled := False;
    Text.Caption := TranslateNewline(ExpandConstant('{cm:Running2}'));
  end else if not AppRunning(AppName) then
  begin
    WizardForm.NextButton.Enabled := True;
    PostMessage(WizardForm.NextButton.Handle, 245, 0, 0);
  end; 
end;

procedure AppRunningTimer(hwnd: LongWord; uMsg: LongWord; idEvent: LongWord; dwTime: LongWord);
begin
  if WizardForm.CurPageID <> PID then
    Exit;

  UpdateControls;
end;

procedure InitializeWizard();
begin
  CreatePages;
end;

function InitializeSetup(): Boolean;
var
  s: String;
  Start: Integer;
begin
  Result := True;

  s := GetCmdTail;

  if Pos('/update', LowerCase(s)) > 0 then
  begin
    Start := GetTickCount;
    while AppRunning(AppName) and (Start > GetTickCount - 60000) do
    begin
      Sleep(100);
    end;

    if AppRunning(AppName) then
    begin
      MsgBox(ExpandConstant('{cm:Running5}'), mbInformation, MB_OK);
      Result := False;
    end;
  end;

  SetTimer(0, 0, 100, WrapTimerProc(@AppRunningTimer,4));
end;

procedure DeinitializeSetup();
begin
  KillTimer(0, 0);
end;

function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  exit;

  if CurPageID = 10 then
  begin
    while AppRunning(AppName) do
    begin
      if MsgBox(TranslateNewline(ExpandConstant('{cm:Running}')), mbConfirmation, MB_YESNO) = IDNO then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;
  if (PageID = PID) and (not AppRunning(AppName)) then
    Result := True;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if (CurPageID = PID) and AppRunning(AppName) then
  begin
    Text.Caption := TranslateNewline(ExpandConstant('{cm:Running}'));
    WizardForm.NextButton.Enabled := False;
    Button.Top := Text.Top + Text.Height + ScaleY(8);
    Button.Enabled := True;

    UpdateControls;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  r: Integer;
  s: String;
begin
  if CurStep = ssPostInstall then
  begin
    s := GetCmdTail;
    if Pos('/update', AnsiLowerCase(s)) > 0 then
    begin
      if Pos('/run', AnsiLowerCase(s)) > 0 then
        Exec(ExpandConstant('{app}') + '\streamwriter.exe', '/updated', ExpandConstant('{app}'), 1, ewNoWait, r)
      else
        MsgBox(TranslateNewline(ExpandConstant('{cm:PleaseRestart}')), mbInformation, MB_OK);
    end;
  end;
end;






































