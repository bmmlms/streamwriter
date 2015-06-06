[Files]
Source: ..\bin\streamwriter.exe; DestDir: {app}; Flags: ignoreversion
Source: .\InnoCallback.dll; DestDir: {tmp}; Flags: dontcopy
Source: .\WizModernSmallImage-IS.bmp; DestDir: {tmp}; Flags: dontcopy

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
Deutsch.Running6=streamWriter läuft und muss beendet werden, damit fortgesetzt werden kann.\nBitte beende streamWriter und klicke "OK", um fortzusetzen.
Deutsch.Running7=streamWriter konnte nicht beendet werden.\nBitte beende streamWriter, um fortzusetzen.
Deutsch.ExitApp=streamWriter beenden
Deutsch.PleaseRestart=Das Update wurde abgeschlossen. Bitte starte streamWriter neu.

English.Launch=Launch streamWriter
English.Running=streamWriter is currently running and needs to be closed to continue setup.\nSetup will continue automatically when streamWriter was closed.
English.Running2=streamWriter is exiting...
English.Running3=streamWriter is running
English.Running4=streamWriter needs to be closed to continue
English.Running5=It seems that streamWriter cannot close correctly. Please download the newest setup-file from streamwriter.org and update manually.
English.Running6=streamWriter is currently running and needs to be closed to continue.\nPlease close streamWriter and click "OK" to continue.
English.Running7=streamWriter could not be closed.\nPlease close streamWriter to continue.
English.ExitApp=Close streamWriter
English.PleaseRestart=The update was installed successfully. Please restart streamWriter.

[Setup]
OutputBaseFilename=streamwriter_setup
InternalCompressLevel=ultra
AppName=streamWriter
AppVerName=streamWriter
PrivilegesRequired=admin
DefaultDirName={pf32}\streamWriter
AllowNetworkDrive=no
DefaultGroupName=streamWriter
ShowLanguageDialog=yes
LanguageDetectionMethod=none
AlwaysShowComponentsList=false
WizardImageFile=compiler:wizmodernimage-is.bmp
WizardSmallImageFile=wizmodernsmallimage-is.bmp
Compression=lzma2/ultra
LicenseFile=license.txt
VersionInfoVersion=5.3.0.0
UninstallDisplayIcon={app}\streamwriter.exe
CloseApplications=no

[Run]
Filename: {app}\streamwriter.exe; WorkingDir: {app}; Flags: waituntilidle postinstall skipifsilent; Description: "{cm:Launch}"

[Code]
type TTimerProc = procedure(h:longword; msg:longword; idevent:longword; dwTime:longword);

// WinAPI
function SetTimer(hWnd: longword; nIDEvent, uElapse: longword; lpTimerFunc: longword): longword; external 'SetTimer@user32 stdcall';
function KillTimer(hWnd, nIDEvent: LongWord): LongWord; external 'KillTimer@user32 stdcall';
function GetTickCount: LongWord; external 'GetTickCount@kernel32 stdcall';

// InnoCallback
function WrapTimerProc(callback: TTimerProc; paramcount: integer):longword; external 'wrapcallback@files:innocallback.dll stdcall setuponly';

var
  ExitApp: Boolean;
  AppCloseError: Boolean;

  TimerAppRunning: THandle;
  TimerAppCheckExit: THandle;
  
  LabelState: TLabel;
  ButtonCloseApp: TNewButton;
  AppRunningPage: TWizardPage;
const
  WM_QUIT = 18;
  WM_CLOSE = 16;
  AppName = 'streamWriter';

function CreateTimer(EventID, Interval: LongWord; P: TTimerProc): THandle;
begin
  Result := SetTimer(0, EventID, Interval, WrapTimerProc(P, 4));
end;

procedure DestroyTimer(TimerID: LongWord);
begin
  KillTimer(0, TimerID);
end;

function AppRunning: Boolean;
begin
  Result := CheckForMutexes(AppName + 'Mutex');
end;

function AppShuttingDown: Boolean;
begin
  Result := not CheckForMutexes(AppName + 'MutexExiting');
end;

function GetWindowHandle: Cardinal;
begin
  Result := FindWindowByClassName('TfrmStreamWriterMain');
end;

function VersionSupportsCloseFromSetup: Boolean;
var
  V: string;
begin
  if RegQueryStringValue(HKEY_CURRENT_USER, 'Software\mistake.ws\streamWriter\Settings', 'LastUsedVersion', V) then
  begin
    Result := True;
    if (Length(V) > 2) and (StrToInt(V[1]) <= 4) and (V[2] = '.') then
      Result := False;
    if (V = '5.0.0.0') or (V = '5.0.0.1') then
      Result := False;
  end else
    Result := False;
end;

function TranslateNewline(Text: String): String;
begin
  Result := Text;
  StringChangeEx(Result, '\n', #13#10, True);
end;

function PosEx(Needle, Haystack: String; Index: Integer): Integer;
begin
  Result := Pos(Needle, AnsiLowerCase(Copy(Haystack, Index + 1, Length(Haystack))));
end;

procedure AppCheckExitTimer(hwnd: LongWord; uMsg: LongWord; idEvent: LongWord; dwTime: LongWord);
begin
  DestroyTimer(TimerAppCheckExit);

  if (AppRunningPage = nil) or (WizardForm.CurPageID <> AppRunningPage.ID) then
    Exit;

  if AppRunning then
  begin
    AppCloseError := True;
    LabelState.Caption := TranslateNewline(ExpandConstant('{cm:Running7}'));
  end;    
end;

procedure CloseApp;
begin
  if AppRunning and (not AppShuttingDown) and (GetWindowHandle > 0) then
  begin
    LabelState.Caption := TranslateNewline(ExpandConstant('{cm:Running2}'));
    ButtonCloseApp.Enabled := False;
    PostMessage(GetWindowHandle, 5432, 6345, 555);

    TimerAppCheckExit := CreateTimer(1, 5000, @AppCheckExitTimer);
  end;
end;

function TextHeight: Integer;
var
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    Result := B.Canvas.TextHeight('qwertzuiopasdfghjklyxcvbnm');
  finally
    B.Free;
  end;
end;

procedure ButtonCloseAppClick(Sender: TObject);
begin
  CloseApp;
  
  ButtonCloseApp.Enabled := False;
end;

procedure CreateControls(Owner: TWinControl);
begin
  LabelState := TLabel.Create(Owner);
  LabelState.WordWrap := True;
  LabelState.AutoSize := False;
  LabelState.Left := 0;
  LabelState.Top := 0;
  LabelState.Width := Owner.ClientWidth - LabelState.Left * 2;
  LabelState.Height := TextHeight * 5;
  LabelState.Parent := Owner;

  LabelState.Caption := TranslateNewline(ExpandConstant('{cm:Running}'))

  ButtonCloseApp := TNewButton.Create(Owner);
  ButtonCloseApp.Top := LabelState.Top + LabelState.Height;
  ButtonCloseApp.Left := 0;
  ButtonCloseApp.Caption := ExpandConstant('{cm:ExitApp}')
  ButtonCloseApp.OnClick := @ButtonCloseAppClick;
  ButtonCloseApp.Width := ScaleX(130);
  ButtonCloseApp.Parent := Owner;
end;

function CreatePage: TWizardPage;
begin
  Result := CreateCustomPage(wpReady, ExpandConstant('{cm:Running3}'), ExpandConstant('{cm:Running4}'));

  CreateControls(Result.Surface);
end;

procedure UpdateControls;
begin
  if AppRunning and AppShuttingDown then
  begin                                 
    ButtonCloseApp.Enabled := False;
    if not AppCloseError then
      LabelState.Caption := TranslateNewline(ExpandConstant('{cm:Running2}'));
  end else if (not AppRunning) then
  begin
    WizardForm.NextButton.Enabled := True;
    PostMessage(WizardForm.NextButton.Handle, 245, 0, 0);
  end; 
end;

procedure AppRunningTimer(hwnd: LongWord; uMsg: LongWord; idEvent: LongWord; dwTime: LongWord);
begin
  if (AppRunningPage = nil) or (WizardForm.CurPageID <> AppRunningPage.ID) then
    Exit;

  UpdateControls;
end;

procedure InitializeWizard;
begin  
  AppCloseError := False;

  AppRunningPage := CreatePage;
end;

function InitializeSetup: Boolean;
var
  s: String;
  Start: Integer;
begin
  Result := True;

  s := GetCmdTail;

  if Pos('/update', LowerCase(s)) > 0 then
  begin
    Start := GetTickCount;
    while AppRunning and (Start > GetTickCount - 60000) do
    begin
      Sleep(100);
    end;

    if AppRunning then
    begin
      MsgBox(ExpandConstant('{cm:Running5}'), mbInformation, MB_OK);
      Result := False;
    end;
  end;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;

  if PageID = AppRunningPage.ID then
  begin
    Result := (not VersionSupportsCloseFromSetup) or (not AppRunning);

    // Wenn man das so macht, sieht der Benutzer nicht, dass der Knopf sich langsam ausgraut...
    if not Result then
      WizardForm.NextButton.Enabled := False;
  end;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  DestroyTimer(TimerAppRunning);
  DestroyTimer(TimerAppCheckExit);
  
  if (CurPageID = AppRunningPage.ID) and AppRunning then
  begin
    LabelState.Caption := TranslateNewline(ExpandConstant('{cm:Running}'));
    ButtonCloseApp.Enabled := True;

    UpdateControls;

    WizardForm.NextButton.Enabled := False;
    TimerAppRunning := CreateTimer(0, 100, @AppRunningTimer);
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  r: Integer;
  s: String;
begin
  if (CurStep = ssInstall) and (not VersionSupportsCloseFromSetup) then
  begin
    while AppRunning do
      if MsgBox(TranslateNewline(ExpandConstant('{cm:Running6}')), mbInformation, MB_OKCANCEL) = IDCANCEL then
        Abort;
  end;

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

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usUninstall then  
  begin
    ExitApp := False;
    while AppRunning do
      if MsgBox(TranslateNewline(ExpandConstant('{cm:Running6}')), mbInformation, MB_OKCANCEL) = IDCANCEL then
        Abort;
  end;
end;

