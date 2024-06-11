#define AppCpu        "x86_64"
#define CurrentYear   GetDateTimeString('yyyy','','')
#define AppVersion    GetFileVersion("..\Build\"+AppCpu+"\streamwriter.exe")
;#define AppCopyright  GetStringFileInfo("..\Build\"+AppCpu+"\streamwriter.exe", LEGAL_COPYRIGHT)

[Files]
Source: ..\Build\{#AppCpu}\streamwriter.exe; DestDir: {app}; Flags: ignoreversion
Source: .\WizModernSmallImage-IS.bmp; DestDir: {tmp}; Flags: dontcopy

[Icons]
Name: {group}\streamWriter; Filename: {app}\streamwriter.exe; IconFilename: {app}\streamwriter.exe; IconIndex: 0

[Languages]
Name: Deutsch; MessagesFile: compiler:Languages\German.isl
Name: English; MessagesFile: compiler:Default.isl
Name: Italian; MessagesFile: compiler:Languages\Italian.isl

[CustomMessages]
Deutsch.Launch=streamWriter starten
Deutsch.Running=streamWriter l�uft gerade und muss beendet werden, damit die Installation fortgesetzt werden kann.\nDie Installation wird automatisch fortgesetzt, wenn streamWriter beendet wurde.
Deutsch.Running2=streamWriter wird beendet...
Deutsch.Running3=streamWriter l�uft
Deutsch.Running4=streamWriter muss beendet werden, damit fortgesetzt werden kann
Deutsch.Running5=Es scheint, dass streamWriter sich nicht ordnungsgem�� beenden kann. Bitte lade die neueste Setup-Version von streamwriter.org herunter und f�hre das Update manuell durch.
Deutsch.Running6=streamWriter l�uft gerade und muss beendet werden, damit die Installation fortgesetzt werden kann.\nBitte beende streamWriter und klicke "OK", um fortzusetzen.
Deutsch.Running7=streamWriter konnte nicht beendet werden.\nBitte beende streamWriter, um fortzusetzen.
Deutsch.Running8=streamWriter l�uft gerade und muss beendet werden, damit die Deinstallation fortgesetzt werden kann.\nKlicke "OK", um streamWriter zu beenden.
Deutsch.ExitApp=streamWriter beenden
Deutsch.PleaseRestart=Das Update wurde abgeschlossen. Bitte starte streamWriter neu.

English.Launch=Launch streamWriter
English.Running=streamWriter is currently running and needs to be closed to continue setup.\nSetup will continue automatically when streamWriter was closed.
English.Running2=streamWriter is exiting...
English.Running3=streamWriter is running
English.Running4=streamWriter needs to be closed to continue
English.Running5=It seems that streamWriter cannot close correctly. Please download the newest setup-file from streamwriter.org and update manually.
English.Running6=streamWriter is currently running and needs to be closed to continue installation.\nPlease close streamWriter and click "OK" to continue.
English.Running7=streamWriter could not be closed.\nPlease close streamWriter to continue.
English.Running8=streamWriter is currently running and needs to be closed to continue uninstallation.\nClick "OK" to close streamWriter.
English.ExitApp=Close streamWriter
English.PleaseRestart=The update was installed successfully. Please restart streamWriter.

Italian.Launch=Esegui streamWriter
Italian.Running=streamWriter � attualmente in esecuzione e deve essere chiuso per continuare la configurazione.\n\nLa configurazione continuer� automaticamente una volta chiuso streamWriter.
Italian.Running2=Chiusura streamWriter...
Italian.Running3=streamWriter � in esecuzione
Italian.Running4=Per continuare streamWriter deve essere chiuso.
Italian.Running5=Sembra che streamWriter non possa chiudersi correttamente.\n\nScarica il file di installazione pi� recente da streamwriter.org e aggiornalo manualmente.
Italian.Running6=streamWriter � attualmente in esecuzione e deve essere chiuso per continuare l'installazione.\n\nChiudi streamWriter e seleziona "OK" per continuare.
Italian.Running7=Impossibile chiudere streamWriter.\nPer continuare chiudi streamWriter.
Italian.Running8=streamWriter � attualmente in esecuzione e deve essere chiuso per continuare la disinstallazione.\n\nSeleziona "OK" per chiudere streamWriter.
Italian.ExitApp=Chiudi streamWriter
Italian.PleaseRestart=L'aggiornamento � stato installato correttamente.\n\nRiavviare streamWriter.

[Setup]
AppName=streamWriter
AppVersion={#AppVersion}
AppVerName=streamWriter {#AppVersion}

ShowLanguageDialog=yes
UsePreviousLanguage=no
LanguageDetectionMethod=uilanguage

AppPublisherURL=https://streamwriter.org
AppSupportURL=https://streamwriter.org
AppUpdatesURL=https://streamwriter.org
AppPublisher=Alexander Nottelmann

UninstallDisplayName=streamWriter
UninstallDisplayIcon={app}\streamwriter.exe
AppCopyright=(c) 2010-{#CurrentYear} Alexander Nottelmann

VersionInfoDescription=streamWriter installer
VersionInfoProductName=streamWriter
VersionInfoVersion={#AppVersion}

OutputBaseFilename=streamwriter_setup
InternalCompressLevel=ultra
PrivilegesRequired=admin
DefaultDirName={autopf}\streamWriter
AllowNetworkDrive=no
DefaultGroupName=streamWriter
AlwaysShowComponentsList=false
WizardSmallImageFile=WizModernSmallImage-IS.bmp
Compression=lzma2/ultra
LicenseFile=license.txt
CloseApplications=no
PrivilegesRequiredOverridesAllowed=commandline dialog
MinVersion=10.0.10240
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64

[Run]
Filename: {app}\streamwriter.exe; WorkingDir: {app}; Flags: waituntilidle postinstall skipifsilent; Description: "{cm:Launch}"

[Code]
type
  TTimerProc = procedure(hWnd: LongWord; uMsg: LongWord; idEvent: LongWord; dwTime: LongWord);
  TFakePointer = Cardinal;

// WinAPI
function SetTimer(hWnd: LongWord; nIDEvent, uElapse: LongWord; lpTimerFunc: LongWord): LongWord; external 'SetTimer@user32 stdcall';
function KillTimer(hWnd, nIDEvent: LongWord): LongWord; external 'KillTimer@user32 stdcall';
function GetTickCount: LongWord; external 'GetTickCount@kernel32 stdcall';
function CloseHandle(hObject: THandle): LongBool; external 'CloseHandle@kernel32 stdcall';

// Following imports are modified since Pascal Script does not seem to support pointers
function CreateFileMapping(hFile: THandle; lpFileMappingAttributes: TFakePointer; flProtect: DWORD; dwMaximumSizeHigh: DWORD; dwMaximumSizeLow: DWORD; lpName: PAnsiChar): THandle; external 'CreateFileMappingA@kernel32 stdcall';
function MapViewOfFile(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: Cardinal): TFakePointer; external 'MapViewOfFile@kernel32 stdcall';
procedure RtlMoveMemory(var Dst: Cardinal; Src: TFakePointer; Len: Cardinal); external 'RtlMoveMemory@kernel32 stdcall';
function UnmapViewOfFile(lpBaseAddress: TFakePointer): LongBool; external 'UnmapViewOfFile@kernel32 stdcall';

const
  AppName = 'streamWriter';
  INVALID_HANDLE_VALUE = -1;
  PAGE_READONLY = 2;
  FILE_MAP_READ = 4;

var
  AppCloseError: Boolean;

  TimerAppRunning: THandle;
  TimerAppCheckExit: THandle;
  
  LabelState: TLabel;
  ButtonCloseApp: TNewButton;
  AppRunningPage: TWizardPage;

function AppRunning: Boolean;
begin
  Result := CheckForMutexes(AppName + 'Mutex');
end;

function AppShuttingDown: Boolean;
begin
  Result := not CheckForMutexes(AppName + 'MutexExiting');
end;

function GetWindowHandle: Cardinal;
var
  FileMapping: THandle;
  Mem: Cardinal;
begin
  Result := 0;
  FileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, 0, PAGE_READONLY, 0, SizeOf(Result), AppName + 'WndHandle');
  if FileMapping = 0 then
    Exit;

  Mem := MapViewOfFile(FileMapping, FILE_MAP_READ, 0, 0, SizeOf(Result));
  if Mem > 0 then
  begin
    RtlMoveMemory(Result, Mem, SizeOf(Result));
    UnmapViewOfFile(Mem);
  end;
  CloseHandle(FileMapping);
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

procedure AppCheckExitTimer(hWnd: LongWord; uMsg: LongWord; idEvent: LongWord; dwTime: LongWord);
begin
  KillTimer(0, TimerAppCheckExit);

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

    TimerAppCheckExit := SetTimer(0, 1, 10000, CreateCallback(@AppCheckExitTimer));
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
  KillTimer(0, TimerAppRunning);
  KillTimer(0, TimerAppCheckExit);
  
  if (CurPageID = AppRunningPage.ID) and AppRunning then
  begin
    LabelState.Caption := TranslateNewline(ExpandConstant('{cm:Running}'));
    ButtonCloseApp.Enabled := True;

    UpdateControls;

    WizardForm.NextButton.Enabled := False;
    TimerAppRunning := SetTimer(0, 0, 100, CreateCallback(@AppRunningTimer));
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
var
  r, i: Integer;
begin
  if (CurUninstallStep = usUninstall) and AppRunning then  
  begin
    r := MsgBox(TranslateNewline(ExpandConstant('{cm:Running8}')), mbInformation, MB_OKCANCEL);
    if r = IDOK then
    begin
      if GetWindowHandle > 0 then
      begin
        PostMessage(GetWindowHandle, 5432, 6345, 555);
        for i := 0 to 10 do
        begin
          Sleep(1000);
          if not AppRunning then
            Exit;
        end;
      end;

      while AppRunning do
        if MsgBox(TranslateNewline(ExpandConstant('{cm:Running7}')), mbInformation, MB_OKCANCEL) = IDCANCEL then
          Abort;
    end else
      Abort;
  end;
end;

