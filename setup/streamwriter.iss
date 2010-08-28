[Files]
Source: ..\bin\streamwriter.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\bin\plugins\settags.dll; DestDir: {app}\plugins; Flags: ignoreversion
;Source: ..\bin\plugins\normalize.dll; DestDir: {app}\plugins; Flags: ignoreversion
;Source: ..\bin\plugins\rename.dll; DestDir: {app}\plugins; Flags: ignoreversion

[Icons]
Name: {group}\streamWriter; Filename: {app}\streamwriter.exe; IconFilename: {app}\streamwriter.exe; IconIndex: 0

[Languages]
Name: Deutsch; MessagesFile: compiler:Languages\German.isl
Name: English; MessagesFile: "compiler:Default.isl"

[CustomMessages]
Deutsch.Launch=streamWriter starten
Deutsch.Running=streamWriter läuft gerade und muss geschlossen werden, damit Setup fortgesetzt werden kann.\nDrücken Sie "Ja" zum wiederholen, "Nein" zum Beenden des Setups.
Deutsch.PleaseRestart=Das Update wurde abgeschlossen. Bitte starte streamWriter neu.

English.Launch=Launch streamWriter
English.Running=streamWriter is currently running and needs to be closed to continue setup.\nChoose "Yes" to retry, "No" to cancel setup.
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
WizardSmallImageFile=compiler:wizmodernsmallimage-is.bmp
Compression=lzma/ultra
VersionInfoVersion=1.1.0.5

[Run]
Filename: {app}\streamwriter.exe; WorkingDir: {app}; Flags: waituntilidle postinstall skipifsilent; Description: "{cm:Launch}"

[Code]
procedure PostQuitMessage; external 'PostQuitMessage@user32.dll stdcall';

function TranslateNewline(Text: String): String;
begin
  Result := Text;
  StringChangeEx(Result, '\n', #13#10, True);
end;

function PosEx(Needle, Haystack: String; Index: Integer): Integer;
begin
  Result := Pos(Needle, AnsiLowerCase(Copy(Haystack, Index + 1, Length(Haystack))));
end;

function InitializeSetup(): Boolean;
var
  s: String;
  Handle: THandle;
begin
  s := GetCmdTail;

  if Pos('/update', AnsiLowerCase(s)) > 0 then
    Sleep(2000);
    
  Result := True;
  Handle := 1;
  while Handle <> 0 do
  begin
    Handle := FindWindowByClassName('TfrmStreamWriterMain');
    if Handle <> 0 then
    begin
      if MsgBox(TranslateNewline(ExpandConstant('{cm:Running}')), mbConfirmation, MB_YESNO) = IDNO then
      begin
        Result := False;
        Break;
      end;
    end;
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
        Exec(ExpandConstant('{app}') + '\streamwriter.exe', '', ExpandConstant('{app}'), 1, ewNoWait, r)
      else
        MsgBox(TranslateNewline(ExpandConstant('{cm:PleaseRestart}')), mbInformation, MB_OK);
    end;
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usDone then
  begin
    //RegDeleteKeyIncludingSubkeys(HKCU, 'Software\mistake.ws\streamWriter');
  end;
end;




























