library sw_setup_helper;

uses
  Windows;

function AppRunning(AppName: PAnsiChar): Boolean; stdcall;
var
  MutexHandle: Cardinal;
begin
  Result := False;
  MutexHandle := OpenMutex(SYNCHRONIZE, False, PChar(string(AppName) + 'Mutex'));
  if MutexHandle <> 0 then
  begin
    CloseHandle(MutexHandle);
    Result := True;
  end;
end;

function AppWndHandle(AppName: PAnsiChar): Cardinal; stdcall;
var
  hFileMapping: THandle;
  SA: TSecurityAttributes;
  pSD: TSecurityDescriptor;
  Mem: PCardinal;
begin
  Result := 0;
  if not InitializeSecurityDescriptor(@pSD, SECURITY_DESCRIPTOR_REVISION) then
    Exit;
  if not SetSecurityDescriptorDacl(@pSD, True, nil, False) then
    Exit;
  SA.nLength := SizeOf(SA);
  SA.lpSecurityDescriptor := @pSD;
  SA.bInheritHandle := True;
  hFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, @SA,
    PAGE_READONLY, 0, SizeOf(Result), PChar(string(AppName) + 'WndHandle'));
  if hFileMapping <> 0 then
  begin
    Mem := MapViewOfFile(hFileMapping, FILE_MAP_READ, 0, 0, SizeOf(Result));
    if Assigned(Mem) then
    begin
      Move(Mem^, Result, SizeOf(Result));
      UnmapViewOfFile(Mem);
    end;
    CloseHandle(hFileMapping);
  end;
end;

exports
  AppRunning,
  AppWndHandle;

begin
end.
