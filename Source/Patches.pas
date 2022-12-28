unit Patches;

{$IFDEF CPU32}

interface

uses
  ActiveX,
  DragDropFile,
  DragDropPIDL,
  Windows;

implementation

function GetPIDLsFromFilenamesNew(const Files: TUnicodeStrings; PIDLs: TPIDLList): Boolean;
var
  i: Integer;
  PIDL: PITEMIDLIST;
  RootPIDL, FilePIDL: string;
begin
  PIDLs.Clear;
  if (Files.Count = 0) then
    Exit(False);

  RootPIDL := GetRootFolderPIDL(Files);
  if RootPIDL = '' then
    Exit(False);

  Result := True;

  PIDLs.Add(RootPIDL);

  for i := 0 to Files.Count - 1 do
  begin
    PIDL := GetFullPIDLFromPath(Files[i]);
    if (PIDL = nil) then
    begin
      Result := False;
      PIDLs.Clear;
      Break;
    end;

    try
      FilePIDL := PIDLToString(PIDL);
    finally
      CoTaskMemFree(PIDL);
    end;

    PIDLs.Add(FilePIDL);

    // PIDLs.Add(Copy(FilePIDL, Length(RootPIDL) - SizeOf(Word) + 1, Length(FilePIDL) - (Length(RootPIDL) - SizeOf(Word))));
  end;
end;

type
  TPatch = packed record
    Call: Byte;
    Proc: Pointer;
    Ret: Byte;
  end;

var
  Patch: TPatch;
  OldProtect: Cardinal;

initialization
  Patch.Call := $E8;
  Patch.Proc := Pointer(Integer(Pointer(@GetPIDLsFromFilenamesNew)) - Integer(Pointer(@GetPIDLsFromFilenames)) - 5);
  Patch.Ret := $C3;
  if VirtualProtect(@GetPIDLsFromFilenames, SizeOf(TPatch), PAGE_EXECUTE_READWRITE, OldProtect) then
    try
      CopyMemory(@GetPIDLsFromFilenames, @Patch, SizeOf(TPatch));
    finally
      VirtualProtect(@GetPIDLsFromFilenames, SizeOf(TPatch), OldProtect, OldProtect);
    end;

{$ELSE}

interface
implementation

{$ENDIF}

end.
