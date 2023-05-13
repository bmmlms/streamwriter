unit Patches;

interface

uses
  ActiveX,
  DDetours,
  DragDropFile,
  DragDropPIDL,
  StrUtils,
  VirtualTrees,
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

procedure FinalizeGlobalStructuresNew;
begin

end;

var
  DrawTextExWOld: function(DC: HDC; lpchText: LPWSTR; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;

function DrawTextExWNew(DC: HDC; lpchText: LPWSTR; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;
begin
  // That char must be the same as in MPageControl, hack is somehow required for proper tabpage captions when caption contains '&'
  if EndsText(' ', lpchText) then
    dwDTFormat := dwDTFormat and not (1 shl DT_EXTERNALLEADING);
  Result := DrawTextExWOld(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

initialization
  InterceptCreate(@FinalizeGlobalStructures, @FinalizeGlobalStructuresNew);
  InterceptCreate(@GetPIDLsFromFilenames, @GetPIDLsFromFilenamesNew);
  @DrawTextExWOld := InterceptCreate(@DrawTextExW, @DrawTextExWNew);

end.
