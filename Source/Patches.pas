{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2024 Alexander Nottelmann

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

unit Patches;

interface

uses
  ActiveX,
  DDetours,
  DragDropFile,
  DragDropPIDL,
  StrUtils,
  SysUtils,
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
  DirectoryExistsOld: function(const Directory: RawByteString; FollowLink: Boolean): Boolean;

function DrawTextExWNew(DC: HDC; lpchText: LPWSTR; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; stdcall;
begin
  // That char must be the same as in MPageControl, hack is somehow required for proper tabpage captions when caption contains '&'
  if EndsText(' ', lpchText) then
    dwDTFormat := dwDTFormat and not (1 shl DT_EXTERNALLEADING);
  Result := DrawTextExWOld(DC, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

function DirectoryExistsNew(const Directory: RawByteString; FollowLink: Boolean): Boolean;
begin
  Result := DirectoryExistsOld(Directory, False);
end;

initialization
  InterceptCreate(@FinalizeGlobalStructures, @FinalizeGlobalStructuresNew);
  InterceptCreate(@GetPIDLsFromFilenames, @GetPIDLsFromFilenamesNew);
  @DirectoryExistsOld := InterceptCreate(@DirectoryExists, @DirectoryExistsNew);
  @DrawTextExWOld := InterceptCreate(@DrawTextExW, @DrawTextExWNew);

end.
