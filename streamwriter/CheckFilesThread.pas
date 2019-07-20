{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2019 Alexander Nottelmann

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

{ This unit contains mechanisms to look for files that got modified }
unit CheckFilesThread;

interface

uses
  Windows, SysUtils, Classes, Functions, Logging;

type
  // Defines what happened to a moved/renamed file
  TFileEntryAction = (eaNone, eaSize, eaRemove);

  TFileEntry = class
  private
    FAction: TFileEntryAction;
    FSize: UInt64;
    FFilename: string;
    FHash: Cardinal;
  public
    constructor Create(Filename: string; Size: UInt64; Action: TFileEntryAction);
    destructor Destroy; override;

    property Action: TFileEntryAction read FAction write FAction;
    property Size: UInt64 read FSize write FSize;
    property Filename: string read FFilename write FFilename;
    property Hash: Cardinal read FHash write FHash;
  end;

  TCheckFilesThread = class(TThread)
  private
    FSuccess: Boolean;
    FFiles: TList;
  protected
    procedure Execute; override;
  public
    constructor Create(Files: TList); overload;
    destructor Destroy; override;

    property Success: Boolean read FSuccess;
    property Files: TList read FFiles;
  end;

implementation

{ TCheckFilesThread }

constructor TCheckFilesThread.Create(Files: TList);
begin
  inherited Create(True);

  FSuccess := False;
  FFiles := Files.Create;

  FreeOnTerminate := True;
end;

destructor TCheckFilesThread.Destroy;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    TFileEntry(FFiles[i]).Free;
  FFiles.Free;
  inherited;
end;

procedure TCheckFilesThread.Execute;
var
  i: Integer;
  NewSize: UInt64;
  E: TFileEntry;
begin
  inherited;

  for i := FFiles.Count - 1 downto 0 do
  begin
    E := TFileEntry(FFiles[i]);
    if Terminated then
      Exit;

    if not FileExists(E.Filename) then
      E.Action := eaRemove
    else
    begin
      NewSize := GetFileSize(E.Filename);
      if E.Size <> NewSize then
      begin
        E.Action := eaSize;
        E.Size := NewSize;
      end;
    end;
  end;
  FSuccess := True;
end;

{ TFileEntry }

constructor TFileEntry.Create(Filename: string; Size: UInt64;
  Action: TFileEntryAction);
begin
  FFilename := Filename;
  FSize := Size;
  FAction := Action;
  FHash := HashString(LowerCase(ExtractFileName(Filename)));
end;

destructor TFileEntry.Destroy;
begin

  inherited;
end;

end.
