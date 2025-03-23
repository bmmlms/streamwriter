{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2025 Alexander Nottelmann

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
  Classes,
  Functions,
  Generics.Collections,
  Logging,
  SysUtils;

type
  // Defines what happened to a moved/renamed file
  TFileEntryAction = (feaNone, feaSize, feaRemove);

  TFileEntry = class
  private
    FAction: TFileEntryAction;
    FSize: Int64;
    FFilename: string;
    FHash: Cardinal;
  public
    constructor Create(Filename: string; Size: Int64; Action: TFileEntryAction);

    property Action: TFileEntryAction read FAction write FAction;
    property Size: Int64 read FSize write FSize;
    property Filename: string read FFilename write FFilename;
    property Hash: Cardinal read FHash write FHash;
  end;

  TCheckFilesThread = class(TThread)
  private
    FSuccess: Boolean;
    FFiles: TList<TFileEntry>;
  protected
    procedure Execute; override;
  public
    constructor Create(Files: TList<TFileEntry>); overload;
    destructor Destroy; override;

    property Success: Boolean read FSuccess;
    property Files: TList<TFileEntry> read FFiles;
  end;

implementation

{ TCheckFilesThread }

constructor TCheckFilesThread.Create(Files: TList<TFileEntry>);
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
    FFiles[i].Free;
  FFiles.Free;

  inherited;
end;

procedure TCheckFilesThread.Execute;
var
  i: Integer;
  NewSize: Int64;
  E: TFileEntry;
begin
  inherited;

  for i := FFiles.Count - 1 downto 0 do
  begin
    E := FFiles[i];
    if Terminated then
      Exit;

    if not FileExists(E.Filename) then
      E.Action := feaRemove
    else
    begin
      if TFunctions.GetFileSize(E.Filename, NewSize) and (E.Size <> NewSize) then
      begin
        E.Action := feaSize;
        E.Size := NewSize;
      end;
    end;
  end;
  FSuccess := True;
end;

{ TFileEntry }

constructor TFileEntry.Create(Filename: string; Size: Int64; Action: TFileEntryAction);
begin
  FFilename := Filename;
  FSize := Size;
  FAction := Action;
  FHash := TFunctions.HashString(LowerCase(ExtractFileName(Filename)));
end;

end.
