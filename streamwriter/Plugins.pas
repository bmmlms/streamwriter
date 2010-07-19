{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010 Alexander Nottelmann

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
unit Plugins;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, Functions, LanguageObjects;

type
  TPlugin = class;
  TFilenameArray = array of string;

  //TPluginResults = (prOk, prError);
  TReadWrite = procedure(Name, Value: PChar);

  TInitialize = procedure(L: PChar; RF, WF: TReadWrite); stdcall;
  TAct = function(FileData: Pointer): Integer; stdcall;
  TGetString = function(Data: PChar; Len: Integer): Integer; stdcall;
  TGetBoolean = function: Boolean; stdcall;
  TConfigure = function(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;

  TProcessingEntry = class
  private
    FFilename: string;
    FPluginsProcessed: TList<TPlugin>;
  public
    constructor Create(Filename: string);
    destructor Destroy; override;
  end;

  TProcessingList = class(TList<TProcessingEntry>)

  end;

  TPluginManager = class
  private
    FPlugins: TList<TPlugin>;
    FProcessingList: TProcessingList;
    FActivePlugin: TPlugin;
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    procedure ProcessFile(Filename: string);

    property Plugins: TList<TPlugin> read FPlugins;
  end;

  TProcessThread = class(TThread)
  private
    FFilename: string;
    FPlugin: TPlugin;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; Filename: string;
      Plugin: TPlugin);
    destructor Destroy; override;
  end;

  TPlugin = class
  private
    FFilename: string;
    FAuthor: string;
    FDLLHandle: Integer;
    FActive: Boolean;

    FInitialize: TInitialize;
    FAct: TAct;
    FConfigure: TConfigure;
    FReadAuthor: TGetString;
    FReadName: TGetString;

    function FGetName: string;
  public
    constructor Create(Handle: THandle; Filename: string);
    destructor Destroy; override;

    procedure Initialize;
    procedure ProcessFile(Filename: string);
    function Configure(Handle: Cardinal; ShowMessages: Boolean): Boolean;

    property Filename: string read FFilename;
    property Author: string read FAuthor;
    property Name: string read FGetName;
    property Active: Boolean read FActive write FActive;
  end;

implementation

uses
  AppData;

{ TPluginManager }

procedure TPluginManager.ProcessFile(Filename: string);
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i].Active then
    begin
      FPlugins[i].ProcessFile(Filename);
      FProcessingList.Add(TProcessingEntry.Create(Filename));
      Break;
    end;
end;

constructor TPluginManager.Create(Path: string);
var
  P: TPlugin;
  Handle: THandle;
  Files: TStringList;
  i: Integer;
begin
  FProcessingList := TProcessingList.Create;

  FActivePlugin := nil;
  FPlugins := TList<TPlugin>.Create;

  Files := TStringList.Create;
  try
    FindFiles(Path + '*.dll', Files);
    for i := 0 to Files.Count - 1 do
    begin
      Handle := LoadLibrary(PChar(Path + Files[i]));
      if Handle <> 0 then
        try
          P := TPlugin.Create(Handle, Path + Files[i]);
          P.Initialize;
          Plugins.Add(P);
        except

        end;
    end;
  finally
    Files.Free;
  end;
end;

destructor TPluginManager.Destroy;
var
  i: Integer;
begin
  // TODO: Gefährlich. Und CallBacks aus DLL abschalten. Prüfen ob was aktiv beim Beenden, etc!
  FProcessingList.Free;

  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Free;
  FPlugins.Free;
  inherited;
end;

{ TPlugin }

procedure TPlugin.ProcessFile(Filename: string);
type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;
var
  Thread: TProcessThread;
begin
  if (@FAct = nil) or (Length(FFilename) = 0) then
    Exit;

  Thread := TProcessThread.Create(True, FFilename, Self);

  Thread.Resume;  // TODO: Der Pluginmanager muss es selbst starten, weil er muss die threads
                  // verwalten. wenn was fertig ist, die queue weiterverarbeiten und so..
                  // und threads freigeben!
end;

function TPlugin.Configure(Handle: Cardinal; ShowMessages: Boolean): Boolean;
begin
  Result := FConfigure(Handle, ShowMessages);
end;

function Read(Name, Value: PChar): Integer;
var
  V: string;
begin
  ZeroMemory(Value, 255);
  AppGlobals.Storage.Read(Name, V, '', 'Plugins\Winamp');
  CopyMemory(Value, @V[1], Length(V) * SizeOf(Char));
  Result := Length(V);
end;

function Write(Name, Value: PChar): Integer;
begin
  AppGlobals.Storage.Write(Name, Value, 'Plugins\Winamp');
  Result := 0;
end;

constructor TPlugin.Create(Handle: THandle; Filename: string);
var
  Data: PChar;
begin
  FDLLHandle := Handle;
  @FInitialize := GetProcAddress(FDLLHandle, 'Initialize');
  @FAct := GetProcAddress(FDLLHandle, 'Act');
  @FConfigure := GetProcAddress(FDLLHandle, 'Configure');
  @FReadAuthor := GetProcAddress(FDLLHandle, 'GetAuthor');
  @FReadName := GetProcAddress(FDLLHandle, 'GetName');
  FFilename := Filename;

  FAuthor := '';

  GetMem(Data, 255);
  try
    ZeroMemory(Data, 255);
    if @FReadAuthor <> nil then
      if FReadAuthor(Data, 255) > -1 then
        FAuthor := Data;
    ZeroMemory(Data, 255);
  finally
    FreeMem(Data);
  end;

  if FAuthor = '' then
    raise Exception.Create('Kein Author angegeben.');
end;

destructor TPlugin.Destroy;
begin

  inherited;
end;

function TPlugin.FGetName: string;
var
  Data: PChar;
begin
  Result := '';
  GetMem(Data, 255);
  try
    ZeroMemory(Data, 255);
    if @FReadName <> nil then
      if FReadName(Data, 255) > -1 then
        Result := Data;
    ZeroMemory(Data, 255);
  finally
    FreeMem(Data);
  end;
end;

procedure TPlugin.Initialize;
begin
  // TODO: checken dass die korrekte sprache benutzt wird
  FInitialize(PChar(Language.CurrentLanguage.ID), @Read, @Write);
end;

{ TProcessingEntry }

constructor TProcessingEntry.Create(Filename: string);
begin
  FFilename := Filename;
  FPluginsProcessed := TList<TPlugin>.Create;
end;

destructor TProcessingEntry.Destroy;
begin
  inherited;
  FPluginsProcessed.Free;
end;

{ TProcessThread }

constructor TProcessThread.Create(CreateSuspended: Boolean;
  Filename: string; Plugin: TPlugin);
begin
  inherited Create(False);
  FFilename := Filename;
  FPlugin := Plugin;
end;

destructor TProcessThread.Destroy;
begin

  inherited;
end;

procedure TProcessThread.Execute;
type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;
var
  i: Integer;
  Mem: PMapBytes;
  Offset: Integer;
  Len: Integer;
  T: Word;
begin
  inherited;

  Len := SizeOf(T);
  Len := Len + (Length(FFilename[i]) * SizeOf(Char)) + SizeOf(T);

  GetMem(Mem, Len);
  try
    Offset := 0;
    T := 1;
    Move(T, Mem^[Offset], SizeOf(T));
    Offset := Offset + SizeOf(T);
    T := Length(FFilename[i]) * SizeOf(Char);
    Move(T, Mem^[Offset], SizeOf(T));
    Offset := Offset + SizeOf(T);
    Move(FFilename[1], Mem^[Offset], T);
    Offset := Offset + T;
    //Result := TPluginResults(FAct(Mem));
  finally
    FreeMem(Mem, Len);
  end;

end;

end.
