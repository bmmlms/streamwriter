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
  TProcessThread = class;
  TFilenameArray = array of string;

  TPluginProcessInformation = record
    Filename, Station, Title: string;
  end;

  //TPluginResults = (prOk, prError);
  TReadWrite = procedure(Name, Value: PChar);

  TInitialize = procedure(L: PChar; RF, WF: TReadWrite); stdcall;
  TAct = function(Filename, Station, Title: PChar): Integer; stdcall;
  TGetString = function(Data: PChar; Len: Integer): Integer; stdcall;
  TGetBoolean = function: Boolean; stdcall;
  TConfigure = function(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;

  TProcessingEntry = class
  private
    FActiveThread: TProcessThread;
    FData: TPluginProcessInformation;
    FPluginsProcessed: TList<TPlugin>;
  public
    constructor Create(ActiveThread: TProcessThread;
      Data: TPluginProcessInformation; FirstPlugin: TPlugin);
    destructor Destroy; override;
  end;

  TProcessingList = class(TList<TProcessingEntry>)

  end;

  TPluginManager = class
  private
    FPlugins: TList<TPlugin>;
    FProcessingList: TProcessingList;
    FActivePlugin: TPlugin;

    procedure ThreadTerminate(Sender: TObject);
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    procedure ProcessFile(Data: TPluginProcessInformation);
    procedure ReInitPlugins;

    property Plugins: TList<TPlugin> read FPlugins;
  end;

  TProcessThread = class(TThread)
  private
    FData: TPluginProcessInformation;
    FPlugin: TPlugin;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: TPluginProcessInformation; Plugin: TPlugin);
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
    function ProcessFile(Data: TPluginProcessInformation): TProcessThread;
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

procedure TPluginManager.ProcessFile(Data: TPluginProcessInformation);
var
  i: Integer;
  Thread: TProcessThread;
begin
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i].Active then
    begin
      Thread := FPlugins[i].ProcessFile(Data);
      Thread.OnTerminate := ThreadTerminate;
      FProcessingList.Add(TProcessingEntry.Create(Thread, Data, FPlugins[i]));
      Thread.Resume;
      Break;
    end;
end;

procedure TPluginManager.ReInitPlugins;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Initialize;
end;

procedure TPluginManager.ThreadTerminate(Sender: TObject);
var
  i, n: Integer;
  Thread: TProcessThread;
begin
  for i := 0 to FProcessingList.Count - 1 do
  begin
    if FProcessingList[i].FActiveThread = Sender then
    begin

      for n := 0 to FPlugins.Count - 1 do
      begin
        if not FPlugins[n].Active then
          Continue;

        if not FProcessingList[i].FPluginsProcessed.Contains(FPlugins[n]) then
        begin
          Thread := FPlugins[n].ProcessFile(FProcessingList[i].FData);
          FProcessingList[i].FActiveThread := Thread;
          Thread.OnTerminate := ThreadTerminate;
          Thread.Resume;
          Exit;
        end;
      end;

      FProcessingList.Delete(i);
      Exit;
    end;
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

function TPlugin.ProcessFile(Data: TPluginProcessInformation): TProcessThread;
type
  TMapBytes = array[0..MAXINT - 1] of Byte;
  PMapBytes = ^TMapBytes;
var
  Thread: TProcessThread;
begin
  Result := nil;
  if (@FAct = nil) or (Length(FFilename) = 0) then
    Exit;

  Thread := TProcessThread.Create(Data, Self);
  Result := Thread;
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
  FActive := False; // TODO: Active wird nirgens gesetzt ansonsten. settings anyone?
  if Pos('settags', Filename) > 0 then
    FActive := True;

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
  FInitialize(PChar(Language.CurrentLanguage.ID), @Read, @Write);
end;

{ TProcessingEntry }

constructor TProcessingEntry.Create(ActiveThread: TProcessThread;
  Data: TPluginProcessInformation; FirstPlugin: TPlugin);
begin
  FActiveThread := ActiveThread;
  FData := Data;
  FPluginsProcessed := TList<TPlugin>.Create;
  FPluginsProcessed.Add(FirstPlugin);
end;

destructor TProcessingEntry.Destroy;
begin
  inherited;
  FPluginsProcessed.Free;
end;

{ TProcessThread }

constructor TProcessThread.Create(Data: TPluginProcessInformation; Plugin: TPlugin);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FData := Data;
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

  FPlugin.FAct(PChar(FData.Filename), PChar(FData.Station), PChar(FData.Title));
end;

end.
