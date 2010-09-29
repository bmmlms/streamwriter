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
  Windows, SysUtils, Classes, Generics.Collections, Functions,
  LanguageObjects, PluginsShared;

type
  TPlugin = class;
  TProcessThread = class;
  TFilenameArray = array of string;

  TActResults = (arWin, arFail);
  TReadWrite = procedure(Name, Value: PChar);

  TInitialize = procedure(L: PChar; RF, WF: TReadWrite); stdcall;
  TAct = function(var Data: TPluginActData): Integer; stdcall;
  TGetInt = function: Integer; stdcall;
  TGetString = function(Data: PChar; Len: Integer): Integer; stdcall;
  TGetBoolean = function: Boolean; stdcall;
  TConfigure = function(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;

  TPluginProcessInformation = record
    Filename, Station, Title: string;
    TrackNumber: Cardinal;
    Filesize: UInt64;
  end;
  PPluginProcessInformation = ^TPluginProcessInformation;

  TProcessingEntry = class
  private
    FActiveThread: TProcessThread;
    FData: PPluginProcessInformation;
    FPluginsProcessed: TList<TPlugin>;
  public
    constructor Create(ActiveThread: TProcessThread;
      Data: TPluginProcessInformation; FirstPlugin: TPlugin);
    destructor Destroy; override;

    property ActiveThread: TProcessThread read FActiveThread;
    property Data: PPluginProcessInformation read FData;
    property PluginsProcessed: TList<TPlugin> read FPluginsProcessed;
  end;

  TProcessingList = class(TList<TProcessingEntry>)

  end;

  TPluginManager = class
  private
    FPlugins: TList<TPlugin>;
    FActivePlugin: TPlugin;
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    function ProcessFile(Data: TPluginProcessInformation): TProcessingEntry; overload;
    function ProcessFile(Entry: TProcessingEntry): Boolean; overload;
    procedure ReInitPlugins;

    property Plugins: TList<TPlugin> read FPlugins;
  end;

  TProcessThread = class(TThread)
  private
    FData: PPluginProcessInformation;
    FActData: TPluginActData;
    FPlugin: TPlugin;
    FResult: TActResults;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPlugin);
    destructor Destroy; override;

    property Plugin: TPlugin read FPlugin;
    property Result: TActResults read FResult;
  end;

  TPlugin = class
  private
    FFilename: string;
    FAuthor: string;
    FDefaultEnabled: Boolean;
    FDLLHandle: Integer;
    FActive: Boolean;

    FInitialize: TInitialize;
    FAct: TAct;
    FConfigure: TConfigure;
    FReadAuthor: TGetString;
    FReadName: TGetString;
    FReadHelp: TGetString;
    FReadDefaultEnabled: TGetBoolean;

    function FGetName: string;
    function FGetHelp: string;
  public
    constructor Create(Handle: THandle; Filename: string);
    destructor Destroy; override;

    procedure Initialize;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThread;
    function Configure(Handle: Cardinal; ShowMessages: Boolean): Boolean;

    property Filename: string read FFilename;
    property Author: string read FAuthor;
    property Name: string read FGetName;
    property Help: string read FGetHelp;
    property DefaultEnabled: Boolean read FDefaultEnabled;
    property Active: Boolean read FActive write FActive;
  end;

implementation

uses
  AppData;

{ TPluginManager }

function TPluginManager.ProcessFile(Data: TPluginProcessInformation): TProcessingEntry;
begin
  Result := TProcessingEntry.Create(nil, Data, nil);
  if not ProcessFile(Result) then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function TPluginManager.ProcessFile(Entry: TProcessingEntry): Boolean;
var
  i: Integer;
begin
  Result := False;

  // Das soeben beendete Plugin der Liste hinzufügen
  if Entry.ActiveThread <> nil then
    Entry.PluginsProcessed.Add(Entry.ActiveThread.Plugin);

  // Nächstes Plugin suchen
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i].Active then
    begin
      if not Entry.FPluginsProcessed.Contains(FPlugins[i]) then
      begin
        Entry.FActiveThread := FPlugins[i].ProcessFile(Entry.FData);
        Result := Entry.FActiveThread <> nil;
        Break;
      end;
    end;
end;

procedure TPluginManager.ReInitPlugins;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Initialize;
end;

constructor TPluginManager.Create(Path: string);
var
  i: Integer;
  Handle: THandle;
  GetVersion: TGetInt;
  P: TPlugin;
  Files: TStringList;
begin
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
          @GetVersion := GetProcAddress(Handle, 'GetVersion');
          if @GetVersion = nil then
            Continue;
          if GetVersion >= 1 then
          begin
            P := TPlugin.Create(Handle, Path + Files[i]);
            P.Initialize;
            Plugins.Add(P);
          end;
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
  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Free;
  FPlugins.Free;
  inherited;
end;

{ TPlugin }

function TPlugin.ProcessFile(Data: PPluginProcessInformation): TProcessThread;
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

function Read(Keyname, Name, Value: PChar): Integer;
var
  V: string;
begin
  ZeroMemory(Value, 255);
  AppGlobals.Storage.Read(Name, V, '', 'Plugins\' + Keyname);
  CopyMemory(Value, @V[1], Length(V) * SizeOf(Char));
  Result := Length(V);
end;

function Write(Keyname, Name, Value: PChar): Integer;
begin
  AppGlobals.Storage.Write(Name, Value, 'Plugins\' + Keyname);
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
  @FReadHelp := GetProcAddress(FDLLHandle, 'GetHelp');
  @FReadDefaultEnabled := GetProcAddress(FDLLHandle, 'GetDefaultEnabled');
  FFilename := Filename;

  FAuthor := '';
  FDefaultEnabled := False;

  GetMem(Data, 255);
  try
    ZeroMemory(Data, 255);
    if @FReadAuthor <> nil then
      if FReadAuthor(Data, 255) > -1 then
        FAuthor := Data;
  finally
    FreeMem(Data);
  end;

  FDefaultEnabled := FReadDefaultEnabled;

  AppGlobals.Storage.Read('Active_' + LowerCase(ExtractFileName(Filename)), FActive, FDefaultEnabled, 'Plugins');

  if FAuthor = '' then
    raise Exception.Create('-');
end;

destructor TPlugin.Destroy;
begin

  inherited;
end;

function TPlugin.FGetHelp: string;
var
  Data: PChar;
begin
  Result := '';
  if @FReadHelp = nil then
    Exit;
  GetMem(Data, 2048);
  try
    ZeroMemory(Data, 2048);
    if FReadHelp(Data, 2048) > -1 then
      Result := Data;
  finally
    FreeMem(Data);
  end;
end;

function TPlugin.FGetName: string;
var
  Data: PChar;
begin
  Result := '';
  if @FReadName = nil then
    Exit;
  GetMem(Data, 255);
  try
    ZeroMemory(Data, 255);
    if FReadName(Data, 255) > -1 then
      Result := Data;
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
  inherited Create;

  FActiveThread := ActiveThread;

  New(FData);

  FData.Filename := Data.Filename;
  FData.Station := Data.Station;
  FData.Title := Data.Title;
  FData.TrackNumber := Data.TrackNumber;
  FData.Filesize := Data.Filesize;

  FPluginsProcessed := TList<TPlugin>.Create;
  if FirstPlugin <> nil then
    FPluginsProcessed.Add(FirstPlugin);
end;

destructor TProcessingEntry.Destroy;
begin
  FPluginsProcessed.Free;
  Dispose(FData);

  inherited;
end;

{ TProcessThread }

constructor TProcessThread.Create(Data: PPluginProcessInformation; Plugin: TPlugin);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FData := Data;
  FPlugin := Plugin;
  FResult := arFail;
end;

destructor TProcessThread.Destroy;
begin

  inherited;
end;

procedure TProcessThread.Execute;
begin
  inherited;
  GetMem(FActData.Filename, 512);
  GetMem(FActData.Station, 512);
  GetMem(FActData.Title, 512);

  StrPCopy(FActData.Filename, FData.Filename);
  StrPCopy(FActData.Station, FData.Station);
  StrPCopy(FActData.Title, FData.Title);
  FActData.TrackNumber := FData.TrackNumber;
  FActData.Filesize := FData.Filesize;

  FResult := TActResults(FPlugin.FAct(FActData));

  if FResult = arWin then
  begin
    // Wenn ein Plugin Daten geändert hat, diese übernehmen, so dass das nächste
    // Plugin in der Liste mit passenden Daten arbeiten kann
    FData.Filename := FActData.Filename;
    FData.Station := FActData.Station;
    FData.Title := FActData.Title;
    FData.TrackNumber := FActData.TrackNumber;
    FData.Filesize := FActData.Filesize;
  end;

  FreeMem(FActData.Filename);
  FreeMem(FActData.Station);
  FreeMem(FActData.Title);
end;

end.
