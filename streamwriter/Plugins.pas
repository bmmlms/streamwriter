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
  TPluginBase = class;
  TExternalPlugin = class;
  TProcessThreadBase = class;
  TFilenameArray = array of string;

  TActResults = (arWin, arTimeout, arFail);
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
    WasCut: Boolean;
  end;
  PPluginProcessInformation = ^TPluginProcessInformation;

  TProcessingEntry = class
  private
    FActiveThread: TProcessThreadBase;
    FData: PPluginProcessInformation;
    FPluginsProcessed: TList<TPluginBase>;
  public
    constructor Create(ActiveThread: TProcessThreadBase;
      Data: TPluginProcessInformation; FirstPlugin: TPluginBase);
    destructor Destroy; override;

    property ActiveThread: TProcessThreadBase read FActiveThread;
    property Data: PPluginProcessInformation read FData;
    property PluginsProcessed: TList<TPluginBase> read FPluginsProcessed;
  end;

  TProcessingList = class(TList<TProcessingEntry>)

  end;

  TPluginManager = class
  private
    FPlugins: TList<TPluginBase>;
    FActivePlugin: TPlugin;
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    function ProcessFile(Data: TPluginProcessInformation): TProcessingEntry; overload;
    function ProcessFile(Entry: TProcessingEntry): Boolean; overload;
    procedure ReInitPlugins;
    function GetID(ID: Integer): TExternalPlugin;

    property Plugins: TList<TPluginBase> read FPlugins;
  end;

  TProcessThreadBase = class(TThread)
  private
    FData: PPluginProcessInformation;
    FActData: TPluginActData;
    FPlugin: TPluginBase;
    FResult: TActResults;
    FOutput: string;
  protected
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
    destructor Destroy; override;

    property Plugin: TPluginBase read FPlugin;
    property Result: TActResults read FResult;
    property Output: string read FOutput;
  end;

  TProcessThread = class(TProcessThreadBase)
  private
  protected
    procedure Execute; override;
  public
  end;

  TPluginBase = class
  private
  protected
    FName: string;
    FActive: Boolean;
    FOrder: Integer;
  public
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; virtual; abstract;

    property Name: string read FName;
    property Active: Boolean read FActive write FActive;
    property Order: Integer read FOrder write FOrder;
  end;

  TPlugin = class(TPluginBase)
  private
    FFilename: string;
    FAuthor: string;
    FHelp: string;
    FDefaultEnabled: Boolean;
    FDLLHandle: Cardinal;

    FInitialize: TInitialize;
    FAct: TAct;
    FConfigure: TConfigure;
    FReadAuthor: TGetString;
    FReadName: TGetString;
    FReadHelp: TGetString;
    FReadDefaultEnabled: TGetBoolean;
  protected
  public
    constructor Create(Handle: THandle; Filename: string);
    destructor Destroy; override;

    procedure Initialize;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Configure(Handle: Cardinal; ShowMessages: Boolean): Boolean;

    property Handle: Cardinal read FDLLHandle;
    property Filename: string read FFilename;
    property Author: string read FAuthor;
    property Help: string read FHelp;
    property DefaultEnabled: Boolean read FDefaultEnabled;
  end;

  TExternalProcessThread = class(TProcessThread)
  private
    FExe: string;
    FParams: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Exe, Params: string; Data: PPluginProcessInformation; Plugin: TExternalPlugin);
    destructor Destroy; override;
  end;

  TExternalPlugin = class(TPluginBase)
  private
    FExe: string;
    FParams: string;
    FIdentifier: Integer;
  protected
  public
    constructor Create(Exe, Params: string; Active: Boolean; Identifier, Order: Integer);
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;

    property Exe: string read FExe write FExe;
    property Params: string read FParams write FParams;
    property Identifier: Integer read FIdentifier write FIdentifier;
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
  i, Order: Integer;
begin
  Result := False;
  Order := 0;

  // Das soeben beendete Plugin der Liste hinzufügen
  if Entry.ActiveThread <> nil then
  begin
    Entry.PluginsProcessed.Add(Entry.ActiveThread.Plugin);
    Order := Entry.ActiveThread.Plugin.Order + 1;
  end;

  // Nächstes Plugin suchen
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i].Active and (FPlugins[i].Order = Order) then
    begin
      Entry.FActiveThread := FPlugins[i].ProcessFile(Entry.FData);
      Result := Entry.FActiveThread <> nil;
      Break;
    end;
end;

procedure TPluginManager.ReInitPlugins;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    if not (FPlugins[i] is TExternalPlugin) then
      TPlugin(FPlugins[i]).Initialize;
end;

function TPluginManager.GetID(ID: Integer): TExternalPlugin;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i] is TExternalPlugin then
      if TExternalPlugin(FPlugins[i]).Identifier = ID then
      begin
        Result := TExternalPlugin(FPlugins[i]);
        Break;
      end;
end;

constructor TPluginManager.Create(Path: string);
var
  i: Integer;
  Handle: THandle;
  GetVersion: TGetInt;
  P: TPlugin;
  Files, Apps: TStringList;
  App, Params: string;
  EP: TExternalPlugin;
  Active: Boolean;
  Order: Integer;
begin
  FActivePlugin := nil;
  FPlugins := TList<TPluginBase>.Create;

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

  i := 0;
  repeat
    AppGlobals.Storage.Read('Exe_' + IntToStr(i), App, '', 'Plugins');
    AppGlobals.Storage.Read('Params_' + IntToStr(i), Params, '', 'Plugins');
    AppGlobals.Storage.Read('OrderExe_' + IntToStr(i), Order, 0, 'Plugins');
    AppGlobals.Storage.Read('Active_' + IntToStr(i), Active, True, 'Plugins');
    if App <> '' then
    begin
      EP := TExternalPlugin.Create(App, Params, Active, i, Order);
      try
        Plugins.Add(EP);
      except
        on E: Exception do
          messagebox(0, pchar(e.Message), '', 0);
      end;
    end;
    Inc(i);
  until (App = '');
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

function TPlugin.ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase;
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
  AppGlobals.Storage.Read('Order_' + LowerCase(ExtractFileName(Filename)), FOrder, 0, 'Plugins');

  if FAuthor = '' then
    raise Exception.Create('-');
end;

destructor TPlugin.Destroy;
begin

  inherited;
end;

procedure TPlugin.Initialize;
var
  Data: PChar;
begin
  FInitialize(PChar(Language.CurrentLanguage.ID), @Read, @Write);

  FHelp := '';
  if @FReadHelp = nil then
    Exit;
  GetMem(Data, 2048);
  try
    ZeroMemory(Data, 2048);
    if FReadHelp(Data, 2048) > -1 then
      FHelp := Data;
  finally
    FreeMem(Data);
  end;

  FName := '';
  if @FReadName = nil then
    Exit;
  GetMem(Data, 255);
  try
    ZeroMemory(Data, 255);
    if FReadName(Data, 255) > -1 then
      FName := Data;
  finally
    FreeMem(Data);
  end;
end;

{ TProcessingEntry }

constructor TProcessingEntry.Create(ActiveThread: TProcessThreadBase;
  Data: TPluginProcessInformation; FirstPlugin: TPluginBase);
begin
  inherited Create;

  FActiveThread := ActiveThread;

  New(FData);

  FData.Filename := Data.Filename;
  FData.Station := Data.Station;
  FData.Title := Data.Title;
  FData.TrackNumber := Data.TrackNumber;
  FData.Filesize := Data.Filesize;
  FData.WasCut := Data.WasCut;

  FPluginsProcessed := TList<TPluginBase>.Create;
  if FirstPlugin <> nil then
    FPluginsProcessed.Add(FirstPlugin);
end;

destructor TProcessingEntry.Destroy;
begin
  FPluginsProcessed.Free;
  Dispose(FData);

  inherited;
end;

{ TProcessThreadBase }

constructor TProcessThreadBase.Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FData := Data;
  FPlugin := Plugin;
  FResult := arFail;
  FOutput := '';
end;

destructor TProcessThreadBase.Destroy;
begin

  inherited;
end;

{ TProcessThread }

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
  FActData.WasCut := FData.WasCut;

  FResult := TActResults(TPlugin(FPlugin).FAct(FActData));

  if FResult = arWin then
  begin
    // Wenn ein Plugin Daten geändert hat, diese übernehmen, so dass das nächste
    // Plugin in der Liste mit passenden Daten arbeiten kann.
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

{ TExternalProcessThread }

constructor TExternalProcessThread.Create(Exe, Params: string; Data: PPluginProcessInformation; Plugin: TExternalPlugin);
begin
  inherited Create(Data, Plugin);
  FreeOnTerminate := True;
  FExe := Exe;
  FParams := Params;
  FData := Data;
  FPlugin := Plugin;
  FResult := arFail;
end;

destructor TExternalProcessThread.Destroy;
begin

  inherited;
end;

procedure TExternalProcessThread.Execute;
var
  Handle: Cardinal;
  Res: Integer;
  CmdLine, Replaced: string;
  Output: AnsiString;
  Arr: TPatternReplaceArray;
  Thread: TProcessThread;
begin
  if Trim(FExe) <> '' then
  begin
    if FileExists(FExe) then
    begin
      SetLength(Arr, 1);
      Arr[0].C := 'f';
      Arr[0].Replace := FData.Filename;
      Replaced := PatternReplace(FParams, Arr);
      if Trim(Replaced) <> '' then
        CmdLine := '"' + FExe + '" ' + Replaced
      else
        CmdLine := FExe;
      Res := RunProcess(CmdLine, 20000, Output);
      FOutput := Output;
      case Res of
        0:
          FResult := arWin;
        1:
          FResult := arFail;
        2:
          FResult := arTimeout;
      end;
    end;
  end;
end;

{ TExternalPlugin }

constructor TExternalPlugin.Create(Exe, Params: string; Active: Boolean; Identifier, Order: Integer);
begin
  inherited Create;

  FActive := Active;
  FExe := Exe;
  FParams := Params;
  FIdentifier := Identifier;
  FOrder := Order;

  FName := ExtractFileName(FExe);
end;

function TExternalPlugin.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
var
  i: Integer;
  Handle: Cardinal;
  Replaced: string;
  Arr: TPatternReplaceArray;
  Thread: TProcessThread;
begin
  Result := nil;
  Thread := TExternalProcessThread.Create(FExe, FParams, Data, Self);
  Result := Thread;
end;

end.
