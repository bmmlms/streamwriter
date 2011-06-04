{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
  LanguageObjects, PluginsShared, Logging;

type
  TDLLPlugin = class;
  TPluginBase = class;
  TExternalPlugin = class;
  TProcessThreadBase = class;
  TFilenameArray = array of string;

  TActResults = (arWin, arTimeout, arFail, arImpossible);
  TReadWrite = procedure(Name, Value: PChar);

  TInitialize = procedure(L: PChar; RF, WF: TReadWrite); stdcall;
  TAct = function(var Data: TPluginActData): Integer; stdcall;
  TGetInt = function: Integer; stdcall;
  TGetString = function(Data: PChar; Len: Integer): Integer; stdcall;
  TGetBoolean = function: Boolean; stdcall;
  TConfigure = function(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;

  TPluginProcessInformation = record
    Filename, Station, Artist, Title: string;
    TrackNumber: Cardinal;
    Filesize: UInt64;
    Length: UInt64;
    WasCut: Boolean;
    FullTitle: Boolean;
    StreamTitle: string;
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

    property ActiveThread: TProcessThreadBase read FActiveThread write FActiveThread;
    property Data: PPluginProcessInformation read FData;
    property PluginsProcessed: TList<TPluginBase> read FPluginsProcessed;
  end;

  TProcessingList = class(TList<TProcessingEntry>)

  end;

  TProcessThreadBase = class(TThread)
  private
    FActData: TPluginActData;
    FPlugin: TPluginBase;
    FOutput: string;
  protected
    FResult: TActResults;
    FData: PPluginProcessInformation;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPluginBase);
    destructor Destroy; override;

    property Plugin: TPluginBase read FPlugin;
    property Result: TActResults read FResult;
    property Output: string read FOutput;
  end;

  TProcessThreadDLL = class(TProcessThreadBase)
  private
  protected
    procedure Execute; override;
  public
  end;

  TPluginBase = class
  private
  protected
    FCanConfigure: Boolean;
    FName: string;
    FHelp: string;
    FActive: Boolean;
    FOrder: Integer;
    FOnlyIfCut: Boolean;
  public
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; virtual; abstract;
    function Copy: TPluginBase; virtual; abstract;
    procedure Assign(Source: TPluginBase); virtual;
    procedure Initialize; virtual;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; virtual;
    procedure Save; virtual;

    property CanConfigure: Boolean read FCanConfigure;
    property Name: string read FName;
    property Help: string read FHelp;
    property Active: Boolean read FActive write FActive;
    property Order: Integer read FOrder write FOrder;
    property OnlyIfCut: Boolean read FOnlyIfCut write FOnlyIfCut;
  end;

  TDLLPlugin = class(TPluginBase)
  private
    FFilename: string;
    FAuthor: string;
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

    procedure Initialize; override;
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;

    property Handle: Cardinal read FDLLHandle;
    property Filename: string read FFilename;
    property Author: string read FAuthor;
    property DefaultEnabled: Boolean read FDefaultEnabled;
  end;

  TInternalPlugin = class(TPluginBase)
  private
  protected
    FDownloadPackage: string;
    FDownloadName: string;

    function FGetReadyForUse: Boolean; virtual;
    function FGetReadyForActivate: Boolean; virtual;
    function FGetFilesInstalled: Boolean; virtual;
  public
    property FilesInstalled: Boolean read FGetFilesInstalled;
    property DownloadPackage: string read FDownloadPackage write FDownloadPackage;
    property DownloadName: string read FDownloadName;
    property ReadyForUse: Boolean read FGetReadyForUse;
    property ReadyForActivate: Boolean read FGetReadyForActivate;
  end;

  TExternalProcessThread = class(TProcessThreadBase)
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

    procedure FSetExe(Value: string);
  protected
  public
    constructor Create(Exe, Params: string; Active, OnlyIfCut: Boolean; Identifier, Order: Integer);
    function ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase; override;
    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;

    property Exe: string read FExe write FSetExe;
    property Params: string read FParams write FParams;
    property Identifier: Integer read FIdentifier write FIdentifier;
  end;

implementation

uses
  AppData;

{ TDLLPlugin }

function TDLLPlugin.ProcessFile(Data: PPluginProcessInformation): TProcessThreadBase;
var
  Thread: TProcessThreadDLL;
begin
  Result := nil;
  if (@FAct = nil) or (Length(FFilename) = 0) then
    Exit;

  Thread := TProcessThreadDLL.Create(Data, Self);
  Result := Thread;
end;

function TDLLPlugin.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
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

function TDLLPlugin.Copy: TPluginBase;
begin
  // REMARK: Es gibt zur Zeit keine DLL-Plugins.
  Result := nil;
end;

constructor TDLLPlugin.Create(Handle: THandle; Filename: string);
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
  AppGlobals.Storage.Read('OnlyIfCut_' + LowerCase(ExtractFileName(Filename)), FOnlyIfCut, False, 'Plugins');

  if FAuthor = '' then
    raise Exception.Create('-');
end;

destructor TDLLPlugin.Destroy;
begin

  inherited;
end;

procedure TDLLPlugin.Initialize;
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
  FData.Artist := Data.Artist;
  FData.Title := Data.Title;
  FData.TrackNumber := Data.TrackNumber;
  FData.Filesize := Data.Filesize;
  FData.Length := Data.Length;
  FData.WasCut := Data.WasCut;
  FData.FullTitle := Data.FullTitle;
  FData.StreamTitle := Data.StreamTitle;

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

{ TProcessThreadDLL }

procedure TProcessThreadDLL.Execute;
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
  FActData.FullTitle := FData.FullTitle;
  StrPCopy(FActData.StreamTitle, FData.StreamTitle);

  FResult := TActResults(TDLLPlugin(FPlugin).FAct(FActData));

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
  Res: Integer;
  CmdLine, Replaced: string;
  Output: AnsiString;
  Arr: TPatternReplaceArray;
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
      begin
        if LowerCase(ExtractFileExt(FExe)) = '.bat' then
        begin
          // & ist für Batch nen Sonderzeichen. Also escapen.
          Replaced := StringReplace(Replaced, '&', '^&', [rfReplaceAll]);
        end;
        CmdLine := '"' + FExe + '" ' + Replaced;
      end else
        CmdLine := FExe;
      Res := RunProcess(CmdLine, ExtractFilePath(FExe), 120000, Output);
      FData.Filesize := GetFileSize(FData.Filename);
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

procedure TExternalPlugin.Assign(Source: TPluginBase);
begin
  FExe := TExternalPlugin(Source).FExe;
  FParams := TExternalPlugin(Source).FParams;
end;

function TExternalPlugin.Copy: TPluginBase;
begin
  Result := TExternalPlugin.Create(FExe, FParams, FActive, FOnlyIfCut, FIdentifier, FOrder);
end;

constructor TExternalPlugin.Create(Exe, Params: string; Active, OnlyIfCut: Boolean; Identifier, Order: Integer);
begin
  inherited Create;

  FActive := Active;
  FOnlyIfCut := OnlyIfCut;
  FExe := Exe;
  FParams := Params;
  FIdentifier := Identifier;
  FOrder := Order;

  FName := ExtractFileName(FExe);
end;

procedure TExternalPlugin.FSetExe(Value: string);
begin
  FExe := Value;
  FName := ExtractFileName(FExe);
end;

function TExternalPlugin.ProcessFile(
  Data: PPluginProcessInformation): TProcessThreadBase;
var
  Thread: TExternalProcessThread;
begin
  Thread := TExternalProcessThread.Create(FExe, FParams, Data, Self);
  Result := Thread;
end;

{ TPluginBase }

procedure TPluginBase.Assign(Source: TPluginBase);
begin

end;

function TPluginBase.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
begin
  Result := False;
end;

procedure TPluginBase.Initialize;
begin

end;

procedure TPluginBase.Save;
begin

end;

{ TInternalPlugin }

function TInternalPlugin.FGetFilesInstalled: Boolean;
begin
  Result := False;
end;

function TInternalPlugin.FGetReadyForActivate: Boolean;
begin
  Result := False;
end;

function TInternalPlugin.FGetReadyForUse: Boolean;
begin
  Result := False;
end;

end.
