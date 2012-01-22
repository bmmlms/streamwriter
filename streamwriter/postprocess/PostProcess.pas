{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
unit PostProcess;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, Functions,
  LanguageObjects, Logging, AddonBase, TypeDefs;

type
  TPostProcessBase = class;
  TExternalPostProcess = class;
  TPostProcessThreadBase = class;
  TFilenameArray = array of string;

  TActResults = (arWin, arTimeout, arFail, arImpossible);
  TReadWrite = procedure(Name, Value: PChar);

  TInitialize = procedure(L: PChar; RF, WF: TReadWrite); stdcall;
  TConfigure = function(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;

  TPostProcessInformation = record
    Filename, WorkFilename, ReEncodedFilename, Station, Artist, Album, Title: string;
    TrackNumber: Cardinal;
    Filesize: UInt64;
    Length: UInt64;
    WasCut: Boolean;
    FullTitle: Boolean;
    StreamTitle: string;
    BitRate: Cardinal;
    OutputFormat: TAudioTypes;
  end;
  PPostProcessInformation = ^TPostProcessInformation;

  TProcessingEntry = class
  private
    // The owner is a TICEClient
    FOwner: TObject;
    FNeedsWave: Boolean;

    FActiveThread: TPostProcessThreadBase;
    FData: PPostProcessInformation;
    FPostProcessList: TList<TPostProcessBase>;
    FActivePostProcessIndex: Integer;
  public
    constructor Create(Owner: TObject; ActiveThread: TPostProcessThreadBase;
      Data: TPostProcessInformation);
    destructor Destroy; override;

    property Owner: TObject read FOwner write FOwner;
    property NeedsWave: Boolean read FNeedsWave write FNeedsWave;
    property ActiveThread: TPostProcessThreadBase read FActiveThread write FActiveThread;
    property Data: PPostProcessInformation read FData;
    property PostProcessList: TList<TPostProcessBase> read FPostProcessList;
    property ActivePostProcessIndex: Integer read FActivePostProcessIndex write FActivePostProcessIndex;
  end;

  TProcessingList = class(TList<TProcessingEntry>)

  end;

  TPostProcessThreadBase = class(TThread)
  private
    FPostProcessor: TPostProcessBase;
    FOutput: string;
  protected
    FResult: TActResults;
    FData: PPostProcessInformation;
  public
    constructor Create(Data: PPostProcessInformation; PostProcessor: TPostProcessBase);
    destructor Destroy; override;

    property PostProcessor: TPostProcessBase read FPostProcessor;
    property Result: TActResults read FResult;
    property Output: string read FOutput;
  end;

  TPostProcessBase = class
  private
  protected
    FCopied: Boolean;

    FNeedsWave: Boolean;
    FHidden: Boolean;
    FCanConfigure: Boolean;
    FName: string;
    FHelp: string;
    FActive: Boolean;
    FOrder: Integer;
    FOnlyIfCut: Boolean;
    FGroupID: Integer;
  public
    constructor Create;

    function CanProcess(Data: PPostProcessInformation): Boolean; virtual;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; virtual; abstract;
    function Copy: TPostProcessBase; virtual; abstract;
    procedure Assign(Source: TPostProcessBase); virtual;
    procedure Initialize; virtual;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; virtual;
    procedure Save; virtual;
    function ShowInitMessage(Handle: THandle): Boolean; virtual;

    property NeedsWave: Boolean read FNeedsWave;
    property Hidden: Boolean read FHidden;
    property CanConfigure: Boolean read FCanConfigure;
    property Name: string read FName;
    property Help: string read FHelp;
    property Active: Boolean read FActive write FActive;
    property Order: Integer read FOrder write FOrder;
    property OnlyIfCut: Boolean read FOnlyIfCut write FOnlyIfCut;
    property GroupID: Integer read FGroupID;
    property Copied: Boolean read FCopied;
  end;

  TInternalPostProcess = class(TPostProcessBase)
  private
  protected
    FNeededAddons: TList;

    function FGetDependenciesMet: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;

    property NeededAddons: TList read FNeededAddons;
    property DependenciesMet: Boolean read FGetDependenciesMet;
  end;

  TExternalProcessThread = class(TPostProcessThreadBase)
  private
    FExe: string;
    FParams: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Exe, Params: string; Data: PPostProcessInformation; PostProcessor: TExternalPostProcess);
    destructor Destroy; override;
  end;

  TExternalPostProcess = class(TPostProcessBase)
  private
    FExe: string;
    FParams: string;
    FIdentifier: Integer;

    procedure FSetExe(Value: string);
  protected
  public
    constructor Create(Exe, Params: string; Active, OnlyIfCut: Boolean; Identifier, Order, GroupID: Integer);
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;

    property Exe: string read FExe write FSetExe;
    property Params: string read FParams write FParams;
    property Identifier: Integer read FIdentifier write FIdentifier;
  end;

implementation

uses
  AppData;

{ TProcessingEntry }

constructor TProcessingEntry.Create(Owner: TObject; ActiveThread: TPostProcessThreadBase;
  Data: TPostProcessInformation);
begin
  inherited Create;

  FOwner := Owner;
  FActiveThread := ActiveThread;
  FActivePostProcessIndex := -1;

  New(FData);

  FData.Filename := Data.Filename;
  FData.Station := Data.Station;
  FData.Artist := Data.Artist;
  FData.Title := Data.Title;
  FData.Album := Data.Album;
  FData.TrackNumber := Data.TrackNumber;
  FData.Filesize := Data.Filesize;
  FData.Length := Data.Length;
  FData.WasCut := Data.WasCut;
  FData.FullTitle := Data.FullTitle;
  FData.StreamTitle := Data.StreamTitle;
  FData.BitRate := Data.BitRate;
  FData.OutputFormat := Data.OutputFormat;

  FPostProcessList := TList<TPostProcessBase>.Create;
end;

destructor TProcessingEntry.Destroy;
begin
  FPostProcessList.Free;
  Dispose(FData);

  inherited;
end;

{ TProcessThreadBase }

constructor TPostProcessThreadBase.Create(Data: PPostProcessInformation; PostProcessor: TPostProcessBase);
begin
  inherited Create(True);

  FreeOnTerminate := True;
  FData := Data;
  FPostProcessor := PostProcessor;
  FResult := arFail;
  FOutput := '';
end;

destructor TPostProcessThreadBase.Destroy;
begin

  inherited;
end;

{ TExternalProcessThread }

constructor TExternalProcessThread.Create(Exe, Params: string; Data: PPostProcessInformation; PostProcessor: TExternalPostProcess);
begin
  inherited Create(Data, PostProcessor);

  FreeOnTerminate := True;
  FExe := Exe;
  FParams := Params;
  FData := Data;
  FPostProcessor := PostProcessor;
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
  EC: DWORD;
begin
  if Trim(FExe) <> '' then
  begin
    if FileExists(FExe) then
    begin
      SetLength(Arr, 9);

      Arr[0].C := 'f';
      if PostProcessor.GroupID = 0 then
        Arr[0].Replace := FData.WorkFilename
      else
        Arr[0].Replace := FData.Filename;
      Arr[1].C := 'a';
      Arr[1].Replace := FData.Artist;
      Arr[2].C := 't';
      Arr[2].Replace := FData.Title;
      Arr[3].C := 'l';
      Arr[3].Replace := FData.Album;
      Arr[4].C := 'u';
      Arr[4].Replace := FData.StreamTitle;
      Arr[5].C := 's';
      Arr[5].Replace := FData.Station;
      Arr[6].C := 'n';
      Arr[6].Replace := IntToStr(FData.TrackNumber);
      Arr[7].C := 'd';
      Arr[7].Replace := FormatDateTime('dd.mm.yy', Now);
      Arr[8].C := 'i';
      Arr[8].Replace := FormatDateTime('hh.nn.ss', Now);

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
      Res := RunProcess(CmdLine, ExtractFilePath(FExe), 120000, Output, EC, @Terminated);
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

{ TExternalPostProcess }

procedure TExternalPostProcess.Assign(Source: TPostProcessBase);
begin
  FExe := TExternalPostProcess(Source).FExe;
  FParams := TExternalPostProcess(Source).FParams;
end;

function TExternalPostProcess.Copy: TPostProcessBase;
begin
  Result := TExternalPostProcess.Create(FExe, FParams, FActive, FOnlyIfCut, FIdentifier, FOrder, FGroupID);
end;

constructor TExternalPostProcess.Create(Exe, Params: string; Active, OnlyIfCut: Boolean; Identifier, Order, GroupID: Integer);
begin
  inherited Create;

  FActive := Active;
  FOnlyIfCut := OnlyIfCut;
  FExe := Exe;
  FParams := Params;
  FIdentifier := Identifier;
  FOrder := Order;
  FGroupID := GroupID;

  FName := ExtractFileName(FExe);
end;

procedure TExternalPostProcess.FSetExe(Value: string);
begin
  FExe := Value;
  FName := ExtractFileName(FExe);
end;

function TExternalPostProcess.ProcessFile(
  Data: PPostProcessInformation): TPostProcessThreadBase;
var
  Thread: TExternalProcessThread;
begin
  Thread := TExternalProcessThread.Create(FExe, FParams, Data, Self);
  Result := Thread;
end;

{ TPostProcessBase }

procedure TPostProcessBase.Assign(Source: TPostProcessBase);
begin

end;

function TPostProcessBase.CanProcess(Data: PPostProcessInformation): Boolean;
begin
  Result := True;
end;

function TPostProcessBase.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
begin
  Result := False;
end;

constructor TPostProcessBase.Create;
begin
  inherited;

end;

procedure TPostProcessBase.Initialize;
begin

end;

procedure TPostProcessBase.Save;
begin

end;

function TPostProcessBase.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

{ TInternalPostProcess }

constructor TInternalPostProcess.Create;
begin
  inherited;

  FNeededAddons := TList.Create;
end;

destructor TInternalPostProcess.Destroy;
begin
  FNeededAddons.Free;

  inherited;
end;

function TInternalPostProcess.FGetDependenciesMet: Boolean;
var
  i: Integer;
  Addon: TAddonBase;
begin
  for i := 0 to FNeededAddons.Count - 1 do
  begin
    Addon := AppGlobals.AddonManager.Find(FNeededAddons[i]);
    if (Addon = nil) or (not Addon.FilesExtracted) then
      Exit(False);
  end;
  Exit(True);
end;

procedure TInternalPostProcess.Initialize;
begin
  inherited;

end;

end.
