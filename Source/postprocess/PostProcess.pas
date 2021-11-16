{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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
  AddonBase,
  Classes,
  ExtendedStream,
  Functions,
  Generics.Collections,
  LanguageObjects,
  Logging,
  SWFunctions,
  SysUtils,
  Windows;

type
  TPostProcessBase = class;
  TExternalPostProcess = class;
  TPostProcessThreadBase = class;
  TFilenameArray = array of string;

  TActResults = (arWin, arTimeout, arFail, arImpossible);

  // Nicht Reihenfolge ändern. Wird im Binärstream genutzt!
  TPostProcessTypes = (ptConvert, ptMP4Box, ptSetTags, ptSoX, ptExternal);

  TReadWrite = procedure(Name, Value: PChar);

  TInitialize = procedure(L: PChar; RF, WF: TReadWrite); stdcall;
  TConfigure = function(Handle: Cardinal; ShowMessages: Boolean): Boolean; stdcall;

  TPostProcessInformation = record
    Filename, FilenameConverted, WorkFilename, ReEncodedFilename, Station, Artist, Album, Title, Genre: string;
    TrackNumber: Cardinal;
    Filesize: UInt64;
    Length: UInt64;
    WasCut: Boolean;
    FullTitle: Boolean;
    StreamTitle: string;
    Bitrate: Cardinal;
    VBR: Boolean;
    EncoderSettings: TObject;
    ServerTitleHash: Cardinal;
    ServerArtistHash: Cardinal;
    RecordBecauseArtist: Boolean;
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
    constructor Create(Owner: TObject; ActiveThread: TPostProcessThreadBase; Data: TPostProcessInformation);
    destructor Destroy; override;

    property Owner: TObject read FOwner write FOwner;
    property NeedsWave: Boolean read FNeedsWave write FNeedsWave;
    property ActiveThread: TPostProcessThreadBase read FActiveThread write FActiveThread;
    property Data: PPostProcessInformation read FData;
    property PostProcessList: TList<TPostProcessBase> read FPostProcessList;
    property ActivePostProcessIndex: Integer read FActivePostProcessIndex write FActivePostProcessIndex;
  end;

  TProcessingList = class(TList<TProcessingEntry>)
  public
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
    property Terminated;
  end;

  TPostProcessBase = class
  private
  protected
    FHidden: Boolean;
    FCanConfigure: Boolean;
    FActive: Boolean;
    FOrder: Integer;
    FOnlyIfCut: Boolean;
    FGroupID: Integer;
    FPostProcessType: TPostProcessTypes;
    FIsNew: Boolean;

    function FGetName: string; virtual;
    function FGetHelp: string; virtual;

    function FGetNeedsWave: Boolean; virtual;
    function FGetHash: Cardinal; virtual;
  public
    constructor Create;

    function CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean; virtual;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; virtual; abstract;
    function Copy: TPostProcessBase; virtual; abstract;
    procedure Assign(Source: TPostProcessBase); virtual;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; virtual;
    procedure Load(Stream: TExtendedStream; Version: Integer); virtual;
    procedure Save(Stream: TExtendedStream); overload; virtual;
    function ShowInitMessage(Handle: THandle): Boolean; virtual;

    property NeedsWave: Boolean read FGetNeedsWave;
    property Hidden: Boolean read FHidden;
    property CanConfigure: Boolean read FCanConfigure;
    property Name: string read FGetName;
    property Help: string read FGetHelp;
    property Active: Boolean read FActive write FActive;
    property Order: Integer read FOrder write FOrder;
    property OnlyIfCut: Boolean read FOnlyIfCut write FOnlyIfCut;
    property GroupID: Integer read FGroupID;
    property PostProcessType: TPostProcessTypes read FPostProcessType;
    property IsNew: Boolean read FIsNew write FIsNew;
    property Hash: Cardinal read FGetHash;
  end;

  TInternalPostProcess = class(TPostProcessBase)
  private
  protected
    FNeededAddons: Classes.TList;

    function FGetDependenciesMet: Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property NeededAddons: Classes.TList read FNeededAddons;
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
    FName: string;
    FExe: string;
    FParams: string;
    FIdentifier: Integer;

    procedure FSetExe(Value: string);
  protected
    function FGetHash: Cardinal; override;
    function FGetName: string; override;
  public
    constructor Create; overload;
    constructor Create(Exe, Params: string; Active, OnlyIfCut: Boolean; Identifier, Order, GroupID: Integer); overload;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;
    procedure Load(Stream: TExtendedStream; Version: Integer); override;
    procedure Save(Stream: TExtendedStream); overload; override;

    property Exe: string read FExe write FSetExe;
    property Params: string read FParams write FParams;
    property Identifier: Integer read FIdentifier write FIdentifier;
  end;

implementation

uses
  AppData,
  DataManager;

{ TProcessingEntry }

constructor TProcessingEntry.Create(Owner: TObject; ActiveThread: TPostProcessThreadBase; Data: TPostProcessInformation);
begin
  inherited Create;

  FOwner := Owner;
  FActiveThread := ActiveThread;
  FActivePostProcessIndex := -1;

  New(FData);

  FData.Filename := Data.Filename;
  FData.FilenameConverted := Data.FilenameConverted;
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
  FData.Bitrate := Data.Bitrate;
  FData.EncoderSettings := TEncoderSettings(Data.EncoderSettings);
  FData.VBR := Data.VBR;
  FData.ServerTitleHash := Data.ServerTitleHash;
  FData.ServerArtistHash := Data.ServerArtistHash;
  FData.RecordBecauseArtist := Data.RecordBecauseArtist;
  FData.Genre := Data.Genre;

  FPostProcessList := TList<TPostProcessBase>.Create;
end;

destructor TProcessingEntry.Destroy;
var
  i: Integer;
begin
  TEncoderSettings(FData.EncoderSettings).Free;
  Dispose(FData);

  for i := 0 to FPostProcessList.Count - 1 do
    FPostProcessList[i].Free;
  FPostProcessList.Free;

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
  Res: TRunProcessResults;
  CmdLine, Replaced: string;
  Output: AnsiString;
  Arr: TPatternReplaceArray;
  EC: DWORD;
begin
  if Trim(FExe) <> '' then
    if FileExists(FExe) then
    begin
      SetLength(Arr, 14);
      Arr[0].C := 'artist';
      Arr[0].Replace := FData.Artist;
      Arr[1].C := 'title';
      Arr[1].Replace := FData.Title;
      Arr[2].C := 'album';
      Arr[2].Replace := FData.Album;
      Arr[3].C := 'streamname';
      Arr[3].Replace := Trim(FData.Station);
      Arr[4].C := 'streamtitle';
      Arr[4].Replace := Trim(FData.StreamTitle);
      Arr[5].C := 'day';
      Arr[5].Replace := FormatDateTime('dd', Now);
      Arr[6].C := 'month';
      Arr[6].Replace := FormatDateTime('mm', Now);
      Arr[7].C := 'year';
      Arr[7].Replace := FormatDateTime('yy', Now);
      Arr[8].C := 'hour';
      Arr[8].Replace := FormatDateTime('hh', Now);
      Arr[9].C := 'minute';
      Arr[9].Replace := FormatDateTime('nn', Now);
      Arr[10].C := 'second';
      Arr[10].Replace := FormatDateTime('ss', Now);
      Arr[11].C := 'number';
      Arr[11].Replace := IntToStr(FData.TrackNumber);
      Arr[12].C := 'filename';
      if PostProcessor.GroupID = 0 then
        Arr[12].Replace := FData.WorkFilename
      else
        Arr[12].Replace := FData.Filename;
      Arr[13].C := 'genre';
      Arr[13].Replace := FData.Genre;

      Replaced := PatternReplaceNew(FParams, Arr);
      if Trim(Replaced) <> '' then
      begin
        if LowerCase(ExtractFileExt(FExe)) = '.bat' then
          Replaced := StringReplace(Replaced, '&', '^&', [rfReplaceAll])// & ist für Batch nen Sonderzeichen. Also escapen.
        ;
        CmdLine := '"' + FExe + '" ' + Replaced;
      end else
        CmdLine := FExe;
      Res := RunProcess(CmdLine, ExtractFilePath(FExe), 120000, Output, EC, @Terminated, False);
      FData.Filesize := Functions.GetFileSize(FData.Filename);
      FOutput := Output;
      case Res of
        rpWin:
          FResult := arWin;
        rpFail, rpTerminated:
          FResult := arFail;
        rpTimeout:
          FResult := arTimeout;
      end;
    end;
end;

{ TExternalPostProcess }

procedure TExternalPostProcess.Assign(Source: TPostProcessBase);
begin
  FIdentifier := TExternalPostProcess(Source).FIdentifier;
  FExe := TExternalPostProcess(Source).FExe;
  FParams := TExternalPostProcess(Source).FParams;
end;

function TExternalPostProcess.Copy: TPostProcessBase;
begin
  Result := TExternalPostProcess.Create(FExe, FParams, FActive, FOnlyIfCut, FIdentifier, FOrder, FGroupID);
  Result.IsNew := Self.IsNew;
end;

constructor TExternalPostProcess.Create;
begin
  inherited;

  FPostProcessType := ptExternal;
end;

constructor TExternalPostProcess.Create(Exe, Params: string; Active, OnlyIfCut: Boolean; Identifier, Order, GroupID: Integer);
begin
  inherited Create;

  FActive := Active;
  FOrder := Order;
  FOnlyIfCut := OnlyIfCut;

  FExe := Exe;
  FParams := Params;
  FIdentifier := Identifier;
  FGroupID := GroupID;

  FPostProcessType := ptExternal;

  FName := ExtractFileName(FExe);
end;

function TExternalPostProcess.FGetHash: Cardinal;
begin
  Result := inherited + HashString(FExe + FParams);
end;

function TExternalPostProcess.FGetName: string;
begin
  Result := FName;
end;

procedure TExternalPostProcess.FSetExe(Value: string);
begin
  FExe := Value;
  FName := ExtractFileName(FExe);
end;

procedure TExternalPostProcess.Load(Stream: TExtendedStream; Version: Integer);
begin
  inherited;

  Stream.Read(FIdentifier);
  Stream.Read(FExe);
  Stream.Read(FParams);
  Stream.Read(FGroupID);
  Stream.Read(FName);

  if Version < 64 then
    FParams := ConvertPattern(FParams);
end;

function TExternalPostProcess.ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase;
var
  Thread: TExternalProcessThread;
begin
  Thread := TExternalProcessThread.Create(FExe, FParams, Data, Self);
  Result := Thread;
end;

procedure TExternalPostProcess.Save(Stream: TExtendedStream);
begin
  inherited;

  Stream.Write(FIdentifier);
  Stream.Write(FExe);
  Stream.Write(FParams);
  Stream.Write(FGroupID);
  Stream.Write(FName);
end;

{ TPostProcessBase }

procedure TPostProcessBase.Assign(Source: TPostProcessBase);
begin

end;

function TPostProcessBase.CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean;
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

function TPostProcessBase.FGetHash: Cardinal;
begin
  Result := HashString(BoolToStr(FActive) + IntToStr(FOrder) + BoolToStr(FOnlyIfCut));
end;

function TPostProcessBase.FGetHelp: string;
begin
  Result := 'Undefined';
end;

function TPostProcessBase.FGetName: string;
begin
  Result := 'Undefined';
end;

function TPostProcessBase.FGetNeedsWave: Boolean;
begin
  Result := GroupID = 0;
end;

procedure TPostProcessBase.Load(Stream: TExtendedStream; Version: Integer);
begin
  Stream.Read(FActive);
  Stream.Read(FOrder);
  Stream.Read(FOnlyIfCut);
end;

procedure TPostProcessBase.Save(Stream: TExtendedStream);
begin
  Stream.Write(FActive);
  Stream.Write(FOrder);
  Stream.Write(FOnlyIfCut);
end;

function TPostProcessBase.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

{ TInternalPostProcess }

constructor TInternalPostProcess.Create;
begin
  inherited;

  FNeededAddons := Classes.TList.Create;
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

end.
