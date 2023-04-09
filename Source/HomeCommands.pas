{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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

unit HomeCommands;

interface

uses
  AudioFunctions,
  Classes,
  Commands,
  StreamHelper,
  Functions,
  SysUtils,
  TypeDefs,
  ZStream;

type
  TSendClientStatTypes = (csSave, csAutoSave);
  TSyncWishlistTypes = (swSync, swAdd, swRemove);

  TSyncWishlistRecord = record
    Hash: Cardinal;
    IsArtist: Boolean;

    constructor Create(Hash: Cardinal; IsArtist: Boolean);
  end;
  TSyncWishlistRecordArray = array of TSyncWishlistRecord;

  TConvertManualToAutomatic = record
  public
    Title: string;
    Hash: Cardinal;
  end;
  TConvertManualToAutomaticArray = array of TConvertManualToAutomatic;

  TCommandHandshake = class(TCommand)
  private
    FID: Cardinal;
    FProtoVersion: Cardinal;
    FVersionMajor, FVersionMinor, FVersionRevision, FVersionBuild: Cardinal;
    FBuild: Cardinal;
    FLanguage: string;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; override;

    property ID: Cardinal read FID write FID;
    property ProtoVersion: Cardinal read FProtoVersion write FProtoVersion;
    property VersionMajor: Cardinal read FVersionMajor write FVersionMajor;
    property VersionMinor: Cardinal read FVersionMinor write FVersionMinor;
    property VersionRevision: Cardinal read FVersionRevision write FVersionRevision;
    property VersionBuild: Cardinal read FVersionBuild write FVersionBuild;
    property Build: Cardinal read FBuild write FBuild;
    property Language: string read FLanguage write FLanguage;
  end;

  TCommandHandshakeResponse = class(TCommand)
  private
    FSuccess: Boolean;
    FServerTime: Cardinal;
    FCommunicationTimeout: Cardinal;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property Success: Boolean read FSuccess;
    property ServerTime: Cardinal read FServerTime;
    property CommunicationTimeout: Cardinal read FCommunicationTimeout;
  end;

  TCommandUpdateStats = class(TCommand)
  public
    constructor Create; override;

    function Process(ToStream: TMemoryStream): Boolean; override;
  end;

  TCommandLogIn = class(TCommand)
  private
    FUser: string;
    FPass: string;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(User, Pass: string); overload;

    property User: string read FUser write FUser;
    property Pass: string read FPass write FPass;
  end;

  TCommandLogInResponse = class(TCommand)
  private
    FSuccess, FIsAdmin: Boolean;
  public
    constructor Create; overload; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property Success: Boolean read FSuccess;
    property IsAdmin: Boolean read FIsAdmin;
  end;

  TCommandLogOut = class(TCommand)
  public
    constructor Create; override;
  end;

  TCommandLogOutResponse = class(TCommand)
  public
    constructor Create; override;
  end;

  TCommandNetworkTitleChangedResponse = class(TCommand)
  private
    FStreamID: Cardinal;
    FStreamName: string;
    FStreamTitle: string;
    FStreamParsedTitle: string;
    FCurrentURL: string;
    FBitrate: Cardinal;
    FFormat: TAudioTypes;
    FRegExes: TStringList;
    FServerHash: Cardinal;
    FServerArtistHash: Cardinal;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property StreamID: Cardinal read FStreamID;
    property StreamName: string read FStreamName;
    property StreamTitle: string read FStreamTitle;
    property StreamParsedTitle: string read FStreamParsedTitle;
    property CurrentURL: string read FCurrentURL;
    property Bitrate: Cardinal read FBitrate;
    property Format: TAudioTypes read FFormat;
    property RegExes: TStringList read FRegExes;
    property ServerHash: Cardinal read FServerHash;
    property ServerArtistHash: Cardinal read FServerArtistHash;
  end;

  TCommandGetServerData = class(TCommand)
  public
    constructor Create; override;
  end;

  TCommandGetServerDataResponse = class(TCommand)
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;
  end;

  TCommandServerInfoResponse = class(TCommand)
  private
    FClientCount: Cardinal;
    FRecordingCount: Cardinal;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property ClientCount: Cardinal read FClientCount;
    property RecordingCount: Cardinal read FRecordingCount;
  end;

  TCommandMessageResponse = class(TCommand)
  private
    FMessageID: Cardinal;
    FMessageMsg: string;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property MessageID: Cardinal read FMessageID;
    property MessageMsg: string read FMessageMsg;
  end;

  TCommandSetSettings = class(TCommand)
  private
    FTitleNotifications: Boolean;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(TitleNotifications: Boolean); overload;

    property TitleNotifications: Boolean read FTitleNotifications write FTitleNotifications;
  end;

  TCommandClientStats = class(TCommand)
  private
    FStatType: TSendClientStatTypes;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(StatType: TSendClientStatTypes); overload;

    property StatType: TSendClientStatTypes read FStatType write FStatType;
  end;

  TCommandSubmitStream = class(TCommand)
  private
    FURL: string;
    FStreamName: string;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(URL, StreamName: string); overload;

    property URL: string read FURL write FURL;
    property StreamName: string read FStreamName write FStreamName;
  end;

  TCommandSetStreamData = class(TCommand)
  private
    FStreamID: Cardinal;
    FRating: Byte;
    FHasRecordingOkay: Boolean;
    FRecordingOkay: Boolean;
    FTitleRegEx: string;
    FHasIgnoreTitles: Boolean;
    FIgnoreTitles: string;
    FSetRegExps: Boolean;
    FRegExps: TStringArray;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; override;

    property StreamID: Cardinal read FStreamID write FStreamID;
    property Rating: Byte read FRating write FRating;
    property HasRecordingOkay: Boolean read FHasRecordingOkay write FHasRecordingOkay;
    property RecordingOkay: Boolean read FRecordingOkay write FRecordingOkay;
    property TitleRegEx: string read FTitleRegEx write FTitleRegEx;
    property HasIgnoreTitles: Boolean read FHasIgnoreTitles write FHasIgnoreTitles;
    property IgnoreTitles: string read FIgnoreTitles write FIgnoreTitles;
    property SetRegExps: Boolean read FSetRegExps write FSetRegExps;
    property RegExps: TStringArray read FRegExps write FRegExps;
  end;

  TCommandTitleChanged = class(TCommand)
  private
    FStreamID: Cardinal;
    FStreamName: string;
    FStreamTitle: string;
    FCurrentURL: string;
    FURL: string;
    FFormat: TAudioTypes;
    FKbps: Cardinal;
    FURLs: string;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(StreamID: Cardinal; StreamName, StreamTitle, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal; URLs: string); overload;
  end;

  TCommandGetMonitorStreams = class(TCommand)
  private
    FCount: Cardinal;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(Count: Cardinal); overload;

    property Count: Cardinal read FCount write FCount;
  end;

  TCommandGetMonitorStreamsResponse = class(TCommand)
  private
    FStreamIDs: TIntArray;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property StreamIDs: TIntArray read FStreamIDs;
  end;

  TCommandSyncWishlist = class(TCommand)
  private
    FSyncType: TSyncWishlistTypes;
    FHashes: TSyncWishlistRecordArray;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(SyncType: TSyncWishlistTypes; Hashes: TSyncWishlistRecordArray); overload;

    property Hashes: TSyncWishlistRecordArray read FHashes write FHashes;
  end;

  TCommandSearchCharts = class(TCommand)
  private
    FTop: Boolean;
    FTerm: string;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(Top: Boolean; Term: string); overload;
  end;

  TCommandSearchChartsResponse = class(TCommand)
  private
    FSuccess: Boolean;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property Success: Boolean read FSuccess;
  end;

  TCommandStreamAnalyzationData = class(TCommand)
  private
    FStreamID: Cardinal;
    FData: TMemoryStream;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(StreamID: Cardinal; Data: TMemoryStream); overload;
    destructor Destroy; override;
  end;

  TCommandConvertManualToAutomatic = class(TCommand)
  private
    FTitles: TStringArray;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(Titles: TStringArray); overload;
    destructor Destroy; override;
  end;

  TCommandConvertManualToAutomaticResponse = class(TCommand)
  private
    FFoundTitles: TConvertManualToAutomaticArray;
    FNotFoundTitles: TStringArray;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property FoundTitles: TConvertManualToAutomaticArray read FFoundTitles;
    property NotFoundTitles: TStringArray read FNotFoundTitles;
  end;

  TCommandGetStreamData = class(TCommand)
  private
    FStreamID: Cardinal;
  protected
    procedure DoGet(S: TMemoryStream); override;
  public
    constructor Create; overload; override;
    constructor Create(StreamID: Cardinal); overload;
  end;

  TCommandGetStreamDataResponse = class(TCommand)
  private
    FLastTitles: TStringArray;
    FOtherUserRegExps: TStringArray;
    FUserRegExps: TStringArray;
  public
    constructor Create; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TMemoryStream); override;

    property LastTitles: TStringArray read FLastTitles;
    property OtherUserRegExps: TStringArray read FOtherUserRegExps;
    property UserRegExps: TStringArray read FUserRegExps;
  end;

implementation

{ TCommandHandshake }

constructor TCommandHandshake.Create;
begin
  inherited;

  FVersion := 2;
  FCommandType := ctHandshake;
end;

procedure TCommandHandshake.DoGet(S: TMemoryStream);
begin
  S.Write(FID, False);
  S.Write(FProtoVersion, False);
  S.Write(FVersionMajor, False);
  S.Write(FVersionMinor, False);
  S.Write(FVersionRevision, False);
  S.Write(FVersionBuild, False);
  S.Write(FBuild, False);
  S.Write(FLanguage, False);
end;

{ TCommandHandshakeResponse }

constructor TCommandHandshakeResponse.Create;
begin
  inherited;

  FCommandType := ctHandshakeResponse;
end;


procedure TCommandHandshakeResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin
  inherited;

  Stream.Read(FSuccess);
  Stream.Read(FServerTime, False);
  Stream.Read(FCommunicationTimeout, False);
end;

{ TCommandGetServerData }

constructor TCommandGetServerData.Create;
begin
  inherited;

  FVersion := 3;
  FCommandType := ctGetServerData;
end;

{ TCommandGetServerDataResponse }

constructor TCommandGetServerDataResponse.Create;
begin
  inherited;

  FCommandType := ctGetServerDataResponse;
end;

procedure TCommandGetServerDataResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin
  LoadStream(Stream);
end;

{ TCommandLogIn }

constructor TCommandLogIn.Create(User, Pass: string);
begin
  Create;

  FUser := User;
  FPass := Pass;
end;

procedure TCommandLogIn.DoGet(S: TMemoryStream);
begin
  S.Write(FUser, False);
  S.Write(FPass, False);
end;

constructor TCommandLogIn.Create;
begin
  inherited;

  FCommandType := ctLogin;
end;

{ TCommandLogInResponse }

procedure TCommandLogInResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin
  inherited;

  Stream.Read(FSuccess);
  Stream.Read(FIsAdmin);
end;

constructor TCommandLogInResponse.Create;
begin

end;

{ TCommandNetworkTitleChangedResponse }

constructor TCommandNetworkTitleChangedResponse.Create;
begin
  inherited;

  FCommandType := ctNetworkTitleChangedResponse;
  FRegExes := TStringList.Create;
end;

destructor TCommandNetworkTitleChangedResponse.Destroy;
begin
  FRegExes.Free;

  inherited;
end;

procedure TCommandNetworkTitleChangedResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
var
  B: Byte;
  i: Integer;
  C: Cardinal;
  Tmp: string;
begin
  inherited;

  Stream.Read(FStreamID, False);
  Stream.Read(FStreamName, False);
  Stream.Read(FStreamTitle, False);
  Stream.Read(FStreamParsedTitle, False);
  Stream.Read(FCurrentURL, False);
  Stream.Read(FBitrate, False);
  Stream.Read(B);
  FFormat := TAudioTypes(B);

  Stream.Read(C, False);
  for i := 0 to C - 1 do
  begin
    Stream.Read(Tmp, False);
    FRegExes.Add(Tmp);
  end;

  Stream.Read(FServerHash, False);
  Stream.Read(FServerArtistHash, False);
end;

{ TCommandUpdateStats }

constructor TCommandUpdateStats.Create;
begin
  inherited;

  FCommandType := ctUpdateStats;
  FStream := TMemoryStream.Create;
end;

function TCommandUpdateStats.Process(ToStream: TMemoryStream): Boolean;
begin
  if FStream.Position = FStream.Size then
    Exit(False);
  ToStream.CopyFrom(FStream, FStream.Size);
  Result := True;
end;

{ TCommandServerInfoResponse }

constructor TCommandServerInfoResponse.Create;
begin
  inherited;

  FCommandType := ctServerInfoResponse;
end;

procedure TCommandServerInfoResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin
  inherited;

  Stream.Read(FClientCount, False);
  Stream.Read(FRecordingCount, False);
end;

{ TCommandMessageResponse }

constructor TCommandMessageResponse.Create;
begin
  inherited;

  FCommandType := ctMessageResponse;
end;

procedure TCommandMessageResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin
  inherited;

  Stream.Read(FMessageID, False);
  Stream.Read(FMessageMsg, False);
end;

{ TCommandSetSettings }

constructor TCommandSetSettings.Create(TitleNotifications: Boolean);
begin
  Create;

  FTitleNotifications := TitleNotifications;
end;

procedure TCommandSetSettings.DoGet(S: TMemoryStream);
begin
  S.Write(FTitleNotifications);
end;

constructor TCommandSetSettings.Create;
begin
  inherited;

  FCommandType := ctSetSettings;
end;

{ TCommandLogOut }

constructor TCommandLogOut.Create;
begin
  inherited;

  FCommandType := ctLogOut;
end;

{ TCommandLogOutResponse }

constructor TCommandLogOutResponse.Create;
begin
  inherited;

  FCommandType := ctLogOutResponse;
end;

{ TCommandClientStats }

constructor TCommandClientStats.Create(StatType: TSendClientStatTypes);
begin
  Create;

  FStatType := StatType;
end;

constructor TCommandClientStats.Create;
begin
  inherited;

  FCommandType := ctClientStats;
end;

procedure TCommandClientStats.DoGet(S: TMemoryStream);
begin
  S.Write(Byte(FStatType));
end;

{ TCommandSubmitStream }

constructor TCommandSubmitStream.Create(URL, StreamName: string);
begin
  Create;

  FURL := URL;
  FStreamName := StreamName;
end;

constructor TCommandSubmitStream.Create;
begin
  inherited;

  FVersion := 2;
  FCommandType := ctSubmitStream;
end;

procedure TCommandSubmitStream.DoGet(S: TMemoryStream);
begin
  S.Write(FURL, False);
  S.Write(FStreamName, False);
end;

{ TCommandSetStreamData }

constructor TCommandSetStreamData.Create;
begin
  inherited;

  FVersion := 2;
  FCommandType := ctSetStreamData;
end;

procedure TCommandSetStreamData.DoGet(S: TMemoryStream);
var
  i: Integer;
begin
  S.Write(FStreamID, False);
  S.Write(FRating);
  S.Write(FHasRecordingOkay);
  S.Write(FRecordingOkay);
  S.Write(FTitleRegEx, False);
  S.Write(FHasIgnoreTitles);
  S.Write(FIgnoreTitles, False);
  S.Write(FSetRegExps);
  S.Write(Cardinal(Length(FRegExps)), False);
  for i := 0 to High(FRegExps) do
    S.Write(FRegExps[i], False);
end;

{ TCommandTitleChanged }

constructor TCommandTitleChanged.Create;
begin
  inherited;

  FCommandType := ctTitleChanged;
end;

constructor TCommandTitleChanged.Create(StreamID: Cardinal; StreamName, StreamTitle, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal; URLs: string);
begin
  Create;

  FStreamID := StreamID;
  FStreamName := StreamName;
  FStreamTitle := StreamTitle;
  FCurrentURL := CurrentURL;
  FURL := URL;
  FFormat := Format;
  FKbps := Kbps;
  FURLs := URLs;
end;

procedure TCommandTitleChanged.DoGet(S: TMemoryStream);
begin
  inherited;

  S.Write(FStreamID, False);
  S.Write(FStreamName, False);
  S.Write(FStreamTitle, False);
  S.Write(FCurrentURL, False);
  S.Write(FURL, False);
  S.Write(Byte(FFormat));
  S.Write(FKbps, False);
  S.Write(FURLs, False);
end;

{ TCommandGetMonitorStreamsResponse }

constructor TCommandGetMonitorStreamsResponse.Create;
begin
  inherited;

  FCommandType := ctGetMonitorStreamsResponse;
end;

procedure TCommandGetMonitorStreamsResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
var
  Count: Cardinal;
  i: Integer;
begin
  inherited;

  Stream.Read(Count, False);
  SetLength(FStreamIDs, Count);
  for i := 0 to High(FStreamIDs) do
    Stream.Read(FStreamIDs[i], False);
end;

{ TCommandGetMonitorStreams }

constructor TCommandGetMonitorStreams.Create(Count: Cardinal);
begin
  Create;

  FCount := Count;
end;

constructor TCommandGetMonitorStreams.Create;
begin
  inherited;

  FCommandType := ctGetMonitorStreams;
end;

procedure TCommandGetMonitorStreams.DoGet(S: TMemoryStream);
begin
  S.Write(FCount, False);
end;

{ TCommandSyncWishlist }

constructor TCommandSyncWishlist.Create;
begin
  inherited;

  FCommandType := ctSyncWishlist;
end;

constructor TCommandSyncWishlist.Create(SyncType: TSyncWishlistTypes; Hashes: TSyncWishlistRecordArray);
begin
  Create;

  FSyncType := SyncType;
  FHashes := Hashes;
end;

procedure TCommandSyncWishlist.DoGet(S: TMemoryStream);
var
  i: Integer;
begin
  inherited;

  S.Write(Byte(FSyncType));
  S.Write(Cardinal(Length(FHashes)), False);
  for i := 0 to High(FHashes) do
  begin
    S.Write(FHashes[i].Hash, False);
    S.Write(FHashes[i].IsArtist);
  end;
end;

{ TCommandSearchCharts }

constructor TCommandSearchCharts.Create;
begin
  inherited;

  FCommandType := ctSearchCharts;
end;

constructor TCommandSearchCharts.Create(Top: Boolean; Term: string);
begin
  Create;

  FTop := Top;
  FTerm := Term;
end;

procedure TCommandSearchCharts.DoGet(S: TMemoryStream);
begin
  inherited;

  S.Write(FTop);
  S.Write(FTerm, False);
end;

{ TCommandSearchChartsResponse }

constructor TCommandSearchChartsResponse.Create;
begin
  inherited;

  FCommandType := ctSearchChartsResponse;
end;

procedure TCommandSearchChartsResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
begin
  LoadStream(Stream);

  TMemoryStream(FStream).Read(FSuccess);
end;

{ TSyncWishlistRecord }

constructor TSyncWishlistRecord.Create(Hash: Cardinal; IsArtist: Boolean);
begin
  Self.Hash := Hash;
  Self.IsArtist := IsArtist;
end;

{ TCommandStreamAnalyzationData }

constructor TCommandStreamAnalyzationData.Create;
begin
  inherited;

  FData := TMemoryStream.Create;
  FCommandType := ctStreamAnalyzationData;
end;

constructor TCommandStreamAnalyzationData.Create(StreamID: Cardinal; Data: TMemoryStream);
begin
  Create;

  FStreamID := StreamID;
  Data.Seek(0, soFromBeginning);
  FData.CopyFrom(Data, Data.Size);
end;

destructor TCommandStreamAnalyzationData.Destroy;
begin
  FData.Free;

  inherited;
end;

procedure TCommandStreamAnalyzationData.DoGet(S: TMemoryStream);
var
  CompressedData: TMemoryStream;
begin
  inherited;

  S.Write(FStreamID, False);

  FData.Seek(0, soFromBeginning);
  CompressedData := TMemoryStream.Create;
  TFunctions.CompressStream(FData, CompressedData, clDefault);
  FData.Free;
  FData := CompressedData;

  FData.Seek(0, soFromBeginning);
  S.CopyFrom(FData, FData.Size);
end;

{ TCommandConvertManualToAutomatic }

constructor TCommandConvertManualToAutomatic.Create;
begin
  inherited;

  FCommandType := ctConvertManualToAutomatic;
end;

constructor TCommandConvertManualToAutomatic.Create(Titles: TStringArray);
begin
  Create;

  FTitles := Titles;
end;

destructor TCommandConvertManualToAutomatic.Destroy;
begin

  inherited;
end;

procedure TCommandConvertManualToAutomatic.DoGet(S: TMemoryStream);
var
  i: Integer;
begin
  inherited;

  S.Write(Cardinal(Length(FTitles)), False);
  for i := 0 to High(FTitles) do
    S.Write(FTitles[i], False);
end;

{ TCommandConvertManualToAutomaticResponse }

constructor TCommandConvertManualToAutomaticResponse.Create;
begin
  inherited;

  FCommandType := ctConvertManualToAutomaticResponse;
end;

procedure TCommandConvertManualToAutomaticResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
var
  Count: Cardinal;
  i: Integer;
begin
  inherited;

  Stream.Read(Count, False);
  SetLength(FFoundTitles, Count);
  for i := 0 to High(FFoundTitles) do
  begin
    Stream.Read(FFoundTitles[i].Title, False);
    Stream.Read(FFoundTitles[i].Hash, False);
  end;

  Stream.Read(Count, False);
  SetLength(FNotFoundTitles, Count);
  for i := 0 to High(FNotFoundTitles) do
    Stream.Read(FNotFoundTitles[i], False);
end;

{ TCommandGetStreamData }

constructor TCommandGetStreamData.Create;
begin
  inherited;

  FCommandType := ctGetStreamData;
end;

constructor TCommandGetStreamData.Create(StreamID: Cardinal);
begin
  Create;

  FStreamID := StreamID;
end;

procedure TCommandGetStreamData.DoGet(S: TMemoryStream);
begin
  inherited;

  S.Write(FStreamID, False);
end;

{ TCommandGetStreamDataResponse }

constructor TCommandGetStreamDataResponse.Create;
begin
  inherited;

  FCommandType := ctGetStreamDataResponse;
end;

procedure TCommandGetStreamDataResponse.Load(CommandHeader: TCommandHeader; Stream: TMemoryStream);
var
  Count: Cardinal;
  i: Integer;
begin
  inherited;

  Stream.Read(Count, False);
  SetLength(FLastTitles, Count);
  for i := 0 to High(FLastTitles) do
    Stream.Read(FLastTitles[i], False);

  Stream.Read(Count, False);
  SetLength(FOtherUserRegExps, Count);
  for i := 0 to High(FOtherUserRegExps) do
    Stream.Read(FOtherUserRegExps[i], False);

  Stream.Read(Count, False);
  SetLength(FUserRegExps, Count);
  for i := 0 to High(FUserRegExps) do
    Stream.Read(FUserRegExps[i], False);
end;

end.
