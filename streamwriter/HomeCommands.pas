{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2014 Alexander Nottelmann

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
  Windows, SysUtils, Classes, ExtendedStream, Commands, AudioFunctions,
  TypeDefs, Generics.Collections, ZLib;

type
  TSendClientStatTypes = (csSave, csAutoSave);
  TSyncWishlistTypes = (swSync, swAdd, swRemove);

  TSyncWishlistRecord = record
    Hash: Cardinal;
    IsArtist: Boolean;

    constructor Create(Hash: Cardinal; IsArtist: Boolean);
  end;
  TSyncWishlistRecordArray = array of TSyncWishlistRecord;

  TWishlistUpgrade = class
  public
    Title: string;
    Hash: Cardinal;
  end;
  TWishlistUpgradeList = TList<TWishlistUpgrade>;

  TCommandHandshake = class(TCommand)
  private
    FID: Cardinal;
    FProtoVersion: Cardinal;
    FVersionMajor, FVersionMinor, FVersionRevision, FVersionBuild: Cardinal;
    FBuild: Cardinal;
    FLanguage: string;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create;

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
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Success: Boolean read FSuccess;
    property ServerTime: Cardinal read FServerTime;
  end;

  TCommandUpdateStats = class(TCommand)
  private
  protected
  public
    constructor Create;

    function Process(ToStream: TExtendedStream): Boolean; override;
  end;

  TCommandLogIn = class(TCommand)
  private
    FUser: string;
    FPass: string;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(User, Pass: string); overload;

    property User: string read FUser write FUser;
    property Pass: string read FPass write FPass;
  end;

  TCommandLogInResponse = class(TCommand)
  private
    FSuccess, FIsAdmin: Boolean;
  protected
  public
    constructor Create; overload;
    constructor Create(User, Pass: string); overload;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Success: Boolean read FSuccess;
    property IsAdmin: Boolean read FIsAdmin;
  end;

  TCommandLogOut = class(TCommand)
  private
  protected
  public
    constructor Create;
  end;

  TCommandLogOutResponse = class(TCommand)
  private
  protected
  public
    constructor Create;
  end;

  TCommandNetworkTitleChangedResponse = class(TCommand)
  private
    FStreamID: Cardinal;
    FStreamName: string;
    FStreamTitle: string;
    FCurrentURL: string;
    FBitrate: Cardinal;
    FFormat: TAudioTypes;
    FRegExes: TStringList;
    FServerHash: Cardinal;
    FServerArtistHash: Cardinal;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property StreamID: Cardinal read FStreamID;
    property StreamName: string read FStreamName;
    property StreamTitle: string read FStreamTitle;
    property CurrentURL: string read FCurrentURL;
    property Bitrate: Cardinal read FBitrate;
    property Format: TAudioTypes read FFormat;
    property RegExes: TStringList read FRegExes;
    property ServerHash: Cardinal read FServerHash;
    property ServerArtistHash: Cardinal read FServerArtistHash;
  end;

  TCommandGetServerData = class(TCommand)
  private
  protected
  public
    constructor Create;
  end;

  TCommandGetServerDataResponse = class(TCommand)
  private
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;
  end;

  TCommandServerInfoResponse = class(TCommand)
  private
    FClientCount: Cardinal;
    FRecordingCount: Cardinal;
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property ClientCount: Cardinal read FClientCount;
    property RecordingCount: Cardinal read FRecordingCount;
  end;

  TCommandMessageResponse = class(TCommand)
  private
    FMessageID: Cardinal;
    FMessageMsg: string;
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property MessageID: Cardinal read FMessageID;
    property MessageMsg: string read FMessageMsg;
  end;

  TCommandSetSettings = class(TCommand)
  private
    FTitleNotifications: Boolean;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(TitleNotifications: Boolean); overload;

    property TitleNotifications: Boolean read FTitleNotifications write FTitleNotifications;
  end;

  TCommandClientStats = class(TCommand)
  private
    FStatType: TSendClientStatTypes;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(StatType: TSendClientStatTypes); overload;

    property StatType: TSendClientStatTypes read FStatType write FStatType;
  end;

  TCommandSubmitStream = class(TCommand)
  private
    FURL: string;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(URL: string); overload;

    property URL: string read FURL write FURL;
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
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create;

    property StreamID: Cardinal read FStreamID write FStreamID;
    property Rating: Byte read FRating write FRating;
    property HasRecordingOkay: Boolean read FHasRecordingOkay write FHasRecordingOkay;
    property RecordingOkay: Boolean read FRecordingOkay write FRecordingOkay;
    property TitleRegEx: string read FTitleRegEx write FTitleRegEx;
    property HasIgnoreTitles: Boolean read FHasIgnoreTitles write FHasIgnoreTitles;
    property IgnoreTitles: string read FIgnoreTitles write FIgnoreTitles;
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
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(StreamID: Cardinal; StreamName, StreamTitle, CurrentURL, URL: string;
      Format: TAudioTypes; Kbps: Cardinal; URLs: string); overload;
  end;

  TCommandGetMonitorStreams = class(TCommand)
  private
    FCount: Cardinal;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(Count: Cardinal); overload;

    property Count: Cardinal read FCount write FCount;
  end;

  TCommandGetMonitorStreamsResponse = class(TCommand)
  private
    FStreamIDs: TIntArray;
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property StreamIDs: TIntArray read FStreamIDs;
  end;

  TCommandSyncWishlist = class(TCommand)
  private
    FSyncType: TSyncWishlistTypes;
    FHashes: TSyncWishlistRecordArray;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(SyncType: TSyncWishlistTypes; Hashes: TSyncWishlistRecordArray); overload;

    property Hashes: TSyncWishlistRecordArray read FHashes write FHashes;
  end;

  TCommandSearchCharts = class(TCommand)
  private
    FTop: Boolean;
    FTerm: string;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(Top: Boolean; Term: string); overload;
  end;

  TCommandSearchChartsResponse = class(TCommand)
  private
    FSuccess: Boolean;
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Success: Boolean read FSuccess;
  end;

  TCommandStreamAnalyzationData = class(TCommand)
  private
    FStreamID: Cardinal;
    FData: TExtendedStream;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(StreamID: Cardinal; Data: TExtendedStream); overload;
    destructor Destroy; override;
  end;

  TCommandGenerateAuthToken = class(TCommand)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCommandGenerateAuthTokenResponse = class(TCommand)
  private
    FToken: Cardinal;
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Token: Cardinal read FToken;
  end;

implementation


{ TCommandHandshake }

constructor TCommandHandshake.Create;
begin
  inherited;

  FCommandType := ctHandshake;
end;

procedure TCommandHandshake.DoGet(S: TExtendedStream);
begin
  S.Write(FID);
  S.Write(FProtoVersion);
  S.Write(FVersionMajor);
  S.Write(FVersionMinor);
  S.Write(FVersionRevision);
  S.Write(FVersionBuild);
  S.Write(FBuild);
  S.Write(FLanguage);
end;

{ TCommandHandshakeResponse }

constructor TCommandHandshakeResponse.Create;
begin
  inherited;

  FCommandType := ctHandshakeResponse;
end;


procedure TCommandHandshakeResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin
  inherited;

  Stream.Read(FSuccess);
  Stream.Read(FServerTime);
end;

{ TCommandGetServerData }

constructor TCommandGetServerData.Create;
begin
  inherited;

  FCommandType := ctGetServerData;
end;

{ TCommandGetServerDataResponse }

constructor TCommandGetServerDataResponse.Create;
begin
  inherited;

  FCommandType := ctGetServerDataResponse;
end;

procedure TCommandGetServerDataResponse.Load(
  CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
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

procedure TCommandLogIn.DoGet(S: TExtendedStream);
begin
  S.Write(FUser);
  S.Write(FPass);
end;

constructor TCommandLogIn.Create;
begin
  inherited;

  FCommandType := ctLogin;
end;

{ TCommandLogInResponse }

constructor TCommandLogInResponse.Create(User, Pass: string);
begin
  inherited Create;

  FCommandType := ctLoginResponse;
end;

procedure TCommandLogInResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
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

procedure TCommandNetworkTitleChangedResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
var
  B: Byte;
  i: Integer;
  C: Cardinal;
  Tmp: string;
begin
  inherited;

  Stream.Read(FStreamID);
  Stream.Read(FStreamName);
  Stream.Read(FStreamTitle);
  Stream.Read(FCurrentURL);
  Stream.Read(FBitrate);
  Stream.Read(B);
  FFormat := TAudioTypes(B);

  Stream.Read(C);
  for i := 0 to C - 1 do
  begin
    Stream.Read(Tmp);
    FRegExes.Add(Tmp);
  end;

  Stream.Read(FServerHash);
  Stream.Read(FServerArtistHash);
end;

{ TCommandUpdateStats }

constructor TCommandUpdateStats.Create;
begin
  inherited;

  FCommandType := ctUpdateStats;
  FStream := TExtendedStream.Create;
end;

function TCommandUpdateStats.Process(ToStream: TExtendedStream): Boolean;
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

procedure TCommandServerInfoResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin
  inherited;

  Stream.Read(FClientCount);
  Stream.Read(FRecordingCount);
end;

{ TCommandMessageResponse }

constructor TCommandMessageResponse.Create;
begin
  inherited;

  FCommandType := ctMessageResponse;
end;

procedure TCommandMessageResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin
  inherited;

  Stream.Read(FMessageID);
  Stream.Read(FMessageMsg);
end;

{ TCommandSetSettings }

constructor TCommandSetSettings.Create(TitleNotifications: Boolean);
begin
  Create;

  FTitleNotifications := TitleNotifications;
end;

procedure TCommandSetSettings.DoGet(S: TExtendedStream);
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

procedure TCommandClientStats.DoGet(S: TExtendedStream);
begin
  S.Write(Byte(FStatType));
end;

{ TCommandSubmitStream }

constructor TCommandSubmitStream.Create(URL: string);
begin
  Create;

  FURL := URL;
end;

constructor TCommandSubmitStream.Create;
begin
  inherited;

  FCommandType := ctSubmitStream;
end;

procedure TCommandSubmitStream.DoGet(S: TExtendedStream);
begin
  S.Write(FURL);
end;

{ TCommandSetStreamData }

constructor TCommandSetStreamData.Create;
begin
  inherited;

  FCommandType := ctSetStreamData;
end;

procedure TCommandSetStreamData.DoGet(S: TExtendedStream);
begin
  S.Write(FStreamID);
  S.Write(FRating);
  S.Write(FHasRecordingOkay);
  S.Write(FRecordingOkay);
  S.Write(FTitleRegEx);
  S.Write(FHasIgnoreTitles);
  S.Write(FIgnoreTitles);
end;

{ TCommandTitleChanged }

constructor TCommandTitleChanged.Create;
begin
  inherited;

  FCommandType := ctTitleChanged;
end;

constructor TCommandTitleChanged.Create(StreamID: Cardinal; StreamName,
  StreamTitle, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal;
  URLs: string);
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

procedure TCommandTitleChanged.DoGet(S: TExtendedStream);
begin
  inherited;

  S.Write(FStreamID);
  S.Write(FStreamName);
  S.Write(FStreamTitle);
  S.Write(FCurrentURL);
  S.Write(FURL);
  S.Write(Byte(FFormat));
  S.Write(FKbps);
  S.Write(FURLs);
end;

{ TCommandGetMonitorStreamsResponse }

constructor TCommandGetMonitorStreamsResponse.Create;
begin
  inherited;

  FCommandType := ctGetMonitorStreamsResponse;
end;

procedure TCommandGetMonitorStreamsResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
var
  Count: Cardinal;
  i: Integer;
begin
  inherited;

  Stream.Read(Count);
  SetLength(FStreamIDs, Count);
  for i := 0 to High(FStreamIDs) do
    Stream.Read(FStreamIDs[i]);
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

procedure TCommandGetMonitorStreams.DoGet(S: TExtendedStream);
begin
  S.Write(FCount);
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

procedure TCommandSyncWishlist.DoGet(S: TExtendedStream);
var
  i: Integer;
begin
  inherited;

  S.Write(Byte(FSyncType));
  S.Write(Cardinal(Length(FHashes)));
  for i := 0 to High(FHashes) do
  begin
    S.Write(FHashes[i].Hash);
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

procedure TCommandSearchCharts.DoGet(S: TExtendedStream);
begin
  inherited;

  S.Write(FTop);
  S.Write(FTerm);
end;

{ TCommandSearchChartsResponse }

constructor TCommandSearchChartsResponse.Create;
begin
  inherited;

  FCommandType := ctSearchChartsResponse;
end;

procedure TCommandSearchChartsResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin
  LoadStream(Stream);

  TExtendedStream(FStream).Read(FSuccess);
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

  FData := TExtendedStream.Create;
  FCommandType := ctStreamAnalyzationData;
end;

constructor TCommandStreamAnalyzationData.Create(StreamID: Cardinal; Data: TExtendedStream);
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

procedure TCommandStreamAnalyzationData.DoGet(S: TExtendedStream);
var
  CompressedData: TExtendedStream;
begin
  inherited;

  S.Write(FStreamID);

  FData.Seek(0, soFromBeginning);
  CompressedData := TExtendedStream.Create;
  ZLib.ZCompressStream(FData, CompressedData, zcDefault);
  FData.Free;
  FData := CompressedData;

  FData.Seek(0, soFromBeginning);
  S.CopyFrom(FData, FData.Size);
end;

{ TCommandGenerateAuthToken }

constructor TCommandGenerateAuthToken.Create;
begin
  FCommandType := ctGenerateAuthToken;
end;

destructor TCommandGenerateAuthToken.Destroy;
begin

  inherited;
end;

{ TCommandGenerateAuthTokenResponse }

constructor TCommandGenerateAuthTokenResponse.Create;
begin
  FCommandType := ctGenerateAuthTokenResponse;
end;

procedure TCommandGenerateAuthTokenResponse.Load(
  CommandHeader: TCommandHeader; Stream: TExtendedStream);
begin
  inherited;

  Stream.Read(FToken);
end;

end.
