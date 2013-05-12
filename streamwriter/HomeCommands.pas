{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  TypeDefs, Generics.Collections;

type
  TSendClientStatTypes = (csSave, csAutoSave);
  TSyncWishlistTypes = (swSync, swAdd, swRemove);

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
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Success: Boolean read FSuccess;
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
    FTitle: string;
    FCurrentURL: string;
    FBitrate: Cardinal;
    FFormat: TAudioTypes;
    FTitleRegEx: string;
    FServerHash: Cardinal;
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property StreamID: Cardinal read FStreamID;
    property StreamName: string read FStreamName;
    property Title: string read FTitle;
    property CurrentURL: string read FCurrentURL;
    property Bitrate: Cardinal read FBitrate;
    property Format: TAudioTypes read FFormat;
    property TitleRegEx: string read FTitleRegEx;
    property ServerHash: Cardinal read FServerHash;
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
    FTitle: string;
    FCurrentURL: string;
    FURL: string;
    FFormat: TAudioTypes;
    FKbps: Cardinal;
    FURLs: string;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(StreamID: Cardinal; StreamName, Title, CurrentURL, URL: string;
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
    FHashes: TCardinalArray;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(SyncType: TSyncWishlistTypes; Hashes: TCardinalArray); overload;

    property Hashes: TCardinalArray read FHashes write FHashes;
  end;

  TCommandSearchCharts = class(TCommand)
  private
    FTerm: string;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(Term: string); overload;
  end;

  TCommandSearchChartsResponse = class(TCommand)
  private
  protected
  public
    constructor Create;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;
  end;

  TCommandGetWishlistUpgrade = class(TCommand)
  private
    FTitles: TStringList;
  protected
    procedure DoGet(S: TExtendedStream); override;
  public
    constructor Create; overload;
    constructor Create(Titles: TStringList); overload;
    destructor Destroy; override;
  end;

  TCommandGetWishlistUpgradeResponse = class(TCommand)
  private
    FTitles: TWishlistUpgradeList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Titles: TWishlistUpgradeList read FTitles;
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
end;

procedure TCommandNetworkTitleChangedResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
var
  B: Byte;
begin
  inherited;

  Stream.Read(FStreamID);
  Stream.Read(FStreamName);
  Stream.Read(FTitle);
  Stream.Read(FCurrentURL);
  Stream.Read(FBitrate);
  Stream.Read(B);
  FFormat := TAudioTypes(B);
  Stream.Read(FTitleRegEx);
  Stream.Read(FServerHash);
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
  Title, CurrentURL, URL: string; Format: TAudioTypes; Kbps: Cardinal;
  URLs: string);
begin
  Create;

  FStreamID := StreamID;
  FStreamName := StreamName;
  FTitle := Title;
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
  S.Write(FTitle);
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

constructor TCommandSyncWishlist.Create(SyncType: TSyncWishlistTypes; Hashes: TCardinalArray);
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
    S.Write(FHashes[i]);
end;

{ TCommandSearchCharts }

constructor TCommandSearchCharts.Create;
begin
  inherited;

  FCommandType := ctSearchCharts;
end;

constructor TCommandSearchCharts.Create(Term: string);
begin
  Create;

  FTerm := Term;
end;

procedure TCommandSearchCharts.DoGet(S: TExtendedStream);
begin
  inherited;

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
  LoadStream(Stream); // TODO: mal zusehen was hier ist... immer komprimieren aufm server dauert auchn bisschen! JA DAS DAUERT. evtl iwann raus!
                      //       oder nen flag ans command, COMPRESSED. dann kann mans on demand im server ändern!
end;

{ TCommandGetWishlistUpgrade }

constructor TCommandGetWishlistUpgrade.Create;
begin
  inherited;

  FCommandType := ctGetWishlistUpgrade;
  FTitles := TStringList.Create;
end;

constructor TCommandGetWishlistUpgrade.Create(Titles: TStringList);
begin
  Create;

  FTitles.Assign(Titles);
end;

destructor TCommandGetWishlistUpgrade.Destroy;
begin
  FTitles.Free;

  inherited;
end;

procedure TCommandGetWishlistUpgrade.DoGet(S: TExtendedStream);
var
  i: Integer;
begin
  inherited;

  S.Write(Cardinal(FTitles.Count));
  for i := 0 to FTitles.Count - 1 do
    S.Write(FTitles[i]);
end;

{ TCommandGetWishlistUpgradeResponse }

constructor TCommandGetWishlistUpgradeResponse.Create;
begin
  inherited;

  FCommandType := ctGetWishlistUpgradeResponse;
  FTitles := TWishlistUpgradeList.Create;
end;

destructor TCommandGetWishlistUpgradeResponse.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTitles.Count - 1 do
    FTitles[i].Free;
  FTitles.Free;

  inherited;
end;

procedure TCommandGetWishlistUpgradeResponse.Load(
  CommandHeader: TCommandHeader; Stream: TExtendedStream);
var
  Count: Cardinal;
  i: Integer;
  WU: TWishlistUpgrade;
begin
  inherited;

  Stream.Read(Count);
  for i := 0 to Count - 1 do
  begin
    WU := TWishlistUpgrade.Create;
    Stream.Read(WU.Hash);
    Stream.Read(WU.Title);
    FTitles.Add(WU);
  end;
end;

end.
