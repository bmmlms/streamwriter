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
  Windows, SysUtils, ExtendedStream, Commands, AudioFunctions, TypeDefs;

type
  TSendClientStatTypes = (csSave, csAutoSave);

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
    constructor Create; overload;
  end;

  TCommandLogOutResponse = class(TCommand)
  private
  protected
  public
    constructor Create; overload;
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
  protected
  public
    constructor Create; overload;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property StreamID: Cardinal read FStreamID;
    property StreamName: string read FStreamName;
    property Title: string read FTitle;
    property CurrentURL: string read FCurrentURL;
    property Bitrate: Cardinal read FBitrate;
    property Format: TAudioTypes read FFormat;
    property TitleRegEx: string read FTitleRegEx;
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
    constructor Create; overload;

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
    constructor Create; overload;

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
    constructor Create; overload;

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

  TCommandGetMonitorStreamsResponse = class(TCommand)
  private
    FStreamIDs: TIntArray;
  protected
  public
    constructor Create; overload;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property StreamIDs: TIntArray read FStreamIDs;
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
  Count, StreamID: Cardinal;
  i: Integer;
begin
  inherited;

  Stream.Read(Count);
  SetLength(FStreamIDs, Count);
  for i := 0 to High(FStreamIDs) do
    Stream.Read(FStreamIDs[i]);
end;

end.
