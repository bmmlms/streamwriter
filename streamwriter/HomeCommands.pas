unit HomeCommands;

interface

uses
  Windows, SysUtils, ExtendedStream, Commands, AudioFunctions;

type
  TCommandHandshake = class(TCommand)
  private
    FID: Cardinal;
    FProtoVersion: Cardinal;
    FVersionMajor, FVersionMinor, FVersionRevision, FVersionBuild: Cardinal;
    FBuild: Cardinal;
    FLanguage: string;
  protected
    function DoGet: TBytes; override;
  public
    constructor Create;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

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
    FSuccess: Boolean; // TODO: das feld muss vom client ausgewertet werden! und falls es false ist, nicht bei reconnecten hammern!!!
  protected
  public
    constructor Create;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Success: Boolean read FSuccess;
  end;

  TCommandUpdateStats = class(TCommand)
  private
  protected
  public
    constructor Create;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;
  end;

  TCommandLogIn = class(TCommand)
  private
    FUser: string;
    FPass: string;
  protected
    function DoGet: TBytes; override;
  public
    constructor Create; overload;
    constructor Create(User, Pass: string); overload;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

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

    function Copy: TCommand; override;   // TODO: wo werden copy und assign von aussen aufgerufen??? ist das über?????
    procedure Assign(Source: TCommand); override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property Success: Boolean read FSuccess;
    property IsAdmin: Boolean read FIsAdmin;
  end;

  TCommandLogOut = class(TCommand)
  private
  protected
  public
    constructor Create; overload;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;
  end;

  TCommandLogOutResponse = class(TCommand)
  private
  protected
  public
    constructor Create; overload;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;
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

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

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

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;
  end;

  TCommandGetServerDataResponse = class(TCommand)
  private
  protected
  public
    constructor Create;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;
  end;

  TCommandServerInfoResponse = class(TCommand)
  private
    FClientCount: Cardinal;
    FRecordingCount: Cardinal;
  protected
  public
    constructor Create; overload;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property ClientCount: Cardinal read FClientCount;
    property RecordingCount: Cardinal read FRecordingCount;
  end;

  TCommandErrorResponse = class(TCommand)
  private
    FErrorID: Cardinal;
    FErrorMsg: string;
  protected
  public
    constructor Create; overload;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

    procedure Load(CommandHeader: TCommandHeader; Stream: TExtendedStream); override;

    property ErrorID: Cardinal read FErrorID;
    property ErrorMsg: string read FErrorMsg;
  end;

  TCommandSetSettings = class(TCommand)
  private
    FTitleNotifications: Boolean;
  protected
    function DoGet: TBytes; override;
  public
    constructor Create; overload;
    constructor Create(TitleNotifications: Boolean); overload;

    function Copy: TCommand; override;
    procedure Assign(Source: TCommand); override;

    property TitleNotifications: Boolean read FTitleNotifications write FTitleNotifications;
  end;

implementation


{ TCommandHandshake }

procedure TCommandHandshake.Assign(Source: TCommand);
var
  Cmd: TCommandHandshake absolute Source;
begin
  FID := Cmd.ID;
  FProtoVersion := Cmd.ProtoVersion;
  FVersionMajor := Cmd.VersionMajor;
  FVersionMinor := Cmd.VersionMinor;
  FVersionRevision := Cmd.VersionRevision;
  FVersionBuild := Cmd.VersionBuild;
  FBuild := Cmd.Build;
  FLanguage := Cmd.Language;
end;

function TCommandHandshake.Copy: TCommand;
begin
  Result := TCommandHandshake.Create;
  Result.Assign(Self);
end;

constructor TCommandHandshake.Create;
begin
  inherited;

  FCommandType := ctHandshake;
end;

function TCommandHandshake.DoGet: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    S.Write(FID);
    S.Write(FProtoVersion);
    S.Write(FVersionMajor);
    S.Write(FVersionMinor);
    S.Write(FVersionRevision);
    S.Write(FVersionBuild);
    S.Write(FBuild);
    S.Write(FLanguage);

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

procedure TCommandHandshake.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin
  // TODO: load ist über bei allen was keine response ist. oder?
  Stream.Read(FID);
  Stream.Read(FProtoVersion);
  Stream.Read(FVersionMajor);
  Stream.Read(FVersionMinor);
  Stream.Read(FVersionRevision);
  Stream.Read(FVersionBuild);
  Stream.Read(FBuild);
  Stream.Read(FLanguage);
end;


{ TCommandHandshakeResponse }

procedure TCommandHandshakeResponse.Assign(Source: TCommand);
var
  Cmd: TCommandHandshakeResponse absolute Source;
begin
  FSuccess := Cmd.Success;
end;

function TCommandHandshakeResponse.Copy: TCommand;
begin
  Result := TCommandHandshakeResponse.Create;
  Result.Assign(Self);
end;

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

  if not fsuccess then
    raise Exception.Create('Fehlermeldung');
end;

{ TCommandGetServerData }

procedure TCommandGetServerData.Assign(Source: TCommand);
begin

end;

function TCommandGetServerData.Copy: TCommand;
begin
  Result := TCommandGetServerData.Create;
end;

constructor TCommandGetServerData.Create;
begin
  inherited;

  FCommandType := ctGetServerData;
end;

{ TCommandGetServerDataResponse }

procedure TCommandGetServerDataResponse.Assign(Source: TCommand);
begin

end;

function TCommandGetServerDataResponse.Copy: TCommand;
begin
  Result := TCommandGetServerDataResponse.Create;
end;

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

procedure TCommandLogIn.Assign(Source: TCommand);
var
  Cmd: TCommandLogIn absolute Source;
begin
  inherited;

  User := Cmd.FUser;
  Pass := Cmd.FPass;
end;

function TCommandLogIn.Copy: TCommand;
begin
//  Result := TCommandLogIn.Create;
//  Result.Assign(Self);
end;

constructor TCommandLogIn.Create(User, Pass: string);
begin
  Create;

  FUser := User;
  FPass := Pass;
end;

function TCommandLogIn.DoGet: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    S.Write(FUser);
    S.Write(FPass);

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

constructor TCommandLogIn.Create;
begin
  inherited;

  FCommandType := ctLogin;
end;

{ TCommandLogInResponse }

procedure TCommandLogInResponse.Assign(Source: TCommand);
var
  Cmd: TCommandLogInResponse absolute Source;
begin
  inherited;

  FSuccess := Cmd.Success;
end;

function TCommandLogInResponse.Copy: TCommand;
begin
  Result := TCommandLogInResponse.Create;
  Result.Assign(Self);
end;

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

procedure TCommandNetworkTitleChangedResponse.Assign(Source: TCommand);
var
  Cmd: TCommandNetworkTitleChangedResponse absolute Source;
begin
  inherited;
                              // TODO: WO NUTZE ICH ASSIGN??? UND COPY???
  FStreamID := Cmd.FStreamID;
  FStreamName := Cmd.FStreamName;
  FTitle := Cmd.FTitle;
  FCurrentURL := Cmd.FCurrentURL;
  FBitrate := Cmd.FBitrate;
  FFormat := Cmd.FFormat;
  FTitleRegEx := Cmd.FTitleRegEx;
end;

function TCommandNetworkTitleChangedResponse.Copy: TCommand;
begin
  Result := TCommandNetworkTitleChangedResponse.Create;
  Result.Assign(Self);
end;

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

procedure TCommandUpdateStats.Assign(Source: TCommand);
var
  Cmd: TCommandUpdateStats absolute Source;
begin
  inherited;

  if Stream <> nil then
    Stream.Free;

  FStream := Cmd.Stream;
end;

function TCommandUpdateStats.Copy: TCommand;
begin
  Result := TCommandUpdateStats.Create;
  Result.Assign(Self);
end;

constructor TCommandUpdateStats.Create;
begin
  inherited;

  FCommandType := ctUpdateStats;
  FStream := TExtendedStream.Create;
end;

{ TCommandServerInfoResponse }

procedure TCommandServerInfoResponse.Assign(Source: TCommand);
var
  Cmd: TCommandServerInfoResponse absolute Source;
begin
  inherited;

  FClientCount := Cmd.ClientCount;
  FRecordingCount := Cmd.RecordingCount;
end;

function TCommandServerInfoResponse.Copy: TCommand;
begin
  Result := TCommandServerInfoResponse.Create;
  Result.Assign(Self);
end;

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

{ TCommandErrorResponse }

procedure TCommandErrorResponse.Assign(Source: TCommand);
var
  Cmd: TCommandErrorResponse absolute Source;
begin
  FErrorID := Cmd.ErrorID;
  FErrorMsg := Cmd.ErrorMsg;
end;

function TCommandErrorResponse.Copy: TCommand;
begin
  Result := TCommandErrorResponse.Create;
  Result.Assign(Self);
end;

constructor TCommandErrorResponse.Create;
begin
  inherited;

  FCommandType := ctErrorResponse;
end;

procedure TCommandErrorResponse.Load(CommandHeader: TCommandHeader;
  Stream: TExtendedStream);
begin
  inherited;

  Stream.Read(FErrorID);
  Stream.Read(FErrorMsg);
end;

{ TCommandSetSettings }

procedure TCommandSetSettings.Assign(Source: TCommand);
var
  Cmd: TCommandSetSettings absolute Source;
begin
  inherited;

  TitleNotifications := Cmd.TitleNotifications;
end;

function TCommandSetSettings.Copy: TCommand;
begin
  Result := TCommandSetSettings.Create;
  Result.Assign(Self);
end;

constructor TCommandSetSettings.Create(TitleNotifications: Boolean);
begin
  Create;

  FTitleNotifications := TitleNotifications;
end;

function TCommandSetSettings.DoGet: TBytes;
var
  S: TExtendedStream;
begin
  S := TExtendedStream.Create;
  try
    S.Write(FTitleNotifications);  // TODO: Warum gibt DoGet nicht direkt den stream zurück und bekommt per parameter einen mit reingereicht zum beschreiben?

    SetLength(Result, S.Size);
    CopyMemory(@Result[0], S.Memory, S.Size);
  finally
    S.Free;
  end;
end;

constructor TCommandSetSettings.Create;
begin
  inherited;

  FCommandType := ctSetSettings;
end;

{ TCommandLogOut }

procedure TCommandLogOut.Assign(Source: TCommand);
begin
  inherited;

end;

function TCommandLogOut.Copy: TCommand;
begin
  Result := TCommandLogOut.Create;
end;

constructor TCommandLogOut.Create;
begin
  inherited;

  FCommandType := ctLogOut;
end;

{ TCommandLogOutResponse }

procedure TCommandLogOutResponse.Assign(Source: TCommand);
begin
  inherited;

end;

function TCommandLogOutResponse.Copy: TCommand;
begin
  Result := TCommandLogOutResponse.Create;
end;

constructor TCommandLogOutResponse.Create;
begin
  inherited;

  FCommandType := ctLogOutResponse;
end;

end.
