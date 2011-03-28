unit Player;

interface

uses
  Windows, SysUtils, Classes, DynBass, Math;

type
  TPlayer = class
  private
    FPlayer: Cardinal;
    FSync, FSyncEnd: Cardinal;
    FPaused: Boolean;
    FStopped: Boolean;
    FFilename: string;
    FVolume: Integer;

    FOnEndReached: TNotifyEvent;

    function FGetPaused: Boolean;
    function FGetStopped: Boolean;
    function FGetPlaying: Boolean;
    procedure FSetVolume(Value: Integer);
    function FGetMaxByte: Cardinal;
    function FGetPositionByte: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play(Filename: string; From: Cardinal);
    procedure Pause;
    procedure Stop;
    procedure SetPosition(Bytes: Cardinal);

    property Paused: Boolean read FGetPaused;
    property Stopped: Boolean read FGetStopped;
    property Playing: Boolean read FGetPlaying;
    property Filename: string read FFilename;
    property Volume: Integer read FVolume write FSetVolume;
    property MaxByte: Cardinal read FGetMaxByte;
    property PositionByte: Cardinal read FGetPositionByte;

    property OnEndReached: TNotifyEvent read FOnEndReached write FOnEndReached;
  end;

implementation

procedure SlideEndSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  BASSChannelRemoveSync(channel, handle);

  P := TPlayer(user);
  if P.FPaused then
  begin
    BASSChannelPause(channel)
  end else if P.FStopped then
  begin
    BASSChannelStop(channel);
    BASSStreamFree(channel);
  end;

  P.FPaused := False;
  P.FStopped := False;
end;

procedure EndSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
var
  P: TPlayer;
begin
  BASSChannelRemoveSync(channel, handle);

  P := TPlayer(user);
  BASSChannelStop(channel);
  BASSStreamFree(channel);

  if Assigned(P.OnEndReached) then
    P.OnEndReached(P);

  P.FPaused := False;
  P.FStopped := False;
end;

{ TPlayer }

constructor TPlayer.Create;
begin
  inherited;

end;

destructor TPlayer.Destroy;
begin
  // TODO: Fadeout hier überall!!!
  Stop;

  inherited;
end;

function TPlayer.FGetMaxByte: Cardinal;
begin
  Result := 0;
  if FPlayer > 0 then
    if BASSChannelGetLength(FPlayer, BASS_POS_BYTE) > 0 then
      Result := BASSChannelGetLength(FPlayer, BASS_POS_BYTE);
end;

function TPlayer.FGetPaused: Boolean;
begin
  Result := False;
  if FPlayer > 0 then
    Result := BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PAUSED;
end;

function TPlayer.FGetPlaying: Boolean;
begin
  Result := False;
  if FPlayer > 0 then
    Result := BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING;
end;

function TPlayer.FGetPositionByte: Cardinal;
begin
  Result := 0;
  if FPlayer > 0 then
    if BASSChannelGetPosition(FPlayer, BASS_POS_BYTE) > 0 then
      Result := BASSChannelGetPosition(FPlayer, BASS_POS_BYTE);
end;

function TPlayer.FGetStopped: Boolean;
begin
  Result := FPlayer = 0;
end;

procedure TPlayer.FSetVolume(Value: Integer);
begin
  FVolume := Value;
  if FPlayer > 0 then
    BASSChannelSetAttribute(FPlayer, 2, Value / 100);
end;

procedure TPlayer.Pause;
var
  Pos, Len: Double;
begin
  if FPlayer > 0 then
  begin
    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PAUSED then
    begin
      BASSChannelSetAttribute(FPlayer, 2, FVolume / 100);
      BASSChannelPlay(FPlayer, False)
    end else
    begin
      if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
        FPaused := True;

      Pos := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_FILEPOS_CURRENT));
      Len := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));

      if Len - Pos < 0.300 then
      begin
        BASSChannelPause(FPlayer);
      end else
      begin
        FSync := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
        BASSChannelSlideAttribute(FPlayer, 2, 0, Trunc(Min(Len - Pos - 10, 300)));
        while BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING do
          Sleep(50);
      end;
    end;
  end;
end;

procedure TPlayer.Play(Filename: string; From: Cardinal);
begin
  if FPlayer > 0 then
    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PAUSED then
    begin
      BASSChannelSetAttribute(FPlayer, 2, FVolume / 100);
      BASSChannelPlay(FPlayer, False);
      Exit;
    end else
      Stop;

  FFilename := Filename;
  FPlayer := BASSStreamCreateFile(False, PChar(Filename), 0, 0, {$IFDEF UNICODE}BASS_UNICODE{$ENDIF});
  BASSChannelSetAttribute(FPlayer, 2, FVolume / 100);
  BASSChannelSetPosition(FPlayer, From, BASS_POS_BYTE);
  FSyncEnd := BASSChannelSetSync(FPlayer, BASS_SYNC_END, 0, EndSyncProc, Self);
  BASSChannelPlay(FPlayer, True);
end;

procedure TPlayer.SetPosition(Bytes: Cardinal);
begin
  if FPlayer > 0 then
  begin
    if Bytes = MaxByte then
    begin
      BASSChannelStop(FPlayer);
      BASSChannelSetPosition(FPlayer, 0, BASS_POS_BYTE);
    end else
      BASSChannelSetPosition(FPlayer, Bytes, BASS_POS_BYTE);
  end;
end;

procedure TPlayer.Stop;
var
  Pos, Len: Double;
begin
  if FPlayer > 0 then
  begin
    FFilename := '';
    FStopped := True;

    if BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING then
    begin
      Pos := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetPosition(FPlayer, BASS_FILEPOS_CURRENT));
      Len := BASSChannelBytes2Seconds(FPlayer, BASSChannelGetLength(FPlayer, BASS_POS_BYTE));

      if Len - Pos < 0.300 then
      begin
        BASSChannelStop(FPlayer);
        BASSStreamFree(FPlayer);
      end else
      begin
        FSync := BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, SlideEndSyncProc, Self);
        BASSChannelSlideAttribute(FPlayer, 2, 0, Min(Trunc(Len - Pos - 10), 300));
        while BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING do
          Sleep(50);
      end;
      FPlayer := 0;
    end else
    begin
      BASSChannelStop(FPlayer);
      BASSStreamFree(FPlayer);
      FPlayer := 0;
    end;
  end;
end;

end.
