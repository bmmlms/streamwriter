unit ICEPlayer;

interface

uses
  Windows, SysUtils, Classes, DynBASS, ExtendedStream, SyncObjs, AppData;

type
  TICEPlayer = class
  private
    FMem: TExtendedStream;
    FLock: TCriticalSection;
    FPlayer: DWORD;
    FFadingOut: Boolean;

    function FGetPlaying: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play;
    procedure Stop;
    procedure SetVolume(Vol: Integer);

    procedure PushData(Buf: Pointer; Len: Integer);

    property Playing: Boolean read FGetPlaying;
    property Mem: TExtendedStream read FMem;
    property FadingOut: Boolean read FFadingOut;
  end;

const
  PLAY_START_BUFFER = 128000;

implementation

procedure EndSyncProc(handle: HSYNC; channel, data: DWORD; user: Pointer); stdcall;
begin
  BASSStreamFree(channel);
  TICEPlayer(user).FFadingOut := False;
end;

procedure BASSClose(user: Pointer); stdcall;
begin

end;

function BASSLen(user: Pointer): QWORD; stdcall;
begin
  Result := 0;
end;

function BASSRead(buffer: Pointer; length: DWORD; user: Pointer): DWORD; stdcall;
var
  Mem: TExtendedStream;
  CopyLen: Cardinal;
begin
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);

  Result := 0;
  // Funktion muss schnell sein, also wenn Lock nicht offen, raus.
  if not TICEPlayer(user).FLock.TryEnter then
    Exit;
  try
    Mem := TICEPlayer(user).FMem;

    CopyLen := Mem.Size;
    if Length < CopyLen then
      CopyLen := Length;

    CopyMemory(Buffer, Mem.Memory, CopyLen);
    Result := CopyLen;

    Mem.RemoveRange(0, CopyLen);
  finally
    TICEPlayer(user).FLock.Leave;
  end;
end;

function BASSSeek(offset: QWORD; user: Pointer): BOOL; stdcall;
begin
  Result := BOOL(0);
end;

{ TPlayThread }

constructor TICEPlayer.Create;
begin
  inherited Create;

  FMem := TExtendedStream.Create;
  FLock := TCriticalSection.Create;
  FPlayer := 0;
end;

destructor TICEPlayer.Destroy;
begin
  if FPlayer > 0 then
  begin
    BASSStreamFree(FPlayer);
    FPlayer := 0;
  end;
  FMem.Free;
  FLock.Free;

  inherited;
end;

function TICEPlayer.FGetPlaying: Boolean;
begin
  Result := (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING) or (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_STALLED);
end;

procedure TICEPlayer.Play;
var
  Funcs: BASS_FILEPROCS;
  State: Cardinal;
begin
  if FMem.Size < PLAY_START_BUFFER then
    Exit;

  State := BASSChannelIsActive(FPlayer);
  if (FPlayer > 0) and (State <> BASS_ACTIVE_PLAYING) and (State <> BASS_ACTIVE_STALLED) then
  begin
    Stop;
  end;

  if not Playing then
  begin
    Funcs.close := BASSClose;
    Funcs.length := BASSLen;
    Funcs.seek := BASSSeek;
    Funcs.read := BASSRead;
    FPlayer := BASSStreamCreateFileUser(STREAMFILE_BUFFER, 0, Funcs, Self);
    SetVolume(AppGlobals.PlayerVolume);

    BASSChannelPlay(FPlayer, False);
  end;
end;

procedure TICEPlayer.Stop;
var
  State: Cardinal;
begin
  if FPlayer > 0 then
  begin
    State := BASSChannelIsActive(FPlayer);
    if State = BASS_ACTIVE_PLAYING then
    begin
      FFadingOut := True;
      BASSChannelSlideAttribute(FPlayer, 2, 0, 200);
      BASSChannelSetSync(FPlayer, BASS_SYNC_SLIDE, 0, EndSyncProc, Self);
    end else
    begin
      BASSStreamFree(FPlayer);
    end;
  end;
  FPlayer := 0;
end;

procedure TICEPlayer.SetVolume(Vol: Integer);
begin
  if FPlayer > 0 then
    BASSChannelSetAttribute(FPlayer, 2, Vol / 100);
end;

procedure TICEPlayer.PushData(Buf: Pointer; Len: Integer);
begin
  FLock.Enter;
  FMem.Seek(0, soFromEnd);
  FMem.Write(Buf^, Len);
  FLock.Leave;
end;

end.
