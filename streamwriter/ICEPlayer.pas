unit ICEPlayer;

interface

uses
  Windows, SysUtils, Classes, DynBASS, ExtendedStream, SyncObjs;

type
  TICEPlayer = class
  private
    FMem: TExtendedStream;
    FLock: TCriticalSection;
    FPlayer: DWord;

    function FGetPlaying: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play;
    procedure Stop;

    procedure PushData(Buf: Pointer; Len: Integer);

    property Playing: Boolean read FGetPlaying;
    property Mem: TExtendedStream read FMem;
  end;

const
  PLAY_START_BUFFER = 128000;

implementation

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
  CopyLen: Integer;
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
  Result := (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING);
end;

procedure TICEPlayer.Play;
var
  Funcs: BASS_FILEPROCS;
begin
  if FMem.Size < PLAY_START_BUFFER then
    Exit;

  if (FPlayer > 0) and (BASSChannelIsActive(FPlayer) <> BASS_ACTIVE_PLAYING) then
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

    BASSChannelPlay(FPlayer, False);
  end;
end;

procedure TICEPlayer.Stop;
begin
  BASSChannelStop(FPlayer);
  BASSStreamFree(FPlayer);
  FPlayer := 0;
end;

procedure TICEPlayer.PushData(Buf: Pointer; Len: Integer);
begin
  FLock.Enter;
  FMem.Seek(0, soFromEnd);
  FMem.Write(Buf^, Len);
  FLock.Leave;
end;

end.
