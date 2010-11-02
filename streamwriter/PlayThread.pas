unit PlayThread;

interface

uses
  Windows, SysUtils, Classes, DynBASS, ExtendedStream, SyncObjs;

type
  TPlayThread = class(TThread)
  private
    FMem: TExtendedStream;
    FLock: TCriticalSection;
    FPlayer: DWord;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PushData(Buf: Pointer; Len: Integer);
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
  if not TPlayThread(user).FLock.TryEnter then
    Exit;
  try
    Mem := TPlayThread(user).FMem;

    CopyLen := Mem.Size;
    if Length < CopyLen then
      CopyLen := Length;

    CopyMemory(Buffer, Mem.Memory, CopyLen);
    Result := CopyLen;

    Mem.RemoveRange(0, CopyLen);
  finally
    TPlayThread(user).FLock.Leave;
  end;
end;

function BASSSeek(offset: QWORD; user: Pointer): BOOL; stdcall;
begin

end;

{ TPlayThread }

constructor TPlayThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FMem := TExtendedStream.Create;
  FLock := TCriticalSection.Create;
  FPlayer := 0;
end;

destructor TPlayThread.Destroy;
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

procedure TPlayThread.Execute;
var
  Funcs: BASS_FILEPROCS;
begin
  inherited;

  BASSSetConfig(0, 1000);

  while (FMem.Size < PLAY_START_BUFFER) and (not Terminated) do
    Sleep(50);

  if Terminated then
    Exit;

  if FPlayer = 0 then // BASSChannelIsActive(FPlayer) <> BASS_ACTIVE_PLAYING
  begin
    Funcs.close := BASSClose;
    Funcs.length := BASSLen;
    Funcs.seek := BASSSeek;
    Funcs.read := BASSRead;
    FPlayer := BASSStreamCreateFileUser(STREAMFILE_BUFFER, 0, Funcs, Self);

    BASSChannelPlay(FPlayer, False);
  end;

  while (BASSChannelIsActive(FPlayer) = BASS_ACTIVE_PLAYING) and (not Terminated) do
    Sleep(50);
end;

procedure TPlayThread.PushData(Buf: Pointer; Len: Integer);
begin
  FLock.Enter;
  FMem.Seek(0, soFromEnd);
  FMem.Write(Buf^, Len);
  FLock.Leave;
end;

end.
