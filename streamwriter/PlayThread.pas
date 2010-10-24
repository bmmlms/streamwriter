unit PlayThread;

interface

uses
  Windows, SysUtils, Classes, DynBASS, ExtendedStream;

type
  TPlayThread = class(TThread)
  private
    FMem: TExtendedStream;
    FPlayer: DWord;
    FPlaying: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure PushData(Buf: Pointer; Len: Integer);
  end;

const
  PLAY_BUFFER = 128000;

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
  Mem := TPlayThread(user).FMem;

  CopyLen := Mem.Size;
  if Length < CopyLen then
    CopyLen := Length;

  CopyMemory(Buffer, Mem.Memory, CopyLen);
  Result := CopyLen;

  Mem.RemoveRange(0, CopyLen);
end;

function BASSSeek(offset: QWORD; user: Pointer): BOOL; stdcall;
begin

end;

{ TPlayThread }

constructor TPlayThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FMem := TExtendedStream.Create;
  FPlayer := 0;
  FPlaying := False;
end;

destructor TPlayThread.Destroy;
begin
  Stop;
  FMem.Free;
  inherited;
end;

procedure TPlayThread.Execute;
begin
  inherited;

end;

procedure TPlayThread.Start;
var
  Funcs: BASS_FILEPROCS;
begin
  if FPlayer = 0 then // BASSChannelIsActive(FPlayer) <> BASS_ACTIVE_PLAYING
  begin
    FPlaying := True;
    if FMem.Size > PLAY_BUFFER then
    begin
      Funcs.close := BASSClose;
      Funcs.length := BASSLen;
      Funcs.seek := BASSSeek;
      Funcs.read := BASSRead;
      FPlayer := BASSStreamCreateFileUser(STREAMFILE_BUFFER, 0, Funcs, Self);

      BASSChannelPlay(FPlayer, False);
    end;
  end;
end;

procedure TPlayThread.Stop;
begin
  if FPlayer > 0 then
  begin
    BASSStreamFree(FPlayer);
    FPlayer := 0;
  end;
  FPlaying := False;
end;

procedure TPlayThread.PushData(Buf: Pointer; Len: Integer);
begin
  FMem.Seek(0, soFromEnd);
  FMem.Write(Buf^, Len);

  if FPlaying and (FPlayer = 0) then
  begin
    Start;
  end;

  if FMem.Size > PLAY_BUFFER * 3 then
  begin
    FMem.RemoveRange(0, PLAY_BUFFER);
  end;
end;

end.
