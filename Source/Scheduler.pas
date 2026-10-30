unit Scheduler;

interface

uses
  Classes,
  DataManager,
  DateUtils,
  Functions,
  Generics.Collections,
  Math,
  SysUtils,
  TypeDefs,
  Windows;

type
  TScheduleEvent = procedure(IsStart: Boolean; Schedule, OriginalSchedule: TSchedule) of object;

  TSchedulerSchedule = class(TSchedule)
  private
    // Reference to the original schedule owned by the main thread. Only used for comparison,
    // never dereference it from the scheduler thread - it may be modified or freed at any time.
    FSchedule: TSchedule;
    FIsStart: Boolean;
    FCalculatedStart: TDateTime;
    FCalculatedEnd: TDateTime;
  public
    constructor Create(Schedule: TSchedule);

    property Schedule: TSchedule read FSchedule write FSchedule;
    property IsStart: Boolean read FIsStart write FIsStart;
    property CalculatedStart: TDateTime read FCalculatedStart write FCalculatedStart;
    property CalculatedEnd: TDateTime read FCalculatedEnd write FCalculatedEnd;
  end;

  { TSchedulerThread }

  TSchedulerThread = class(TThread)
  private
    FSchedulesLock: TRTLCriticalSection;
    // Non-owning references to schedule copies owned by TScheduler.
    FSchedules: TList<TSchedulerSchedule>;
    FReloadGeneration: QWord;
    FBatchGeneration: QWord;

    FReloadEvent: THandle;
    FTerminateEvent: THandle;

    FLogText: string;

    FSchedule: TSchedule;
    FOriginalSchedule: TSchedule;
    FIsStart: Boolean;

    FOnLog: TLogEvent;
    FOnSchedule: TScheduleEvent;

    procedure DoSyncLog(Text: string);
    procedure DoSyncSchedule(const Schedule, OriginalSchedule: TSchedule; const IsStart: Boolean);

    procedure SyncLog;
    procedure SyncSchedule;

    function IsBatchCurrent: Boolean;
    function GetTimerTime(const Time: TDateTime): Int64;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reload(const ScheduleList: TList<TSchedulerSchedule>);

    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnSchedule: TScheduleEvent read FOnSchedule write FOnSchedule;
  end;

  TScheduler = class
  private
    FThread: TSchedulerThread;
    FStopped: Boolean;
    // Owns the schedule copies referenced by FThread.
    FSchedules: TList<TSchedulerSchedule>;

    FOnLog: TLogEvent;
    FOnSchedule: TScheduleEvent;

    procedure ReloadThread;
    procedure ThreadLog(Text, Data: string);
    procedure ThreadSchedule(IsStart: Boolean; Schedule, OriginalSchedule: TSchedule);
    procedure ThreadTerminate(Sender: TObject);
    procedure ThreadTerminateQueued;

    function FGetActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure SetSchedules(ScheduleList: TList<TSchedule>);

    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnSchedule: TScheduleEvent read FOnSchedule write FOnSchedule;

    property Active: Boolean read FGetActive;
  end;

implementation

{ TScheduleThread }

constructor TSchedulerThread.Create;
begin
  inherited Create(True);

  FSchedules := TList<TSchedulerSchedule>.Create;

  FreeOnTerminate := False;

  InitCriticalSection(FSchedulesLock);
  FReloadEvent := CreateEvent(nil, True, False, nil);
  FTerminateEvent := CreateEvent(nil, True, False, nil);
end;

destructor TSchedulerThread.Destroy;
begin
  CloseHandle(FTerminateEvent);
  CloseHandle(FReloadEvent);
  DoneCriticalSection(FSchedulesLock);

  FSchedules.Free;

  inherited;
end;

procedure TSchedulerThread.DoSyncLog(Text: string);
begin
  FLogText := Text;

  Synchronize(SyncLog);
end;

procedure TSchedulerThread.DoSyncSchedule(const Schedule, OriginalSchedule: TSchedule; const IsStart: Boolean);
begin
  FSchedule := Schedule;
  FOriginalSchedule := OriginalSchedule;
  FIsStart := IsStart;

  Synchronize(SyncSchedule);
end;

procedure TSchedulerThread.Execute;
var
  i: Integer;
  Lowest, LowestWakeup: TDateTime;
  FoundSchedules, FoundLowestWakeup, StopEventsTriggered: Boolean;
  WakeTime: Int64;
  StartTime: TDateTime;
  StartSchedules, StopSchedules: TList<TSchedule>;
  StartScheduleReferences, StopScheduleReferences: TList<TSchedule>;
  Timer, TimerWakeup: THandle;
  WaitHandles: TWOHandleArray;
  Res, ArrLen: Cardinal;
begin

  Timer := CreateWaitableTimer(nil, True, nil);
  if Timer = 0 then
  begin
    DoSyncLog(Format('Unable to create scheduler timer: %s', [SysErrorMessage(GetLastError)]));
    Sleep(1000);
    Exit;
  end;

  TimerWakeup := CreateWaitableTimer(nil, True, nil);
  if TimerWakeup = 0 then
  begin
    CloseHandle(Timer);
    DoSyncLog(Format('Unable to create scheduler wake timer: %s', [SysErrorMessage(GetLastError)]));
    Sleep(1000);
    Exit;
  end;

  StartSchedules := TList<TSchedule>.Create;
  StopSchedules := TList<TSchedule>.Create;
  StartScheduleReferences := TList<TSchedule>.Create;
  StopScheduleReferences := TList<TSchedule>.Create;
  try
    while True do
    begin
      CancelWaitableTimer(TimerWakeup);

      for i := 0 to StartSchedules.Count - 1 do
        StartSchedules[i].Free;
      for i := 0 to StopSchedules.Count - 1 do
        StopSchedules[i].Free;
      StartSchedules.Clear;
      StopSchedules.Clear;
      StartScheduleReferences.Clear;
      StopScheduleReferences.Clear;

      // Niedrigsten nächsten Zeitpunkt suchen
      FoundSchedules := False;
      Lowest := MaxDouble;
      FoundLowestWakeup := False;
      LowestWakeup := MaxDouble;
      EnterCriticalSection(FSchedulesLock);
      try
        for i := FSchedules.Count - 1 downto 0 do
        begin
          StartTime := FSchedules[i].GetStartTime(False);

          // Wenn beide Punkte in der Vergangenheit liegen echte Zeiten neu berechnen.
          if (StartTime < Now) and (FSchedules[i].GetEndTime(StartTime) > Now) then
          begin
            FSchedules[i].CalculatedStart := StartTime;
            FSchedules[i].CalculatedEnd := FSchedules[i].GetEndTime(FSchedules[i].CalculatedStart);
          end else if (FSchedules[i].CalculatedStart < Now) and (FSchedules[i].CalculatedEnd < Now) then
          begin
            FSchedules[i].CalculatedStart := FSchedules[i].GetStartTime(True);
            FSchedules[i].CalculatedEnd := FSchedules[i].GetEndTime(FSchedules[i].CalculatedStart);
          end;

          if (FSchedules[i].CalculatedStart > Now) then
          begin
            if FSchedules[i].CalculatedStart < LowestWakeup then
            begin
              LowestWakeup := FSchedules[i].CalculatedStart;
              FoundLowestWakeup := True;
            end;

            if FSchedules[i].CalculatedStart < Lowest then
            begin
              Lowest := FSchedules[i].CalculatedStart;
              FoundSchedules := True;
            end;
          end;

          if (FSchedules[i].CalculatedEnd < Lowest) and (FSchedules[i].CalculatedEnd > Now) then
          begin
            Lowest := FSchedules[i].CalculatedEnd;
            FoundSchedules := True;
          end;
        end;

        if FoundSchedules then
          // Alle Aktionen suchen, die zu dem niedrigsten Zeitpunkt ausgeführt werden müssen
          for i := 0 to FSchedules.Count - 1 do
          begin
            FSchedules[i].IsStart := False;
            if FSchedules[i].CalculatedStart = Lowest then
              FSchedules[i].IsStart := True;
            if (FSchedules[i].CalculatedStart = Lowest) or (FSchedules[i].CalculatedEnd = Lowest) then
              if FSchedules[i].IsStart then
              begin
                StartSchedules.Add(FSchedules[i].Copy);
                StartScheduleReferences.Add(FSchedules[i].Schedule);
              end else
              begin
                StopSchedules.Add(FSchedules[i].Copy);
                StopScheduleReferences.Add(FSchedules[i].Schedule);
              end;
          end;

        FBatchGeneration := FReloadGeneration;
        ResetEvent(FReloadEvent);
      finally
        LeaveCriticalSection(FSchedulesLock);
      end;

      if not FoundSchedules then
      begin
        //DoSyncLog('Keine Schedules gefunden, warte auf ReloadEvent');

        WaitHandles[0] := FReloadEvent;
        WaitHandles[1] := FTerminateEvent;
        ArrLen := 2;
        Res := WaitForMultipleObjects(ArrLen, @WaitHandles, False, INFINITE);
        if Res <= WAIT_OBJECT_0 + ArrLen - 1 then
        begin
          if Res - WAIT_OBJECT_0 = 1 then
            Exit;
        end else if Res = WAIT_FAILED then
        begin
          DoSyncLog(Format('Scheduler wait failed: %s', [SysErrorMessage(GetLastError)]));
          Sleep(1000);
          Exit;
        end;

        Continue;
      end;

      if (StartSchedules.Count = 0) and (StopSchedules.Count = 0) then
        Continue;

      DoSyncLog(Format('It is %s, next action is at %s', [DateTimeToStr(Now), DateTimeToStr(Lowest)]));

      //MSeconds := MilliSecondsBetween(Now, Lowest);
      //NSeconds := MSeconds * -10000;
      WakeTime := GetTimerTime(Lowest);
      if WakeTime = 0 then
      begin
        DoSyncLog('Unable to convert the next schedule time to a waitable timer value');
        Sleep(1000);
        Exit;
      end;

      // Timer zum Start der Aufnahme
      if not SetWaitableTimer(Timer, WakeTime, 0, nil, nil, False) then
      begin
        DoSyncLog(Format('Unable to set scheduler timer: %s', [SysErrorMessage(GetLastError)]));
        Sleep(1000);
        Exit;
      end;

      // Timer für Aufwachen aus Standby setzen, nur für nächstes Start-Event
      if FoundLowestWakeup then
      begin
        WakeTime := GetTimerTime(IncSecond(LowestWakeup, -30));
        if WakeTime = 0 then
        begin
          DoSyncLog('Unable to convert the wake-up time to a waitable timer value');
          Sleep(1000);
          Exit;
        end;
        if not SetWaitableTimer(TimerWakeup, WakeTime, 0, nil, nil, True) then
        begin
          DoSyncLog(Format('Unable to set scheduler wake timer: %s', [SysErrorMessage(GetLastError)]));
          Sleep(1000);
          Exit;
        end;
      end;

      //DoSyncLog(Format('Sleeping for %ds', [Trunc(MSeconds / 1000)]));
      //DoSyncLog(Format('Sleeping until %s (%ds)', [DateTimeToStr(Lowest), SecondsBetween(Now, Lowest)]));

      // Jetzt solange schlafen
      WaitHandles[0] := FTerminateEvent;
      WaitHandles[1] := FReloadEvent;
      WaitHandles[2] := Timer;
      ArrLen := 3;
      Res := WaitForMultipleObjectsEx(ArrLen, @WaitHandles, False, INFINITE, False);
      if Res = WAIT_OBJECT_0 then
        Exit;
      if Res = WAIT_OBJECT_0 + 1 then
        Continue;
      if Res = WAIT_FAILED then
      begin
        DoSyncLog(Format('Scheduler wait failed: %s', [SysErrorMessage(GetLastError)]));
        Sleep(1000);
        Exit;
      end;
      if Res <> WAIT_OBJECT_0 + 2 then
      begin
        DoSyncLog(Format('Scheduler wait returned unexpected result %d', [Res]));
        Sleep(1000);
        Exit;
      end;

      // Nun Aktionen ausführen
      if Terminated then
        Exit;
      if Now < Lowest then
        Continue;
      if not IsBatchCurrent then
        Continue;

      StopEventsTriggered := StopSchedules.Count > 0;
      for i := 0 to StopSchedules.Count - 1 do
      begin
        DoSyncSchedule(StopSchedules[i], StopScheduleReferences[i], False);
        if not IsBatchCurrent then
          Break;
      end;

      if not IsBatchCurrent then
        Continue;

      // Etwas Zeit geben, falls eine Aufnahme gerade stoppt und gleich wieder gestartet werden soll.
      // Das hier sollte helfen. Solange wir nur kurz schlafen (unter einer Minute) ist das okay.
      // Keine schöne Lösung, aber sollte klappen...
      if StopEventsTriggered then
        Sleep(2000);

      if not IsBatchCurrent then
        Continue;

      for i := 0 to StartSchedules.Count - 1 do
      begin
        DoSyncSchedule(StartSchedules[i], StartScheduleReferences[i], True);
        if not IsBatchCurrent then
          Break;
      end;
    end;
  finally
    for i := 0 to StartSchedules.Count - 1 do
      StartSchedules[i].Free;
    for i := 0 to StopSchedules.Count - 1 do
      StopSchedules[i].Free;
    StartSchedules.Free;
    StopSchedules.Free;
    StartScheduleReferences.Free;
    StopScheduleReferences.Free;
    CloseHandle(Timer);
    CloseHandle(TimerWakeup);

    EnterCriticalSection(FSchedulesLock);
    try
      FSchedules.Clear;
    finally
      LeaveCriticalSection(FSchedulesLock);
    end;
  end;
end;

procedure TSchedulerThread.TerminatedSet;
begin
  inherited TerminatedSet;

  SetEvent(FTerminateEvent);
end;

function TSchedulerThread.IsBatchCurrent: Boolean;
begin
  EnterCriticalSection(FSchedulesLock);
  try
    Result := FBatchGeneration = FReloadGeneration;
  finally
    LeaveCriticalSection(FSchedulesLock);
  end;
end;

function TSchedulerThread.GetTimerTime(const Time: TDateTime): Int64;
begin
  try
    Exit(Int64(TFunctions.DateTimeToFileTime(TFunctions.LocalToUTC(Time))));
  except
    Exit(0);
  end;
end;

procedure TSchedulerThread.Reload(const ScheduleList: TList<TSchedulerSchedule>);
var
  i: Integer;
begin
  EnterCriticalSection(FSchedulesLock);
  try
    FSchedules.Clear;

    for i := 0 to ScheduleList.Count - 1 do
      FSchedules.Add(ScheduleList[i]);

    Inc(FReloadGeneration);
    SetEvent(FReloadEvent);
  finally
    LeaveCriticalSection(FSchedulesLock);
  end;
end;

procedure TSchedulerThread.SyncLog;
begin
  if Assigned(FOnLog) then
    FOnLog(FLogText, '');
end;

procedure TSchedulerThread.SyncSchedule;
begin
  if IsBatchCurrent and Assigned(FOnSchedule) then
    FOnSchedule(FIsStart, FSchedule, FOriginalSchedule);
end;

{ TSchedulerSchedule }

constructor TSchedulerSchedule.Create(Schedule: TSchedule);
begin
  inherited Create;

  Assign(Schedule);
  FSchedule := Schedule;

  FCalculatedStart := 0;
  FCalculatedEnd := 0;
end;

{ TScheduler }

constructor TScheduler.Create;
begin
  inherited;
  FSchedules := TList<TSchedulerSchedule>.Create;
end;

destructor TScheduler.Destroy;
var
  i: Integer;
begin
  Stop;
  for i := 0 to FSchedules.Count - 1 do
    FSchedules[i].Free;
  FSchedules.Free;

  inherited;
end;

function TScheduler.FGetActive: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TScheduler.SetSchedules(ScheduleList: TList<TSchedule>);
var
  i: Integer;
  L: TList<TSchedulerSchedule>;
  OldSchedules: TList<TSchedulerSchedule>;
  S: TSchedulerSchedule;
begin
  L := TList<TSchedulerSchedule>.Create;
  try
    if ScheduleList <> nil then
    begin
      for i := 0 to ScheduleList.Count - 1 do
        if ScheduleList[i].Active then
        begin
          S := TSchedulerSchedule.Create(ScheduleList[i]);
          L.Add(S);
        end;
    end;

    OldSchedules := FSchedules;
    FSchedules := L;
    L := nil;
  finally
    if L <> nil then
    begin
      for i := 0 to L.Count - 1 do
        L[i].Free;
      L.Free;
    end;
  end;

  try
    if (FThread = nil) and not FStopped then
      Start
    else if (FThread <> nil) and not FStopped and not FThread.Terminated then
      ReloadThread;
  finally
    for i := 0 to OldSchedules.Count - 1 do
      OldSchedules[i].Free;
    OldSchedules.Free;
  end;
end;

procedure TScheduler.Start;
begin
  FStopped := False;

  if FThread <> nil then
    Exit;

  FThread := TSchedulerThread.Create;
  FThread.OnLog := ThreadLog;
  FThread.OnSchedule := ThreadSchedule;
  FThread.OnTerminate := ThreadTerminate;
  ReloadThread;
  FThread.Start;
end;

procedure TScheduler.ReloadThread;
begin
  if FStopped or (FThread = nil) or FThread.Terminated then
    Exit;

  FThread.Reload(FSchedules);
end;

procedure TScheduler.Stop;
begin
  FStopped := True;

  if FThread <> nil then
  begin
    FThread.OnLog := nil;
    FThread.OnSchedule := nil;
    FThread.OnTerminate := nil;
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

procedure TScheduler.ThreadLog(Text, Data: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Text, Data);
end;

procedure TScheduler.ThreadSchedule(IsStart: Boolean; Schedule, OriginalSchedule: TSchedule);
begin
  try
    if Assigned(FOnSchedule) and (Schedule <> nil) then
      FOnSchedule(IsStart, Schedule, OriginalSchedule);
  except
    on E: Exception do
      ThreadLog(Format('Exception while executing a scheduled action: %s: %s', [E.ClassName, E.Message]), '');
  end;
end;

procedure TScheduler.ThreadTerminate(Sender: TObject);
begin
  if FStopped or (Sender <> FThread) then
    Exit;

  TThread.ForceQueue(TThread(Sender), ThreadTerminateQueued);
end;

procedure TScheduler.ThreadTerminateQueued;
var
  Thread: TSchedulerThread;
begin
  if FStopped or (FThread = nil) then
    Exit;

  Thread := FThread;
  Thread.WaitFor;
  if FStopped then
    Exit;

  Thread.OnLog := nil;
  Thread.OnSchedule := nil;
  Thread.OnTerminate := nil;
  FThread := nil;
  Thread.Free;

  ThreadLog('Scheduler thread terminated unexpectedly, restarting', '');
  if not FStopped then
    Start;
end;

end.
