unit Scheduler;

interface

uses
  Classes,
  DataManager,
  DateUtils,
  Generics.Collections,
  Generics.Defaults,
  Math,
  SysUtils,
  TypeDefs,
  Windows;

type
  TScheduleThreadEvent = procedure(IsStart: Boolean; ScheduleID: Integer) of object;
  TScheduleEvent = procedure(IsStart: Boolean; Schedule: TSchedule) of object;

  TSchedulerSchedule = class(TSchedule)
  private
    FID: Cardinal;
    FIsStart: Boolean;
    FCalculatedStart: TDateTime;
    FCalculatedEnd: TDateTime;
  public
    constructor Create(Schedule: TSchedule);

    property ID: Cardinal read FID write FID;
    property IsStart: Boolean read FIsStart write FIsStart;
    property CalculatedStart: TDateTime read FCalculatedStart write FCalculatedStart;
    property CalculatedEnd: TDateTime read FCalculatedEnd write FCalculatedEnd;
  end;

  TSchedulerThread = class(TThread)
  private
    //    FSchedulesLock: _RTL_CRITICAL_SECTION;
    FSchedules: TList<TSchedulerSchedule>;

    FReloadEvent: THandle;
    FTerminateEvent: THandle;
    FReloadDoneEvent: THandle;

    FLogText: string;

    FScheduleID: Integer;
    FIsStart: Boolean;

    FOnLog: TLogEvent;
    FOnSchedule: TScheduleThreadEvent;

    procedure DoSyncLog(Text: string);
    procedure DoSyncSchedule(ScheduleID: Integer; IsStart: Boolean);

    procedure SyncLog;
    procedure SyncSchedule;

    function GetTimerTime(Time: TDateTime): Int64;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate; reintroduce;
    procedure Reload(ScheduleList: TList<TSchedulerSchedule>);

    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnSchedule: TScheduleThreadEvent read FOnSchedule write FOnSchedule;
  end;

  TScheduler = class
  private
    FThread: TSchedulerThread;
    FSchedules: TList<TSchedule>;

    FOnLog: TLogEvent;
    FOnSchedule: TScheduleEvent;

    procedure ThreadLog(Text, Data: string);
    procedure ThreadSchedule(IsStart: Boolean; ScheduleID: Integer);
    procedure ThreadTerminate(Sender: TObject);

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

  FreeOnTerminate := True;

  //  InitializeCriticalSection(FSchedulesLock);
  FReloadEvent := CreateEvent(nil, True, False, nil);
  FTerminateEvent := CreateEvent(nil, True, False, nil);
  FReloadDoneEvent := CreateEvent(nil, True, False, nil);
end;

procedure TSchedulerThread.Terminate;
begin
  inherited;

  SetEvent(FTerminateEvent);
end;

destructor TSchedulerThread.Destroy;
begin
  CloseHandle(FReloadDoneEvent);
  CloseHandle(FTerminateEvent);
  CloseHandle(FReloadEvent);
  //  DeleteCriticalSection(FSchedulesLock);

  FSchedules.Free;

  inherited;
end;

procedure TSchedulerThread.DoSyncLog(Text: string);
begin
  FLogText := Text;

  Synchronize(SyncLog);
end;

procedure TSchedulerThread.DoSyncSchedule(ScheduleID: Integer; IsStart: Boolean);
begin
  FScheduleID := ScheduleID;
  FIsStart := IsStart;

  Synchronize(SyncSchedule);
end;

procedure TSchedulerThread.Execute;
var
  i: Integer;
  Lowest, LowestWakeup: TDateTime;
  FoundSchedules, FoundLowestWakeup, StopEventsTriggered: Boolean;
  WakeTime: Int64;
  S: TList<TSchedulerSchedule>;
  Timer, TimerWakeup: THandle;
  WaitHandles: TWOHandleArray;
  Res, ArrLen: Cardinal;
begin
  inherited;

  Timer := CreateWaitableTimer(nil, True, nil);
  if Timer = 0 then
  begin
    Sleep(1000);
    Exit;
  end;

  TimerWakeup := CreateWaitableTimer(nil, True, nil);
  if TimerWakeup = 0 then
  begin
    CloseHandle(Timer);
    Sleep(1000);
    Exit;
  end;

  S := TList<TSchedulerSchedule>.Create;
  try
    while True do
    begin
      // Niedrigsten nächsten Zeitpunkt suchen
      FoundSchedules := False;
      Lowest := MaxDouble;
      FoundLowestWakeup := False;
      LowestWakeup := MaxDouble;
      //      EnterCriticalSection(FSchedulesLock);
      try
        for i := FSchedules.Count - 1 downto 0 do
        begin
          // Wenn beide Punkte in der Vergangenheit liegen echte Zeiten neu berechnen.

          if (FSchedules[i].GetStartTime(False) < Now) and (FSchedules[i].GetEndTime(FSchedules[i].GetStartTime(False)) > Now) then
          begin
            FSchedules[i].CalculatedStart := FSchedules[i].GetStartTime(False);
            FSchedules[i].CalculatedEnd := FSchedules[i].GetEndTime(FSchedules[i].CalculatedStart);
          end else if (FSchedules[i].CalculatedStart < Now) and (FSchedules[i].CalculatedEnd < Now) then
          begin
            FSchedules[i].CalculatedStart := FSchedules[i].GetStartTime(True);
            FSchedules[i].CalculatedEnd := FSchedules[i].GetEndTime(FSchedules[i].CalculatedStart);
          end;

          if (FSchedules[i].CalculatedStart < LowestWakeup) and (FSchedules[i].CalculatedStart > Now) then
          begin
            LowestWakeup := FSchedules[i].CalculatedStart;
            FoundLowestWakeup := True;
          end;

          if (FSchedules[i].CalculatedStart < Lowest) and (FSchedules[i].CalculatedStart > Now) then
          begin
            Lowest := FSchedules[i].CalculatedStart;
            FoundSchedules := True;
          end else if (FSchedules[i].CalculatedEnd < Lowest) and (FSchedules[i].CalculatedEnd > Now) then
          begin
            Lowest := FSchedules[i].CalculatedEnd;
            FoundSchedules := True;
          end;
        end;
      finally
        //        LeaveCriticalSection(FSchedulesLock);
      end;

      SetEvent(FReloadDoneEvent);
      ResetEvent(FReloadDoneEvent);

      if not FoundSchedules then
      begin
        //DoSyncLog('Keine Schedules gefunden, warte auf ReloadEvent');

        WaitHandles[0] := FReloadEvent;
        WaitHandles[1] := FTerminateEvent;
        ArrLen := 2;
        Res := WaitForMultipleObjects(ArrLen, @WaitHandles, False, INFINITE);
        if Res <= WAIT_OBJECT_0 + ArrLen - 1 then
        begin
          if Res - WAIT_OBJECT_0 = 0 then
            ResetEvent(FReloadEvent)
          else if Res - WAIT_OBJECT_0 = 1 then
            Exit;
        end else if Res = WAIT_FAILED then
        begin
          Sleep(1000);
          Exit;
        end;

        Continue;
      end;

      ResetEvent(FReloadEvent);

      // Alle Aktionen suchen, die zu dem niedrigsten Zeitpunkt ausgeführt werden müssen
      S.Clear;
      //      EnterCriticalSection(FSchedulesLock);
      try
        for i := 0 to FSchedules.Count - 1 do
        begin
          FSchedules[i].IsStart := False;
          if FSchedules[i].CalculatedStart = Lowest then
            FSchedules[i].IsStart := True;
          if (FSchedules[i].CalculatedStart = Lowest) or (FSchedules[i].CalculatedEnd = Lowest) then
            S.Add(FSchedules[i]);
        end;

        if S.Count = 0 then
          Continue;
      finally
        //        LeaveCriticalSection(FSchedulesLock);
      end;

      DoSyncLog(Format('It is %s, next action is at %s', [DateTimeToStr(Now), DateTimeToStr(Lowest)]));

      //MSeconds := MilliSecondsBetween(Now, Lowest);
      //NSeconds := MSeconds * -10000;
      WakeTime := GetTimerTime(Lowest);
      if WakeTime = 0 then
      begin
        Sleep(1000);
        Exit;
      end;

      // Timer zum Start der Aufnahme
      if not SetWaitableTimer(Timer, WakeTime, 0, nil, nil, False) then
      begin
        Sleep(1000);
        Exit;
      end;

      // Timer für Aufwachen aus Standby setzen, nur für nächstes Start-Event
      if FoundLowestWakeup then
      begin
        WakeTime := GetTimerTime(IncSecond(LowestWakeup, -30));
        if WakeTime = 0 then
        begin
          Sleep(1000);
          Exit;
        end;
        if not SetWaitableTimer(TimerWakeup, WakeTime, 0, nil, nil, True) then
        begin
          Sleep(1000);
          Exit;
        end;
      end;

      //DoSyncLog(Format('Sleeping for %ds', [Trunc(MSeconds / 1000)]));
      //DoSyncLog(Format('Sleeping until %s (%ds)', [DateTimeToStr(Lowest), SecondsBetween(Now, Lowest)]));

      // Jetzt solange schlafen
      WaitHandles[0] := Timer;
      WaitHandles[1] := FReloadEvent;
      WaitHandles[2] := FTerminateEvent;
      ArrLen := 3;
      Res := WaitForMultipleObjectsEx(ArrLen, @WaitHandles, False, INFINITE, False);
      if Res <= WAIT_OBJECT_0 + ArrLen - 1 then
      begin
        if Res - WAIT_OBJECT_0 = 1 then
        begin
          ResetEvent(FReloadEvent);
          Continue;
        end else if Res - WAIT_OBJECT_0 = 2 then
          Exit;
      end else if Res = WAIT_FAILED then
      begin
        Sleep(1000);
        Exit;
      end;

      // Nun Aktionen ausführen
      if WaitForSingleObject(Timer, 0) = WAIT_OBJECT_0 then
      begin
        StopEventsTriggered := False;
        for i := 0 to S.Count - 1 do
          if not s[i].IsStart then
          begin
            StopEventsTriggered := True;
            DoSyncSchedule(S[i].ID, S[i].IsStart);
          end;

        // Etwas Zeit geben, falls eine Aufnahme gerade stoppt und gleich wieder gestartet werden soll.
        // Das hier sollte helfen. Solange wir nur kurz schlafen (unter einer Minute) ist das okay.
        // Keine schöne Lösung, aber sollte klappen...
        if StopEventsTriggered then
          Sleep(2000);

        for i := 0 to S.Count - 1 do
          if s[i].IsStart then
            DoSyncSchedule(S[i].ID, S[i].IsStart);
      end;
    end;
  finally
    S.Free;
    CloseHandle(Timer);
    CloseHandle(TimerWakeup);

    //    EnterCriticalSection(FSchedulesLock);
    try
      for i := 0 to FSchedules.Count - 1 do
        FSchedules[i].Free;
      FSchedules.Clear;
    finally
      //      LeaveCriticalSection(FSchedulesLock);
    end;
  end;
end;

function TSchedulerThread.GetTimerTime(Time: TDateTime): Int64;
var
  ST: TSystemTime;
  FT: TFileTime;
begin
  DateTimeToSystemTime(Time, ST);
  if not SystemTimeToFileTime(ST, FT) then
    Exit(0);

  if not LocalFileTimeToFileTime(FT, FT) then
    Exit(0);

  Result := Int64(FT);
end;

procedure TSchedulerThread.Reload(ScheduleList: TList<TSchedulerSchedule>);
var
  i: Integer;
begin
  //  EnterCriticalSection(FSchedulesLock);
  try
    for i := 0 to FSchedules.Count - 1 do
      FSchedules[i].Free;
    FSchedules.Clear;

    for i := 0 to ScheduleList.Count - 1 do
      FSchedules.Add(ScheduleList[i]);

    SetEvent(FReloadEvent);
  finally
    //    LeaveCriticalSection(FSchedulesLock);
  end;
end;

procedure TSchedulerThread.SyncLog;
begin
  if Assigned(FOnLog) then
    FOnLog(FLogText, '');
end;

procedure TSchedulerThread.SyncSchedule;
begin
  if Assigned(FOnSchedule) then
    FOnSchedule(FIsStart, FScheduleID);
end;

{ TSchedulerSchedule }

constructor TSchedulerSchedule.Create(Schedule: TSchedule);
begin
  inherited Create;

  Assign(Schedule);

  FCalculatedStart := 0;
  FCalculatedEnd := 0;
end;

{ TScheduler }

constructor TScheduler.Create;
begin
  inherited;

  FSchedules := TList<TSchedule>.Create;
end;

destructor TScheduler.Destroy;
begin
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
  S: TSchedulerSchedule;
begin
  if not Active then
    Start;

  FThread.OnSchedule := nil;
  try
    FSchedules.Clear;

    if (ScheduleList = nil) and (FThread <> nil) and (not FThread.Terminated) then
    begin
      L := TList<TSchedulerSchedule>.Create;
      try
        FThread.Reload(L);

        // Wait for the new data to be loaded...
        WaitForSingleObject(FThread.FReloadDoneEvent, 500);
      finally
        L.Free;
      end;
      Exit;
    end;

    // If the thread is running create a list with copies of schedules for it to use.
    // The real schedules are cached in this component, so it is really important
    // to call this procedure whenever a schedule changes or is added/removed!
    if (FThread <> nil) and (not FThread.Terminated) then
    begin
      L := TList<TSchedulerSchedule>.Create;
      try
        for i := 0 to ScheduleList.Count - 1 do
          if ScheduleList[i].Active then
          begin
            FSchedules.Add(ScheduleList[i]);
            S := TSchedulerSchedule.Create(ScheduleList[i]);
            S.ID := FSchedules.Count - 1;
            L.Add(S);
          end;
        FThread.Reload(L);

        // Wait for the new data to be loaded...
        WaitForSingleObject(FThread.FReloadDoneEvent, 500);
      finally
        // Don't free the items - the thread owns them now.
        L.Free;
      end;
    end;
  finally
    FThread.OnSchedule := ThreadSchedule;
  end;
end;

procedure TScheduler.Start;
begin
  if FThread <> nil then
    Exit;

  FThread := TSchedulerThread.Create;
  FThread.OnLog := ThreadLog;
  FThread.OnSchedule := ThreadSchedule;
  FThread.OnTerminate := ThreadTerminate;
  FThread.Start;
end;

procedure TScheduler.Stop;
begin
  if (FThread <> nil) and (not FThread.Terminated) then
    FThread.Terminate;
end;

procedure TScheduler.ThreadLog(Text, Data: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Text, Data);
end;

procedure TScheduler.ThreadSchedule(IsStart: Boolean; ScheduleID: Integer);
begin
  try
    FOnSchedule(IsStart, FSchedules[ScheduleID]);
  except
  end;
end;

procedure TScheduler.ThreadTerminate(Sender: TObject);
var
  Restart: Boolean;
begin
  Restart := not FThread.Terminated;

  FThread := nil;

  if Restart then
    Start;
end;

end.
