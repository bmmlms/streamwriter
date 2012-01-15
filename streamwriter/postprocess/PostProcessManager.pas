{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2012 Alexander Nottelmann

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
unit PostProcessManager;

// TODO: Immer wenn ich convertiere sollte ich die UR-BITRATE beibehalten und so.
// TODO: ein aac stream hat 48 bitrate. man encoded zu mp3. dann hat der auch nur 48, das ist doch mist!

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, PostProcess, PostProcessSetTags,
  PostProcessSoX, Logging, PostProcessMP4Box, PluginBase, Forms, Functions,
  LanguageObjects, TypeDefs, PostProcessConvert, Generics.Defaults;

type
  TPostProcessManager = class
  private
    FPostProcessors: TList<TPostProcessBase>;
    FProcessingList: TProcessingList;

    procedure WriteDebug(Sender: TObject; Text, Data: string; T: TDebugTypes; Level: TDebugLevels); overload;
    procedure WriteDebug(Sender: TObject; Text: string; T: TDebugTypes; Level: TDebugLevels); overload;

    function FindNextIdx(Entry: TProcessingEntry; Group: Integer): Integer;
    procedure BuildProcessingList(Entry: TProcessingEntry);

    procedure ThreadTerminate(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function ProcessFile(Owner: TObject; Data: TPluginProcessInformation): Boolean; overload;
    function ProcessFile(Entry: TProcessingEntry): Boolean; overload;
    procedure ReInitPlugins;
    function Find(Plugin: TPostProcessBase): TPostProcessBase; overload;
    function Find(ClassType: TClass): TPostProcessBase; overload;
    function EnablePostProcess(Owner: TCustomForm; Enable: Boolean; PostProcess: TInternalPostProcess): Boolean;
    function WorkingForClient(Client: TObject): Boolean;

    property PostProcessors: TList<TPostProcessBase> read FPostProcessors;
  end;

implementation

uses
  AppData, DownloadAddons, FileConvertor, ICEClient;

{ TPluginManager }

function TPostProcessManager.ProcessFile(Owner: TObject; Data: TPluginProcessInformation): Boolean;
var
  Entry: TProcessingEntry;
begin
  Result := False;
  Entry := TProcessingEntry.Create(Owner, nil, Data);
  if not ProcessFile(Entry) then
  begin
    Entry.Free;
  end else
  begin
    Result := True;

    WriteDebug(Entry.Owner, Format('Plugin "%s" starting.', [Entry.ActiveThread.Plugin.Name]), dtMessage, dlDebug);

    FProcessingList.Add(Entry);
  end;
end;

procedure TPostProcessManager.BuildProcessingList(Entry: TProcessingEntry);
var
  i, n: Integer;

  function PostProcessingNeedsWave: Boolean;
  var
    i: Integer;
  begin
    if (Entry.Data.OutputFormat <> atNone) and (Entry.Data.OutputFormat <> FiletypeToFormat(Entry.Data.Filename)) then
      Exit(True);

    for i := 0 to FPostProcessors.Count - 1 do
      if FPostProcessors[i].CanProcess(Entry.Data) and FPostProcessors[i].Active and FPostProcessors[i].NeedsWave then
        Exit(True);

    Exit(False);
  end;
begin
  Entry.NeedsWave := PostProcessingNeedsWave;

  if Entry.NeedsWave then
  begin
    Entry.PluginList.Add(FPostProcessors[0]);
  end;

  // Nach Order sortieren
  FPostProcessors.Sort(TComparer<TPostProcessBase>.Construct(
    function (const L, R: TPostProcessBase): integer
    begin
      Result := CmpInt(L.Order, R.Order);;
    end
  ));

  // Erst die mit GroupID 0 fitmachen (WAVE-Phase)
  for i := 0 to FPostProcessors.Count - 1 do
    if FPostProcessors[i].Active and (FPostProcessors[i].CanProcess(Entry.Data)) and (not FPostProcessors[i].Hidden) and
       (FPostProcessors[i].GroupID = 0) and ((FPostProcessors[i].OnlyIfCut and Entry.Data.WasCut) or (not FPostProcessors[i].OnlyIfCut)) then
    begin
      Entry.PluginList.Add(FPostProcessors[i].Copy);
    end;

  if Entry.NeedsWave then
  begin
    Entry.PluginList.Add(FPostProcessors[0]);
  end;

  // Jetzt GroupID 1 (Nach WAVE-Phase)
  for i := 0 to FPostProcessors.Count - 1 do
    if FPostProcessors[i].Active and (FPostProcessors[i].CanProcess(Entry.Data)) and (not FPostProcessors[i].Hidden) and
       (FPostProcessors[i].GroupID = 1) and ((FPostProcessors[i].OnlyIfCut and Entry.Data.WasCut) or (not FPostProcessors[i].OnlyIfCut)) then
    begin
      Entry.PluginList.Add(FPostProcessors[i].Copy);
    end;
end;

function TPostProcessManager.ProcessFile(Entry: TProcessingEntry): Boolean;
var
  i, n, Order, SmallestActive: Integer;
  NextIdx: Integer;
  Output: string;

  function PostProcessingNeedsWave: Boolean;
  var
    i: Integer;
  begin
    if Entry.Data.OutputFormat <> FiletypeToFormat(Entry.Data.Filename) then
      Exit(True);

    for i := 0 to FPostProcessors.Count - 1 do
      if FPostProcessors[i].CanProcess(Entry.Data) and FPostProcessors[i].Active and FPostProcessors[i].NeedsWave then
        Exit(True);

    Exit(False);
  end;
begin
  Result := False;

  if Entry.PluginList.Count = 0 then
    BuildProcessingList(Entry);

  if Entry.PluginList.Count = 0 then
    Exit(False);

  NextIdx := FindNextIdx(Entry, 0);
  if NextIdx > -1 then
    Exit(True);

  if Entry.Data.WorkFilename <> '' then
    DeleteFile(PChar(Entry.Data.WorkFilename));

  if Entry.Data.ReEncodedFilename <> '' then
  begin
    DeleteFile(PChar(Entry.Data.Filename));
    Entry.Data.Filename := RemoveFileExt(Entry.Data.Filename) + FormatToFiletype(Entry.Data.OutputFormat);
    MoveFileEx(PChar(Entry.Data.ReEncodedFilename), PChar(Entry.Data.Filename), MOVEFILE_REPLACE_EXISTING);

    Entry.Data.WorkFilename := '';
    Entry.Data.ReEncodedFilename := '';
  end;

  NextIdx := FindNextIdx(Entry, 1);
  if NextIdx > -1 then
    Exit(True);
end;

procedure TPostProcessManager.ReInitPlugins;
var
  i: Integer;
begin
  for i := 0 to FPostProcessors.Count - 1 do
    FPostProcessors[i].Initialize;
end;

procedure TPostProcessManager.ThreadTerminate(Sender: TObject);
var
  i: Integer;
  Entry: TProcessingEntry;
begin
  for i := 0 to FProcessingList.Count - 1 do
  begin
    if FProcessingList[i].ActiveThread = Sender then
    begin
      Entry := FProcessingList[i];

      case Entry.ActiveThread.Result of
        arWin:
          WriteDebug(Entry.Owner, Format(_('Postprocessor "%s" successfully finished.'), [Entry.ActiveThread.Plugin.Name]), dtPlugin, dlNormal);
        arTimeout:
          WriteDebug(Entry.Owner, Format(_('Postprocessor "%s" timed out.'), [Entry.ActiveThread.Plugin.Name]), dtError, dlNormal);
        arFail:
          WriteDebug(Entry.Owner, Format(_('Postprocessor "%s" failed.'), [Entry.ActiveThread.Plugin.Name]), dtError, dlNormal);
      end;


      // Wenn das Result nicht gut ist, dann wird die Chain hier beendet und der Song gilt als gespeichert.
      if (Entry.ActiveThread.Result <> arWin) and (Entry.ActiveThread.Result <> arImpossible) then
      begin
        if Entry.Data.WorkFilename <> '' then
          DeleteFile(PChar(Entry.Data.WorkFilename));
        if Entry.Data.ReEncodedFilename <> '' then
          DeleteFile(PChar(Entry.Data.ReEncodedFilename));

        // Wird hier gemacht, damit WorkingForClient() false zurückliefert. Wichtig!
        FProcessingList.Remove(Entry);

        TICEClient(Entry.Owner).PostProcessingFinished(Entry.Data.Filename, Entry.Data.StreamTitle, Entry.Data.Artist, Entry.Data.Title,
          Entry.Data.Filesize, Entry.Data.Length, Entry.Data.WasCut, Entry.Data.FullTitle, False);

        Entry.Free;

        Exit;
      end;


      // Eine externe App könnte das File gelöscht haben
      if Entry.Data.Filesize <> High(UInt64) then // GetFileSize = Int64 => -1
      begin
        if ProcessFile(Entry) then
        begin
          WriteDebug(Entry.Owner, Format('Postprocessor "%s" starting.', [Entry.ActiveThread.Plugin.Name]), dtMessage, dlDebug);

        end else
        begin
          WriteDebug(Entry.Owner, 'All postprocessors done', dtMessage, dlDebug);

          // Wird hier gemacht, damit WorkingForClient() false zurückliefert. Wichtig!
          FProcessingList.Delete(i);

          TICEClient(Entry.Owner).PostProcessingFinished(Entry.Data.Filename, Entry.Data.StreamTitle, Entry.Data.Artist, Entry.Data.Title,
            Entry.Data.Filesize, Entry.Data.Length, Entry.Data.WasCut, Entry.Data.FullTitle, False);

          Entry.Free;
        end;
      end else
      begin
        WriteDebug(Entry.Owner, _('An external application or postprocessor has deleted the saved file.'), dtError, dlNormal);

        Entry.Free;
        FProcessingList.Delete(i);
      end;

      Break;
    end;
  end;
end;

procedure TPostProcessManager.WriteDebug(Sender: TObject; Text,
  Data: string; T: TDebugTypes; Level: TDebugLevels);
begin
  if Sender <> nil then
    TICEClient(Sender).WriteDebug(Text, Data, T, Level);
end;

function TPostProcessManager.WorkingForClient(Client: TObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FProcessingList.Count - 1 do
    if FProcessingList[i].Owner = Client then
      Exit(True);
end;

procedure TPostProcessManager.WriteDebug(Sender: TObject; Text: string;
  T: TDebugTypes; Level: TDebugLevels);
begin
  if Sender <> nil then
    TICEClient(Sender).WriteDebug(Text, T, Level);
end;

constructor TPostProcessManager.Create;
var
  i: Integer;
  App, Params: string;
  IP: TInternalPostProcess;
  EP: TExternalPostProcess;
  Active, OnlyIfCut: Boolean;
  Order, GroupID: Integer;
begin
  FPostProcessors := TList<TPostProcessBase>.Create;
  FProcessingList := TProcessingList.Create;

  // Der Convert muss der erste sein! Greife auf die Liste mal mit [0] zu!
  PostProcessors.Add(TPostProcessConvert.Create);
  PostProcessors.Add(TPostProcessSetTags.Create);
  PostProcessors.Add(TPostProcessSoX.Create);
  PostProcessors.Add(TPostProcessMP4Box.Create);

  for i := 0 to PostProcessors.Count - 1 do
    if PostProcessors[i].ClassType.InheritsFrom(TInternalPostProcess) then
    begin
      IP := TInternalPostProcess(PostProcessors[i]);
      if (IP.Active) and (not IP.DependenciesMet) then
        IP.Active := False;
    end;

  i := 0;
  repeat
    AppGlobals.Storage.Read('Exe_' + IntToStr(i), App, '', 'Plugins');
    AppGlobals.Storage.Read('Params_' + IntToStr(i), Params, '', 'Plugins');
    AppGlobals.Storage.Read('OrderExe_' + IntToStr(i), Order, 0, 'Plugins');
    AppGlobals.Storage.Read('Active_' + IntToStr(i), Active, True, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + IntToStr(i), OnlyIfCut, False, 'Plugins');
    AppGlobals.Storage.Read('Group_' + IntToStr(i), GroupID, 1, 'Plugins');
    if App <> '' then
    begin
      EP := TExternalPostProcess.Create(App, Params, Active, OnlyIfCut, i, Order, GroupID);
      try
        PostProcessors.Add(EP);
      except

      end;
    end;
    Inc(i);
  until (App = '');
end;

destructor TPostProcessManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPostProcessors.Count - 1 do
    FPostProcessors[i].Free;
  FPostProcessors.Free;
  FreeAndNil(FProcessingList);
  inherited;
end;

function TPostProcessManager.EnablePostProcess(Owner: TCustomForm;
  Enable: Boolean; PostProcess: TInternalPostProcess): Boolean;
var
  i: Integer;
  Res: Integer;
  DA: TfrmDownloadAddons;
begin
  if not Enable then
    Exit(False);

  if Enable and (not PostProcess.DependenciesMet) then
  begin
    if not PostProcess.ShowInitMessage(Owner.Handle) then
    begin
      Exit(False);
    end;

    if MsgBox(Owner.Handle, _('This postprocessor needs additional plugins. Do you want to download these plugins now?'), _('Question'), MB_YESNO or MB_DEFBUTTON1 or MB_ICONQUESTION) = IDYES then
    begin
      for i := 0 to PostProcess.NeededPlugins.Count - 1 do
        if not AppGlobals.PluginManager.EnablePlugin(Owner, AppGlobals.PluginManager.Find(PostProcess.NeededPlugins[i]), False) then
          Exit(False);
    end else
      Exit(False);
  end;

  if Enable and (not PostProcess.DependenciesMet) then
  begin
    MsgBox(Owner.Handle, _('The postprocessor is not ready for use. This might happen when required plugins'' files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);
    Exit(False);
  end;

  Exit(True);
end;

function TPostProcessManager.Find(ClassType: TClass): TPostProcessBase;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FPostProcessors.Count - 1 do
    if FPostProcessors[i].ClassType = ClassType then
    begin
      Result := FPostProcessors[i];
      Break;
    end;
end;

function TPostProcessManager.FindNextIdx(Entry: TProcessingEntry;
  Group: Integer): Integer;
var
  i, NextIdx: Integer;
begin
  Result := -1;

  if Entry.ActivePluginIndex > -1 then
    NextIdx := Entry.ActivePluginIndex + 1
  else
    NextIdx := 0;

  for i := NextIdx to Entry.PluginList.Count - 1 do
  begin
    if Entry.PluginList[i].CanProcess(Entry.Data) and (Group = Entry.PluginList[i].GroupID) then
    begin
      Entry.ActiveThread := Entry.PluginList[i].ProcessFile(Entry.Data);
      if Entry.ActiveThread <> nil then
      begin
        Entry.ActivePluginIndex := i;

        if Entry.PluginList[i] is TPostProcessConvert then
        begin
          if Entry.Data.WorkFilename = '' then
          begin
            Entry.Data.WorkFilename := RemoveFileExt(Entry.Data.Filename) + '_convert.wav';
            TPostProcessConvertThread(Entry.ActiveThread).Convert(Entry.Data.Filename, Entry.Data.WorkFilename, Entry.Data.BitRate);
          end else if Entry.Data.ReEncodedFilename = '' then
          begin
            Entry.Data.ReEncodedFilename := RemoveFileExt(Entry.Data.Filename) + '_convert' + FormatToFiletype(Entry.Data.OutputFormat);
            TPostProcessConvertThread(Entry.ActiveThread).Convert(Entry.Data.WorkFilename, Entry.Data.ReEncodedFilename, Entry.Data.BitRate);
          end;
        end;

        Entry.ActiveThread.OnTerminate := ThreadTerminate;
        Entry.ActiveThread.Resume;

        Exit(i);
      end;
    end;
  end;
end;

function TPostProcessManager.Find(Plugin: TPostProcessBase): TPostProcessBase;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FPostProcessors.Count - 1 do
    if (Plugin is TExternalPostProcess) and (FPostProcessors[i] is TExternalPostProcess) then
    begin
      if TExternalPostProcess(Plugin).Identifier = TExternalPostProcess(FPostProcessors[i]).Identifier then
      begin
        Result := FPostProcessors[i];
        Break;
      end;
    end else if Plugin.ClassType.InheritsFrom(TInternalPostProcess) and FPostProcessors[i].ClassType.InheritsFrom(TInternalPostProcess) then
    begin
      if Plugin.ClassType = FPostProcessors[i].ClassType then
      begin
        Result := FPostProcessors[i];
        Break;
      end;
    end;
end;

end.
