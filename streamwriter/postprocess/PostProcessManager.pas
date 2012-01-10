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

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, PostProcess, PostProcessSetTags,
  PostProcessSoX, Logging, PostProcessAAC, PluginBase, Forms, Functions,
  LanguageObjects, TypeDefs, PostProcessConvert;

type
  TPostProcessManager = class
  private
    FPostProcessors: TList<TPostProcessBase>;
    FActivePlugin: TPostProcessBase;
    FProcessingList: TProcessingList;

    procedure WriteDebug(Sender: TObject; Text, Data: string; T: TDebugTypes; Level: TDebugLevels); overload;
    procedure WriteDebug(Sender: TObject; Text: string; T: TDebugTypes; Level: TDebugLevels); overload;

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

    property PostProcessors: TList<TPostProcessBase> read FPostProcessors;
  end;

implementation

uses
  AppData, DownloadAddons, FileConvertor, ICEClient;

{ TPluginManager }

// TODO: HIER ALLES CHECKEN WAS MIT POSTPROCESSING ZU TUN HAT!!!
// TODO: und sehen, was passiert, wenn noch threads laufen aber app beendet wird..

function TPostProcessManager.ProcessFile(Owner: TObject; Data: TPluginProcessInformation): Boolean;
var
  Entry: TProcessingEntry;
begin
  Result := False;
  Entry := TProcessingEntry.Create(Owner, nil, Data, atMPEG); // TODO: AppGlobals.OutputFormat);
  if not ProcessFile(Entry) then
  begin
    Entry.Free;

    // TODO: Was soll hier passieren?
  end else
  begin
    Result := True;

    WriteDebug(Entry.Owner, Format('Plugin "%s" starting.', [Entry.ActiveThread.Plugin.Name]), dtMessage, dlDebug);

    FProcessingList.Add(Entry);
  end;
end;

// TODO: Der PPManager sollte die gesamte kontrolle übers postprocessing haben.
//       sprich erst decoden wenn plugins an sind, die das wollen, dann die plugins durchlaufen,
//       dann ins wunschformat konvertieren und danach plugins ohne wave-format-benötigt durchlaufen.
//       dabei auch threads etc verwalten!

function TPostProcessManager.ProcessFile(Entry: TProcessingEntry): Boolean;
var
  i, n, Order, SmallestActive: Integer;
  Output: string;

  function FiletypeToFormat(Filename: string): TAudioTypes;
  begin
    Filename := LowerCase(ExtractFileExt(Filename));

    if Filename = '.mp3' then
      Exit(atMPEG)
    else if Filename = '.aac' then
      Exit(atAAC)
    else if Filename = '.ogg' then
      Exit(atOGG);
  end;

  function FormatToFiletype(Format: TAudioTypes): string;
  begin
    Result := '';
    case Format of
      atNone: ;
      atMPEG: Result := '.mp3';
      atAAC: Result := '.aac';
      atOGG: Result := '.ogg';
    end;
  end;

  function PostProcessingNeedsWave: Boolean;
  var
    i: Integer;
  begin
    if Entry.OutputFormat <> FiletypeToFormat(Entry.Data.Filename) then
      Exit(True);

    for i := 0 to FPostProcessors.Count - 1 do
      if FPostProcessors[i].CanProcess(Entry.Data) and FPostProcessors[i].Active and FPostProcessors[i].NeedsWave then
        Exit(True);

    Exit(False);
  end;
begin
  Result := False;
  Order := -1;

  // Das soeben beendete Plugin der Liste hinzufügen
  if Entry.ActiveThread <> nil then
  begin
    Entry.PluginsProcessed.Add(TPostProcessThreadBase(Entry.ActiveThread).Plugin);
    if not (Entry.ActiveThread.Plugin is TPostProcessConvert) then
      Order := Entry.ActiveThread.Plugin.Order;
    Entry.ActiveThread := nil;
  end else
    Entry.NeedsWave := PostProcessingNeedsWave;

  // Erstmal sehen, ob wir dekodieren müssen. Falls ja startet zuerst ein Thread zum dekodieren.
  // TODO: Das hier machen wir natürlich nur, wenn auch irgendein plugin das wave format braucht, z.b. sox.
  if Entry.NeedsWave and (not Entry.PluginsProcessed.Contains(FPostProcessors[0])) then
  begin
    // TODO: Was passiert wenn für TPostProcessConvert ein Plugin nicht am start ist???
    // TODO: Nach jedem schritt result vom vorherigen auswerten..

    Entry.ActiveThread := TPostProcessConvertThread.Create(Entry.Data, FPostProcessors[0]); // todo: hinten objekt freigeben!
    Entry.ActiveThread.OnTerminate := ThreadTerminate;
    Entry.Data.WorkFilename := RemoveFileExt(Entry.Data.Filename) + '_convert' + '.wav';

    TPostProcessConvertThread(Entry.ActiveThread).Convert(Entry.Data.Filename, RemoveFileExt(Entry.Data.Filename) + '_convert' + '.wav');

    Exit(True);
  end;

  SmallestActive := MaxInt;
  for i := 0 to FPostProcessors.Count - 1 do
    if FPostProcessors[i].Active and (FPostProcessors[i].Order < SmallestActive) and (FPostProcessors[i].Order >= Order) and
       (not Entry.PluginsProcessed.Contains(FPostProcessors[i])) and
       (FPostProcessors[i].CanProcess(Entry.Data)) and (not FPostProcessors[i].Hidden) and
       ((FPostProcessors[i].OnlyIfCut and Entry.Data.WasCut) or (not FPostProcessors[i].OnlyIfCut)) then
    begin
      SmallestActive := FPostProcessors[i].Order;
    end;

  if SmallestActive <> MaxInt then
  begin
    for i := 0 to FPostProcessors.Count - 1 do
      if FPostProcessors[i].Order = SmallestActive then
      begin
        Entry.ActiveThread := FPostProcessors[i].ProcessFile(Entry.Data);
        Result := Entry.ActiveThread <> nil;
        if Result then
        begin
          Entry.ActiveThread.OnTerminate := ThreadTerminate;
          Entry.ActiveThread.Resume;
        end;
        Break;
      end;
  end;

  if (Entry.ActiveThread = nil) and (Entry.NeedsWave) then
  begin
    // Von WAVE in ein anderes Format umwandeln wenn alles gelaufen ist
    if Entry.Data.ReEncodedFilename = '' then
    begin
      Output := FormatToFiletype(Entry.OutputFormat);
      if Output = '' then
        Output := ExtractFileExt(Entry.Data.Filename);
      Entry.ActiveThread := TPostProcessConvertThread.Create(Entry.Data, FPostProcessors[0]); // todo: hinten objekt freigeben!
      Entry.ActiveThread.OnTerminate := ThreadTerminate;
      Entry.Data.ReEncodedFilename := RemoveFileExt(Entry.Data.Filename) + '_convert' + Output;
      TPostProcessConvertThread(Entry.ActiveThread).Convert(Entry.Data.WorkFilename, Entry.Data.ReEncodedFilename);

      Exit(True);
    end else
    begin
      // Jetzt noch die Datei, die umgewandelt wurde, in den Original-Dateinamen umbenennen

      DeleteFile(PChar(Entry.Data.WorkFilename));

      // TODO: FileSize und so übernehmen für den saved track!

      MoveFileEx(PChar(Entry.Data.ReEncodedFilename), PChar(Entry.Data.Filename), MOVEFILE_REPLACE_EXISTING);

      Exit(False);
    end;
  end;
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
  // TODO: Der ConverThread braucht ne property, ob er erfolgreich war, die ich hier auswerten kann!!!

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

      // Eine externe App könnte das File gelöscht haben
      if Entry.Data.Filesize <> High(UInt64) then // GetFileSize = Int64 => -1
      begin
        if ProcessFile(Entry) then
        begin
          WriteDebug(Entry.Owner, Format('Postprocessor "%s" starting.', [Entry.ActiveThread.Plugin.Name]), dtMessage, dlDebug);

        end else
        begin
          WriteDebug(Entry.Owner, 'All postprocessors done', dtMessage, dlDebug);

          {
          if Assigned(FOnSongSaved) then
            FOnSongSaved(Self, Entry.Data.Filename, Entry.Data.StreamTitle,
              Entry.Data.Artist, Entry.Data.Title, Entry.Data.Filesize, Entry.Data.Length,
              Entry.Data.WasCut, Entry.Data.FullTitle, False);
          if Assigned(FOnRefresh) then
            FOnRefresh(Self);
          }

          Entry.Free;
          FProcessingList.Delete(i);
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
  Order: Integer;
begin
  FActivePlugin := nil;
  FPostProcessors := TList<TPostProcessBase>.Create;
  FProcessingList := TProcessingList.Create;

  // Der Convert muss der erste sein! Greife auf die Liste mal mit [0] zu!
  PostProcessors.Add(TPostProcessConvert.Create);
  PostProcessors.Add(TPostProcessSetTags.Create);
  PostProcessors.Add(TPostProcessSoX.Create);
  PostProcessors.Add(TPostProcessAAC.Create);

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
    if App <> '' then
    begin
      EP := TExternalPostProcess.Create(App, Params, Active, OnlyIfCut, i, Order);
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
