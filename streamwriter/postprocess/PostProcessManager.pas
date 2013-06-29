{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, PostProcess, PostProcessSetTags,
  PostProcessSoX, Logging, PostProcessMP4Box, AddonBase, Forms, Functions,
  LanguageObjects, PostProcessConvert, Generics.Defaults, AudioFunctions, TypeDefs;

type
  TPostProcessManager = class
  private
    FProcessingList: TProcessingList;

    procedure WriteDebug(Sender: TObject; Text, Data: string; T: TDebugTypes; Level: TDebugLevels); overload;
    procedure WriteDebug(Sender: TObject; Text: string; T: TDebugTypes; Level: TDebugLevels); overload;

    function FindNextIdx(Entry: TProcessingEntry; Group: Integer): Integer;
    procedure BuildProcessingList(Entry: TProcessingEntry);

    procedure ThreadTerminate(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Terminate;

    function ProcessFile(Owner: TObject; Data: TPostProcessInformation): Boolean; overload;
    function ProcessFile(Entry: TProcessingEntry): Boolean; overload;
    procedure ReInitPostProcessors;
    function EnablePostProcess(Owner: TCustomForm; Enable: Boolean; PostProcess: TInternalPostProcess): Boolean;
    function WorkingForClient(Client: TObject): Boolean;
  end;

implementation

uses
  AppData, DownloadAddons, FileConvertor, ICEClient;

{ TPostProcessManager }

function TPostProcessManager.ProcessFile(Owner: TObject; Data: TPostProcessInformation): Boolean;
var
  Entry: TProcessingEntry;
begin
  Result := False;
  Entry := TProcessingEntry.Create(Owner, nil, Data);

  BuildProcessingList(Entry);

  if not ProcessFile(Entry) then
  begin
    Entry.Free;
  end else
  begin
    Result := True;

    WriteDebug(Entry.Owner, Format('Postprocessor "%s" starting.', [Entry.ActiveThread.PostProcessor.Name]), dtMessage, dlDebug);

    FProcessingList.Add(Entry);
  end;
end;

procedure TPostProcessManager.BuildProcessingList(Entry: TProcessingEntry);
var
  i: Integer;
  Client: TICEClient;

  function PostProcessingNeedsWave(Entry: TProcessingEntry): Boolean;
  var
    i: Integer;
    FormatChanged: Boolean;
    Client: TICEClient;
  begin
    Client := TICEClient(Entry.Owner);

    FormatChanged := TEncoderSettings(Entry.Data.EncoderSettings).AudioType <> atNone;

    if FormatChanged then
      Exit(True);

    for i := 0 to Client.Entry.Settings.PostProcessors.Count - 1 do
      if (Client.Entry.Settings.PostProcessors[i].Active) and
         ((Client.Entry.Settings.PostProcessors[i].CanProcess(Entry.Data, nil) and Client.Entry.Settings.PostProcessors[i].NeedsWave and
         ((Client.Entry.Settings.PostProcessors[i].OnlyIfCut and Entry.Data.WasCut) or (not Client.Entry.Settings.PostProcessors[i].OnlyIfCut))) or
         ((Client.Entry.Settings.PostProcessors[i] is TExternalPostProcess) and (TExternalPostProcess(Client.Entry.Settings.PostProcessors[i]).GroupID = 0))) then
      begin
        Exit(True);
      end;

    Exit(False);
  end;
begin
  Client := TICEClient(Entry.Owner);

  if Client.Entry.Settings.PostProcessors.Count = 0 then
    Exit;

  Entry.NeedsWave := PostProcessingNeedsWave(Entry);

  if Entry.NeedsWave then
  begin
    Entry.PostProcessList.Add(Client.Entry.Settings.PostProcessors[0].Copy);
  end;

  // Nach Order sortieren
  Client.Entry.Settings.PostProcessors.Sort(TComparer<TPostProcessBase>.Construct(
    function (const L, R: TPostProcessBase): integer
    begin
      Result := CmpInt(L.Order, R.Order);
    end
  ));

  // Erst die mit GroupID 0 fitmachen (WAVE-Phase)
  for i := 0 to Client.Entry.Settings.PostProcessors.Count - 1 do
    if Client.Entry.Settings.PostProcessors[i].Active and (Client.Entry.Settings.PostProcessors[i].CanProcess(Entry.Data, Client.Entry.Settings.PostProcessors)) and (not Client.Entry.Settings.PostProcessors[i].Hidden) and
       (Client.Entry.Settings.PostProcessors[i].GroupID = 0) and ((Client.Entry.Settings.PostProcessors[i].OnlyIfCut and Entry.Data.WasCut) or (not Client.Entry.Settings.PostProcessors[i].OnlyIfCut)) then
    begin
      Entry.PostProcessList.Add(Client.Entry.Settings.PostProcessors[i].Copy);
    end;

  if Entry.NeedsWave then
  begin
    Entry.PostProcessList.Add(Client.Entry.Settings.PostProcessors[0].Copy);
  end;

  // Jetzt GroupID 1 (Nach WAVE-Phase)
  for i := 0 to Client.Entry.Settings.PostProcessors.Count - 1 do
    if Client.Entry.Settings.PostProcessors[i].Active and (Client.Entry.Settings.PostProcessors[i].CanProcess(Entry.Data, Client.Entry.Settings.PostProcessors)) and (not Client.Entry.Settings.PostProcessors[i].Hidden) and
       (Client.Entry.Settings.PostProcessors[i].GroupID = 1) and ((Client.Entry.Settings.PostProcessors[i].OnlyIfCut and Entry.Data.WasCut) or (not Client.Entry.Settings.PostProcessors[i].OnlyIfCut)) then
    begin
      Client.Entry.Settings.PostProcessors[i].CanProcess(Entry.Data, Entry.PostProcessList);
      Entry.PostProcessList.Add(Client.Entry.Settings.PostProcessors[i].Copy);
    end;
end;

function TPostProcessManager.ProcessFile(Entry: TProcessingEntry): Boolean;
var
  NextIdx: Integer;
begin
  Result := False;

  if Entry.PostProcessList.Count = 0 then
    Exit(False);

  NextIdx := FindNextIdx(Entry, 0);
  if NextIdx > -1 then
    Exit(True);

  if Entry.Data.WorkFilename <> '' then
    DeleteFile(PChar(Entry.Data.WorkFilename));

  if Entry.Data.ReEncodedFilename <> '' then
  begin
    DeleteFile(PChar(Entry.Data.Filename));

    Entry.Data.Filename := Entry.Data.FilenameConverted;
    MoveFileEx(PChar(Entry.Data.ReEncodedFilename), PChar(Entry.Data.FilenameConverted), MOVEFILE_REPLACE_EXISTING);

    Entry.Data.WorkFilename := '';
    Entry.Data.ReEncodedFilename := '';
  end;

  NextIdx := FindNextIdx(Entry, 1);
  if NextIdx > -1 then
    Exit(True);
end;

procedure TPostProcessManager.ReInitPostProcessors;
var
  i: Integer;
begin
  for i := 0 to AppGlobals.StreamSettings.PostProcessors.Count - 1 do
    AppGlobals.StreamSettings.PostProcessors[i].Initialize;
end;

procedure TPostProcessManager.Terminate;
var
  i: Integer;
begin
  for i := 0 to FProcessingList.Count - 1 do
    if FProcessingList[i].ActiveThread <> nil then
      FProcessingList[i].ActiveThread.Terminate;
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
          WriteDebug(Entry.Owner, Format(_('Postprocessor "%s" successfully finished.'), [Entry.ActiveThread.PostProcessor.Name]), dtPostProcess, dlNormal);
        arTimeout:
          WriteDebug(Entry.Owner, Format(_('Postprocessor "%s" timed out.'), [Entry.ActiveThread.PostProcessor.Name]), dtError, dlNormal);
        arFail:
          WriteDebug(Entry.Owner, Format(_('Postprocessor "%s" failed.'), [Entry.ActiveThread.PostProcessor.Name]), dtError, dlNormal);
      end;

      // Wenn das Result nicht gut ist, dann wird die Chain hier beendet und der Song gilt als gespeichert.
      if Entry.ActiveThread.Terminated or ((Entry.ActiveThread.Result <> arWin) and (Entry.ActiveThread.Result <> arImpossible)) then
      begin
        if Entry.Data.WorkFilename <> '' then
          DeleteFile(PChar(Entry.Data.WorkFilename));
        if Entry.Data.ReEncodedFilename <> '' then
          DeleteFile(PChar(Entry.Data.ReEncodedFilename));

        // Wird hier gemacht, damit WorkingForClient() False zurückliefert. Wichtig!
        FProcessingList.Remove(Entry);

        TICEClient(Entry.Owner).PostProcessingFinished(Entry.Data.Filename, Entry.Data.StreamTitle, Entry.Data.Artist, Entry.Data.Title,
          Entry.Data.ServerTitle, Entry.Data.Filesize, Entry.Data.Length, 0, False, Entry.Data.WasCut, Entry.Data.FullTitle, False,
          Entry.Data.ServerTitleHash, Entry.Data.ServerArtistHash);

        Entry.Free;

        Exit;
      end;


      // Eine externe App könnte das File gelöscht haben
      if Entry.Data.Filesize <> High(UInt64) then // GetFileSize = Int64 => -1
      begin
        if ProcessFile(Entry) then
        begin
          WriteDebug(Entry.Owner, Format('Postprocessor "%s" starting.', [Entry.ActiveThread.PostProcessor.Name]), dtMessage, dlDebug);

        end else
        begin
          WriteDebug(Entry.Owner, 'All postprocessors done', dtMessage, dlDebug);

          // Wird hier gemacht, damit WorkingForClient() False zurückliefert. Wichtig!
          FProcessingList.Delete(i);

          TICEClient(Entry.Owner).PostProcessingFinished(Entry.Data.Filename, Entry.Data.StreamTitle, Entry.Data.Artist, Entry.Data.Title,
          Entry.Data.ServerTitle, Entry.Data.Filesize, Entry.Data.Length, Entry.Data.BitRate, Entry.Data.VBR, Entry.Data.WasCut,
            Entry.Data.FullTitle, False, Entry.Data.ServerTitleHash, Entry.Data.ServerArtistHash);

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
  FProcessingList := TProcessingList.Create;

  for i := 0 to AppGlobals.StreamSettings.PostProcessors.Count - 1 do
    if AppGlobals.StreamSettings.PostProcessors[i].ClassType.InheritsFrom(TInternalPostProcess) then
    begin
      IP := TInternalPostProcess(AppGlobals.StreamSettings.PostProcessors[i]);
      if (IP.Active) and (not IP.DependenciesMet) then
        IP.Active := False;
    end;

  i := 0;
  repeat
    AppGlobals.Storage.Read('Exe_' + IntToStr(i), App, '', 'Plugins');
    AppGlobals.Storage.Read('Params_' + IntToStr(i), Params, '', 'Plugins');
    AppGlobals.Storage.Read('OrderExe_' + IntToStr(i), Order, 0, 'Plugins');
    AppGlobals.Storage.Read('Active_' + IntToStr(i), Active, False, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + IntToStr(i), OnlyIfCut, False, 'Plugins');
    AppGlobals.Storage.Read('Group_' + IntToStr(i), GroupID, 1, 'Plugins');
    if App <> '' then
    begin
      EP := TExternalPostProcess.Create(App, Params, Active, OnlyIfCut, i, Order, GroupID);
      try
        AppGlobals.StreamSettings.PostProcessors.Add(EP);
      except

      end;
    end;
    Inc(i);
  until (App = '');

  for i := 0 to AppGlobals.StreamSettings.EncoderSettings.Count - 1 do
    AppGlobals.StreamSettings.EncoderSettings[i].Load;
end;

destructor TPostProcessManager.Destroy;
begin
  FreeAndNil(FProcessingList);

  inherited;
end;

function TPostProcessManager.EnablePostProcess(Owner: TCustomForm;
  Enable: Boolean; PostProcess: TInternalPostProcess): Boolean;
var
  i: Integer;
begin
  if not Enable then
    Exit(False);

  if Enable and (not PostProcess.DependenciesMet) then
  begin
    if not PostProcess.ShowInitMessage(Owner.Handle) then
    begin
      Exit(False);
    end;

    if MsgBox(Owner.Handle, _('This postprocessor needs additional addons. Do you want to download these addons now?'), _('Question'), MB_YESNO or MB_DEFBUTTON1 or MB_ICONQUESTION) = IDYES then
    begin
      for i := 0 to PostProcess.NeededAddons.Count - 1 do
        if not AppGlobals.AddonManager.EnableAddon(Owner, AppGlobals.AddonManager.Find(PostProcess.NeededAddons[i]), False) then
          Exit(False);
    end else
      Exit(False);
  end;

  if Enable and (not PostProcess.DependenciesMet) then
  begin
    MsgBox(Owner.Handle, _('The postprocessor is not ready for use. This might happen when required addons'' files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);
    Exit(False);
  end;

  Exit(True);
end;

function TPostProcessManager.FindNextIdx(Entry: TProcessingEntry;
  Group: Integer): Integer;
var
  i, NextIdx: Integer;
  Output: string;
begin
  Result := -1;

  if Entry.ActivePostProcessIndex > -1 then
    NextIdx := Entry.ActivePostProcessIndex + 1
  else
    NextIdx := 0;

  for i := NextIdx to Entry.PostProcessList.Count - 1 do
  begin
    if Entry.PostProcessList[i].CanProcess(Entry.Data, Entry.PostProcessList) and (Group = Entry.PostProcessList[i].GroupID) then
    begin
      Entry.ActiveThread := Entry.PostProcessList[i].ProcessFile(Entry.Data);
      if Entry.ActiveThread <> nil then
      begin
        Entry.ActivePostProcessIndex := i;

        if Entry.PostProcessList[i] is TPostProcessConvert then
        begin
          if Entry.Data.WorkFilename = '' then
          begin
            Entry.Data.WorkFilename := RemoveFileExt(Entry.Data.Filename) + '_temp.wav';
            TPostProcessConvertThread(Entry.ActiveThread).Convert(Entry.Data.Filename, Entry.Data.WorkFilename, nil);
          end else if Entry.Data.ReEncodedFilename = '' then
          begin
            if TEncoderSettings(Entry.Data.EncoderSettings).AudioType = atNone then
              Output := ExtractFileExt(Entry.Data.Filename)
            else
              Output := FormatToFiletype(TEncoderSettings(Entry.Data.EncoderSettings).AudioType);

            Entry.Data.ReEncodedFilename := RemoveFileExt(Entry.Data.Filename) + '_temp' + Output;
            TPostProcessConvertThread(Entry.ActiveThread).Convert(Entry.Data.WorkFilename, Entry.Data.ReEncodedFilename, Entry.Data.EncoderSettings);
          end;
        end;

        Entry.ActiveThread.OnTerminate := ThreadTerminate;
        Entry.ActiveThread.Resume;

        Exit(i);
      end;
    end;
  end;
end;

end.


