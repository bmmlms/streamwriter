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

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, PostProcess, PostProcessSetTags,
  PostProcessSoX, Logging, PostProcessAAC, PluginBase, Forms, Functions,
  LanguageObjects;

type
  TPostProcessManager = class
  private
    FPostProcessors: TList<TPostProcessBase>;
    FActivePlugin: TPostProcessBase;
  public
    constructor Create;
    destructor Destroy; override;

    function ProcessFile(Data: TPluginProcessInformation): TProcessingEntry; overload;
    function ProcessFile(Entry: TProcessingEntry): Boolean; overload;
    procedure ReInitPlugins;
    function Find(Plugin: TPostProcessBase): TPostProcessBase; overload;
    function Find(ClassType: TClass): TPostProcessBase; overload;
    function EnablePostProcess(Owner: TCustomForm; Enable: Boolean; PostProcess: TInternalPostProcess): Boolean;

    property PostProcessors: TList<TPostProcessBase> read FPostProcessors;
  end;

implementation

uses
  AppData, DownloadAddons;

{ TPluginManager }

function TPostProcessManager.ProcessFile(Data: TPluginProcessInformation): TProcessingEntry;
begin
  Result := TProcessingEntry.Create(nil, Data, nil);
  if not ProcessFile(Result) then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function TPostProcessManager.ProcessFile(Entry: TProcessingEntry): Boolean;
var
  i, Order, SmallestActive: Integer;
begin
  Result := False;
  Order := 0;

  // Das soeben beendete Plugin der Liste hinzufügen
  if Entry.ActiveThread <> nil then
  begin
    Entry.PluginsProcessed.Add(Entry.ActiveThread.Plugin);
    Order := Entry.ActiveThread.Plugin.Order;
  end;

  SmallestActive := MaxInt;
  for i := 0 to FPostProcessors.Count - 1 do
    if FPostProcessors[i].Active and (FPostProcessors[i].Order < SmallestActive) and (FPostProcessors[i].Order >= Order) and
       (not Entry.PluginsProcessed.Contains(FPostProcessors[i])) and
       (FPostProcessors[i].CanProcess(Entry.Data)) and
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
        Break;
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
