{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit PluginManager;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, Plugins, SetTags, SoX,
  MP3Gain, Logging;

type
  TPluginManager = class
  private
    FPlugins: TList<TPluginBase>;
    FActivePlugin: TPluginBase;
  public
    constructor Create(Path: string);
    destructor Destroy; override;

    function ProcessFile(Data: TPluginProcessInformation): TProcessingEntry; overload;
    function ProcessFile(Entry: TProcessingEntry): Boolean; overload;
    procedure ReInitPlugins;
    //function GetID(ID: Integer): TExternalPlugin;
    function Find(Plugin: TPluginBase): TPluginBase;

    property Plugins: TList<TPluginBase> read FPlugins;
  end;

implementation

uses
  AppData;

{ TPluginManager }

function TPluginManager.ProcessFile(Data: TPluginProcessInformation): TProcessingEntry;
begin
  Result := TProcessingEntry.Create(nil, Data, nil);
  if not ProcessFile(Result) then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function TPluginManager.ProcessFile(Entry: TProcessingEntry): Boolean;
var
  i, Order, SmallestActive: Integer;
begin
  Result := False;
  Order := 0;

  // TODO: wenn alles bei sox aus ist und es das erste plugin ist werden die folgenden nicht bearbeitet.

  // Das soeben beendete Plugin der Liste hinzufügen
  if Entry.ActiveThread <> nil then
  begin
    Entry.PluginsProcessed.Add(Entry.ActiveThread.Plugin);
    Order := Entry.ActiveThread.Plugin.Order;
  end;

  SmallestActive := MaxInt;
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i].Active and (FPlugins[i].Order < SmallestActive) and (FPlugins[i].Order >= Order) and
       (not Entry.PluginsProcessed.Contains(FPlugins[i])) and
       ((FPlugins[i].OnlyIfCut and Entry.Data.WasCut) or (not FPlugins[i].OnlyIfCut)) then
    begin
      SmallestActive := FPlugins[i].Order;
    end;

  if SmallestActive <> MaxInt then
  begin
    for i := 0 to FPlugins.Count - 1 do
      if FPlugins[i].Order = SmallestActive then
      begin
        Entry.ActiveThread := FPlugins[i].ProcessFile(Entry.Data);
        Result := Entry.ActiveThread <> nil;
        Break;
      end;
  end;
end;

procedure TPluginManager.ReInitPlugins;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Initialize;
end;

{
function TPluginManager.GetID(ID: Integer): TExternalPlugin;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i] is TExternalPlugin then
      if TExternalPlugin(FPlugins[i]).Identifier = ID then
      begin
        Result := TExternalPlugin(FPlugins[i]);
        Break;
      end;
end;
}

constructor TPluginManager.Create(Path: string);
var
  i: Integer;
  //Handle: THandle;
  //GetVersion: TGetInt;
  //P: TDLLPlugin;
  //Files: TStringList;
  App, Params: string;
  EP: TExternalPlugin;
  Active, OnlyIfCut: Boolean;
  Order: Integer;
begin
  FActivePlugin := nil;
  FPlugins := TList<TPluginBase>.Create;

  {
  Files := TStringList.Create;
  try
    FindFiles(Path + '*.dll', Files);
    for i := 0 to Files.Count - 1 do
    begin
      Handle := LoadLibrary(PChar(Path + Files[i]));
      if Handle <> 0 then
        try
          @GetVersion := GetProcAddress(Handle, 'GetVersion');
          if @GetVersion = nil then
            Continue;
          if GetVersion >= 1 then
          begin
            P := TPlugin.Create(Handle, Path + Files[i]);
            P.Initialize;
            Plugins.Add(P);
          end;
        except

        end;
    end;
  finally
    Files.Free;
  end;
  }

  Plugins.Add(TSetTagsPlugin.Create);
  Plugins.Add(TSoXPlugin.Create);
  //Plugins.Add(TNormalizePlugin.Create);

  i := 0;
  repeat
    AppGlobals.Storage.Read('Exe_' + IntToStr(i), App, '', 'Plugins');
    AppGlobals.Storage.Read('Params_' + IntToStr(i), Params, '', 'Plugins');
    AppGlobals.Storage.Read('OrderExe_' + IntToStr(i), Order, 0, 'Plugins');
    AppGlobals.Storage.Read('Active_' + IntToStr(i), Active, True, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + IntToStr(i), OnlyIfCut, False, 'Plugins');
    if App <> '' then
    begin
      EP := TExternalPlugin.Create(App, Params, Active, OnlyIfCut, i, Order);
      try
        Plugins.Add(EP);
      except

      end;
    end;
    Inc(i);
  until (App = '');
end;

destructor TPluginManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
    FPlugins[i].Free;
  FPlugins.Free;
  inherited;
end;

function TPluginManager.Find(Plugin: TPluginBase): TPluginBase;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FPlugins.Count - 1 do
    if (Plugin is TDLLPlugin) and (FPlugins[i] is TDLLPlugin) then
    begin
      // REMARK: DLL-Plugins sind komplett raus zur Zeit.
      {
      if TDLLPlugin(Plugin).Filename = TDLLPlugin(FPlugins[i]).Filename then
      begin
        Result := Plugins[i];
        Break;
      end;
      }
    end else if (Plugin is TExternalPlugin) and (FPlugins[i] is TExternalPlugin) then
    begin
      if TExternalPlugin(Plugin).Identifier = TExternalPlugin(FPlugins[i]).Identifier then
      begin
        Result := FPlugins[i];
        Break;
      end;
    end else if Plugin.ClassType.InheritsFrom(TInternalPlugin) and FPlugins[i].ClassType.InheritsFrom(TInternalPlugin) then
    begin
      if Plugin.ClassType = FPlugins[i].ClassType then
      begin
        Result := FPlugins[i];
        Break;
      end;
    end;
end;

end.
