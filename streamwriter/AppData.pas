{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010 Alexander Nottelmann

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
unit AppData;

interface

uses
  Windows, SysUtils, Classes, Registry, SyncObjs, AppDataBase,
  LanguageObjects, LanguageIcons, Plugins;

type
  TClientActions = (caStartStop, caStream, caRelay, caFile);

  TAppData = class(TAppDataBase)
  private
    FDir: string;
    FSeperateDirs: Boolean;
    FSkipShort: Boolean;
    FTrayClose: Boolean;
    FShowSidebar: Boolean;
    FSidebarWidth: Integer;
    FRelay: Boolean;
    FSubmitStreams: Boolean;
    FShortSize: Integer;
    FSongBuffer: Integer;
    FMaxRetries: Integer;
    FRetryDelay: Cardinal;
    FMinDiskSpace: Integer;
    FDefaultAction: TClientActions;

    FPluginManager: TPluginManager;
    FLanguageIcons: TLanguageIcons;

    function FGetDataFile: string;
    function FGetRecentFile: string;
    function FGetListFile: string;
  public
    constructor Create(AppName: String);
    destructor Destroy; override;
    procedure Save; override;

    property Dir: String read FDir write FDir;
    property SeperateDirs: Boolean read FSeperateDirs write FSeperateDirs;
    property SkipShort: Boolean read FSkipShort write FSkipShort;
    property TrayClose: Boolean read FTrayClose write FTrayClose;
    property ShowSidebar: Boolean read FShowSidebar write FShowSidebar;
    property SidebarWidth: Integer read FSidebarWidth write FSidebarWidth;
    property Relay: Boolean read FRelay write FRelay;
    property SubmitStreams: Boolean read FSubmitStreams write FSubmitStreams;
    property ShortSize: Integer read FShortSize write FShortSize;
    property SongBuffer: Integer read FSongBuffer write FSongBuffer;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RetryDelay: Cardinal read FRetryDelay write FRetryDelay;
    property MinDiskSpace: Integer read FMinDiskSpace write FMinDiskSpace;
    property DefaultAction: TClientActions read FDefaultAction write FDefaultAction;

    property DataFile: string read FGetDataFile;
    // Diese beiden können irgendwann raus
    property RecentFile: string read FGetRecentFile;
    property ListFile: string read FGetListFile;

    property PluginManager: TPluginManager read FPluginManager;
    property LanguageIcons: TLanguageIcons read FLanguageIcons;
  end;

var
  AppGlobals: TAppData;

implementation

constructor TAppData.Create(AppName: string);
var
  i: Integer;
begin
  inherited Create(AppName, True, 800, 450);

  FStorage.Read('Dir', FDir, '');
  if FDir <> '' then
    FDir := IncludeTrailingBackslash(FDir);

  FStorage.Read('SeperateDirs', FSeperateDirs, True);
  FStorage.Read('SkipShort', FSkipShort, True);
  FStorage.Read('TrayClose', FTrayClose, False);
  FStorage.Read('ShowSidebar', FShowSidebar, True);
  FStorage.Read('SidebarWidth', FSidebarWidth, 200);
  FStorage.Read('Relay', FRelay, False);
  FStorage.Read('SubmitStreams', FSubmitStreams, True);
  FStorage.Read('ShortSize', FShortSize, 1000);
  FStorage.Read('SongBuffer', FSongBuffer, 100);
  FStorage.Read('MaxRetries', FMaxRetries, 20);
  FStorage.Read('RetryDelay', FRetryDelay, 5);
  FStorage.Read('MinDiskSpace', FMinDiskSpace, 5);
  FStorage.Read('DefaultAction', i, Integer(caStartStop));

  if (i > Ord(High(TClientActions))) or (i < Ord(Low(TClientActions))) then
    FDefaultAction := caStartStop
  else
    FDefaultAction := TClientActions(i);

  FLanguageIcons := TLanguageIcons.Create;
end;

destructor TAppData.Destroy;
begin
  FLanguageIcons.Free;
  FPluginManager.Free;
  inherited;
end;

function TAppData.FGetDataFile;
begin
  Result := FStorage.GetFilePath('data.dat');
end;

function TAppData.FGetListFile: string;
begin
  Result := FStorage.GetFilePath('list.dat');
end;

function TAppData.FGetRecentFile: string;
begin
  Result := FStorage.GetFilePath('recent.dat');
end;

procedure TAppData.Save;
var
  i: Integer;
begin
  inherited;

  FStorage.Write('Dir', FDir);
  FStorage.Write('SeperateDirs', FSeperateDirs);
  FStorage.Write('SkipShort', FSkipShort);
  FStorage.Write('TrayClose', FTrayClose);
  FStorage.Write('ShowSidebar', FShowSidebar);
  FStorage.Write('SidebarWidth', FSidebarWidth);
  FStorage.Write('Relay', FRelay);
  FStorage.Write('SubmitStreams', FSubmitStreams);
  FStorage.Write('ShortSize', FShortSize);
  FStorage.Write('SongBuffer', FSongBuffer);
  FStorage.Write('MaxRetries', FMaxRetries);
  FStorage.Write('RetryDelay', FRetryDelay);
  FStorage.Write('MinDiskSpace', FMinDiskSpace);
  FStorage.Write('DefaultAction', Integer(FDefaultAction));

  for i := 0 to FPluginManager.Plugins.Count - 1 do
    FStorage.Write('Active_' + ExtractFileName(FPluginManager.Plugins[i].Filename), FPluginManager.Plugins[i].Active, 'Plugins');
end;

initialization
begin
  try
    if Language = nil then
      raise Exception.Create('Language is not initialized');
    AppGlobals := TAppData.Create('streamWriter');

    // PluginManager wird hier erstellt, da erstellte Plugin-Items Zugriff
    // auf ein bereits zugewiesenes AppGlobals brauchen.
    AppGlobals.FPluginManager := TPluginManager.Create(AppGlobals.AppPath + 'plugins\');
  except
    on E: Exception do
    begin
      MessageBox(0, PChar(_('Error:') + ' ' + E.Message), PChar(_('Error')), MB_ICONERROR);
      Halt;
    end;
  end;
end;

finalization
  FreeAndNil(AppGlobals);

end.
