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
  LanguageObjects, LanguageIcons, Plugins, Forms;

type
  TClientActions = (caStartStop, caStream, caRelay, caFile);

  TIntArray = array of Integer;

  TAppData = class(TAppDataBase)
  private
    FDir: string;
    FFilePattern: string;
    FSkipShort: Boolean;
    FSearchSilence: Boolean;
    FSilenceLevel: Cardinal;
    FSilenceLength: Cardinal;
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
    FHeaderWidth: TIntArray;

    FPluginManager: TPluginManager;
    FLanguageIcons: TLanguageIcons;

    function FGetDataFile: string;
    function FGetRecentFile: string;
    function FGetListFile: string;
  protected
    procedure DoSave; override;
  public
    constructor Create(AppName: String);
    destructor Destroy; override;

    procedure Load; override;

    property Dir: string read FDir write FDir;
    property FilePattern: string read FFilePattern write FFilePattern;
    property SkipShort: Boolean read FSkipShort write FSkipShort;
    property SearchSilence: Boolean read FSearchSilence write FSearchSilence;
    property SilenceLevel: Cardinal read FSilenceLevel write FSilenceLevel;
    property SilenceLength: Cardinal read FSilenceLength write FSilenceLength;
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
    property HeaderWidth: TIntArray read FHeaderWidth write FHeaderWidth;

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
  W: Integer;
begin
  W := 900;
  if Screen.Width < W then
    W := Screen.Width - 20;

  SetLength(FHeaderWidth, 6);

  inherited Create(AppName, True, W, 500);

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

procedure TAppData.Load;
var
  i, DefaultActionTmp: Integer;
begin
  inherited;

  FStorage.Read('Dir', FDir, '');
  if FDir <> '' then
    FDir := IncludeTrailingBackslash(FDir);
  FStorage.Read('FilePattern', FFilePattern, '%s\%a - %t');
  FStorage.Read('SkipShort', FSkipShort, True);
  FStorage.Read('SearchSilence', FSearchSilence, True);
  FStorage.Read('SilenceLevel', FSilenceLevel, 5);
  FStorage.Read('SilenceLength', FSilenceLength, 10);
  FStorage.Read('TrayClose', FTrayClose, False);
  FStorage.Read('ShowSidebar', FShowSidebar, True);
  FStorage.Read('SidebarWidth', FSidebarWidth, 230);
  FStorage.Read('Relay', FRelay, False);
  FStorage.Read('SubmitStreams', FSubmitStreams, True);
  FStorage.Read('ShortSize', FShortSize, 1000);
  FStorage.Read('SongBuffer', FSongBuffer, 0);
  FStorage.Read('MaxRetries', FMaxRetries, 50);
  FStorage.Read('RetryDelay', FRetryDelay, 5);
  FStorage.Read('MinDiskSpace', FMinDiskSpace, 5);
  FStorage.Read('DefaultAction', DefaultActionTmp, Integer(caStartStop));

  FStorage.Read('HeaderWidth0', i, -1, 'Cols');
  if i = -1 then
  begin
    for i := 0 to High(FHeaderWidth) do
      FHeaderWidth[i] := 100;
    FStorage.Read('HeaderWidth0', FHeaderWidth[0], 150, 'Cols');
    FStorage.Read('HeaderWidth2', FHeaderWidth[2], 70, 'Cols');
    FStorage.Read('HeaderWidth3', FHeaderWidth[3], 60, 'Cols');
    FStorage.Read('HeaderWidth4', FHeaderWidth[4], 90, 'Cols');
    FStorage.Read('HeaderWidth5', FHeaderWidth[5], 85, 'Cols');
  end else
  begin
    for i := 0 to High(FHeaderWidth) do
      FStorage.Read('HeaderWidth' + IntToStr(i), FHeaderWidth[i], 130, 'Cols');
  end;

  if (DefaultActionTmp > Ord(High(TClientActions))) or
     (DefaultActionTmp < Ord(Low(TClientActions))) then
    FDefaultAction := caStartStop
  else
    FDefaultAction := TClientActions(DefaultActionTmp);
end;

procedure TAppData.DoSave;
var
  i: Integer;
begin
  inherited;

  FStorage.Write('Dir', FDir);
  FStorage.Write('FilePattern', FFilePattern);
  FStorage.Write('SkipShort', FSkipShort);
  FStorage.Write('SearchSilence', FSearchSilence);
  FStorage.Write('SilenceLevel', FSilenceLevel);
  FStorage.Write('SilenceLength', FSilenceLength);
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

  for i := 0 to High(FHeaderWidth) do
    if i <> 1 then
      FStorage.Write('HeaderWidth' + IntToStr(i), HeaderWidth[i], 'Cols');

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
      MessageBox(0, PChar(_('The application could not be started.'#13#10'Message:') + ' ' + E.Message), PChar(_('Error')), MB_ICONERROR);
      Halt;
    end;
  end;
end;

finalization
  FreeAndNil(AppGlobals);

end.

