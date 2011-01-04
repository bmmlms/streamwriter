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
  Windows, SysUtils, Classes, Generics.Collections, Registry, SyncObjs, AppDataBase,
  LanguageObjects, LanguageIcons, Plugins, ExtendedStream, Forms;

type
  TClientActions = (caStartStop, caStreamIntegrated, caStream, caFile);
  TUseFilters = (ufNone, ufWish, ufIgnore);

  TIntArray = array of Integer;

  TStreamSettings = class
  private
    FFilePattern: string;
    FDeleteStreams: Boolean;
    FAddSavedToIgnore: Boolean;
    FSkipShort: Boolean;
    FSearchSilence: Boolean;
    FSilenceLevel: Cardinal;
    FSilenceLength: Cardinal;
    FShortSize: Integer;
    FSongBuffer: Integer;
    FMaxRetries: Integer;
    FRetryDelay: Cardinal;
    FFilter: TUseFilters;
    FSeparateTracks: Boolean;
    FSaveToMemory: Boolean;
  public
    class function Load(Stream: TExtendedStream; Version: Integer): TStreamSettings;
    procedure Save(Stream: TExtendedStream);
    procedure Assign(From: TStreamSettings);
    function Copy: TStreamSettings;

    property FilePattern: string read FFilePattern write FFilePattern;
    property DeleteStreams: Boolean read FDeleteStreams write FDeleteStreams;
    property AddSavedToIgnore: Boolean read FAddSavedToIgnore write FAddSavedToIgnore;
    property SkipShort: Boolean read FSkipShort write FSkipShort;
    property SearchSilence: Boolean read FSearchSilence write FSearchSilence;
    property SilenceLevel: Cardinal read FSilenceLevel write FSilenceLevel;
    property SilenceLength: Cardinal read FSilenceLength write FSilenceLength;
    property ShortSize: Integer read FShortSize write FShortSize;
    property SongBuffer: Integer read FSongBuffer write FSongBuffer;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
    property RetryDelay: Cardinal read FRetryDelay write FRetryDelay;
    property Filter: TUseFilters read FFilter write FFilter;
    property SeparateTracks: Boolean read FSeparateTracks write FSeparateTracks;
    property SaveToMemory: Boolean read FSaveToMemory write FSaveToMemory;
  end;

  TStreamSettingsArray = array of TStreamSettings;

  TAppData = class(TAppDataBase)
  private
    FStreamSettings: TStreamSettings;

    FDir: string;
    FTray: Boolean;
    FTrayOnMinimize: Boolean;
    FShowSidebar: Boolean;
    FSidebarWidth: Integer;
    FSubmitStreams: Boolean;
    FMinDiskSpace: Integer;
    FDefaultAction: TClientActions;
    FPlayerVolume, FCutVolume: Integer;

    FShortcutPlay: Cardinal;
    FShortcutStop: Cardinal;
    FShortcutNext: Cardinal;
    FShortcutPrev: Cardinal;

    FHeaderWidth: TIntArray;

    FPluginManager: TPluginManager;
    FLanguageIcons: TLanguageIcons;

    function FGetDataFile: string;
  protected
    procedure DoSave; override;
  public
    constructor Create(AppName: String);
    destructor Destroy; override;

    procedure Load; override;
    procedure BuildThanksText; override;

    property StreamSettings: TStreamSettings read FStreamSettings;

    property Dir: string read FDir write FDir;
    property Tray: Boolean read FTray write FTray;
    property TrayOnMinimize: Boolean read FTrayOnMinimize write FTrayOnMinimize;
    property ShowSidebar: Boolean read FShowSidebar write FShowSidebar;
    property SidebarWidth: Integer read FSidebarWidth write FSidebarWidth;
    property SubmitStreams: Boolean read FSubmitStreams write FSubmitStreams;
    property MinDiskSpace: Integer read FMinDiskSpace write FMinDiskSpace;
    property DefaultAction: TClientActions read FDefaultAction write FDefaultAction;
    property PlayerVolume: Integer read FPlayerVolume write FPlayerVolume;
    property CutVolume: Integer read FCutVolume write FCutVolume;
    property ShortcutPlay: Cardinal read FShortcutPlay write FShortcutPlay;
    property ShortcutStop: Cardinal read FShortcutStop write FShortcutStop;
    property ShortcutNext: Cardinal read FShortcutNext write FShortcutNext;
    property ShortcutPrev: Cardinal read FShortcutPrev write FShortcutPrev;

    property HeaderWidth: TIntArray read FHeaderWidth write FHeaderWidth;

    property DataFile: string read FGetDataFile;

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
  FStreamSettings := TStreamSettings.Create;

  W := 900;
  if Screen.Width < W then
    W := Screen.Width - 20;

  SetLength(FHeaderWidth, 6);

  inherited Create(AppName, True, W, 500);

  FBuildNumber := 0;
  {$IFDEF DEBUG}
  FProjectUpdateLink := 'http://streamwriter.gaia/';
  {$ELSE}
  FProjectUpdateLink := 'http://streamwriter.org/';
  {$ENDIF}

  FProjectHomepageLink := 'http://streamwriter.org/';
  FProjectLink := 'http://streamwriter.org/';
  FProjectHelpLink := 'http://streamwriter.org/wiki/artikel/help/';
  FProjectForumLink := 'http://streamwriter.org/forum/';

  FProjectDonateLink := 'http://streamwriter.org/inhalt/donate/';

  BuildThanksText;

  FLanguageIcons := TLanguageIcons.Create;
end;

destructor TAppData.Destroy;
begin
  FLanguageIcons.Free;
  FPluginManager.Free;
  FStreamSettings.Free;

  inherited;
end;

function TAppData.FGetDataFile;
begin
  Result := FStorage.GetFilePath('data.dat');
end;

{
function TAppData.FGetListFile: string;
begin
  Result := FStorage.GetFilePath('list.dat');
end;

function TAppData.FGetRecentFile: string;
begin
  Result := FStorage.GetFilePath('recent.dat');
end;
}

procedure TAppData.BuildThanksText;
  procedure ShuffleFisherYates(var A: TArray);
  var
    i, j: Integer;
    Tmp: TArrayElement;
  begin
    for i := Low(A) to High(A) do
    begin
      j := i + Random(Length(A) - i + Low(A));
      Tmp := A[j];
      A[j] := A[i];
      A[i] := Tmp;
    end;
  end;
var
  i: Integer;
  FDonors, FHelpers{, FBoard}: TArray;
  Text: TStringList;
begin
  inherited;

  Text := TStringList.Create;
  try
    Text.Add(_('&U&12Thanks go out to...'));

    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10...everybody who donated something'));
    Text.Add('');
    SetLength(FDonors, 6);
    FDonors[0] := 'Thomas Franke';
    FDonors[1] := '''bastik''';
    FDonors[2] := 'Reto Pitsch';
    FDonors[3] := '''RogerPP''';
    FDonors[4] := 'Gabor Kubik';
    FDonors[5] := '''Peter Parker''';
    ShuffleFisherYates(FDonors);
    for i := 0 to Length(FDonors) - 1 do
      Text.Add(FDonors[i]);
    Text.Add(_('and everyone who does not want to be mentioned'));

    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10...people who contributed code, documentation,'));
    Text.Add(_('&U&10images or translations'));
    Text.Add('');
    SetLength(FHelpers, 2);
    FHelpers[0] := '''HostedDinner''';
    FHelpers[1] := '''bastik''';
    ShuffleFisherYates(FHelpers);
    for i := 0 to Length(FHelpers) - 1 do
      Text.Add(FHelpers[i]);

    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10...everyone supporting streamWriter'#13#10'&U&10at http://streamwriter.org/forum/'));
    {
    Text.Add('');
    SetLength(FBoard, 12);
    FBoard[0] := 'bastik';
    FBoard[1] := 'mondstern';
    FBoard[2] := 'HostedDinner';
    FBoard[3] := 'Max';
    FBoard[4] := 'Nemesis';
    FBoard[5] := 'MASH';
    FBoard[6] := 'Jim';
    FBoard[7] := 'Robin Hood';
    FBoard[8] := 'Udo';
    FBoard[9] := 'LexTiger';
    FBoard[10] := 'ebop';
    FBoard[11] := 'Fantatierchen';
    ShuffleFisherYates(FBoard);
    for i := 0 to Length(FBoard) - 1 do
      Text.Add('''' + FBoard[i] + '''');
    }

    {
    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10...everyone I forgot to mention here'));
    }

    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10...and all other sweet people I know!'));

    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add(_('&U&10Also, thanks to some other projects I make use of'#13#10'&U&10to develop streamWriter and it''s website'));
    Text.Add('');
    Text.Add('Apache HTTP Server');
    Text.Add('Bass');
    Text.Add('Django');
    Text.Add('Drag and Drop Component Suite');
    Text.Add('Delphi-Praxis');
    Text.Add('Embarcadero');
    Text.Add('famfamfam');
    Text.Add('FastMM');
    Text.Add('freecsstemplates.org');
    Text.Add('Fugue Icons');
    Text.Add('Gimp');
    Text.Add('Inno Setup');
    Text.Add('jQuery');
    Text.Add('MySQL');
    Text.Add('Notepad++');
    Text.Add('Mp3FileUtils');
    Text.Add('Python');
    Text.Add('Tango Desktop Project');
    Text.Add('Virtual Treeview');
    Text.Add('XMLLib');

    Text.Add('');
    Text.Add('');

    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add('&IMG');

    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');
    Text.Add('');

    Text.Add('D1734FA178BF7D5AE50CB1AD54442494');

    FProjectThanksText := Text.Text;
  finally
    Text.Free;
  end;
end;

procedure TAppData.Load;
var
  i, DefaultActionTmp, DefaultFilterTmp: Integer;
begin
  inherited;

  FStorage.Read('FilePattern', FStreamSettings.FFilePattern, '%s\%a - %t');
  FStorage.Read('Dir', FDir, '');
  if FDir <> '' then
    FDir := IncludeTrailingBackslash(FDir);
  FStorage.Read('DeleteStreams', FStreamSettings.FDeleteStreams, True);
  FStorage.Read('AddSavedToIgnore', FStreamSettings.FAddSavedToIgnore, True);
  FStorage.Read('SkipShort', FStreamSettings.FSkipShort, True);
  FStorage.Read('SearchSilence', FStreamSettings.FSearchSilence, True);
  FStorage.Read('SilenceLevel', FStreamSettings.FSilenceLevel, 5);
  FStorage.Read('SilenceLength', FStreamSettings.FSilenceLength, 150);
  FStorage.Read('SaveToMemory', FStreamSettings.FSaveToMemory, False);
  FStorage.Read('TrayClose', FTray, False);
  FStorage.Read('TrayOnMinimize', FTrayOnMinimize, False);

  if (FStreamSettings.FSilenceLevel < 1) or (FStreamSettings.FSilenceLevel > 100) then
    FStreamSettings.FSilenceLevel := 20;
  if FStreamSettings.FSilenceLength < 20 then
    FStreamSettings.FSilenceLength := 20;

  FShowSidebar := True;

  FStorage.Read('SidebarWidth', FSidebarWidth, 230);
  FStorage.Read('SubmitStreams', FSubmitStreams, True);
  FStorage.Read('ShortSize', FStreamSettings.FShortSize, 1500);
  FStorage.Read('SongBuffer', FStreamSettings.FSongBuffer, 0);
  FStorage.Read('MaxRetries', FStreamSettings.FMaxRetries, 100);
  FStorage.Read('RetryDelay', FStreamSettings.FRetryDelay, 5);
  FStorage.Read('SeparateTracks', FStreamSettings.FSeparateTracks, True);
  FStorage.Read('MinDiskSpace', FMinDiskSpace, 5);
  FStorage.Read('DefaultAction', DefaultActionTmp, Integer(caStartStop));
  FStorage.Read('DefaultFilter', DefaultFilterTmp, Integer(ufNone));
  FStorage.Read('PlayerVolume', FPlayerVolume, 50);
  FStorage.Read('CutVolume', FCutVolume, 50);

  FStorage.Read('ShortcutPlay', FShortcutPlay, 0);
  FStorage.Read('ShortcutStop', FShortcutStop, 0);
  FStorage.Read('ShortcutNext', FShortcutNext, 0);
  FStorage.Read('ShortcutPrev', FShortcutPrev, 0);

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

  if (DefaultFilterTmp > Ord(High(TUseFilters))) or
     (DefaultFilterTmp < Ord(Low(TUseFilters))) then
    FStreamSettings.Filter := ufNone
  else
    FStreamSettings.Filter := TUseFilters(DefaultFilterTmp);


  if FStreamSettings.FSaveToMemory then
  begin
    FStreamSettings.FSeparateTracks := True;
    FStreamSettings.FDeleteStreams := False;
  end;

  if not FStreamSettings.FSeparateTracks then
    FStreamSettings.FDeleteStreams := False;
end;

procedure TAppData.DoSave;
var
  i, n: Integer;
begin
  inherited;

  FStorage.Write('FilePattern', FStreamSettings.FFilePattern);
  FStorage.Write('Dir', FDir);
  FStorage.Write('DeleteStreams', FStreamSettings.FDeleteStreams);
  FStorage.Write('AddSavedToIgnore', FStreamSettings.FAddSavedToIgnore);
  FStorage.Write('SkipShort', FStreamSettings.FSkipShort);
  FStorage.Write('SearchSilence', FStreamSettings.FSearchSilence);
  FStorage.Write('SilenceLevel', FStreamSettings.FSilenceLevel);
  FStorage.Write('SilenceLength', FStreamSettings.FSilenceLength);
  FStorage.Write('SaveToMemory', FStreamSettings.FSaveToMemory);
  FStorage.Write('TrayClose', FTray);
  FStorage.Write('TrayOnMinimize', FTrayOnMinimize);

  FStorage.Write('SidebarWidth', FSidebarWidth);
  FStorage.Write('SubmitStreams', FSubmitStreams);
  FStorage.Write('ShortSize', FStreamSettings.FShortSize);
  FStorage.Write('SongBuffer', FStreamSettings.FSongBuffer);
  FStorage.Write('MaxRetries', FStreamSettings.FMaxRetries);
  FStorage.Write('RetryDelay', FStreamSettings.FRetryDelay);
  FStorage.Write('DefaultFilter', Integer(FStreamSettings.Filter));
  FStorage.Write('SeparateTracks', FStreamSettings.FSeparateTracks);
  FStorage.Write('MinDiskSpace', FMinDiskSpace);
  FStorage.Write('DefaultAction', Integer(FDefaultAction));
  FStorage.Write('PlayerVolume', FPlayerVolume);
  FStorage.Write('CutVolume', FCutVolume);

  FStorage.Write('ShortcutPlay', FShortcutPlay);
  FStorage.Write('ShortcutStop', FShortcutStop);
  FStorage.Write('ShortcutNext', FShortcutNext);
  FStorage.Write('ShortcutPrev', FShortcutPrev);

  for i := 0 to High(FHeaderWidth) do
    if i <> 1 then
      FStorage.Write('HeaderWidth' + IntToStr(i), HeaderWidth[i], 'Cols');

  FStorage.DeleteKey('Plugins');
  n := 0;
  for i := 0 to FPluginManager.Plugins.Count - 1 do
    if (FPluginManager.Plugins[i] is TExternalPlugin) then
    begin
      FStorage.Write('Active_' + IntToStr(n), TExternalPlugin(FPluginManager.Plugins[i]).Active, 'Plugins');
      FStorage.Write('Exe_' + IntToStr(n), TExternalPlugin(FPluginManager.Plugins[i]).Exe, 'Plugins');
      FStorage.Write('Params_' + IntToStr(n), TExternalPlugin(FPluginManager.Plugins[i]).Params, 'Plugins');
      FStorage.Write('OrderExe_' + IntToStr(n), FPluginManager.Plugins[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + IntToStr(n), FPluginManager.Plugins[i].OnlyIfCut, 'Plugins');
      Inc(n);
    end else if (FPluginManager.Plugins[i] is TDLLPlugin) then
    begin
      FStorage.Write('Active_' + ExtractFileName(TDLLPlugin(FPluginManager.Plugins[i]).Filename), FPluginManager.Plugins[i].Active, 'Plugins');
      FStorage.Write('Order_' + ExtractFileName(TDLLPlugin(FPluginManager.Plugins[i]).Filename), FPluginManager.Plugins[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + ExtractFileName(TDLLPlugin(FPluginManager.Plugins[i]).Filename), FPluginManager.Plugins[i].OnlyIfCut, 'Plugins');
    end else if (FPluginManager.Plugins[i] is TInternalPlugin) then
    begin
      FStorage.Write('Active_' + FPluginManager.Plugins[i].ClassName, FPluginManager.Plugins[i].Active, 'Plugins');
      FStorage.Write('Order_' + FPluginManager.Plugins[i].ClassName, FPluginManager.Plugins[i].Order, 'Plugins');
      FStorage.Write('OnlyIfCut_' + FPluginManager.Plugins[i].ClassName, FPluginManager.Plugins[i].OnlyIfCut, 'Plugins');
    end;
end;

{ TStreamSettings }

function TStreamSettings.Copy: TStreamSettings;
begin
  Result := TStreamSettings.Create;
  Result.Assign(Self);
end;

class function TStreamSettings.Load(Stream: TExtendedStream;
  Version: Integer): TStreamSettings;
var
  FilterTmp: Integer;
begin
  Result := TStreamSettings.Create;

  Stream.Read(Result.FFilePattern);
  Stream.Read(Result.FDeleteStreams);
  Stream.Read(Result.FAddSavedToIgnore);
  Stream.Read(Result.FSkipShort);
  Stream.Read(Result.FSearchSilence);
  Stream.Read(Result.FSilenceLevel);
  Stream.Read(Result.FSilenceLength);
  Stream.Read(Result.FShortSize);
  Stream.Read(Result.FSongBuffer);
  Stream.Read(Result.FMaxRetries);
  Stream.Read(FilterTmp);
  Stream.Read(Result.FSeparateTracks);
  Stream.Read(Result.FSaveToMemory);

  if Result.FSaveToMemory then
  begin
    Result.FSeparateTracks := True;
    Result.FDeleteStreams := False;
  end;

  if not Result.FSeparateTracks then
    Result.FDeleteStreams := False;

  if (FilterTmp > Ord(High(TUseFilters))) or
     (FilterTmp < Ord(Low(TUseFilters))) then
    Result.FFilter := ufNone
  else
    Result.FFilter := TUseFilters(FilterTmp);
end;

procedure TStreamSettings.Save(Stream: TExtendedStream);
begin
  Stream.Write(FFilePattern);
  Stream.Write(FDeleteStreams);
  Stream.Write(FAddSavedToIgnore);
  Stream.Write(FSkipShort);
  Stream.Write(FSearchSilence);
  Stream.Write(FSilenceLevel);
  Stream.Write(FSilenceLength);
  Stream.Write(FShortSize);
  Stream.Write(FSongBuffer);
  Stream.Write(FMaxRetries);
  Stream.Write(Integer(FFilter));
  Stream.Write(FSeparateTracks);
  Stream.Write(FSaveToMemory);
end;

procedure TStreamSettings.Assign(From: TStreamSettings);
begin
  FFilePattern := From.FFilePattern;
  FDeleteStreams := From.FDeleteStreams;
  FAddSavedToIgnore := From.FAddSavedToIgnore;
  FSkipShort := From.FSkipShort;
  FSearchSilence := From.FSearchSilence;
  FSilenceLevel := From.FSilenceLevel;
  FSilenceLength := From.FSilenceLength;
  FShortSize := From.FShortSize;
  FSongBuffer := From.FSongBuffer;
  FMaxRetries := From.FMaxRetries;
  FRetryDelay := From.FRetryDelay;
  FFilter := From.FFilter;
  FSeparateTracks := From.FSeparateTracks;
  FSaveToMemory := From.SaveToMemory;
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
      MessageBox(0, PChar(Format('The application could not be started.'#13#10'Message: %s', [E.Message])), PChar(_('Error')), MB_ICONERROR);
      Halt;
    end;
  end;
end;

finalization
  FreeAndNil(AppGlobals);

end.

