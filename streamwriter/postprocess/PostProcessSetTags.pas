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
unit PostProcessSetTags;

interface

uses
  Windows, SysUtils, Classes, PostProcess, LanguageObjects, AudioGenie,
  PluginAudioGenie, Functions, Logging, ConfigureSetTags, TypeDefs;

type
  TPostProcessSetTagsThread = class(TPostProcessThreadBase)
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPluginProcessInformation; Plugin: TPostProcessBase);
  end;

  TPostProcessSetTags = class(TInternalPostProcess)
  private
    FArtist: string;
    FTitle: string;
    FAlbum: string;
    FComment: string;
  protected
  public
    constructor Create;

    function CanProcess(Data: PPluginProcessInformation): Boolean; virtual;
    function ProcessFile(Data: PPluginProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;
    procedure Initialize; override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
    procedure Save; override;
    procedure LoadSharedSettings; override;
  end;

implementation

uses
  AppData;

{ TPostProcessSetTagsThread }

constructor TPostProcessSetTagsThread.Create(Data: PPluginProcessInformation;
  Plugin: TPostProcessBase);
begin
  inherited Create(Data, Plugin);
end;

procedure TPostProcessSetTagsThread.Execute;
var
  Artist, Title, Album, Comment: string;
  Arr: TPatternReplaceArray;
  AG: TAudioGenie3;
begin
  inherited;

  FResult := arFail;

  AppGlobals.Storage.Read('Shared_Tags_Artist', Artist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Title', Title, '%t', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Album', Album, '%l', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Comment', Comment, '%s / %u / Recorded using streamWriter', 'Plugins');

  SetLength(Arr, 7);
  Arr[0].C := 'a';
  Arr[0].Replace := FData.Artist;
  Arr[1].C := 't';
  Arr[1].Replace := FData.Title;
  Arr[2].C := 'l';
  Arr[2].Replace := FData.Album;
  Arr[3].C := 's';
  Arr[3].Replace := Trim(FData.Station);
  Arr[4].C := 'u';
  Arr[4].Replace := Trim(FData.StreamTitle);
  Arr[5].C := 'd';
  Arr[5].Replace := FormatDateTime('dd.mm.yy', Now);
  Arr[6].C := 'i';
  Arr[6].Replace := FormatDateTime('hh.nn.ss', Now);

  AG := TAudioGenie3.Create(TPluginAudioGenie(AppGlobals.PluginManager.Find(TPluginAudioGenie)).DLLPath);
  try
    try
      if AG.AUDIOAnalyzeFileW(FData.Filename) <> UNKNOWN then
      begin
        Artist := PatternReplace(Artist, Arr);
        Title := PatternReplace(Title, Arr);
        Album := PatternReplace(Album, Arr);
        Comment := PatternReplace(Comment, Arr);

        AG.AUDIOArtistW := Artist;
        AG.AUDIOTitleW := Title;
        AG.AUDIOAlbumW := Album;
        AG.AUDIOTrackW := IntToStr(FData.TrackNumber);
        AG.AUDIOCommentW := Comment;

        if AG.AUDIOSaveChangesW then
        begin
          FData.Filesize := GetFileSize(FData.Filename);
          FResult := arWin;
        end;
      end;
    except
    end;
  finally
    AG.Free;
  end;
end;
                     // TODO: passen die standard plugin reihenfolgen? das mp4box sollte z.b. immer vor tags setzen sein. evtl sonst warnen oder so?
{ TPostProcessSetTags }

procedure TPostProcessSetTags.Assign(Source: TPostProcessBase);
begin
  inherited;

  FArtist := TPostProcessSetTags(Source).FArtist;
  FTitle := TPostProcessSetTags(Source).FTitle;
  FAlbum := TPostProcessSetTags(Source).FAlbum;
  FComment := TPostProcessSetTags(Source).FComment;
end;

function TPostProcessSetTags.CanProcess(
  Data: PPluginProcessInformation): Boolean;
begin
  Result := (FiletypeToFormat(Data.Filename) <> atNone) and FGetDependenciesMet;
end;

function TPostProcessSetTags.Configure(AOwner: TComponent; Handle: Cardinal;
  ShowMessages: Boolean): Boolean;
var
  F: TfrmConfigureSetTags;
begin
  Result := True;

  F := TfrmConfigureSetTags.Create(AOwner, Self, FArtist, FTitle, FAlbum, FComment);
  try
    F.ShowModal;

    if F.SaveData then
    begin
      FArtist := F.Artist;
      FTitle := F.Title;
      FAlbum := F.Album;
      FComment := F.Comment;

      Save;
    end;
  finally
    F.Free;
  end;
end;

function TPostProcessSetTags.Copy: TPostProcessBase;
begin
  Result := TPostProcessSetTags.Create;

  Result.Active := FActive;
  Result.Order := FOrder;
  Result.OnlyIfCut := FOnlyIfCut;

  Result.Assign(Self);
end;

constructor TPostProcessSetTags.Create;
begin
  inherited;

  FNeededPlugins.Add(TPluginAudioGenie);

  FActive := True;
  FOrder := 100;
  FCanConfigure := True;
  FGroupID := 1;

  FName := _('Write tags to recorded songs');
  FHelp := _('This postprocessor writes tags to recorded songs.');

  try
    AppGlobals.Storage.Read('Active_' + ClassName, FActive, False, 'Plugins');
    AppGlobals.Storage.Read('Order_' + ClassName, FOrder, 100, 'Plugins');
    AppGlobals.Storage.Read('OnlyIfCut_' + ClassName, FOnlyIfCut, False, 'Plugins');

    LoadSharedSettings;

    if not FGetDependenciesMet then
      FActive := False;
  except end;
end;

procedure TPostProcessSetTags.Initialize;
begin
  inherited;

  FName := _('Write tags to recorded songs');
  FHelp := _('This postprocessor writes tags to recorded songs.');
end;

procedure TPostProcessSetTags.LoadSharedSettings;
var
  Tmp: string;
begin
  inherited;

  AppGlobals.Storage.Read('Shared_Tags_Artist', FArtist, '%a', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Title', FTitle, '%t', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Album', FAlbum, '%l', 'Plugins');
  AppGlobals.Storage.Read('Shared_Tags_Comment', FComment, '%s / %u / Recorded using streamWriter', 'Plugins');

  // Wenn die alten Werte noch existieren, einmal einlesen und dann löschen
  AppGlobals.Storage.Read('Artist_' + ClassName, Tmp, 'dummyasdftest', 'Plugins');
  if Tmp <> 'dummyasdftest' then
  begin
    AppGlobals.Storage.Read('Artist_' + ClassName, FArtist, '%a', 'Plugins');
    AppGlobals.Storage.Read('Album_' + ClassName, FAlbum, '%l', 'Plugins');
    AppGlobals.Storage.Read('Title_' + ClassName, FTitle, '%t', 'Plugins');
    AppGlobals.Storage.Read('Comment_' + ClassName, FComment, '%s / %u / Recorded using streamWriter', 'Plugins');

    AppGlobals.Storage.Delete('Artist_' + ClassName, 'Plugins');
    AppGlobals.Storage.Delete('Album_' + ClassName, 'Plugins');
    AppGlobals.Storage.Delete('Title_' + ClassName, 'Plugins');
    AppGlobals.Storage.Delete('Comment_' + ClassName, 'Plugins');
  end;
end;

function TPostProcessSetTags.ProcessFile(
  Data: PPluginProcessInformation): TPostProcessThreadBase;
begin
  Result := TPostProcessSetTagsThread.Create(Data, Self);
end;

procedure TPostProcessSetTags.Save;
begin
  inherited;

  AppGlobals.Storage.Write('Shared_Tags_Artist', FArtist, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Title', FTitle, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Album', FAlbum, 'Plugins');
  AppGlobals.Storage.Write('Shared_Tags_Comment', FComment, 'Plugins');
end;

end.
