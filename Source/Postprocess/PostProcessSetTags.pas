{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  AudioFunctions,
  Classes,
  ConfigureSetTags,
  FileTagger,
  Functions,
  Generics.Collections,
  LanguageObjects,
  Logging,
  PostProcess,
  StreamHelper,
  SWFunctions,
  SysUtils;

type
  TPostProcessSetTagsThread = class(TPostProcessThreadBase)
  private
    FArtist, FTitle, FAlbum, FGenre, FComment: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Data: PPostProcessInformation; PostProcessor: TPostProcessBase; Artist, Title, Album, Genre, Comment: string);
  end;

  TPostProcessSetTags = class(TInternalPostProcess)
  private
    FArtist: string;
    FTitle: string;
    FAlbum: string;
    FGenre: string;
    FComment: string;
  protected
    function FGetHash: Cardinal; override;
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostprocessBase>): Boolean; override;
    function ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase; override;
    function Copy: TPostProcessBase; override;
    procedure Assign(Source: TPostProcessBase); override;
    procedure Load(Stream: TStream; Version: Integer); override;
    procedure Save(Stream: TMemoryStream); override;
    function Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean; override;
  end;

implementation

uses
  AppData;

{ TPostProcessSetTagsThread }

constructor TPostProcessSetTagsThread.Create(Data: PPostProcessInformation; PostProcessor: TPostProcessBase; Artist, Title, Album, Genre, Comment: string);
begin
  inherited Create(Data, PostProcessor);

  FArtist := Artist;
  FTitle := Title;
  FAlbum := Album;
  FGenre := Genre;
  FComment := Comment;
end;

procedure TPostProcessSetTagsThread.Execute;
var
  Artist, Title, Album, Genre, Comment: string;
  Arr: TPatternReplaceArray;
  FileTagger: TFileTagger;
  FileSize: Int64;
begin
  inherited;

  FResult := arFail;

  SetLength(Arr, 12);
  Arr[0].C := 'artist';
  Arr[0].Replace := FData.Artist;
  Arr[1].C := 'title';
  Arr[1].Replace := FData.Title;
  Arr[2].C := 'album';
  Arr[2].Replace := FData.Album;
  Arr[3].C := 'genre';
  Arr[3].Replace := FData.Genre;
  Arr[4].C := 'streamname';
  Arr[4].Replace := Trim(FData.Station);
  Arr[5].C := 'streamtitle';
  Arr[5].Replace := Trim(FData.StreamTitle);
  Arr[6].C := 'day';
  Arr[6].Replace := FormatDateTime('dd', Now);
  Arr[7].C := 'month';
  Arr[7].Replace := FormatDateTime('mm', Now);
  Arr[8].C := 'year';
  Arr[8].Replace := FormatDateTime('yy', Now);
  Arr[9].C := 'hour';
  Arr[9].Replace := FormatDateTime('hh', Now);
  Arr[10].C := 'minute';
  Arr[10].Replace := FormatDateTime('nn', Now);
  Arr[11].C := 'second';
  Arr[11].Replace := FormatDateTime('ss', Now);

  FileTagger := TFileTagger.Create;
  try
    try
      if FileTagger.Read(FData.Filename) then
      begin
        Artist := TFunctions.PatternReplaceNew(FArtist, Arr);
        Title := TFunctions.PatternReplaceNew(FTitle, Arr);
        Album := TFunctions.PatternReplaceNew(FAlbum, Arr);
        Genre := TFunctions.PatternReplaceNew(FGenre, Arr);
        Comment := TFunctions.PatternReplaceNew(FComment, Arr);

        FileTagger.Tag.Artist := Artist;
        FileTagger.Tag.Title := Title;
        FileTagger.Tag.Album := Album;
        FileTagger.Tag.Genre := Genre;
        FileTagger.Tag.TrackNumber := IntToStr(FData.TrackNumber);
        FileTagger.Tag.Comment := Comment;

        if FileTagger.Write(Language.CurrentLanguage.LCID, FData.Filename) then
        begin
          if TFunctions.GetFileSize(FData.Filename, FileSize) then
            FData.Filesize := FileSize
          else
            FData.Filesize := -1;

          FResult := arWin;
        end;
      end;
    except
    end;
  finally
    FileTagger.Free;
  end;
end;

{ TPostProcessSetTags }

procedure TPostProcessSetTags.Assign(Source: TPostProcessBase);
begin
  inherited;

  FArtist := TPostProcessSetTags(Source).FArtist;
  FTitle := TPostProcessSetTags(Source).FTitle;
  FAlbum := TPostProcessSetTags(Source).FAlbum;
  FGenre := TPostProcessSetTags(Source).FGenre;
  FComment := TPostProcessSetTags(Source).FComment;
end;

function TPostProcessSetTags.CanProcess(Data: PPostProcessInformation; ProcessingList: TList<TPostProcessBase>): Boolean;
var
  i: Integer;
  M4AActive: Boolean;
begin
  M4AActive := False;
  if ProcessingList <> nil then
    for i := 0 to ProcessingList.Count - 1 do
      if (ProcessingList[i].PostProcessType = ptMP4Box) and (ProcessingList[i].Active) then
      begin
        M4AActive := True;
        Break;
      end;

  Result := ((FilenameToFormat(Data.FilenameConverted) in [atMPEG, atOGG]) or M4AActive) and FGetDependenciesMet;
end;

function TPostProcessSetTags.Configure(AOwner: TComponent; Handle: Cardinal; ShowMessages: Boolean): Boolean;
var
  F: TfrmConfigureSetTags;
begin
  Result := True;

  F := TfrmConfigureSetTags.Create(AOwner, Self, FArtist, FTitle, FAlbum, FGenre, FComment);
  try
    F.ShowModal;

    if F.SaveData then
    begin
      FArtist := F.Artist;
      FTitle := F.Title;
      FAlbum := F.Album;
      FGenre := F.Genre;
      FComment := F.Comment;
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

  //  FNeededAddons.Add(TAddonAudioGenie);

  FCanConfigure := True;
  FGroupID := 1;
  FOrder := 1010;

  FPostProcessType := ptSetTags;

  FArtist := '%artist%';
  FTitle := '%title%';
  FAlbum := '%album%';
  FGenre := '%genre%';
  FComment := _('%streamname% / %streamtitle% / Recorded using streamWriter');
end;

function TPostProcessSetTags.FGetHash: Cardinal;
begin
  Result := inherited + TFunctions.HashString(FArtist + FAlbum + FTitle + FComment);
end;

function TPostProcessSetTags.FGetHelp: string;
begin
  Result := _('This postprocessor writes tags to recorded songs.');
end;

function TPostProcessSetTags.FGetName: string;
begin
  Result := _('Write tags to recorded songs');
end;

procedure TPostProcessSetTags.Load(Stream: TStream; Version: Integer);
begin
  inherited;

  Stream.Read(FArtist, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(FAlbum, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(FTitle, IfThen<Boolean>(Version > 68, True, False));
  Stream.Read(FComment, IfThen<Boolean>(Version > 68, True, False));

  if Version > 67 then
    Stream.Read(FGenre, IfThen<Boolean>(Version > 68, True, False));

  if Version < 64 then
  begin
    FArtist := ConvertPattern(FArtist);
    FAlbum := ConvertPattern(FAlbum);
    FTitle := ConvertPattern(FTitle);
    FComment := ConvertPattern(FComment);
  end;
end;

function TPostProcessSetTags.ProcessFile(Data: PPostProcessInformation): TPostProcessThreadBase;
begin
  Result := TPostProcessSetTagsThread.Create(Data, Self, FArtist, FTitle, FAlbum, FGenre, FComment);
end;

procedure TPostProcessSetTags.Save(Stream: TMemoryStream);
begin
  inherited;

  Stream.Write(FArtist, True);
  Stream.Write(FAlbum, True);
  Stream.Write(FTitle, True);
  Stream.Write(FComment, True);
  Stream.Write(FGenre, True);
end;

end.
