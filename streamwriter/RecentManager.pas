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
unit RecentManager;

interface

uses
  Windows, Classes, SysUtils, ExtendedStream, ClientManager, ICEClient,
  Generics.Collections, ComCtrls, AppData, Functions;

type
  TStreamList = class;
  TDataLists = class;

  EVersionException = class(Exception);

  TTrackInfo = class
  private
    FTime: TDateTime;
    FFilename: string;
    FFilesize: UInt64;
    FWasCut: Boolean;
    FHash: Cardinal;
  public
    constructor Create(Time: TDateTime; Filename: string);
    property Time: TDateTime read FTime;
    property Filename: string read FFilename write FFilename;
    property Filesize: UInt64 read FFilesize write FFilesize;
    property WasCut: Boolean read FWasCut write FWasCut;
    property Hash: Cardinal read FHash;
  end;

  TTitleInfo = class
  private
    FStreamTitle: string;
  public
    constructor Create(StreamTitle: string); overload;

    class function Load(Stream: TExtendedStream; Version: Integer): TTitleInfo;
    procedure Save(Stream: TExtendedStream);

    property StreamTitle: string read FStreamTitle;
  end;

  TStreamEntry = class(TObject)
  private
    FParent: TStreamList;

    FName: string;
    FStartURL: string;
    FURLs: TStringList;
    FBitRate: Cardinal;
    FGenre: string;
    FSkipShort: Boolean;
    FUseFilter: TUseFilters;
    FSubmitted: Boolean;

    FIsInList: Boolean;
    FRecentIndex: Integer;
    FLastTouched: TDateTime;
    FTracks: TList<TTrackInfo>;
    FSongsSaved: Cardinal;
    FBytesReceived: UInt64;

    procedure FSetName(Value: string);
    procedure FSetIsInList(Value: Boolean);
    procedure FSetRecentIndex(Value: Integer);
    procedure Changed;
  public
    constructor Create(Parent: TStreamList);
    destructor Destroy; override;

    function Copy: TStreamEntry;
    class function Load(Stream: TExtendedStream; Version: Integer): TStreamEntry;
    procedure Save(Stream: TExtendedStream);

    property Parent: TStreamList read FParent write FParent;
    property Name: string read FName write FSetName;
    property StartURL: string read FStartURL write FStartURL;
    property URLs: TStringList read FURLs;
    property BitRate: Cardinal read FBitRate write FBitRate;
    property Genre: string read FGenre write FGenre;
    property SkipShort: Boolean read FSkipShort write FSkipShort;
    property UseFilter: TUseFilters read FUseFilter write FUseFilter;
    property Submitted: Boolean read FSubmitted write FSubmitted;

    property IsInList: Boolean read FIsInList write FSetIsInList;
    property RecentIndex: Integer read FRecentIndex write FSetRecentIndex;
    property LastTouched: TDateTime read FLastTouched write FLastTouched;
    property Tracks: TList<TTrackInfo> read FTracks write FTracks;
    property SongsSaved: Cardinal read FSongsSaved write FSongsSaved;
    property BytesReceived: UInt64 read FBytesReceived write FBytesReceived;
  end;

  TStreamChangedEvent = procedure(Sender: TObject; Stream: TStreamEntry) of object;

  TStreamList = class(TList<TStreamEntry>)
  private
    FOnStreamChanged: TStreamChangedEvent;
  public
    function Add(Name: string; URL: string; URLs: TStringList; BitRate: Cardinal; Genre: string;
      SkipShort: Boolean; UseFilter: TUseFilters; SongsSaved: Cardinal): TStreamEntry; overload;
    function Add(Entry: TStreamEntry): TStreamEntry; overload;
    function Get(Client: TICEClient): TStreamEntry; overload;
    function Get(Name, URL: string; URLs: TStringList): TStreamEntry; overload;
    procedure RemoveTrack(Track: TTrackInfo);

    property OnStreamChanged: TStreamChangedEvent read FOnStreamChanged write FOnStreamChanged;
  end;

  TTitleList = class(TList<TTitleInfo>)
  end;

  TDataLists = class
  private
    FStreamList: TStreamList;
    FSaveList: TTitleList;
    FIgnoreList: TTitleList;
    FLoadError: Boolean;
    FReceived: UInt64;

    procedure CleanUp;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load;
    function Save: Boolean;

    property StreamList: TStreamList read FStreamList;
    property SaveList: TTitleList read FSaveList;
    property IgnoreList: TTitleList read FIgnoreList;

    property LoadError: Boolean read FLoadError write FLoadError;
    property Received: UInt64 read FReceived write FReceived;
  end;

  // Alte Klasse, die irgendwann raus kann.
  TRecent = class
  private
    FList: TStreamList;
    FLoadError: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    property LoadError: Boolean read FLoadError write FLoadError;
    property List: TStreamList read FList;
  end;

  // Alte Klasse, die irgendwann raus kann.
  TListList = class
  private
    FList: TStreamList;
    FLoadError: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    property LoadError: Boolean read FLoadError write FLoadError;
    property List: TStreamList read FList;
  end;

const
  DATAVERSION = 3;

  // Irgendwann raus damit, z.B. wenn Computerbild endlich mal den Download aktualisiert!
  RECENTVERSION = 3;
  LISTVERSION = 2;

implementation

{ TStreamEntry }

procedure TStreamEntry.Changed;
begin
  if FParent <> nil then
    if Assigned(FParent.OnStreamChanged) then
      FParent.OnStreamChanged(FParent, Self);
end;

function TStreamEntry.Copy: TStreamEntry;
begin
  Result := TStreamEntry.Create(nil);
  Result.Name := Name;
  Result.IsInList := IsInList;
  Result.RecentIndex := RecentIndex;
  Result.StartURL := StartURL;
  Result.BitRate := BitRate;
  Result.Genre := Genre;
  Result.SkipShort := SkipShort;
  Result.SongsSaved := SongsSaved;
  Result.Submitted := Submitted;
  Result.UseFilter := UseFilter;
  Result.URLs.Assign(URLs);
end;

constructor TStreamEntry.Create(Parent: TStreamList);
begin
  FParent := Parent;
  FURLs := TStringList.Create;
  FRecentIndex := -1;
  FIsInList := False;
  FLastTouched := Now;
  FTracks := TList<TTrackInfo>.Create;
  FSongsSaved := 0;
  FBitRate := 0;
  FSubmitted := False;
  FUseFilter := ufNone;
end;

destructor TStreamEntry.Destroy;
var
  i: Integer;
begin
  FURLs.Free;
  for i := 0 to FTracks.Count - 1 do
    FTracks[i].Free;
  FTracks.Free;
  inherited;
end;

procedure TStreamEntry.FSetName(Value: string);
begin
  FName := Value;
  Changed;
end;

class function TStreamEntry.Load(Stream: TExtendedStream; Version: Integer): TStreamEntry;
var
  B: Byte;
  i: Integer;
  Count: Cardinal;
  URL: string;
  TrackInfo: TTrackInfo;
begin
  Result := TStreamEntry.Create(nil);
  Stream.Read(Result.FName);
  Stream.Read(Result.FStartURL);
  Stream.Read(Count);
  for i := 0 to Count - 1 do
  begin
    Stream.Read(URL);
    Result.FURLs.Add(URL);
  end;
  Stream.Read(Result.FBitRate);
  Stream.Read(Result.FGenre);
  Stream.Read(Result.FSkipShort);
  if Version >= 3 then
  begin
    Stream.Read(B);
    Result.FUseFilter := TUseFilters(B);
  end;
  Stream.Read(Result.FSubmitted);

  Stream.Read(Result.FIsInList);
  Stream.Read(Result.FRecentIndex);
  Stream.Read(Result.FLastTouched);

  Stream.Read(Count);
  for i := 0 to Count - 1 do
  begin
    TrackInfo := TTrackInfo.Create(Now, '');
    Stream.Read(TrackInfo.FTime);
    Stream.Read(TrackInfo.FFilename);
    TrackInfo.FHash := HashString(LowerCase(ExtractFileName(TrackInfo.FFilename)));
    if Version > 1 then
    begin
      Stream.Read(TrackInfo.FFilesize);
      Stream.Read(TrackInfo.FWasCut);
    end;
    Result.FTracks.Add(TrackInfo);
  end;
  Stream.Read(Result.FSongsSaved);
  Stream.Read(Result.FBytesReceived);
end;

procedure TStreamEntry.Save(Stream: TExtendedStream);
var
  i: Integer;
begin
  Stream.Write(FName);
  Stream.Write(FStartURL);
  Stream.Write(FURLs.Count);
  for i := 0 to FURLs.Count - 1 do
  begin
    Stream.Write(FURLs[i]);
  end;
  Stream.Write(FBitRate);
  Stream.Write(FGenre);
  Stream.Write(FSkipShort);

  Stream.Write(Byte(FUseFilter));
  Stream.Write(FSubmitted);

  Stream.Write(FIsInList);
  Stream.Write(FRecentIndex);
  Stream.Write(FLastTouched);

  Stream.Write(FTracks.Count);
  for i := 0 to FTracks.Count - 1 do
  begin
    Stream.Write(FTracks[i].FTime);
    Stream.Write(FTracks[i].FFilename);
    Stream.Write(FTracks[i].FFilesize);
    Stream.Write(FTracks[i].FWasCut);
  end;
  Stream.Write(FSongsSaved);
  Stream.Write(FBytesReceived);
end;

procedure TStreamEntry.FSetIsInList(Value: Boolean);
begin
  FIsInList := Value;
  Changed;
end;

procedure TStreamEntry.FSetRecentIndex(Value: Integer);
  function RemoveOld: Boolean;
  var
    i: Integer;
    Greatest, GreatestIndex, RecentCount: Integer;
  begin
    Result := False;
    Greatest := -1;
    GreatestIndex := -1;
    RecentCount := 0;
    for i := 0 to FParent.Count - 1 do
    begin
      if FParent[i].RecentIndex > -1 then
        Inc(RecentCount);
      if FParent[i].RecentIndex > GreatestIndex then
      begin
        Greatest := i;
        GreatestIndex := FParent[i].RecentIndex;
      end;
    end;

    if RecentCount > 15 then
    begin
      FParent[Greatest].RecentIndex := -1;
      Result := True;
    end;
  end;
var
  i: Integer;
  HasZero: Boolean;
begin
  FRecentIndex := Value;
  if (FParent <> nil) and (Value = 0) then
  begin
    HasZero := False;
    for i := 0 to FParent.Count - 1 do
    begin
      if (FParent[i].RecentIndex = 0) and (FParent[i] <> Self) then
      begin
        HasZero := True;
        Break;
      end;
    end;

    if HasZero then
      for i := 0 to FParent.Count - 1 do
      begin
        if FParent[i].FRecentIndex > -1 then
          FParent[i].FRecentIndex := FParent[i].FRecentIndex + 1;
      end;

    while RemoveOld do
      RemoveOld;
  end;
  Changed;
end;

{ TStreamDataList }

procedure TDataLists.CleanUp;
var
  i: Integer;
begin
  for i := FStreamList.Count - 1 downto 0 do
    if (FStreamList[i].FLastTouched < Now - 60) and (FStreamList[i].FTracks.Count = 0) and
       (not FStreamList[i].IsInList) and (FStreamList[i].RecentIndex = -1) then
    begin
      FStreamList[i].Free;
      FStreamList.Delete(i);
    end;
end;

constructor TDataLists.Create;
begin
  inherited;

  FLoadError := False;
  FReceived := 0;
  FStreamList := TStreamList.Create;
  FSaveList := TTitleList.Create;
  FIgnoreList := TTitleList.Create;
end;

destructor TDataLists.Destroy;
var
  i: Integer;
begin
  for i := 0 to FStreamList.Count - 1 do
    FStreamList[i].Free;
  FStreamList.Free;

  for i := 0 to FSaveList.Count - 1 do
    FSaveList[i].Free;
  FSaveList.Free;

  for i := 0 to FIgnoreList.Count - 1 do
    FIgnoreList[i].Free;
  FIgnoreList.Free;

  inherited;
end;

procedure TDataLists.Load;
var
  Entry: TStreamEntry;
  TitleInfo: TTitleInfo;
  S: TExtendedStream;
  Version, EntryCount: Integer;
  i: Integer;
begin
  if AppGlobals.DataFile = '' then
    Exit;

  S := TExtendedStream.Create;
  try
    try
      S.LoadFromFile(AppGlobals.DataFile);
    except
      Exit;
    end;

    try
      S.Read(Version);

      if Version > DATAVERSION then
        raise EVersionException.Create(AppGlobals.DataFile);

      S.Read(FReceived);

      if Version <= 2 then
      begin
        while S.Position < S.Size do
        begin
          Entry := TStreamEntry.Load(S, Version);
          Entry.FParent := FStreamList;
          FStreamList.Add(Entry);
        end;
      end else
      begin
        S.Read(EntryCount);
        for i := 0 to EntryCount - 1 do
        begin
          Entry := TStreamEntry.Load(S, Version);
          Entry.FParent := FStreamList;
          FStreamList.Add(Entry);
        end;

        if Version >= 3 then
        begin
          S.Read(EntryCount);
          for i := 0 to EntryCount - 1 do
          begin
            TitleInfo := TTitleInfo.Load(S, Version);
            FSaveList.Add(TitleInfo);
          end;
          S.Read(EntryCount);
          for i := 0 to EntryCount - 1 do
          begin
            TitleInfo := TTitleInfo.Load(S, Version);
            FIgnoreList.Add(TitleInfo);
          end;
        end;
      end;
    except
      on E: EVersionException do
      begin
        FReceived := 0;
        FLoadError := True;
        raise;
      end;
      on E: Exception do
      begin
        FReceived := 0;
        FLoadError := True;
        raise Exception.Create(AppGlobals.DataFile);
      end;
    end;
  finally
    S.Free;
  end;
end;

function TDataLists.Save: Boolean;
var
  i: Integer;
  S: TExtendedStream;
begin
  Result := False;

  if (AppGlobals.SkipSave) or (AppGlobals.DataFile = '') then
  begin
    Result := True;
    Exit;
  end;

  CleanUp;

  if (FStreamList.Count = 0) and (FIgnoreList.Count = 0) and (FSaveList.Count = 0) and not (FileExists(AppGlobals.DataFile)) then
  begin
    Result := True;
    Exit;
  end;

  if not FLoadError then
  begin
    S := TExtendedStream.Create;
    try
      S.Write(Integer(DATAVERSION));

      S.Write(FReceived);

      S.Write(FStreamList.Count);
      for i := 0 to FStreamList.Count - 1 do
      begin
        FStreamList[i].Save(S);
      end;

      S.Write(FSaveList.Count);
      for i := 0 to FSaveList.Count - 1 do
      begin
        FSaveList[i].Save(S);
      end;

      S.Write(FIgnoreList.Count);
      for i := 0 to FIgnoreList.Count - 1 do
      begin
        FIgnoreList[i].Save(S);
      end;

      S.SaveToFile(AppGlobals.DataFile);
    finally
      S.Free;
    end;
  end;

  Result := True;
end;

{ TListList }

constructor TListList.Create;
begin
  inherited Create;
  FLoadError := False;
  FList := TStreamList.Create;
end;

destructor TListList.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Free;
  FList.Free;
end;

procedure TListList.Load;
var
  n, Count, Version: Integer;
  Tmp: AnsiString;
  Name, URL: string;
  Rec: TStreamEntry;
  S: TExtendedStream;
  F: Boolean;
begin
  if AppGlobals.ListFile = '' then
    Exit;

  S := TExtendedStream.Create;
  try
    try
      S.LoadFromFile(AppGlobals.ListFile);
    except
      Exit;
    end;

    try
      S.Read(Version);

      if Version > LISTVERSION then
        raise EVersionException.Create(AppGlobals.ListFile);

      while S.Position < S.Size do
      begin
        Rec := TStreamEntry.Create(nil);
        if Version = 1 then
        begin
          S.Read(Tmp);
          Rec.Name := string(Tmp);
          S.Read(Tmp);
          Rec.StartURL := string(Tmp);
        end else
        begin
          S.Read(Name);
          Rec.Name := Name;
          S.Read(URL);
          Rec.StartURL := URL;
        end;
        S.Read(F);
        S.Read(F);
        S.Read(F);
        Rec.SkipShort := F;
        S.Read(Count);
        for n := 0 to Count - 1 do
        begin
          if Version = 1 then
          begin
            S.Read(Tmp);
            URL := string(Tmp);
          end else
            S.Read(URL);
          Rec.URLs.Add(URL);
        end;
        Rec.FParent := List;
        List.Add(Rec);
      end;
    except
      on E: EVersionException do
      begin
        FLoadError := True;
        raise;
      end;
      on E: Exception do
      begin
        FLoadError := True;
        raise Exception.Create(AppGlobals.ListFile);
      end;
    end;
  finally
    S.Free;
  end;
end;

{ TRecent }

constructor TRecent.Create;
begin
  FLoadError := False;
  FList := TStreamList.Create;
end;

destructor TRecent.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].Free;
  FList.Free;
end;

procedure TRecent.Load;
var
  n, Count: Integer;
  Tmp: AnsiString;
  Name, URL: string;
  Rec: TStreamEntry;
  S: TExtendedStream;
  Version: Integer;
  F: Boolean;
begin
  if AppGlobals.RecentFile = '' then
    Exit;

  S := TExtendedStream.Create;
  try
    try
      S.LoadFromFile(AppGlobals.RecentFile);
    except
      Exit;
    end;

    try
      S.Read(Version);

      if Version > RECENTVERSION then
        raise EVersionException.Create(AppGlobals.RecentFile);

      while S.Position < S.Size do
      begin
        Rec := TStreamEntry.Create(nil);
        if Version <= 2 then
        begin
          S.Read(Tmp);
          Rec.Name := string(Tmp);
          s.Read(Tmp);
          Rec.StartURL := string(Tmp);
        end else
        begin
          S.Read(Name);
          Rec.Name := Name;
          S.Read(URL);
          Rec.StartURL := URL;
        end;
        if Version >= 2 then
        begin
          S.Read(F);
          S.Read(F);
          S.Read(F);
          Rec.SkipShort := F;
        end else
        begin
          // Die Felder wurden vor Version 2 noch nicht gespeichert, also Default-Werte benutzen
          Rec.SkipShort := True;
        end;
        S.Read(Count);
        for n := 0 to Count - 1 do
        begin
          if Version <= 2 then
          begin
            S.Read(Tmp);
            URL := string(Tmp);
          end else
            S.Read(URL);
          Rec.URLs.Add(URL);
        end;
        Rec.FParent := FList;
        FList.Add(Rec);
      end;
    except
      on E: EVersionException do
      begin
        FLoadError := True;
        raise;
      end;
      on E: Exception do
      begin
        FLoadError := True;
        raise Exception.Create(AppGlobals.RecentFile);
      end;
    end;
  finally
    S.Free;
  end;
end;

{ TTrackInfo }

constructor TTrackInfo.Create(Time: TDateTime; Filename: string);
begin
  FTime := Time;
  FFilename := Filename;
  FWasCut := False;
  FHash := HashString(LowerCase(ExtractFileName(Filename)));
end;

{ TStreamList }

function TStreamList.Add(Entry: TStreamEntry): TStreamEntry;
begin
  Result := Get(Entry.Name, Entry.StartURL, Entry.URLs);

  if Result <> nil then
  begin
    Exit;
  end;

  Result := Entry;
  inherited Add(Result);

  if Assigned(FOnStreamChanged) then
    FOnStreamChanged(Self, Entry);
end;

function TStreamList.Get(Client: TICEClient): TStreamEntry;
begin
  Result := Get(Client.StreamName, Client.StartURL, Client.URLs);
end;

function TStreamList.Add(Name, URL: string;
  URLs: TStringList; BitRate: Cardinal; Genre: string; SkipShort: Boolean; UseFilter: TUseFilters; SongsSaved: Cardinal): TStreamEntry;
var
  Entry: TStreamEntry;
begin
  Result := Get(Name, URL, URLs);

  if Result <> nil then
  begin
    if BitRate > 0 then
      Result.BitRate := BitRate;
    if Genre <> '' then
      Result.Genre := Genre;
    Exit;
  end;

  Entry := TStreamEntry.Create(Self);
  Entry.Name := Name;
  Entry.StartURL := URL;
  Entry.URLs.Assign(URLs);
  Entry.SkipShort := SkipShort;
  Entry.BitRate := BitRate;
  Entry.Genre := Genre;
  Entry.SongsSaved := SongsSaved;
  Entry.UseFilter := UseFilter;

  Entry.FParent := Self;

  Add(Entry);

  Result := Entry;
end;

function TStreamList.Get(Name, URL: string;
  URLs: TStringList): TStreamEntry;
var
  i, n, j: Integer;
begin
  Name := Trim(Name);
  URL := Trim(URL);

  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Name <> '' then
      if LowerCase(Items[i].Name) = LowerCase(Name) then
      begin
        Result := Items[i];
        Exit;
      end;

    if URL <> '' then
      if LowerCase(Items[i].StartURL) = LowerCase(URL) then
      begin
        Result := Items[i];
        Exit;
      end;
    for n := 0 to Items[i].URLs.Count - 1 do
    begin
      if URL <> '' then
        if LowerCase(Items[i].URLs[n]) = LowerCase(URL) then
        begin
          Result := Items[i];
          Exit;
        end;
      if URLs <> nil then
        for j := 0 to URLs.Count - 1 do
          if LowerCase(URL) = LowerCase(URLs[j]) then
          begin
            Result := Items[i];
            Exit;
          end;
    end;
  end;
end;

procedure TStreamList.RemoveTrack(Track: TTrackInfo);
var
  i: Integer;
  n: Integer;
begin
  for i := 0 to Count - 1 do
    for n := Items[i].Tracks.Count - 1 downto 0 do
      if Items[i].Tracks[n] = Track then
      begin
        Items[i].Tracks[n].Free;
        Items[i].Tracks.Delete(n);
        Exit;
      end;
end;

{ TTitleInfo }

constructor TTitleInfo.Create(StreamTitle: string);
begin
  inherited Create;

  FStreamTitle := StreamTitle;
end;

class function TTitleInfo.Load(Stream: TExtendedStream;
  Version: Integer): TTitleInfo;
begin
  Result := TTitleInfo.Create;
  Stream.Read(Result.FStreamTitle);
end;

procedure TTitleInfo.Save(Stream: TExtendedStream);
begin
  Stream.Write(FStreamTitle);
end;

end.

