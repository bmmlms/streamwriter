{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2017 Alexander Nottelmann

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

unit AppMessages;

interface

uses
  Windows, SysUtils, MessageBus, TypeDefs, Logging;

type
  TSWMessageBus = class(TMessageBus)
  public
    procedure SendMessage(Msg: TMessageBase); override;
  end;

  TFileModifyMsg = class(TMessageBase)
  private
    FFilename: string;
  public
    constructor Create(Filename: string);
    property Filename: string read FFilename;
  end;

  TVolumeChangedMsg = class(TMessageBase)
  private
    FVolume: Integer;
  public
    constructor Create(Volume: Integer);
    property Volume: Integer read FVolume;
  end;

  TPlayingObjectChangedMsg = class(TMessageBase)
  private
    FSender: TObject;
    FArtist, FTitle, FStream, FFilename: string;
  public
    constructor Create(Sender: TObject; Artist, Title, Stream, Filename: string);

    property Sender: TObject read FSender;
    property Artist: string read FArtist;
    property Title: string read FTitle;
    property Stream: string read FStream;
    property Filename: string read FFilename;
  end;

  TPlayingObjectStopped = class(TMessageBase)
  private
    FSender: TObject;
  public
    constructor Create(Sender: TObject);
    property Sender: TObject read FSender;
  end;

  TRefreshServerDataMsg = class(TMessageBase)
  end;

  TListsChangedMsg = class(TMessageBase)
  end;

  TSongSavedMsg = class(TMessageBase)
  private
    FServerTitleHash: Cardinal;
    FServerArtistHash: Cardinal;
  public
    constructor Create(Sender: TObject; ServerTitleHash, ServerArtistHash: Cardinal);

    property ServerTitleHash: Cardinal read FServerTitleHash;
    property ServerArtistHash: Cardinal read FServerArtistHash;
  end;

  TSelectSavedSongsMsg = class(TMessageBase)
  private
    FTitleHashes: TCardinalArray;
    FArtistHashes: TCardinalArray;
  public
    constructor Create(Sender: TObject; TitleHashes, ArtistHashes: TCardinalArray);

    property TitleHashes: TCardinalArray read FTitleHashes;
    property ArtistHashes: TCardinalArray read FArtistHashes;
  end;

  TLogMsg = class(TMessageBase)
  private
    FSource: TLogSource;
    FLogType: TLogType;
    FLogLevel: TLogLevel;
    FTime: TDateTime;
    FSourceText: string;
    FText: string;
  public
    constructor Create(Sender: TObject; Source: TLogSource; LogType: TLogType; LogLevel: TLogLevel; SourceText, Text: string);

    property Source: TLogSource read FSource;
    property LogType: TLogType read FLogType;
    property LogLevel: TLogLevel read FLogLevel;
    property Time: TDateTime read FTime;
    property SourceText: string read FSourceText;
    property Text: string read FText;
  end;

implementation

{ TDeleteFileMessage }

constructor TFileModifyMsg.Create(Filename: string);
begin
  inherited Create;

  FFilename := Filename;
end;

{ TVolumeChangedMsg }

constructor TVolumeChangedMsg.Create(Volume: Integer);
begin
  inherited Create;

  FVolume := Volume;
end;

{ TPlayingObjectChangedMsg }

constructor TPlayingObjectChangedMsg.Create(Sender: TObject; Artist, Title, Stream, Filename: string);
begin
  inherited Create;

  FSender := Sender;
  FArtist := Artist;
  FTitle := Title;
  FStream := Stream;
  FFilename := Filename;
end;

{ TPlayingObjectStopped }

constructor TPlayingObjectStopped.Create(Sender: TObject);
begin
  inherited Create;

  FSender := Sender;
end;

{ TSongSavedMsg }

constructor TSongSavedMsg.Create(Sender: TObject; ServerTitleHash, ServerArtistHash: Cardinal);
begin
  inherited Create;

  FServerTitleHash := ServerTitleHash;
  FServerArtistHash := ServerArtistHash;
end;

{ TSelectSavedSongs }

constructor TSelectSavedSongsMsg.Create(Sender: TObject; TitleHashes,
  ArtistHashes: TCardinalArray);
begin
  inherited Create;

  FTitleHashes := TitleHashes;
  FArtistHashes := ArtistHashes;
end;

{ TLogMsg }

constructor TLogMsg.Create(Sender: TObject; Source: TLogSource; LogType: TLogType; LogLevel: TLogLevel; SourceText, Text: string);
begin
  inherited Create;

  FSource := Source;
  FLogType := LogType;
  FTime := Now;
  FLogLevel := LogLevel;
  FSourceText := SourceText;
  FText := Text;
end;

{ TSWMessageBus }

procedure TSWMessageBus.SendMessage(Msg: TMessageBase);
var
  LogMsg: TLogMsg absolute Msg;
begin
  if Msg is TLogMsg then
  begin
    {$IFNDEF DEBUG}
    if LogMsg.FLogLevel = llDebug then
    begin
      LogMsg.Free;
      Exit;
    end;
    {$ENDIF}

    TLogger.Write(FormatDateTime('yyyy-mm-dd - hh:nn:ss', LogMsg.FTime) + ' - ' + LogMsg.FSourceText + ' - ' + LogMsg.FText);
  end;

  // Inherited unten, weil da Msg.Free gemacht wird.
  inherited;
end;

end.
