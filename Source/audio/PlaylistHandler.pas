{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2021 Alexander Nottelmann

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

unit PlaylistHandler;

interface

uses
  Windows, SysUtils, StrUtils, Classes, Functions, ExtendedStream,
  AudioFunctions;

type
  TPlaylistHandler = class
  private
    FURLs: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function ParsePlaylist(Filename: string): Boolean; overload;
    function ParsePlaylist(Data: string; PlaylistType: TPlaylistTypes; OriginURL: string): Boolean; overload;

    property URLs: TStringList read FURLs;
  end;

implementation

{ TPlaylistHandler }

constructor TPlaylistHandler.Create;
begin
  inherited;

  FURLs := TStringList.Create;
end;

destructor TPlaylistHandler.Destroy;
begin
  FURLs.Free;

  inherited;
end;

function TPlaylistHandler.ParsePlaylist(Filename: string): Boolean;
var
  Ext: string;
  MS: TExtendedStream;
begin
  Result := False;

  Ext := LowerCase(ExtractFileExt(Filename));

  if (Ext <> '.m3u') and (Ext <> '.pls') then
    Exit;

  MS := TExtendedStream.Create;
  try
    MS.LoadFromFile(Filename);

    if Ext = '.m3u' then
      Result := ParsePlaylist(MS.ToString, ptM3U, '')
    else if Ext = '.pls' then
      Result := ParsePlaylist(MS.ToString, ptPLS, '');
  finally
    MS.Free;
  end;
end;

function TPlaylistHandler.ParsePlaylist(Data: string; PlaylistType: TPlaylistTypes; OriginURL: string): Boolean;
  procedure ParseLine(Line: string);
  var
    Res: TParseURLRes;
    SlashPos: Integer;
    OriginPort: Integer;
    OriginProto, LineProto: string;
  begin
    if Pos('\', Line) > 0 then
      Exit;

    // Wenn es eine vorherige URL gab, und die nun empfangene keine "ganze" URL ist,
    // dann müssen wir die empfangene an den Pfad der vorherigen dranhängen.
    if (OriginURL <> '') and (Length(Line) > 0) and (Copy(LowerCase(Line), 1, 7) <> 'http://') and (Copy(LowerCase(Line), 1, 8) <> 'https://') then
    begin
      Res := ParseURL(OriginURL);
      if Res.Success then
      begin
        if Res.PortDetected then
          OriginPort := Res.Port
        else if Res.Secure then
          OriginPort := 443
        else
          OriginPort := 80;

        if Res.Secure then
          OriginProto := 'https://'
        else
          OriginProto := 'http://';

        if Line[1] = '/' then
        begin
          Line := OriginProto + Res.Host + ':' + IntToStr(OriginPort) + Line
        end else
        begin
          SlashPos := LastDelimiter('/', Res.Data);
          if SlashPos > 0 then
            Res.Data := Copy(Res.Data, 1, SlashPos);

          Line := OriginProto + Res.Host + ':' + IntToStr(OriginPort) + Res.Data + Line
        end;
      end;
    end;

    Res := ParseURL(Line);
    if Res.Success then
    begin
      if Res.Secure then
        LineProto := 'https://'
      else
        LineProto := 'http://';

      if not Res.PortDetected then
      begin
        // Es gibt keinen Standard scheinbar - beide nehmen.
        FURLs.Add(LineProto + Res.Host + Res.Data);
        FURLs.Add('http://' + Res.Host + ':6666' + Res.Data);
      end else
      begin
        FURLs.Add(LineProto + Res.Host + ':' + IntToStr(Res.Port) + Res.Data);
      end;
    end;
  end;
var
  Offset, Offset2, Offset3: Integer;
  Line: string;
begin
  FURLs.Clear;
  Offset := 1;

  case PlaylistType of
    ptM3U:
      while True do
      begin
        Offset2 := PosEx(#10, Data, Offset);
        if Offset2 > 0 then
          Line := Trim(Copy(Data, Offset, Offset2 - Offset))
        else
          Line := Trim(Copy(Data, Offset, Length(Data)));

        Offset := Offset2 + 1;

        if (Length(Line) >= 1) and (Line[1] <> '#') then
          ParseLine(Line);

        if Offset2 = 0 then
          Break;
      end;
    ptPLS:
      while True do
      begin
        Offset2 := PosEx(#10, Data, Offset);
        if Offset2 > 0 then
          Line := Trim(Copy(Data, Offset, Offset2 - Offset))
        else
          Line := Trim(Copy(Data, Offset, Length(Data)));

        Offset := Offset2 + 1;

        if Copy(LowerCase(Line), 1, 4) = 'file' then
        begin
          Offset3 := Pos('=', Line);
          if Offset3 > 0 then
          begin
            Line := Trim(Copy(Line, Offset3 + 1, Length(Line) - Offset3));
            if (Line <> '') then
              ParseLine(Line);
          end;
        end;

        if Offset2 = 0 then
          Break;
      end;
    ptUnknown:
      while True do
      begin
        Offset2 := PosEx(#10, Data, Offset);
        if Offset2 > 0 then
          Line := Trim(Copy(Data, Offset, Offset2 - Offset))
        else
          Line := Trim(Copy(Data, Offset, Length(Data)));

        Offset := Offset2 + 1;

        if (Line <> '') then
          ParseLine(Line);

        if Offset2 = 0 then
          Break;
      end;
  end;

  Result := FURLs.Count > 0;
end;

end.
