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

{ Unit TypeDefs }
unit TypeDefs;

interface

uses
  Windows, SysUtils;

type
  TDebugTypes = (dtSocket, dtMessage, dtSong, dtError, dtSaved, dtPostProcess);
  TDebugLevels = (dlNormal, dlDebug);

  // Defines all possible types of audio-data
  TAudioTypes = (atNone, atMPEG, atAAC, atOGG, atM4A);
  // Defines all possible types of lists
  TListType = (ltSave, ltIgnore);

function FiletypeToFormat(Filename: string): TAudioTypes;
function FormatToFiletype(Format: TAudioTypes): string;
function FormatToDesc(Format: TAudioTypes): string;

implementation

function FiletypeToFormat(Filename: string): TAudioTypes;
begin
  Result := atNone;

  Filename := LowerCase(ExtractFileExt(Filename));

  if Filename = '.mp3' then
    Exit(atMPEG)
  else if Filename = '.aac' then
    Exit(atAAC)
  else if Filename = '.ogg' then
    Exit(atOGG)
  else if Filename = '.m4a' then
    Exit(atM4A);
end;

function FormatToFiletype(Format: TAudioTypes): string;
begin
  Result := '';
  case Format of
    atNone: ;
    atMPEG: Result := '.mp3';
    atAAC: Result := '.aac';
    atOGG: Result := '.ogg';
    atM4A: Result := '.m4a';
  end;
end;

function FormatToDesc(Format: TAudioTypes): string;
begin
  Result := '';
  case Format of
    atNone: ;
    atMPEG: Result := 'MP3';
    atAAC: Result := 'AAC';
    atOGG: Result := 'OGG';
  end;
end;

end.
