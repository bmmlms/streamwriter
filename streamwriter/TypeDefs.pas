{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2013 Alexander Nottelmann

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
  Windows, SysUtils, Classes;

type
  TStringEvent = procedure(Sender: TObject; Data: string) of object;

  TDebugTypes = (dtSocket, dtMessage, dtSong, dtError, dtSaved, dtPostProcess);
  TDebugLevels = (dlNormal, dlDebug);

  // Defines all possible types of lists
  TListType = (ltSave, ltIgnore, ltAutoDetermine);

  // Do not change the values' order since the enum is used when saving settings
  TStreamOpenActions = (oaStart, oaPlay, oaPlayExternal, oaAdd, oaOpenWebsite, oaBlacklist,
    oaCopy, oaSave, oaSetData, oaRefresh, oaRate1, oaRate2, oaRate3, oaRate4,
    oaRate5, oaNone);

  TStartStreamingInfo = record
  public
    ID, Bitrate: Cardinal;
    Name, URL, TitlePattern: string;
    IgnoreTitles: TStringList;
    constructor Create(ID, Bitrate: Cardinal; Name, URL, TitlePattern: string; IgnoreTitles: TStringList);
  end;
  TStartStreamingInfoArray = array of TStartStreamingInfo;

implementation

{ TStartStreamingInfo }

constructor TStartStreamingInfo.Create(ID, Bitrate: Cardinal; Name, URL,
  TitlePattern: string; IgnoreTitles: TStringList);
begin
  Self.ID := ID;
  Self.Bitrate := Bitrate;
  Self.Name := Name;
  Self.URL := Trim(URL);
  Self.TitlePattern := TitlePattern;
  Self.IgnoreTitles := IgnoreTitles;
end;

end.
