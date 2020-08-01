{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2020 Alexander Nottelmann

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

{ This unit encapsulates basic functionality for tabe used in the main-window }
unit Tabs;

interface

uses
  Windows, SysUtils, Classes, Messages, ComCtrls, ActiveX, Controls, Buttons,
  StdCtrls, Menus, VirtualTrees, DragDrop, DragDropFile, ShellApi,
  Themes, ImgList, AppData, GUIFunctions, LanguageObjects, MControls,
  DataManager, Dialogs, Functions, Logging, SWFunctions;

type
  TMainTabSheet = class;

  TTrackEvent = procedure(Station: TStreamEntry; Track: TTrackInfo) of object;

  TPlaylistEntry = record
    Name: string;
    URL: string;
  end;
  TPlaylistEntryArray = array of TPlaylistEntry;

  TMainPageControl = class(TMPageControl)
  public
    function FindCut(Filename: string): TMainTabSheet;
  end;

  TMainTabSheet = class(TMTabSheet)
  protected
    procedure SavePlaylist(Entries: TPlaylistEntryArray; Open: Boolean);
  end;

implementation

uses
  CutTab;

{ TMainPageControl }

function TMainPageControl.FindCut(Filename: string): TMainTabSheet;
var
  i: Integer;
begin
  Result := nil;
  Filename := LowerCase(Filename);
  for i := 0 to PageCount - 1 do
  begin
    if Pages[i] is TCutTab then
      if LowerCase(TCutTab(Pages[i]).Filename) = Filename then
      begin
        Result := TCutTab(Pages[i]);
        Break;
      end;
  end;
end;

{ TMainTabSheet }

procedure TMainTabSheet.SavePlaylist(Entries: TPlaylistEntryArray; Open: Boolean);
  function GetURLString(URL: string): string;
  var
    Res: TParseURLRes;
  begin
    URL := SecureSWURLToInsecure(URL);
    Res := ParseURL(URL);
    if Res.Success then
    begin
      Result := 'http://' + Res.Host + ':' + IntToStr(Res.Port) + Res.Data;
    end else
      Result := URL;
  end;
 procedure BuildPLS(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('[playlist]');
    List.Add('numberofentries=' + IntToStr(Length(Entries)));
    for i := 0 to Length(Entries) - 1 do
    begin
      List.Add('File' + IntToStr(i + 1) + '=' + GetURLString(Entries[i].URL));
      List.Add('Title' + IntToStr(i + 1) + '=' + Entries[i].Name);
      List.Add('Length' + IntToStr(i + 1) + '=-1');
    end;
  end;
  procedure BuildM3U(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('#EXTM3U');
    for i := 0 to Length(Entries) - 1 do
    begin
      List.Add('#EXTINF:-1,' + Entries[i].Name);
      List.Add(GetURLString(Entries[i].URL));
    end;
  end;
var
  Res: Integer;
  List: TStringList;
  Dlg: TSaveDialog;
begin
  if Length(Entries) = 0 then
    Exit;
  List := TStringList.Create;
  try
    if not Open then
    begin
      Dlg := TSaveDialog.Create(Self);
      try
        Dlg.FileName := '';
        Dlg.Filter := '.M3U Playlist|*.m3u|.PLS Playlist|*.pls';
        Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofPathMustExist];
        if Dlg.Execute(Handle) then
        begin
          try
            if (LowerCase(ExtractFileExt(Dlg.FileName)) <> '.m3u') and
               (LowerCase(ExtractFileExt(Dlg.FileName)) <> '.pls') then
              if Dlg.FilterIndex = 1 then
                Dlg.FileName := Dlg.FileName + '.m3u'
              else
                Dlg.FileName := Dlg.FileName + '.pls';

            if LowerCase(ExtractFileExt(Dlg.FileName)) = '.m3u' then
              BuildM3U(Entries, List)
            else
              BuildPLS(Entries, List);

            List.SaveToFile(Dlg.FileName);
          except
            MsgBox(Handle, Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [ExtractFilePath(Dlg.FileName)]), _('Error'), MB_ICONEXCLAMATION);
          end;
        end;
      finally
        Dlg.Free;
      end;
    end else
    begin
      try
        BuildM3U(Entries, List);
        List.SaveToFile(AppGlobals.TempDir + 'playlist.m3u');
        Res := ShellExecute(Handle, 'open', PChar(AppGlobals.TempDir + 'playlist.m3u'), nil, nil, 1);
        if Res <= 32 then
        begin
          BuildPLS(Entries, List);
          List.SaveToFile(AppGlobals.TempDir + 'playlist.pls');
          Res := ShellExecute(Handle, 'open', PChar(AppGlobals.TempDir + 'playlist.pls'), nil, nil, 1);
          if Res <= 32 then
            ShellExecute(Handle, nil, 'rundll32.exe', PChar('shell32.dll,OpenAs_RunDLL ' + AppGlobals.TempDir + 'playlist.pls'), nil, 1);
        end;
      except
        MsgBox(Handle, Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [AppGlobals.TempDir]), _('Error'), MB_ICONEXCLAMATION);
      end;
    end;
  finally
    List.Free;
  end;
end;

end.
