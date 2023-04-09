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

{ This unit encapsulates basic functionality for tabe used in the main-window }
unit Tabs;

interface

uses
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  Controls,
  DataManager,
  Dialogs,
  DragDrop,
  DragDropFile,
  Forms,
  Functions,
  ImgList,
  LanguageObjects,
  Logging,
  MControls,
  Menus,
  MPageControl,
  SWFunctions,
  SysUtils,
  Themes,
  VirtualTrees,
  Windows;

type
  TMainTabSheet = class;

  TTrackEvent = procedure(Station: TStreamEntry; Track: TTrackInfo) of object;

  TPlaylistEntry = record
    Name: string;
    URL: string;
  end;
  TPlaylistEntryArray = array of TPlaylistEntry;

  { TMainPageControl }

  TMainPageControl = class(TMPageControl)
  public
    constructor Create(AOwner: TComponent); override;

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

constructor TMainPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TabHeight := MulDiv(22, Screen.PixelsPerInch, 96);
end;

function TMainPageControl.FindCut(Filename: string): TMainTabSheet;
var
  i: Integer;
begin
  Result := nil;
  Filename := LowerCase(Filename);
  for i := 0 to PageCount - 1 do
    if Pages[i] is TCutTab then
      if LowerCase(TCutTab(Pages[i]).Filename) = Filename then
      begin
        Result := TCutTab(Pages[i]);
        Break;
      end;
end;

{ TMainTabSheet }

procedure TMainTabSheet.SavePlaylist(Entries: TPlaylistEntryArray; Open: Boolean);

  function GetURLString(URL: string): string;
  var
    Res: TParseURLRes;
  begin
    URL := SecureSWURLToInsecure(URL);
    Res := TFunctions.ParseURL(URL);
    if Res.Success then
      Result := 'http://' + Res.Host + ':' + IntToStr(Res.Port) + Res.Data
    else
      Result := URL;
  end;

  procedure BuildPLS(Entries: TPlaylistEntryArray; List: TStringList);
  var
    i: Integer;
  begin
    List.Clear;
    List.Add('[playlist]');
    List.Add('numberofentries=' + IntToStr(Length(Entries)));
    for i := 0 to High(Entries) do
    begin
      List.Add('File' + IntToStr(i + 1) + '=' + GetURLString(Entries[i].URL));
      List.Add('Title' + IntToStr(i + 1) + '=' + Entries[i].Name);
      List.Add('Length' + IntToStr(i + 1) + '=-1');
    end;
  end;

  procedure BuildM3U(Entries: TPlaylistEntryArray; List: TStringList);
  var
    Entry: TPlaylistEntry;
  begin
    List.Clear;
    List.Add('#EXTM3U');
    for Entry in Entries do
    begin
      List.Add('#EXTINF:-1,' + Entry.Name);
      List.Add(GetURLString(Entry.URL));
    end;
  end;

var
  Res: Boolean;
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
        Dlg.Title := _('Save file');
        Dlg.FileName := '';
        Dlg.Filter := '.M3U Playlist|*.m3u|.PLS Playlist|*.pls';
        Dlg.Options := Dlg.Options + [ofOverwritePrompt, ofPathMustExist];

        if Dlg.Execute then
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
            TFunctions.MsgBox(Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [ExtractFilePath(Dlg.FileName)]), _('Error'), MB_ICONEXCLAMATION);
          end;
      finally
        Dlg.Free;
      end;
    end else
      try
        BuildM3U(Entries, List);
        List.SaveToFile(ConcatPaths([AppGlobals.TempDir, 'playlist.m3u']));
        Res := TFunctions.ShellExecute(Handle, 'open', ConcatPaths([AppGlobals.TempDir, 'playlist.m3u']));
        if not Res then
        begin
          BuildPLS(Entries, List);
          List.SaveToFile(ConcatPaths([AppGlobals.TempDir, 'playlist.pls']));
          Res := TFunctions.ShellExecute(Handle, 'open', ConcatPaths([AppGlobals.TempDir, 'playlist.pls']));
          if not Res then
            TFunctions.ShellExecute(Handle, '', 'rundll32.exe', 'shell32.dll,OpenAs_RunDLL ' + ConcatPaths([AppGlobals.TempDir, 'playlist.pls']));
        end;
      except
        TFunctions.MsgBox(Format(_('The playlist could not be saved.'#13#10'Verify that you have write permissions to "%s".'), [AppGlobals.TempDir]), _('Error'), MB_ICONEXCLAMATION);
      end;
  finally
    List.Free;
  end;
end;

end.
