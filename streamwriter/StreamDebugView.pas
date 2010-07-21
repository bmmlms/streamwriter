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
unit StreamDebugView;

interface

uses
  Windows, SysUtils, Classes, Controls, StdCtrls, ExtCtrls, ImgList,
  RecentManager, VirtualTrees, LanguageObjects, GUIFunctions,
  Generics.Collections, Graphics, Forms, DebugView, ICEClient;

type
  TMStreamDebugView = class(TPanel)
  private
    FClient: TICEClient;
    FDebug: TDebugView;
    FPanelBottom: TPanel;
    FBtnCopy: TButton;
    FBtnClear: TButton;

    FOnClear: TNotifyEvent;

    procedure BtnCopyClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    procedure ShowDebug(Client: TICEClient);

    property Client: TICEClient read FClient;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

implementation

{ TStreamDebugView }

procedure TMStreamDebugView.BtnClearClick(Sender: TObject);
begin
  FDebug.Clear;
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

procedure TMStreamDebugView.BtnCopyClick(Sender: TObject);
begin
  FDebug.Copy;
end;

constructor TMStreamDebugView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Align := alClient;
  BevelOuter := bvNone;

  Caption := _('Please select a stream.');

  FDebug := TDebugView.Create(Self);
  FDebug.Parent := Self;
  FDebug.Align := alClient;
  FDebug.Visible := False;

  FPanelBottom := TPanel.Create(Self);
  FPanelBottom.Parent := Self;
  FPanelBottom.Align := alBottom;
  FPanelBottom.BevelOuter := bvNone;
  FPanelBottom.Visible := False;
  FPanelBottom.Height := 40;
  FPanelBottom.Padding.Top := 4;

  FBtnCopy := TButton.Create(Self);
  FBtnCopy.Caption := _('Copy');
  FBtnCopy.Align := alRight;
  FBtnCopy.Parent := FPanelBottom;
  FBtnCopy.Visible := True;
  FBtnCopy.OnClick := BtnCopyClick;

  FBtnClear := TButton.Create(Self);
  FBtnClear.Caption := _('Clear');
  FBtnClear.Align := alRight;
  FBtnClear.Parent := FPanelBottom;
  FBtnClear.Visible := True;
  FBtnClear.OnClick := BtnClearClick;
end;

destructor TMStreamDebugView.Destroy;
begin

  inherited;
end;

procedure TMStreamDebugView.ShowDebug(Client: TICEClient);
var
  i: Integer;
  Entry: TDebugEntry;
begin
  if Client <> FClient then
  begin
    FDebug.Clear;
  end;
  FClient := Client;
  if Client <> nil then
  begin
    if Client.Killed then
      Exit;
    FDebug.Visible := True;
    FPanelBottom.Visible := True;
    for i := FDebug.RootNodeCount to Client.DebugLog.Count - 1 do
    begin
      FDebug.AddData(Client.DebugLog[i].Time, Client.DebugLog[i].Text, Client.DebugLog[i].Data);
    end;
  end else
  begin
    FDebug.Visible := False;
    FPanelBottom.Visible := False;
  end;
end;

end.
