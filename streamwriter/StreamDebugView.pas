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

    procedure ShowDebug(Client: TICEClient);
  protected
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;

    property Client: TICEClient read FClient;
    property OnClear: TNotifyEvent read FOnClear write FOnClear;
  end;

  TMStreamDebugContainer = class(TPanel)
  private
    FDebugView: TMStreamDebugView;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure ShowDebug(Client: TICEClient);

    property DebugView: TMStreamDebugView read FDebugView;
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

  FDebug := TDebugView.Create(Self);
  FDebug.Parent := Self;
  FDebug.Align := alClient;
  FDebug.Visible := True;

  FPanelBottom := TPanel.Create(Self);
  FPanelBottom.Parent := Self;
  FPanelBottom.Align := alBottom;
  FPanelBottom.BevelOuter := bvNone;
  FPanelBottom.Visible := True;
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
  i, C: Integer;
begin
  FDebug.BeginUpdate;
  FClient := Client;
  if Client <> nil then
  begin
    C := FDebug.RootNodeCount;
    for i := C to Client.DebugLog.Count - 1 do
      FDebug.AddData(Client.DebugLog[i].Time, Client.DebugLog[i].Text, Client.DebugLog[i].Data);
  end else
    FDebug.RootNodeCount := 0;
  FDebug.EndUpdate;
end;

{ TMStreamDebugContainer }

constructor TMStreamDebugContainer.Create(AOwner: TComponent);
begin
  inherited;

  Caption := _('Please select a stream.');
  BevelOuter := bvNone;
  Align := alClient;

  FDebugView := TMStreamDebugView.Create(Self);
  FDebugView.Parent := Self;
  FDebugView.Visible := False;
end;

procedure TMStreamDebugContainer.ShowDebug(Client: TICEClient);
begin
  FDebugView.ShowDebug(Client);
  FDebugView.Visible := (Client <> nil) and (not Client.Killed);
end;

end.
