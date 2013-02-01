unit MistakeDesign1;

interface

uses
  Windows, SysUtils, Classes, DesignEditors, Controls, Forms, DesignIntf, MistakeRun1, SeekBar,
  VolumePanel;

type
  TMPageControlEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TMTabSheetEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('mistake.ws', [TMPageControl, TMSeekBar, TMVolumePanel, TMMainMenu, TMPopupMenu]);
  RegisterComponentEditor(TMPageControl, TMPageControlEditor);
  RegisterComponentEditor(TMTabSheet, TMTabSheetEditor);
end;

{ TFarbAuswahlComponentEditor }

procedure TMPageControlEditor.ExecuteVerb(Index: Integer);
var
  N: string;
  Idx: Integer;
  T: TMTabSheet;
  Ctrl: TControl;
  RootWnd: TControl;
begin
  inherited;

  Idx := 1;
  while Component.Owner.FindComponent('Tab' + IntToStr(Idx)) <> nil do
    Inc(Idx);

  T := TMTabSheet.Create(Component.Owner);
  T.Parent := Component.Owner as TWinControl;
  T.PageControl := Component as TMPageControl;
  T.Name := 'Tab' + IntToStr(Idx);


  {
  RootWnd := Component as TControl;
  while RootWnd.Parent <> nil do
    RootWnd := RootWnd.Parent;

  Idx := 1;
  while True do
  begin
    Ctrl := RootWnd.FindComponent('Tab' + IntToStr(Idx)) as TControl;
    if Ctrl = nil then
      Break
    else
      Inc(Idx);
  end;

  T := TMTabSheet.Create(RootWnd);
  T.Parent := RootWnd as TWinControl;
  T.PageControl := Component as TMPageControl;
  T.Name := 'Tab' + IntToStr(Idx);
  }
end;

function TMPageControlEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Neue Seite';
end;

function TMPageControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TMTabSheetEditor }

function TMTabSheetEditor.GetVerbCount: Integer;
begin
  Result := 0;
end;

end.
