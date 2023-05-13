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

unit Equalizer;

interface

uses
  AppData,
  Buttons,
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  LanguageObjects,
  MSeekBar,
  MSpeedButton,
  MStringFunctions,
  PlayerManager,
  SharedControls,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type
  TEqualizer = class(TWinControl)
  private
    FLabels: array[0..9] of TLabel;
    FEqualizers: array[0..9] of TMSeekBar;
  protected
    procedure EQPositionChanged(Sender: TObject);

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Reset;
  end;

  { TfrmEqualizer }

  TfrmEqualizer = class(TForm)
    btnReset: TMSpeedButton;
    chkEqualizer: TCheckBox;
    Panel1: TPanel;
    pnlEqualizer: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure chkEqualizerClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FEqualizer: TEqualizer;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TEqualizer }

constructor TEqualizer.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  AppGlobals.Lock;
  try
    for i := 0 to High(FEqualizers) do
    begin
      FEqualizers[i] := TMSeekBar.Create(Self);
      FEqualizers[i].Position := AppGlobals.EQGain[i];
      FEqualizers[i].Orientation := sbVertical;
      FEqualizers[i].GripperVisible := True;
      FEqualizers[i].Top := 0;
      FEqualizers[i].Max := 30;
      FEqualizers[i].BorderSpacing.Left := 1;
      FEqualizers[i].BorderSpacing.Right := 1;
      FEqualizers[i].OnPositionChanged := EQPositionChanged;
      FEqualizers[i].Parent := Self;

      FLabels[i] := TLabel.Create(Self);
      FLabels[i].Parent := Self;

      case i of
        0: FLabels[i].Caption := '60';
        1: FLabels[i].Caption := '170';
        2: FLabels[i].Caption := '310';
        3: FLabels[i].Caption := '600';
        4: FLabels[i].Caption := '1K';
        5: FLabels[i].Caption := '3K';
        6: FLabels[i].Caption := '6K';
        7: FLabels[i].Caption := '12K';
        8: FLabels[i].Caption := '14K';
        9: FLabels[i].Caption := '16K';
      end;
    end;
  finally
    AppGlobals.Unlock;
  end;
end;

destructor TEqualizer.Destroy;
begin

  inherited;
end;

procedure TEqualizer.EQPositionChanged(Sender: TObject);
var
  i: Integer;
begin
  AppGlobals.Lock;
  try
    for i := 0 to High(FEqualizers) do
      if Sender = FEqualizers[i] then
      begin
        Players.SetEQ((TMSeekBar(Sender).Position - 15) * -1, i);
        AppGlobals.EQGain[i] := (TMSeekBar(Sender).Position - 15) * -1;
        Break;
      end;
  finally
    AppGlobals.Unlock;
  end;
end;

procedure TEqualizer.Reset;
var
  i: Integer;
begin
  for i := 0 to High(FEqualizers) do
  begin
    FEqualizers[i].Position := 15;
    EQPositionChanged(FEqualizers[i]);
  end;
end;

procedure TEqualizer.Resize;
var
  i, WidthPerBar: Integer;
begin
  inherited;

  WidthPerBar := Trunc(ClientWidth / ((Length(FEqualizers))));

  for i := 0 to High(FEqualizers) do
  begin
    FEqualizers[i].Left := i * WidthPerBar;
    FEqualizers[i].Width := WidthPerBar;
    FEqualizers[i].Height := ClientHeight - TMStringFunctions.GetTextSize(FLabels[i].Caption, FLabels[i].Font).cy;

    AppGlobals.Lock;
    try
      FEqualizers[i].Position := (AppGlobals.EQGain[i] * -1 + 15);
    finally
      AppGlobals.Unlock;
    end;

    FLabels[i].Top := FEqualizers[i].Top + FEqualizers[i].Height;
    FLabels[i].Left := i * WidthPerBar + FEqualizers[i].ClientWidth div 2 - FLabels[i].ClientWidth div 2;
  end;
end;

{ TfrmEqualizer }

procedure TfrmEqualizer.btnResetClick(Sender: TObject);
begin
  FEqualizer.Reset;
end;

procedure TfrmEqualizer.chkEqualizerClick(Sender: TObject);
begin
  Players.EQEnabled := chkEqualizer.Checked;
  AppGlobals.Lock;
  try
    AppGlobals.EQEnabled := chkEqualizer.Checked;
  finally
    AppGlobals.Unlock;
  end;
end;

constructor TfrmEqualizer.Create(AOwner: TComponent);
begin
  inherited;

  FEqualizer := TEqualizer.Create(Self);
  FEqualizer.Parent := pnlEqualizer;
  FEqualizer.Align := alClient;
end;

procedure TfrmEqualizer.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  chkEqualizer.Checked := AppGlobals.EQEnabled;
end;

procedure TfrmEqualizer.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmEqualizer.FormShow(Sender: TObject);
begin
  chkEqualizer.SetFocus;
end;

end.
