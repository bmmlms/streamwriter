unit Equalizer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SharedControls, StdCtrls, PlayerManager, LanguageObjects,
  ExtCtrls, AppData, GUIFunctions;

type
  TEqualizer = class(TWinControl)
  private
    FLabels: array[0..9] of TLabel;
    FEqualizers: array[0..9] of TSeekBar;
  protected
    procedure EQPositionChanged(Sender: TObject);

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TfrmEqualizer = class(TForm)
    pnlEqualizer: TPanel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FEqualizer: TEqualizer;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TEqualizer }

constructor TEqualizer.Create(AOwner: TComponent);
var
  i: Integer;
  EQ: TSeekBar;
  L: TLabel;
begin
  inherited;

  for i := 0 to High(FEqualizers) do
  begin
    EQ := TSeekBar.Create(Self);
    FEqualizers[i] := EQ;
    EQ.Parent := Self;
    EQ.Position := AppGlobals.EQGain[i];
    EQ.OnPositionChanged := EQPositionChanged;

    L := TLabel.Create(Self);
    FLabels[i] := L;
    L.Parent := Self;

    case i of
      0: L.Caption := '60';
      1: L.Caption := '170';
      2: L.Caption := '310';
      3: L.Caption := '600';
      4: L.Caption := '1K';
      5: L.Caption := '3K';
      6: L.Caption := '6K';
      7: L.Caption := '12K';
      8: L.Caption := '14K';
      9: L.Caption := '16K';
    end;
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
  for i := 0 to High(FEqualizers) do
    if Sender = FEqualizers[i] then
    begin
      Players.SetEQ((TSeekBar(Sender).Position - 15) * -1, i);
      AppGlobals.EQGain[i] := (TSeekBar(Sender).Position - 15) * -1;
      Break;
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
    FEqualizers[i].Top := 0;
    FEqualizers[i].Left := i * WidthPerBar;
    FEqualizers[i].Width := WidthPerBar;
    FEqualizers[i].Height := ClientHeight - GetTextSize(FLabels[i].Caption, FLabels[i].Font).cy;
    FEqualizers[i].Max := 30;
    FEqualizers[i].Position := (AppGlobals.EQGain[i] * -1 + 15);
    FEqualizers[i].Orientation := sbVertical;
    FEqualizers[i].GripperVisible := True;

    FLabels[i].Top := FEqualizers[i].Top + FEqualizers[i].Height;
    FLabels[i].Left := i * WidthPerBar + FEqualizers[i].ClientWidth div 2 - FLabels[i].ClientWidth div 2;
  end;
end;

{ TfrmEqualizer }

procedure TfrmEqualizer.CheckBox1Click(Sender: TObject);
begin
  Players.EQEnabled := CheckBox1.Checked;
  AppGlobals.EQEnabled := CheckBox1.Checked;
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

  CheckBox1.Checked := AppGlobals.EQEnabled;
end;

procedure TfrmEqualizer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

end.
