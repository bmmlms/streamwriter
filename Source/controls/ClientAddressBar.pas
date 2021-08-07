unit ClientAddressBar;

interface

uses
  Buttons,
  Classes,
  Controls,
  DropComboTarget,
  ExtCtrls,
  Graphics,
  Images,
  SharedData,
  StationCombo,
  StdCtrls,
  SysUtils;

type
  TClientAddressBar = class(TPanel)
  private
    FLabel: TLabel;
    FStations: TMStationCombo;
    FStart: TSpeedButton;
    FDropTarget: TDropComboTarget;

    FOnStart: TNotifyEvent;

    procedure FStationsChange(Sender: TObject);
    procedure FStationsKeyPress(Sender: TObject; var Key: Char);
    procedure FStartClick(Sender: TObject);

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    property Stations: TMStationCombo read FStations;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
  end;

implementation

{ TClientAddressBar }

constructor TClientAddressBar.Create(AOwner: TComponent);
begin
  inherited;

  BevelOuter := bvNone;

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.Align := alLeft;
  FLabel.Layout := tlCenter;
  FLabel.Caption := 'Playlist/Stream-URL:';

  FStart := TSpeedButton.Create(Self);
  FStart.Parent := Self;
  FStart.Align := alRight;
  FStart.Width := 24;
  FStart.Height := 22;
  FStart.Flat := True;
  FStart.Hint := 'Add and start recording';
  FStart.ShowHint := True;
  FStart.NumGlyphs := 1;
  FStart.OnClick := FStartClick;
  FStart.Images := modSharedData.imgImages;
  FStart.ImageIndex := TImages.ADD;

  FStations := TMStationCombo.Create(Self);
  FStations.Parent := Self;
  FStations.Align := alClient;
  FStations.DropDownCount := 15;
  FStations.OnKeyPress := FStationsKeyPress;
  FStations.OnChange := FStationsChange;
  FStations.Images := modSharedData.imgImages;

  FDropTarget := TDropComboTarget.Create(Self); // TODO: wirklich self? eigentlich doch die combobox oder?
  FDropTarget.Formats := [mfText, mfURL, mfFile];
  FDropTarget.Register(FStations);
  FDropTarget.OnDrop := DropTargetDrop;

  FStart.Enabled := False;
end;

procedure TClientAddressBar.DropTargetDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin
  FStations.ItemIndex := -1;
  if FDropTarget.URL <> '' then
    FStations.Text := string(FDropTarget.URL)
  else if FDropTarget.Text <> '' then
    FStations.Text := string(FDropTarget.Text)
  else if FDropTarget.Files.Count > 0 then
    FStations.Text := string(FDropTarget.Files[0]);
end;

procedure TClientAddressBar.FStationsChange(Sender: TObject);
begin
  FStart.Enabled := (Length(Trim(FStations.Text)) > 0) or (FStations.ItemIndex > -1);
end;

procedure TClientAddressBar.FStationsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    FStart.Click
  else
    FStations.ItemIndex := -1;
end;

procedure TClientAddressBar.FStartClick(Sender: TObject);
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;

end.
