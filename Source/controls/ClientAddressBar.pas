unit ClientAddressBar;

interface

uses
  AppData,
  Buttons,
  Classes,
  Controls,
  DropComboTarget,
  ExtCtrls,
  Graphics,
  Images,
  LanguageObjects,
  SharedData,
  StationCombo,
  StdCtrls,
  SysUtils,
  TypeDefs;

type

  { TClientAddressBar }

  TClientAddressBar = class(TPanel, IPostTranslatable)
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

    procedure PostTranslate;

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
  FStart.Flat := True;
  FStart.Hint := 'Add';
  FStart.ShowHint := True;
  FStart.OnClick := FStartClick;
  FStart.Images := modSharedData.imgImages;
  FStart.ImageIndex := TImages.ADD;

  FStations := TMStationCombo.Create(Self);
  FStations.Parent := Self;
  FStations.Align := alClient;
  FStations.DropDownCount := 16;
  FStations.OnKeyPress := FStationsKeyPress;
  FStations.OnChange := FStationsChange;
  FStations.Images := modSharedData.imgImages;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfText, mfURL, mfFile];
  FDropTarget.Register(FStations);
  FDropTarget.OnDrop := DropTargetDrop;

  FStart.Enabled := False;
end;

procedure TClientAddressBar.PostTranslate;
begin
  case AppGlobals.DefaultActionNewStream of
    oaPlay: FStart.ImageIndex := TImages.PLAY_BLUE;
    oaStart: FStart.ImageIndex := TImages.RECORD_RED;
    oaAdd: FStart.ImageIndex := TImages.ADD;
    else
      raise Exception.Create('Invalid DefaultActionNewStream');
  end;
end;

procedure TClientAddressBar.DropTargetDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin
  FStations.ItemIndex := -1;
  if FDropTarget.URL <> '' then
    FStations.Text := FDropTarget.URL
  else if FDropTarget.Text <> '' then
    FStations.Text := FDropTarget.Text
  else if FDropTarget.Files.Count > 0 then
    FStations.Text := FDropTarget.Files[0];
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
