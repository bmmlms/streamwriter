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
  MSpeedButton,
  SharedData,
  StationCombo,
  StdCtrls,
  SysUtils,
  TypeDefs,
  Windows;

type

  { TClientAddressBar }

  TClientAddressBar = class(TPanel, IPostTranslatable)
  private
    FLabel: TLabel;
    FStations: TMStationCombo;
    FStart: TMSpeedButton;
    FDropTarget: TDropComboTarget;

    FOnStart: TNotifyEvent;

    procedure FStationsChange(Sender: TObject);
    procedure FStationsKeyPress(Sender: TObject; var Key: Char);
    procedure FStartClick(Sender: TObject);

    procedure DropTargetDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
  protected
    procedure CreateHandle; override;
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
  ChildSizing.HorizontalSpacing := 4;

  FLabel := TLabel.Create(Self);
  FLabel.Align := alLeft;
  FLabel.Layout := tlCenter;
  FLabel.Caption := 'Playlist/Stream-URL:';
  FLabel.Parent := Self;

  FStart := TMSpeedButton.Create(Self);
  FStart.Align := alRight;
  FStart.Flat := True;
  FStart.Enabled := False;
  FStart.Hint := 'Add';
  FStart.OnClick := FStartClick;
  FStart.Images := modSharedData.imgImages;
  FStart.Parent := Self;

  FStations := TMStationCombo.Create(Self);
  FStations.Align := alClient;
  FStations.DropDownCount := 16;
  FStations.OnKeyPress := FStationsKeyPress;
  FStations.OnChange := FStationsChange;
  FStations.Images := modSharedData.imgImages;
  FStations.Parent := Self;

  FDropTarget := TDropComboTarget.Create(Self);
  FDropTarget.Formats := [mfText, mfURL, mfFile];
  FDropTarget.Register(Self); // Register(Self) since Register(FStations) causes drawing issues/flickering at the combobox
  FDropTarget.OnDrop := DropTargetDrop;
end;

procedure TClientAddressBar.PostTranslate;
begin
  case AppGlobals.DefaultActionNewStream of
    oaPlay:
    begin
      FStart.Hint := _('Listen to stream');
      FStart.ImageIndex := TImages.PLAY_BLUE;
    end;
    oaStart:
    begin
      FStart.Hint := _('Start recording');
      FStart.ImageIndex := TImages.RECORD_RED;
    end;
    oaAdd:
    begin
      FStart.Hint := _('Add');
      FStart.ImageIndex := TImages.ADD;
    end else
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

procedure TClientAddressBar.CreateHandle;
begin
  inherited CreateHandle;

  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_COMPOSITED);
end;

procedure TClientAddressBar.FStationsChange(Sender: TObject);
begin
  FStart.Enabled := (Length(Trim(FStations.Text)) > 0) or (FStations.ItemIndex > -1);
end;

procedure TClientAddressBar.FStationsKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Char(VK_RETURN) then
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
