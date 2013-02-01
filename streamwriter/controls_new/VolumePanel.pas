unit VolumePanel;

interface

uses
  Windows, Classes, Forms, Controls, ExtCtrls, SeekBar, PngSpeedButton,
  PngImage;

type
  TOnGetVolumeBeforeMute = function(Sender: TObject): Integer of object;

  TMVolumePanel = class(TPanel)
  private
    FTrackBarPanel: TPanel;
    FTrackBar: TMSeekBar;
    FMute: TPngSpeedButton;
    FVolume: Integer;
    FVolumeBeforeDrag: Integer;
    FVolumeChange: TNotifyEvent;
    FVolumePng: TPngImage;
    FVolumeMutedPng: TPngImage;

    FOnGetVolumeBeforeMute: TOnGetVolumeBeforeMute;

    procedure MuteClick(Sender: TObject);
    procedure VolumeChange(Sender: TObject);
    procedure RefreshButtonState(DoIt: Boolean);
    procedure FSetVolume(Volume: Integer);
    procedure FSetNotifyOnMove(Value: Boolean);
    function FGetNotifyOnMove: Boolean;
    function FGetVolume: Integer;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Setup;

    property Volume: Integer read FGetVolume write FSetVolume;
    property VolumeBeforeDrag: Integer read FVolumeBeforeDrag;
  published
    //property NotifyOnMove: Boolean read FGetNotifyOnMove write FSetNotifyOnMove;
    property OnVolumeChange: TNotifyEvent read FVolumeChange write FVolumeChange;
    property OnGetVolumeBeforeMute: TOnGetVolumeBeforeMute read FOnGetVolumeBeforeMute write FOnGetVolumeBeforeMute;
  end;

implementation

{ TVolumePanel }

procedure TMVolumePanel.Setup;
var
  ResStream: TResourceStream;
begin
//  if csDesigning in ComponentState then
//    Exit;

  BevelOuter := bvNone;
  Caption := '';

  FMute := TPngSpeedButton.Create(Self);
  FMute.Hint := 'Mute';
  FMute.ShowHint := True;
  FMute.Flat := True;
  FMute.Align := alLeft;
  FMute.Width := 25;
  FMute.GroupIndex := 1;
  FMute.AllowAllUp := True;
  FMute.Down := True;
  FMute.OnClick := MuteClick;
  FMute.Parent := Self;

  ResStream := TResourceStream.Create(HInstance, 'VOLUME', RT_RCDATA);
  try
    FVolumePng := TPngImage.Create;
    FVolumePng.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;

  ResStream := TResourceStream.Create(HInstance, 'VOLUME_MUTED', RT_RCDATA);
  try
    FVolumeMutedPng := TPngImage.Create;
    FVolumeMutedPng.LoadFromStream(ResStream);
  finally
    ResStream.Free;
  end;

  FTrackBarPanel := TPanel.Create(Self);
  FTrackBarPanel.Align := alClient;
  FTrackBarPanel.BevelOuter := bvNone;
  FTrackBarPanel.Padding.Left := 4;
  FTrackBarPanel.Padding.Right := 2;
  FTrackBarPanel.Parent := Self;

  FTrackBar := TMSeekBar.Create(Self);
  FTrackBar.Max := 100;
  FTrackBar.Align := alClient;
  FTrackBar.OnPositionChanged := VolumeChange;
  FTrackBar.Parent := FTrackBarPanel;
  FTrackBar.GripperVisible := True;
  FTrackBar.NotifyOnMove := True;
  FTrackBar.NotifyOnDown := True;

  RefreshButtonState(True);
end;

procedure TMVolumePanel.MuteClick(Sender: TObject);
var
  P: Integer;
begin
  if FMute.Down then
  begin
    FTrackBar.PositionBeforeDrag := FTrackBar.Position;
    FTrackBar.Position := 0;

    FMute.PngImage := FVolumeMutedPng;
    if not FMute.Down then
      FMute.Down := True;
  end else
  begin
    if Assigned(FOnGetVolumeBeforeMute) then
    begin
      P := FOnGetVolumeBeforeMute(Self);
      FTrackBar.Position := P;
      FMute.PngImage := FVolumePng;
    end;
  end;
end;

procedure TMVolumePanel.VolumeChange(Sender: TObject);
begin
  RefreshButtonState(False);

  FVolume := FTrackBar.Position;
  FVolumeBeforeDrag := FTrackBar.PositionBeforeDrag;

  if Assigned(OnVolumeChange) then
    OnVolumeChange(Self);
end;

procedure TMVolumePanel.RefreshButtonState(DoIt: Boolean);
begin
  if Volume = 0 then
  begin
    if not FMute.Down or DoIt then
    begin
      FMute.Down := True;
      FMute.PngImage := FVolumeMutedPng;
    end;
  end else
  begin
    if FMute.Down or DoIt then
    begin
      FMute.Down := False;
      FMute.PngImage := FVolumePng;
    end;
  end;
end;

procedure TMVolumePanel.FSetVolume(Volume: Integer);
begin
  FTrackBar.Position := Volume;
  RefreshButtonState(False);
end;

procedure TMVolumePanel.Loaded;
begin
  inherited;

  Setup;
end;

function TMVolumePanel.FGetNotifyOnMove: Boolean;
begin
  // TODO: !!!
  {
  if FTrackBar <> nil then
    Result := FTrackBar.NotifyOnMove
  else
    Result := False;
  }
end;

function TMVolumePanel.FGetVolume: Integer;
begin
  Result := FTrackBar.Position;
end;

procedure TMVolumePanel.FSetNotifyOnMove(Value: Boolean);
begin
  //FTrackBar.NotifyOnMove := Value;
end;

constructor TMVolumePanel.Create(AOwner: TComponent);
begin
  inherited;
// TODO: Setup hier und in loaded? ist das in ordnung?
  Setup;
end;

destructor TMVolumePanel.Destroy;
begin
  inherited;

  if FVolumePng <> nil then
    FVolumePng.Destroy;
  if FVolumeMutedPng <> nil then
    FVolumeMutedPng.Destroy;
end;

end.
