{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2011 Alexander Nottelmann

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
unit SharedControls;

interface

uses
  Windows, SysUtils, Classes, ComCtrls, ExtCtrls, Controls, Graphics,
  Functions, AppData, PngSpeedButton, PngImage, LanguageObjects;

type
  TVolumePanel = class(TPanel)
  private
    FTrackBar: TTrackBar;
    FMute: TPngSpeedButton;
    FVolume: Integer;
    FVolumeChange: TNotifyEvent;
    FVolumePng: TPngImage;
    FVolumeMutedPng: TPngImage;

    procedure MuteClick(Sender: TObject);
    procedure VolumeChange(Sender: TObject);
    procedure RefreshButtonState;
    procedure FSetVolume(Volume: Integer);
    function FGetVolume: Integer;
  public
    procedure Setup;

    property OnVolumeChange: TNotifyEvent read FVolumeChange write FVolumeChange;
    property Volume: Integer read FGetVolume write FSetVolume;

    destructor Destroy; override;
  end;

implementation

{ TVolumePanel }

procedure TVolumePanel.Setup;
var
  ResStream: TResourceStream;
begin
  BevelOuter := bvNone;

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

  // Damit der Knopf noch 'drückbar' ist (Unmute), wenn App
  // im Mute-Modus beendet wurde. Nicht wegmachen.
  FVolume := 50;

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

  FTrackBar := TTrackBar.Create(Self);
  FTrackBar.Max := 100;
  FTrackBar.Min := 0;
  FTrackBar.BorderWidth := 0;
  FTrackBar.ThumbLength := 20;
  FTrackBar.TickStyle := tsNone;
  FTrackBar.Align := alClient;
  FTrackBar.OnChange := VolumeChange;
  FTrackBar.Parent := Self;
end;

procedure TVolumePanel.MuteClick(Sender: TObject);
begin
  if FMute.Down or (FVolume = 0) then
  begin
    FVolume := FTrackBar.Position;
    FTrackBar.Position := 0;
    FMute.PngImage := FVolumeMutedPng;
    FMute.Down := True;
  end else
  begin
    FTrackBar.Position := FVolume;
    FMute.PngImage := FVolumePng;
  end;
end;

procedure TVolumePanel.VolumeChange(Sender: TObject);
begin
  RefreshButtonState;
  if Assigned(OnVolumeChange) then
    OnVolumeChange(Self);
end;

procedure TVolumePanel.RefreshButtonState;
begin
  if Volume = 0 then
  begin
    FMute.Down := True;
    FMute.PngImage := FVolumeMutedPng;
  end
  else
  begin
    if FMute.Down then
    begin
      FMute.Down := False;
      FMute.PngImage := FVolumePng;
    end;
  end;
end;

procedure TVolumePanel.FSetVolume(Volume: Integer);
begin
  FTrackBar.Position := Volume;
  RefreshButtonState;

  if Assigned(OnVolumeChange) then
    OnVolumeChange(Self);
end;

function TVolumePanel.FGetVolume: integer;
begin
  Result := FTrackBar.Position;
end;

destructor TVolumePanel.Destroy;
begin
  inherited;

  FVolumePng.Destroy;
  FVolumeMutedPng.Destroy;
end;

end.

