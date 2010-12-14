unit SharedControls;

interface

uses
  Windows, SysUtils, Classes, ComCtrls, ExtCtrls, Controls, Graphics,
  Functions, AppData;

type
  TVolumePanel = class(TPanel)
  private
    FImage: TImage;
    FTrackBar: TTrackBar;
  public
    procedure Setup;
    property TrackBar: TTrackBar read FTrackBar;
  end;

implementation

{ TVolumePanel }

procedure TVolumePanel.Setup;
var
  B: TBitmap;
  ImgPnl: TPanel;
begin
  BevelOuter := bvNone;

  ImgPnl := TPanel.Create(Self);
  ImgPnl.BevelOuter := bvNone;
  ImgPnl.Align := alLeft;
  ImgPnl.Width := 25;
  ImgPnl.Parent := Self;

  FImage := TImage.Create(Self);
  FImage.Top := ImgPnl.ClientHeight div 2 - 8;
  FImage.Left := ImgPnl.ClientWidth div 2 - 8;
  FImage.Height := 16;
  FImage.Width := 16;
  FImage.Parent := ImgPnl;

  B := TBitmap.Create;
  try
    GetBitmap('VOLUME', 1, B);
    FImage.Canvas.Draw(0, 0, B);
    FImage.Transparent := True;
  finally
    B.Free;
  end;

  FTrackBar := TTrackBar.Create(Self);
  FTrackBar.Max := 100;
  FTrackBar.Min := 0;
  FTrackBar.BorderWidth := 0;
  FTrackBar.ThumbLength := 20;
  FTrackBar.TickStyle := tsNone;
  FTrackBar.Align := alClient;
  FTrackBar.Parent := Self;
end;

end.
