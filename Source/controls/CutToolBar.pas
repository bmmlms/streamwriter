unit CutToolBar;

interface

uses
  Classes,
  ComCtrls,
  Images,
  SharedControls,
  SysUtils;

type

  { TCutToolBar }

  TCutToolBar = class(TToolbarForcedHorizontal)
  private
    FSave: TToolButton;
    FZoomIn: TToolButton;
    FZoomOut: TToolButton;
    FPosEdit: TToolButton;
    FPosPlay: TToolButton;
    FAutoCut: TToolButton;
    FCut: TToolButton;
    FUndo: TToolButton;
    FPosEffectsMarker: TToolButton;
    FApplyFadeIn: TToolButton;
    FApplyFadeOut: TToolButton;
    FApplyEffects: TToolButton;
    FPlay: TToolButton;
    FStop: TToolButton;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Save: TToolButton read FSave;
    property ZoomIn: TToolButton read FZoomIn;
    property ZoomOut: TToolButton read FZoomOut;
    property PosEdit: TToolButton read FPosEdit;
    property PosPlay: TToolButton read FPosPlay;
    property AutoCut: TToolButton read FAutoCut;
    property Cut: TToolButton read FCut;
    property Undo: TToolButton read FUndo;
    property PosEffectsMarker: TToolButton read FPosEffectsMarker;
    property ApplyFadeIn: TToolButton read FApplyFadeIn;
    property ApplyFadeOut: TToolButton read FApplyFadeOut;
    property ApplyEffects: TToolButton read FApplyEffects;
    property Play: TToolButton read FPlay;
    property Stop: TToolButton read FStop;
  end;

implementation

{ TCutToolbar }

constructor TCutToolBar.Create(AOwner: TComponent);
var
  Sep: TToolButton;
begin
  inherited Create(AOwner);

  FStop := TToolButton.Create(Self);
  FStop.Parent := Self;
  FStop.Hint := 'Stop (Space bar)';
  FStop.ImageIndex := TImages.STOP_BLUE;

  FPlay := TToolButton.Create(Self);
  FPlay.Parent := Self;
  FPlay.Hint := 'Play (Space bar)';
  FPlay.ImageIndex := TImages.PLAY_BLUE;

  FPosPlay := TToolButton.Create(Self);
  FPosPlay.Parent := Self;
  FPosPlay.Hint := 'Set playposition (P)';
  FPosPlay.ImageIndex := TImages.LINE_PLAY;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FAutoCut := TToolButton.Create(Self);
  FAutoCut.Parent := Self;
  FAutoCut.Hint := 'Show silence...';
  FAutoCut.ImageIndex := TImages.WAND;

  {$IFDEF DEBUG}
  //FAutoCutAutoDetect := TToolButton.Create(Self);
  //FAutoCutAutoDetect.Parent := Self;
  //FAutoCutAutoDetect.Hint := 'Show silence...';
  //FAutoCutAutoDetect.ImageIndex := 19;
  {$ENDIF}

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FUndo := TToolButton.Create(Self);
  FUndo.Parent := Self;
  FUndo.Hint := 'Undo (Ctrl+Z)';
  FUndo.ImageIndex := TImages.ARROW_UNDO;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FApplyEffects := TToolButton.Create(Self);
  FApplyEffects.Parent := Self;
  FApplyEffects.Hint := 'Apply effects...';
  FApplyEffects.ImageIndex := TImages.LIGHTNING;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FCut := TToolButton.Create(Self);
  FCut.Parent := Self;
  FCut.Hint := 'Cut song';
  FCut.ImageIndex := TImages.CUT;

  FPosEdit := TToolButton.Create(Self);
  FPosEdit.Parent := Self;
  FPosEdit.Hint := 'Set cutpositions (left mousebutton sets start, right button sets end) (C)';
  FPosEdit.ImageIndex := TImages.LINES_COMBINED;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FApplyFadeout := TToolButton.Create(Self);
  FApplyFadeout.Parent := Self;
  FApplyFadeout.Hint := 'Apply fadeout (F)';
  FApplyFadeout.ImageIndex := TImages.FADE_OUT;

  FApplyFadein := TToolButton.Create(Self);
  FApplyFadein.Parent := Self;
  FApplyFadein.Hint := 'Apply fadein (F)';
  FApplyFadein.ImageIndex := TImages.FADE_IN;

  FZoomOut := TToolButton.Create(Self);
  FZoomOut.Parent := Self;
  FZoomOut.Hint := 'Zoom out (-)';
  FZoomOut.ImageIndex := TImages.ZOOM_OUT;

  FZoomIn := TToolButton.Create(Self);
  FZoomIn.Parent := Self;
  FZoomIn.Hint := 'Zoom in (+)';
  FZoomIn.ImageIndex := TImages.ZOOM_IN;

  FPosEffectsMarker := TToolButton.Create(Self);
  FPosEffectsMarker.Parent := Self;
  FPosEffectsMarker.Hint := 'Select area (S)';
  FPosEffectsMarker.ImageIndex := TImages.TIMELINE_MARKER;

  Sep := TToolButton.Create(Self);
  Sep.Parent := Self;
  Sep.Style := tbsSeparator;

  FSave := TToolButton.Create(Self);
  FSave.Parent := Self;
  FSave.Hint := 'Save (Ctrl+S)';
  FSave.ImageIndex := TImages.DISK;
end;

end.
