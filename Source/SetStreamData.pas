unit SetStreamData;

interface

uses
  AppData,
  Buttons,
  Classes,
  ComCtrls,
  Constants,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  GraphType,
  HomeCommunication,
  Images,
  ImgList,
  LanguageObjects,
  MControls,
  regexpr,
  SharedData,
  StdCtrls,
  SWFunctions,
  SysUtils,
  TypeDefs,
  Variants,
  VirtualTrees,
  Windows;

type
  TTitleNodeData = record
    Title: string;
    ParsedArtist: string;
    ParsedTitle: string;
    MatchedRegExp: Boolean;
    MatchedOtherRegExp: Boolean;
    DataSet: Boolean;
  end;
  PTitleNodeData = ^TTitleNodeData;

  { TTitleTree }

  TTitleTree = class(TVirtualStringTree)
  private
  protected
    function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList; override;
    procedure PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
  end;

  { TfrmSetStreamData }

  TfrmSetStreamData = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    btnResetTitlePattern: TSpeedButton;
    txtRegEx: TLabeledEdit;
    btnAddRegEx: TButton;
    btnRemoveRegEx: TButton;
    lstRegExps: TListView;
    Label21: TLabel;
    lstOtherRegExps: TListView;
    Label1: TLabel;
    btnCancel: TBitBtn;
    procedure btnAddRegExClick(Sender: TObject);
    procedure btnRemoveRegExClick(Sender: TObject);
    procedure btnResetTitlePatternClick(Sender: TObject);
    procedure lstRegExpsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure txtRegExChange(Sender: TObject);
    procedure lstTitlesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FormResize(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lstTitlesMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lstTitlesMeasureTextWidth(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; var Extent: Integer);
    procedure lstTitlesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure lstRegExpsEdited(Sender: TObject; Item: TListItem; var S: string);
  private
    FTitleTree: TTitleTree;
    FStreamID: Integer;
    FMaxTextWidth: Integer;

    procedure InvalidateTree;
    procedure SetState(Enable: Boolean);
    procedure SetNodeData(NodeData: PTitleNodeData);

    procedure HomeCommGetStreamData(Sender: TObject; LastTitles: TStringArray; OtherUserRegExps: TStringArray; UserRegExps: TStringArray);
  public
    constructor Create(AOwner: TComponent; StreamID: Integer); reintroduce;
  end;

implementation

{$R *.lfm}

{ TTitleTree }

function TTitleTree.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: Integer): TCustomImageList;
begin
  Result := inherited;
  Index := 19;
end;

procedure TTitleTree.PaintImage(var PaintInfo: TVTPaintInfo; ImageInfoIndex: TVTImageInfoIndex; DoOverlay: Boolean);
var
  ImageIndex: Integer;
  NodeData: PTitleNodeData;
begin
  NodeData := GetNodeData(PaintInfo.Node);

  if NodeData.MatchedRegExp then
    ImageIndex := TImages.FONT_USER
  else if NodeData.MatchedOtherRegExp then
    ImageIndex := TImages.FONT_GROUP
  else
    Exit;

  modSharedData.imgImages.Resolution[16].Draw(PaintInfo.Canvas, PaintInfo.ImageInfo[ImageInfoIndex].XPos, PaintInfo.ImageInfo[ImageInfoIndex].YPos, ImageIndex, gdeNormal);
end;

constructor TTitleTree.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TTitleTree.Destroy;
begin
  inherited Destroy;
end;

{ TfrmSetStreamData }

procedure TfrmSetStreamData.btnAddRegExClick(Sender: TObject);
var
  Item: TListItem;
  RegExp: string;
begin
  RegExp := txtRegEx.Text;
  if not CheckRegExp(Handle, RegExp, lstRegExps, nil) then
    Exit;

  Item := lstRegExps.Items.Add;
  Item.Caption := RegExp;
  Item.ImageIndex := TImages.FONT_USER;
  txtRegEx.Text := '';
  txtRegEx.ApplyFocus;
  InvalidateTree;
end;

procedure TfrmSetStreamData.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmSetStreamData.btnOKClick(Sender: TObject);
var
  i: Integer;
  RegExps: TStringArray;
begin
  if (Length(Trim(txtRegEx.Text)) > 0) and (LowerCase(Trim(txtRegEx.Text)) <> LowerCase(DEFAULT_TITLE_REGEXP)) then
    if TFunctions.MsgBox(_('A regular expression was entered into the text field but not added to the list.'#13#10'Do you want to continue without saving that regular expression?'), _('Question'), MB_YESNO or MB_ICONQUESTION) = IDNO then
      Exit;

  SetLength(RegExps, 0);
  for i := 0 to lstRegExps.Items.Count - 1 do
  begin
    SetLength(RegExps, Length(RegExps) + 1);
    RegExps[High(RegExps)] := lstRegExps.Items[i].Caption;
  end;

  if not HomeComm.SendSetStreamData(FStreamID, RegExps) then
    TFunctions.MsgBox(_('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION)
  else
    Close;
end;

procedure TfrmSetStreamData.btnRemoveRegExClick(Sender: TObject);
begin
  txtRegEx.Text := lstRegExps.Selected.Caption;
  lstRegExps.Items.Delete(lstRegExps.Selected.Index);
  InvalidateTree;
end;

procedure TfrmSetStreamData.btnResetTitlePatternClick(Sender: TObject);
begin
  txtRegEx.Text := DEFAULT_TITLE_REGEXP;
  txtRegEx.ApplyFocus;
end;

constructor TfrmSetStreamData.Create(AOwner: TComponent; StreamID: Integer);
begin
  inherited Create(AOwner);

  modSharedData.imgImages.GetIcon(TImages.TRANSMIT_EDIT, Icon);

  SetState(False);

  FTitleTree := TTitleTree.Create(Self);
  FTitleTree.Parent := Self;
  FTitleTree.Align := alLeft;
  FTitleTree.Images := modSharedData.imgImages;

  FTitleTree.NodeDataSize := SizeOf(TTitleNodeData);
  FTitleTree.IncrementalSearch := isVisibleOnly;

  FTitleTree.TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect];
  FTitleTree.TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  FTitleTree.TreeOptions.MiscOptions := FTitleTree.TreeOptions.MiscOptions + [toVariableNodeHeight] - [toAcceptOLEDrop];
  FTitleTree.Header.Options := FTitleTree.Header.Options - [hoVisible];
  FTitleTree.ShowHint := False;

  FTitleTree.OnGetText := lstTitlesGetText;
  FTitleTree.OnMeasureItem := lstTitlesMeasureItem;
  FTitleTree.OnMeasureTextWidth := lstTitlesMeasureTextWidth;
  FTitleTree.OnBeforeCellPaint := lstTitlesBeforeCellPaint;

  FStreamID := StreamID;

  HomeComm.OnGetStreamDataReceived := HomeCommGetStreamData;

  HomeComm.SendGetStreamData(StreamID);
end;

procedure TfrmSetStreamData.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  AppGlobals.SetDataWidth := Width;
  AppGlobals.SetDataHeight := Height;

  HomeComm.OnGetStreamDataReceived := nil;
end;

procedure TfrmSetStreamData.FormCreate(Sender: TObject);
begin
  Language.Translate(Self);

  Width := AppGlobals.SetDataWidth;
  Height := AppGlobals.SetDataHeight;
end;

procedure TfrmSetStreamData.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmSetStreamData.FormResize(Sender: TObject);
begin
  lstRegExps.Columns[0].Width := lstRegExps.ClientWidth - 25;
  lstOtherRegExps.Columns[0].Width := lstOtherRegExps.ClientWidth - 25;
end;

procedure TfrmSetStreamData.HomeCommGetStreamData(Sender: TObject; LastTitles: TStringArray; OtherUserRegExps: TStringArray; UserRegExps: TStringArray);
var
  i, W: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Item: TListItem;
begin
  FTitleTree.Clear;
  for i := 0 to High(LastTitles) do
  begin
    Node := FTitleTree.AddChild(nil);
    NodeData := FTitleTree.GetNodeData(Node);
    NodeData.DataSet := False;

    NodeData.Title := LastTitles[i];
    FTitleTree.MultiLine[Node] := True;

    W := TFunctions.GetTextSize(NodeData.Title, FTitleTree.Font).cx;
    if W > FMaxTextWidth then
      FMaxTextwidth := W;
  end;

  lstOtherRegExps.Items.Clear;
  for i := 0 to High(OtherUserRegExps) do
  begin
    Item := lstOtherRegExps.Items.Add;
    Item.Caption := OtherUserRegExps[i];
    Item.ImageIndex := TImages.FONT_GROUP;
  end;

  lstRegExps.Items.Clear;
  for i := 0 to High(UserRegExps) do
  begin
    Item := lstRegExps.Items.Add;
    Item.Caption := UserRegExps[i];
    Item.ImageIndex := TImages.FONT_USER;
  end;

  InvalidateTree;

  SetState(True);

  FTitleTree.ApplyFocus;
end;

procedure TfrmSetStreamData.InvalidateTree;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := FTitleTree.GetFirst;
  while Node <> nil do
  begin
    NodeData := FTitleTree.GetNodeData(Node);
    NodeData.DataSet := False;
    Node := FTitleTree.GetNext(Node);
  end;

  FTitleTree.Invalidate;
end;

procedure TfrmSetStreamData.lstRegExpsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  btnRemoveRegEx.Enabled := lstRegExps.Selected <> nil;
end;

procedure TfrmSetStreamData.lstRegExpsEdited(Sender: TObject; Item: TListItem; var S: string);
begin
  if not CheckRegExp(Handle, S, lstRegExps, Item) then
  begin
    S := Item.Caption;
    Exit;
  end;

  Item.Caption := S;
  InvalidateTree;
end;

procedure TfrmSetStreamData.lstTitlesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  NodeData: PTitleNodeData;
begin
  NodeData := FTitleTree.GetNodeData(Node);

  CellText := Format('%s'#13#10'%s %s'#13#10'%s %s', [NodeData.Title, _('Artist:'), NodeData.ParsedArtist, _('Title:'), NodeData.ParsedTitle]);
end;

procedure TfrmSetStreamData.lstTitlesMeasureItem(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  NodeHeight := Trunc(TFunctions.GetTextSize('Wyg', TargetCanvas.Font).cy * 3) + 4;
end;

procedure TfrmSetStreamData.lstTitlesMeasureTextWidth(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; var Extent: Integer);
begin
  Extent := FMaxTextWidth;
end;

procedure TfrmSetStreamData.lstTitlesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  NodeData: PTitleNodeData;
begin
  NodeData := FTitleTree.GetNodeData(Node);

  SetNodeData(NodeData);
end;

procedure TfrmSetStreamData.SetNodeData(NodeData: PTitleNodeData);
var
  n: Integer;
  RegExps: TStringList;
  OtherRegExps: TStringList;
  AllRegExps: TStringList;
  R: TRegExpr;
  RegExp: string;
begin
  if NodeData.DataSet then
    Exit;

  RegExps := TStringList.Create;
  OtherRegExps := TStringList.Create;
  AllRegExps := TStringList.Create;
  try
    NodeData.ParsedArtist := '';
    NodeData.ParsedTitle := '';
    NodeData.MatchedRegExp := False;
    NodeData.MatchedOtherRegExp := False;
    NodeData.DataSet := True;

    RegExps.Clear;
    OtherRegExps.Clear;
    AllRegExps.Clear;

    for n := 0 to lstRegExps.Items.Count - 1 do
      RegExps.Add(lstRegExps.Items[n].Caption);
    RegExps.Add(Trim(txtRegEx.Text));

    for n := 0 to lstOtherRegExps.Items.Count - 1 do
      OtherRegExps.Add(lstOtherRegExps.Items[n].Caption);

    AllRegExps.AddStrings(RegExps);
    AllRegExps.AddStrings(OtherRegExps);

    RegExp := GetBestRegEx(NodeData.Title, AllRegExps);

    R := TRegExpr.Create(RegExp);
    R.ModifierI := True;
    try
      try
        if R.Exec(NodeData.Title) then
        begin
          try
            if R.MatchIndexFromName('a') > 0 then
              NodeData.ParsedArtist := Trim(R.MatchFromName('a'));
          except
          end;
          try
            if R.MatchIndexFromName('t') > 0 then
              NodeData.ParsedTitle := Trim(R.MatchFromName('t'));
          except
          end;
        end;
      except
      end;
    finally
      R.Free;
    end;

    if (Length(NodeData.ParsedArtist) > 0) and (Length(NodeData.ParsedTitle) > 0) then
      if RegExps.IndexOf(RegExp) > -1 then
        NodeData.MatchedRegExp := True
      else if OtherRegExps.IndexOf(RegExp) > -1 then
        NodeData.MatchedOtherRegExp := True;
  finally
    RegExps.Free;
    OtherRegExps.Free;
    AllRegExps.Free;
  end;
end;

procedure TfrmSetStreamData.SetState(Enable: Boolean);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    Controls[i].Enabled := Enable;

  btnOK.Enabled := Enable;
  btnCancel.Enabled := True;
  pnlNav.Enabled := True;

  btnAddRegEx.Enabled := Length(Trim(txtRegEx.Text)) >= 1;
  btnRemoveRegEx.Enabled := lstRegExps.Selected <> nil;
end;

procedure TfrmSetStreamData.txtRegExChange(Sender: TObject);
begin
  btnAddRegEx.Enabled := Length(Trim(txtRegEx.Text)) >= 1;
  InvalidateTree;
end;

end.
