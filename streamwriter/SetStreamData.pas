unit SetStreamData;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Constants, AppData,
  Vcl.Buttons, Vcl.ExtCtrls, Vcl.ComCtrls, PngSpeedButton, PerlRegEx, Functions, LanguageObjects,
  MControls, TypeDefs, HomeCommunication, Generics.Collections, SharedData, SWFunctions,
  GUIFunctions;

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

  TfrmSetStreamData = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    lstTitles: TVirtualStringTree;
    btnResetTitlePattern: TPngSpeedButton;
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
    procedure lstRegExpsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure txtRegExChange(Sender: TObject);
    procedure lstTitlesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure FormResize(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lstTitlesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure lstTitlesMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure lstTitlesMeasureTextWidth(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const Text: string; var Extent: Integer);
    procedure lstRegExpsEdited(Sender: TObject; Item: TListItem;
      var S: string);
  private
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

{$R *.dfm}

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
  Item.ImageIndex := 30;
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
  begin
    if MsgBox(Handle, _('A regular expression was entered into the text field but not added to the list.'#13#10'Do you want to continue without saving that regular expression?'), _('Question'), MB_YESNO or MB_ICONQUESTION) = IDNO then
      Exit;
  end;

  SetLength(RegExps, 0);
  for i := 0 to lstRegExps.Items.Count - 1 do
  begin
    SetLength(RegExps, Length(RegExps) + 1);
    RegExps[High(RegExps)] := lstRegExps.Items[i].Caption;
  end;

  if not HomeComm.SendSetStreamData(FStreamID, RegExps) then
  begin
    MsgBox(Handle, _('streamWriter is not connected to the server.'#13#10'Please make sure your internet connection is up.'), _('Info'), MB_ICONINFORMATION)
  end else
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

constructor TfrmSetStreamData.Create(AOwner: TComponent;
  StreamID: Integer);
begin
  inherited Create(AOwner);

  SetState(False);

  lstTitles.NodeDataSize := SizeOf(TTitleNodeData);
  lstTitles.IncrementalSearch := isVisibleOnly;

  lstTitles.TreeOptions.SelectionOptions := [toDisableDrawSelection, toRightClickSelect, toFullRowSelect];
  lstTitles.TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect];
  lstTitles.TreeOptions.MiscOptions := lstTitles.TreeOptions.MiscOptions + [toVariableNodeHeight] - [toAcceptOLEDrop];
  lstTitles.Header.Options := lstTitles.Header.Options - [hoVisible];
  lstTitles.ShowHint := False;

  FStreamID := StreamID;

  HomeComm.OnGetStreamDataReceived := HomeCommGetStreamData;

  HomeComm.SendGetStreamData(StreamID);
end;

procedure TfrmSetStreamData.FormClose(Sender: TObject;
  var Action: TCloseAction);
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

procedure TfrmSetStreamData.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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

procedure TfrmSetStreamData.HomeCommGetStreamData(Sender: TObject;
  LastTitles, OtherUserRegExps, UserRegExps: TStringArray);
var
  i, W: Integer;
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
  Item: TListItem;
begin
  lstTitles.Clear;
  for i := 0 to High(LastTitles) do
  begin
    Node := lstTitles.AddChild(nil);
    NodeData := lstTitles.GetNodeData(Node);
    NodeData.DataSet := False;

    NodeData.Title := LastTitles[i];
    lstTitles.MultiLine[Node] := True;

    W := GUIFunctions.GetTextSize(NodeData.Title, lstTitles.Font).cx;
    if W > FMaxTextWidth then
      FMaxTextwidth := W;
  end;

  lstOtherRegExps.Items.Clear;
  for i := 0 to High(OtherUserRegExps) do
  begin
    Item := lstOtherRegExps.Items.Add;
    Item.Caption := OtherUserRegExps[i];
    Item.ImageIndex := 30;
  end;

  lstRegExps.Items.Clear;
  for i := 0 to High(UserRegExps) do
  begin
    Item := lstRegExps.Items.Add;
    Item.Caption := UserRegExps[i];
    Item.ImageIndex := 30;
  end;

  InvalidateTree;

  SetState(True);

  lstTitles.ApplyFocus;
end;

procedure TfrmSetStreamData.InvalidateTree;
var
  Node: PVirtualNode;
  NodeData: PTitleNodeData;
begin
  Node := lstTitles.GetFirst;
  while Node <> nil do
  begin
    NodeData := lstTitles.GetNodeData(Node);
    NodeData.DataSet := False;
    Node := lstTitles.GetNext(Node);
  end;

  lstTitles.Invalidate;
end;

procedure TfrmSetStreamData.lstRegExpsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  btnRemoveRegEx.Enabled := lstRegExps.Selected <> nil;
end;

procedure TfrmSetStreamData.lstRegExpsEdited(Sender: TObject;
  Item: TListItem; var S: string);
begin
  if not CheckRegExp(Handle, S, lstRegExps, Item) then
  begin
    S := Item.Caption;
    Exit;
  end;

  Item.Caption := S;
  InvalidateTree;
end;

procedure TfrmSetStreamData.lstTitlesGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  NodeData: PTitleNodeData;
begin
  NodeData := lstTitles.GetNodeData(Node);

  SetNodeData(NodeData);

  if Kind <> ikState then
  begin
   if NodeData.MatchedRegExp then
     ImageIndex := 107
   else if NodeData.MatchedOtherRegExp then
    ImageIndex := 106
   else
    ImageIndex := 105;
  end;
end;

procedure TfrmSetStreamData.lstTitlesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PTitleNodeData;
begin
  NodeData := lstTitles.GetNodeData(Node);

  SetNodeData(NodeData);

  CellText := Format('%s'#13#10'%s %s'#13#10'%s %s', [NodeData.Title, _('Artist:'), NodeData.ParsedArtist, _('Title:'), NodeData.ParsedTitle]);
end;

procedure TfrmSetStreamData.lstTitlesMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  NodeHeight := Trunc(GUIFunctions.GetTextSize('Wyg', TargetCanvas.Font).cy * 3) + 4;
end;

procedure TfrmSetStreamData.lstTitlesMeasureTextWidth(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; var Extent: Integer);
begin
  Extent := FMaxTextWidth;
end;

procedure TfrmSetStreamData.SetNodeData(NodeData: PTitleNodeData);
var
  n: Integer;
  RegExps: TStringList;
  OtherRegExps: TStringList;
  AllRegExps: TStringList;
  R: TPerlRegEx;
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

    R := TPerlRegEx.Create;
    R.Options := R.Options + [preCaseLess];
    try
      R.Subject := NodeData.Title;
      R.RegEx := RegExp;
      try
        if R.Match then
        begin
          try
            if R.NamedGroup('a') > 0 then
              NodeData.ParsedArtist := Trim(R.Groups[R.NamedGroup('a')]);
          except end;
          try
            if R.NamedGroup('t') > 0 then
              NodeData.ParsedTitle := Trim(R.Groups[R.NamedGroup('t')]);
          except end;
        end;
      except end;
    finally
      R.Free;
    end;

    if (Length(NodeData.ParsedArtist) > 0) and (Length(NodeData.ParsedTitle) > 0) then
    begin
      if RegExps.IndexOf(RegExp) > -1 then
        NodeData.MatchedRegExp := True
      else if OtherRegExps.IndexOf(RegExp) > -1 then
        NodeData.MatchedOtherRegExp := True;
    end;
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

