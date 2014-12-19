{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2015 Alexander Nottelmann

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

unit Timers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Mask, Buttons, ExtCtrls, LanguageObjects,
  VirtualTrees, AppData, Functions, DateUtils, Logging, DataManager,
  PowerManagement, GUIFunctions;

type
  TScheduleTreeNodeData = record
    Schedule: TSchedule;
  end;
  PScheduleTreeNodeData =^ TScheduleTreeNodeData;

  TScheduleTree = class(TVirtualStringTree)
  private
  protected
    procedure Resize; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
    procedure DoChecked(Node: PVirtualNode); override;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure Add(Interval: TScheduleInterval; Day: TScheduleDay; SH, SM, EH, EM: Integer); overload;
    procedure Add(Date: TDateTime; SH, SM, EH, EM: Integer; AutoRemove: Boolean); overload;
    procedure Add(S: TSchedule); overload;
  end;

  TfrmTimers = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Panel2: TPanel;
    Panel1: TPanel;
    pnlConfig: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    rbRecurring: TRadioButton;
    rbDate: TRadioButton;
    lstInterval: TComboBox;
    lstDay: TComboBox;
    dtpDate: TDateTimePicker;
    txtStartHour: TEdit;
    txtStartMinute: TEdit;
    txtEndHour: TEdit;
    txtEndMinute: TEdit;
    pnlTree: TPanel;
    Panel3: TPanel;
    btnAdd: TButton;
    btnRemove: TButton;
    Label3: TLabel;
    Label4: TLabel;
    chkAutoRemove: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure rbRecurringClick(Sender: TObject);
    procedure rbDateClick(Sender: TObject);
    procedure lstIntervalChange(Sender: TObject);
    procedure lstDayChange(Sender: TObject);
    procedure dtpDateChange(Sender: TObject);
    procedure txtStartChange(Sender: TObject);
    procedure txtEndChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FOkay: Boolean;
    Tree: TScheduleTree;
    FEntry: TStreamEntry;

    function TimesOkay: Boolean;
    procedure UpdateButtons;

    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    constructor Create(AOwner: TComponent; Entry: TStreamEntry); reintroduce;
    destructor Destroy; override;

    property Okay: Boolean read FOkay;
    property Entry: TStreamEntry read FEntry;
  end;

implementation

{$R *.dfm}

procedure TfrmTimers.btnOKClick(Sender: TObject);
var
  i: Integer;
  Node: PVirtualNode;
  NodeData: PScheduleTreeNodeData;
begin
  for i := 0 to FEntry.Schedules.Count - 1 do
    FEntry.Schedules[i].Free;
  FEntry.Schedules.Clear;

  Node := Tree.GetFirst;
  while Node <> nil do
  begin
    NodeData := Tree.GetNodeData(Node);
    FEntry.Schedules.Add(NodeData.Schedule.Copy);
    Node := Tree.GetNext(Node);
  end;

  FOkay := True;

  Close;
end;

procedure TfrmTimers.btnAddClick(Sender: TObject);
begin
  if not TimesOkay then
  begin
    MsgBox(Handle, _('Please enter a valid start and end time for the schedule.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if rbRecurring.Checked then
  begin
    Tree.Add(TScheduleInterval(lstInterval.ItemIndex), TScheduleDay(lstDay.ItemIndex),
      StrToInt(txtStartHour.Text), StrToInt(txtStartMinute.Text), StrToInt(txtEndHour.Text),
      StrToInt(txtEndMinute.Text));
  end else
  begin
    Tree.Add(dtpDate.DateTime, StrToInt(txtStartHour.Text), StrToInt(txtStartMinute.Text),
      StrToInt(txtEndHour.Text), StrToInt(txtEndMinute.Text), chkAutoRemove.Checked);
  end;
end;

procedure TfrmTimers.btnRemoveClick(Sender: TObject);
var
  Node, FreeNode: PVirtualNode;
  NodeData: PScheduleTreeNodeData;
begin
  Node := Tree.GetFirstSelected;
  while Node <> nil do
  begin
    NodeData := Tree.GetNodeData(Tree.GetFirstSelected);

    rbRecurring.Checked := NodeData.Schedule.Recurring;
    rbDate.Checked := not NodeData.Schedule.Recurring;
    if NodeData.Schedule.Recurring then
    begin
      lstInterval.ItemIndex := Integer(NodeData.Schedule.Interval);
      lstDay.ItemIndex := Integer(NodeData.Schedule.Day);
      lstIntervalChange(lstInterval);
    end else
    begin
      dtpDate.DateTime := NodeData.Schedule.Date;
      chkAutoRemove.Checked := NodeData.Schedule.AutoRemove;
    end;
    txtStartHour.Text := IntToStr(NodeData.Schedule.StartHour);
    txtStartMinute.Text := IntToStr(NodeData.Schedule.StartMinute);
    txtEndHour.Text := IntToStr(NodeData.Schedule.EndHour);
    txtEndMinute.Text := IntToStr(NodeData.Schedule.EndMinute);

    NodeData.Schedule.Free;
    FreeNode := Node;
    Node := Tree.GetNextSelected(Node);
    Tree.DeleteNode(FreeNode);
  end;
end;

constructor TfrmTimers.Create(AOwner: TComponent;
  Entry: TStreamEntry);
begin
  inherited Create(AOwner);

  // TODO: und was ist bei überlappenden schedules. die können auch durchaus "erlaubt" sein!
  //       täglich von 13:00-15:00 aufnehmen und nur donnerstags von 12:00-16:00 .....

  FEntry := Entry.Copy;
end;

destructor TfrmTimers.Destroy;
begin
  FEntry.Free;

  inherited;
end;

procedure TfrmTimers.dtpDateChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Node: PVirtualNode;
  NodeData: PScheduleTreeNodeData;
begin
  Node := Tree.GetFirst;
  while Node <> nil do
  begin
    NodeData := Tree.GetNodeData(Node);
    NodeData.Schedule.Free;
    Node := Tree.GetNext(Node);
  end;
  Tree.Clear;
end;

procedure TfrmTimers.FormCreate(Sender: TObject);
begin
  Tree := TScheduleTree.Create(Self);
  Tree.Parent := pnlTree;
  Tree.Align := alClient;

  Tree.OnChange := TreeChange;

  UpdateButtons;
end;

procedure TfrmTimers.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TfrmTimers.FormShow(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FEntry.Schedules.Count - 1 do
    Tree.Add(FEntry.Schedules[i].Copy);

  Language.Translate(Self);

  dtpDate.DateTime := Now;
  lstInterval.ItemIndex := 0;
  lstDay.ItemIndex := 0;
end;

procedure TfrmTimers.lstDayChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.lstIntervalChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.rbDateClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.rbRecurringClick(Sender: TObject);
begin
  UpdateButtons;
end;

function TfrmTimers.TimesOkay: Boolean;
var
  SH, SM, EH, EM: Integer;
begin
  Result := False;

  SH := StrToIntDef(txtStartHour.Text, -1);
  SM := StrToIntDef(txtStartMinute.Text, -1);
  EH := StrToIntDef(txtEndHour.Text, -1);
  EM := StrToIntDef(txtEndMinute.Text, -1);

  if (SH = -1) or (SH > 23) or (SH < 0) then
  begin
    Exit;
  end;

  if (SM = -1) or (SM > 59) or (SM < 0) then
  begin
    Exit;
  end;

  if (EH = -1) or (EH > 23) or (EH < 0) then
  begin
    Exit;
  end;

  if (EM = -1) or (EM > 59) or (EM < 0) then
  begin
    Exit;
  end;

  Result := True;
end;

procedure TfrmTimers.TreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  btnRemove.Enabled := Tree.SelectedCount > 0;
end;

procedure TfrmTimers.txtEndChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.txtStartChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.UpdateButtons;
begin
  lstInterval.Enabled := rbRecurring.Checked;
  lstDay.Enabled := rbRecurring.Checked and (lstInterval.ItemIndex > 0);
  dtpDate.Enabled := rbDate.Checked;
  chkAutoRemove.Enabled := rbDate.Checked;
end;

{ TScheduleTree }

procedure TScheduleTree.Add(Interval: TScheduleInterval; Day: TScheduleDay;
  SH, SM, EH, EM: Integer);
var
  P: PVirtualNode;
  Data: PScheduleTreeNodeData;
begin
  P := AddChild(nil);
  P.CheckType := ctCheckBox;
  P.CheckState := csCheckedNormal;

  Data := GetNodeData(P);
  Data.Schedule := TSchedule.Create;
  Data.Schedule.Active := True;
  Data.Schedule.Recurring := True;
  Data.Schedule.Interval := Interval;
  Data.Schedule.Day := Day;
  Data.Schedule.Date := 0;
  Data.Schedule.StartHour := SH;
  Data.Schedule.StartMinute := SM;
  Data.Schedule.EndHour := EH;
  Data.Schedule.EndMinute := EM;
end;

procedure TScheduleTree.Add(Date: TDateTime; SH, SM, EH, EM: Integer; AutoRemove: Boolean);
var
  P: PVirtualNode;
  Data: PScheduleTreeNodeData;
begin
  P := AddChild(nil);
  P.CheckType := ctCheckBox;
  P.CheckState := csCheckedNormal;

  Data := GetNodeData(P);
  Data.Schedule := TSchedule.Create;
  Data.Schedule.Active := True;
  Data.Schedule.Recurring := False;
  Data.Schedule.Interval := siNone;
  Data.Schedule.Day := sdNone;
  Data.Schedule.Date := Date;
  Data.Schedule.AutoRemove := AutoRemove;
  Data.Schedule.StartHour := SH;
  Data.Schedule.StartMinute := SM;
  Data.Schedule.EndHour := EH;
  Data.Schedule.EndMinute := EM;
end;

procedure TScheduleTree.Add(S: TSchedule);
var
  P: PVirtualNode;
  Data: PScheduleTreeNodeData;
begin
  P := AddChild(nil);
  P.CheckType := ctCheckBox;

  Data := GetNodeData(P);
  Data.Schedule := S;

  if S.Active then
    P.CheckState := csCheckedNormal;
end;

constructor TScheduleTree.Create(AOwner: TComponent);
var
  C: TVirtualTreeColumn;
begin
  inherited;

  NodeDataSize := SizeOf(TScheduleTreeNodeData);

  Header.Height := GetTextSize('Wyg', Font).cy + 5;

  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport];
  TreeOptions.SelectionOptions := [toMultiSelect, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect] - [toShowTreeLines];

  Indent := 4;

  Header.Options := [hoVisible];

  C := Header.Columns.Add;
  C.Text := _('Scheduled recordings');

  C := Header.Columns.Add;
  C.Text := _('Remove');
end;

procedure TScheduleTree.DoChecked(Node: PVirtualNode);
var
  NodeData: PScheduleTreeNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
  NodeData.Schedule.Active := Node.CheckState = csCheckedNormal;
end;

procedure TScheduleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  NodeData: PScheduleTreeNodeData;
  Day: string;
  DTStart, DTEnd: TDateTime;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if TextType = ttNormal then
    case Column of
      0:
        begin
          DTStart := EncodeTime(NodeData.Schedule.StartHour, NodeData.Schedule.StartMinute, 0, 0);
          DTEnd := EncodeTime(NodeData.Schedule.EndHour, NodeData.Schedule.EndMinute, 0, 0);

          if NodeData.Schedule.Recurring then
          begin
            if NodeData.Schedule.Interval = siDaily then
            begin
              Text := _('Record daily from %s to %s');
              Text := Format(Text, [TimeToStr(DTStart), TimeToStr(DTEnd)]);
            end else
            begin
              Text := _('Record every %s from %s to %s');

              case NodeData.Schedule.Day of
                sdMonday: Day := _('monday');
                sdTuesday: Day := _('tuesday');
                sdWednesday: Day := _('wednesday');
                sdThursday: Day := _('thursday');
                sdFriday: Day := _('friday');
                sdSaturday: Day := _('saturday');
                sdSunday: Day := _('sunday');
              end;

              Text := Format(Text, [Day, TimeToStr(DTStart), TimeToStr(DTEnd)]);
            end;
          end else
          begin
            Text := _('Record on %s from %s to %s');
            Text := Format(Text, [DateToStr(NodeData.Schedule.Date), TimeToStr(DTStart), TimeToStr(DTEnd)]);
          end;
        end;
      1:
        if NodeData.Schedule.AutoRemove then
          Text := _('Yes')
        else
          Text := _('No');
    end;
end;

procedure TScheduleTree.DoMeasureItem(TargetCanvas: TCanvas;
  Node: PVirtualNode; var NodeHeight: Integer);
begin
  inherited;

  NodeHeight := GetTextSize('Wyg', Font).cy + 5;
end;

procedure TScheduleTree.Resize;
begin
  inherited;

  Header.Columns[0].Width := ClientWidth - 70;
  Header.Columns[1].Width := 70;
end;

end.
