{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010-2023 Alexander Nottelmann

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
  AppData,
  Buttons,
  Classes,
  ComboEx,
  ComCtrls,
  Controls,
  DataManager,
  DateTimePicker,
  DateUtils,
  Dialogs,
  ExtCtrls,
  Forms,
  Functions,
  Graphics,
  Images,
  LanguageObjects,
  LCLType,
  Logging,
  MVirtualTree,
  PowerManagement,
  SharedControls,
  SharedData,
  StdCtrls,
  SysUtils,
  Variants,
  VirtualTrees;

type
  TScheduleTreeNodeData = record
    Schedule: TSchedule;
  end;
  PScheduleTreeNodeData = ^ TScheduleTreeNodeData;

  TScheduleTree = class(TMVirtualTree)
  private
  protected
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string); override;
    procedure DoChecked(Node: PVirtualNode); override;
  public
    constructor Create(AOwner: TComponent); reintroduce;

    procedure Add(Interval: TScheduleInterval; Day: TScheduleDay; SH, SM, EH, EM: Integer); overload;
    procedure Add(Date: TDateTime; SH, SM, EH, EM: Integer; AutoRemove: Boolean); overload;
    procedure Add(S: TSchedule); overload;
  end;

  { TfrmTimers }

  TfrmTimers = class(TForm)
    btnAdd: TButton;
    dtpStartTime: TDateTimePicker;
    dtpEndTime: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    Panel1: TPanel;
    pnlConfig: TPanel;
    rbRecurring: TRadioButton;
    rbDate: TRadioButton;
    lstInterval: TComboBoxEx;
    lstDay: TComboBoxEx;
    dtpDate: TDateTimePicker;
    pnlTree: TPanel;
    Panel3: TPanel;
    btnRemove: TButton;
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
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FOkay: Boolean;
    Tree: TScheduleTree;
    FEntry: TStreamEntry;

    procedure UpdateButtons;

    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    constructor Create(AOwner: TComponent; Entry: TStreamEntry); reintroduce;
    destructor Destroy; override;

    property Okay: Boolean read FOkay;
    property Entry: TStreamEntry read FEntry;
  end;

implementation

{$R *.lfm}

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
  try
    TFunctions.DateTimeToFileTime(TFunctions.LocalToUTC(dtpStartTime.DateTime));
    TFunctions.DateTimeToFileTime(TFunctions.LocalToUTC(dtpEndTime.DateTime));
  except
    TFunctions.MsgBox(_('Please enter a valid start and end time for the schedule.'), _('Info'), MB_ICONINFORMATION);
    Exit;
  end;

  if rbRecurring.Checked then
    Tree.Add(TScheduleInterval(lstInterval.ItemIndex), TScheduleDay(lstDay.ItemIndex), HourOf(dtpStartTime.Time), MinuteOf(dtpStartTime.Time), HourOf(dtpEndTime.Time), MinuteOf(dtpEndTime.Time))
  else
    Tree.Add(dtpDate.DateTime, HourOf(dtpStartTime.Time), MinuteOf(dtpStartTime.Time), HourOf(dtpEndTime.Time), MinuteOf(dtpEndTime.Time), chkAutoRemove.Checked);
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
    dtpStartTime.Time := EncodeTime(NodeData.Schedule.StartHour, NodeData.Schedule.StartMinute, 0, 0);
    dtpEndTime.Time := EncodeTime(NodeData.Schedule.EndHour, NodeData.Schedule.EndMinute, 0, 0);

    NodeData.Schedule.Free;
    FreeNode := Node;
    Node := Tree.GetNextSelected(Node);
    Tree.DeleteNode(FreeNode);
  end;
end;

constructor TfrmTimers.Create(AOwner: TComponent; Entry: TStreamEntry);
begin
  inherited Create(AOwner);

  modSharedData.imgImages.GetIcon(TImages.Time, Icon);

  FEntry := Entry.Copy;

  Constraints.MinWidth := Scale96ToFont(Constraints.MinWidth);
  Constraints.MinHeight := Scale96ToFont(Constraints.MinHeight);

  dtpStartTime.Time := 0;
  dtpEndTime.Time := 0;
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

  lstInterval.ItemHeight := Scale96ToFont(17);
  lstDay.ItemHeight := Scale96ToFont(17);

  UpdateButtons;
end;

procedure TfrmTimers.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TfrmTimers.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
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

procedure TScheduleTree.Add(Interval: TScheduleInterval; Day: TScheduleDay; SH, SM, EH, EM: Integer);
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

  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport];
  TreeOptions.SelectionOptions := [toMultiSelect, toFullRowSelect];
  TreeOptions.PaintOptions := [toThemeAware, toHideFocusRect] - [toShowTreeLines];

  Indent := 0;

  Header.Options := [hoAutoResize, hoVisible];

  C := Header.Columns.Add;
  C.Text := _('Scheduled recordings');

  C := Header.Columns.Add;
  C.Text := _('Remove');
  C.FitColumn;
  C.Options := C.Options - [coResizable];

  Header.AutoSizeIndex := 0;
end;

procedure TScheduleTree.DoChecked(Node: PVirtualNode);
var
  NodeData: PScheduleTreeNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
  NodeData.Schedule.Active := Node.CheckState = csCheckedNormal;
end;

procedure TScheduleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var Text: string);
var
  NodeData: PScheduleTreeNodeData;
  Day: string;
  DTStart, DTEnd: TDateTime;
begin
  NodeData := GetNodeData(Node);

  case Column of
    0:
    begin
      DTStart := EncodeTime(NodeData.Schedule.StartHour, NodeData.Schedule.StartMinute, 0, 0);
      DTEnd := EncodeTime(NodeData.Schedule.EndHour, NodeData.Schedule.EndMinute, 0, 0);

      if NodeData.Schedule.Recurring then
      begin
        if NodeData.Schedule.Interval = siDaily then
          Text := _('Record daily from %s to %s').Format([TimeToStr(DTStart), TimeToStr(DTEnd)])
        else
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

          Text := Text.Format([Day, TimeToStr(DTStart), TimeToStr(DTEnd)]);
        end;
      end else
        Text := _('Record on %s from %s to %s').Format([DateToStr(NodeData.Schedule.Date), TimeToStr(DTStart), TimeToStr(DTEnd)]);
    end;
    1:
      if NodeData.Schedule.AutoRemove then
        Text := _('Yes')
      else
        Text := _('No');
  end;
end;

end.
