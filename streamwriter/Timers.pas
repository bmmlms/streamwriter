unit Timers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Mask, Buttons, ExtCtrls, LanguageObjects,
  VirtualTrees, AppData, Functions, DateUtils, Logging;

type
  // Die ..None-Dinger müssen am Ende stehen!
  TScheduleInterval = (siDaily, siWeekly, siNone);
  TScheduleDay = (sdMonday, sdTuesday, sdWednesday, sdThursday, sdFriday, sdSaturday, sdSunday, sdNone);

  TScheduleTreeNodeData = record
    Recurring: Boolean;
    Specific: Boolean;
    Interval: TScheduleInterval;
    Day: TScheduleDay;
    Date: TDateTime;
    StartHour, StartMinute, EndHour, EndMinute: Integer;
  end;
  PScheduleTreeNodeData =^ TScheduleTreeNodeData;

  TScheduleTree = class(TVirtualStringTree)
  private
  protected
    procedure Resize; override;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: string); override;
  public
    constructor Create(AOwner: TComponent);

    procedure Add(Interval: TScheduleInterval; Day: TScheduleDay; SH, SM, EH, EM: Integer); overload;
    procedure Add(Date: TDateTime; SH, SM, EH, EM: Integer); overload;
  end;

  TfrmTimers = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
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
    btnRemove: TButton;
    Panel4: TPanel;
    btnAdd: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbRecurringClick(Sender: TObject);
    procedure rbDateClick(Sender: TObject);
    procedure lstIntervalChange(Sender: TObject);
    procedure lstDayChange(Sender: TObject);
    procedure dtpDateChange(Sender: TObject);
    procedure txtStartChange(Sender: TObject);
    procedure txtEndChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    Tree: TScheduleTree;
    FSettings: TStreamSettings;

    function TimesOkay: Boolean;
    procedure UpdateButtons;

    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    constructor Create(AOwner: TComponent; Settings: TStreamSettings);
  end;

implementation

{$R *.dfm}

procedure TfrmTimers.btnAddClick(Sender: TObject);
begin
  if lstInterval.ItemIndex = -1 then
  begin
    MsgBox(Handle, 'TODO: !!!', 'TODO: !!!', MB_ICONINFORMATION);
    Exit;
  end;

  if lstDay.ItemIndex = -1 then
  begin
    MsgBox(Handle, 'TODO: !!!', 'TODO: !!!', MB_ICONINFORMATION);
    Exit;
  end;

  if not TimesOkay then
  begin
    MsgBox(Handle, 'TODO: !!!', 'TODO: !!!', MB_ICONINFORMATION);
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
      StrToInt(txtEndHour.Text), StrToInt(txtEndMinute.Text));
  end;
end;

procedure TfrmTimers.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTimers.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmTimers.btnRemoveClick(Sender: TObject);
begin
  if Tree.GetFirstSelected <> nil then
    Tree.DeleteNode(Tree.GetFirstSelected);
end;

constructor TfrmTimers.Create(AOwner: TComponent;
  Settings: TStreamSettings);
begin
  inherited Create(AOwner);

  FSettings := Settings;
end;

procedure TfrmTimers.dtpDateChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmTimers.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FSettings.Free;
end;

procedure TfrmTimers.FormCreate(Sender: TObject);
begin
  Tree := TScheduleTree.Create(Self);
  Tree.Parent := pnlTree;
  Tree.Align := alClient;

  Tree.OnChange := TreeChange;

  UpdateButtons;
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
  btnRemove.Enabled := Node <> nil;
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

  Data := GetNodeData(P);
  Data.Recurring := True;
  Data.Interval := Interval;
  Data.Day := Day;
  Data.Date := 0;
  Data.StartHour := SH;
  Data.StartMinute := SM;
  Data.EndHour := EH;
  Data.EndMinute := EM;
end;

procedure TScheduleTree.Add(Date: TDateTime; SH, SM, EH, EM: Integer);
var
  P: PVirtualNode;
  Data: PScheduleTreeNodeData;
begin
  P := AddChild(nil);
  P.CheckType := ctCheckBox;

  Data := GetNodeData(P);
  Data.Recurring := False;
  Data.Interval := siNone;
  Data.Day := sdNone;
  Data.Date := Date;
  Data.StartHour := SH;
  Data.StartMinute := SM;
  Data.EndHour := EH;
  Data.EndMinute := EM;
end;

constructor TScheduleTree.Create(AOwner: TComponent);
var
  C: TVirtualTreeColumn;
begin
  inherited;

  NodeDataSize := SizeOf(TScheduleTreeNodeData);

  TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines];
  TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect];

  Indent := 4;

  Header.Options := [hoVisible];

  C := Header.Columns.Add;
  C.Text := _('Scheduled recordings');
end;

procedure TScheduleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  NodeData: PScheduleTreeNodeData;
  Day, StartTime, EndTime: string;
  DTStart, DTEnd: TDateTime;
begin
  inherited;

  NodeData := GetNodeData(Node);

  if (Column = 0) and (TextType = ttNormal) then
  begin
    DTStart := StrToTime(IntToStr(NodeData.StartHour) + FormatSettings.TimeSeparator + IntToStr(NodeData.StartMinute) + FormatSettings.TimeSeparator + '00');
    DTEnd := StrToTime(IntToStr(NodeData.EndHour) + FormatSettings.TimeSeparator + IntToStr(NodeData.EndMinute) + FormatSettings.TimeSeparator + '00');

    if NodeData.Recurring then
    begin
      if NodeData.Interval = siDaily then
      begin
        Text := _('Record daily from %s to %s');
        Text := Format(Text, [TimeToStr(DTStart), TimeToStr(DTEnd)]);
      end else
      begin
        Text := _('Record every %s from %s to %s');

        case NodeData.Day of
          sdMonday: Day := _('Monday');
          sdTuesday: Day := _('Tuesday');
          sdWednesday: Day := _('Wednesday');
          sdThursday: Day := _('Thursday');
          sdFriday: Day := _('Friday');
          sdSaturday: Day := _('Saturday');
          sdSunday: Day := _('Sunday');
        end;

        Text := Format(Text, [Day, TimeToStr(DTStart), TimeToStr(DTEnd)]);
      end;
    end else
    begin
      Text := _('Record on %s from %s to %s');
      Text := Format(Text, [DateToStr(NodeData.Date), TimeToStr(DTStart), TimeToStr(DTEnd)]);
    end;
  end else
    Text := '';
end;

procedure TScheduleTree.Resize;
begin
  inherited;

  Header.Columns[0].Width := ClientWidth;
end;

end.
