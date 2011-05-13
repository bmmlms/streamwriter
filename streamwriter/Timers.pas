unit Timers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Mask, Buttons, ExtCtrls, LanguageObjects,
  VirtualTrees, AppData, Functions, Logging;

type
  TScheduleInterval = (siDaily, siWeekly);
  TScheduleDay = (sdMonday, sdTuesday, sdWednesday, sdThursday, sdFriday, sdSaturday, sdSunday);

  TScheduleTreeNodeData = record
    Recurring: Boolean;
    Specific: Boolean;
    Interval: TScheduleInterval;
    Day: TScheduleDay;
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

    procedure Add(Interval: TScheduleInterval; Day: TScheduleDay; SH, SM, EH, EM: Integer);
  end;

  TfrmTimers = class(TForm)
    pnlNav: TPanel;
    Bevel2: TBevel;
    btnOK: TBitBtn;
    pnlConfig: TPanel;
    rbRecurring: TRadioButton;
    rbDate: TRadioButton;
    lstInterval: TComboBox;
    lstDay: TComboBox;
    dtpDate: TDateTimePicker;
    Label1: TLabel;
    Label2: TLabel;
    btnCancel: TBitBtn;
    btnAdd: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    btnRemove: TButton;
    txtStartHour: TEdit;
    txtStartMinute: TEdit;
    txtEndHour: TEdit;
    txtEndMinute: TEdit;
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
  private
    Tree: TScheduleTree;
    FSettings: TStreamSettings;

    function TimesOkay: Boolean;
    procedure UpdateButtons;
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
    // TODO: !!!
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
  Tree.Parent := Panel2;
  Tree.Align := alClient;
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
  SM:= StrToIntDef(txtStartMinute.Text, -1);
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

end;

{ TScheduleTree }

procedure TScheduleTree.Add(Interval: TScheduleInterval; Day: TScheduleDay;
  SH, SM, EH, EM: Integer);
var
  P: PVirtualNode;
  Data: PScheduleTreeNodeData;
begin
  P := AddChild(nil);
  Data := GetNodeData(P);
  Data.Recurring := True;
  Data.Interval := Interval;
  Data.Day := Day;
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

  Header.Options := [hoVisible];

  C := Header.Columns.Add;
  C.Text := _('Active');

  C := Header.Columns.Add;
  C.Text := _('Details');
end;

procedure TScheduleTree.DoGetText(Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var Text: string);
var
  NodeData: PScheduleTreeNodeData;
begin
  inherited;

  NodeData := GetNodeData(Node);
  Text := IntToStr(NodeData.StartHour);
end;

procedure TScheduleTree.Resize;
begin
  inherited;

  Header.Columns[0].Width := 75;
  Header.Columns[1].Width := ClientWidth - Header.Columns[0].Width;
end;

end.
