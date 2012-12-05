unit HomeTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Commands, HomeCommands, HomeCommunication2, ComCtrls, StdCtrls,
  Protocol, Generics.Collections, DataManager;

type
  TfrmHomeTest = class(TForm)
    lstEvents: TListBox;
    Button1: TButton;
    prgTransfer: TProgressBar;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    HC: THomeCommunication;

    procedure Start;

    procedure HomeCommunicationStateChanged(Sender: TObject);
    procedure HomeCommunicationBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
      CommandHeader: TCommandHeader; Transferred: UInt64);

    procedure HomeCommunicationLogInReceived(Sender: TObject; Success: Boolean);
    procedure HomeCommunicationServerDataReceived(Sender: TObject; Streams: TList<TStreamBrowserEntry>; Charts: TList<TChartEntry>);
  public
  end;

implementation

{$R *.dfm}

procedure TfrmHomeTest.Button1Click(Sender: TObject);
begin
  HC.GetServerData;
end;

procedure TfrmHomeTest.Button2Click(Sender: TObject);
begin
  HC.LogOn('user', 'pass');
end;

procedure TfrmHomeTest.FormCreate(Sender: TObject);
begin
  TCommand.RegisterCommand(ctHandshakeResponse, TCommandHandshakeResponse);
  TCommand.RegisterCommand(ctLogInResponse, TCommandLogInResponse);
  TCommand.RegisterCommand(ctLogOutResponse, TCommandLogOutResponse);
  TCommand.RegisterCommand(ctGetServerDataResponse, TCommandGetServerDataResponse);


  Start;
end;

procedure TfrmHomeTest.Start;
begin
  { TODO: !!!
  HC := THomeCommunication.Create;
  HC.OnStateChanged := HomeCommunicationStateChanged;
  HC.OnBytesTransferred := HomeCommunicationBytesTransferred;

  HC.OnLogInReceived := HomeCommunicationLogInReceived;
  HC.OnServerDataReceived := HomeCommunicationServerDataReceived;
  }
end;

procedure TfrmHomeTest.HomeCommunicationBytesTransferred(Sender: TObject;
  Direction: TTransferDirection; CommandID: Cardinal;
  CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  prgTransfer.Min := 0;
  prgTransfer.Max := CommandHeader.CommandLength;
  if Transferred > 1 then
    prgTransfer.Position := Transferred - 1;
  prgTransfer.Position := Transferred;
end;

procedure TfrmHomeTest.HomeCommunicationLogInReceived(Sender: TObject;
  Success: Boolean);
begin
  if Success then
    lstEvents.Items.Add('Eingeloggt!')
  else
    lstEvents.Items.Add('NICHT Eingeloggt!');
end;

procedure TfrmHomeTest.HomeCommunicationServerDataReceived(Sender: TObject;
  Streams: TList<TStreamBrowserEntry>; Charts: TList<TChartEntry>);
begin

end;

procedure TfrmHomeTest.HomeCommunicationStateChanged(Sender: TObject);
begin
  Button1.Enabled := HC.Connected;

  if HC.Connected then
    lstEvents.Items.Add('Connected')
  else
  begin
    if HC.WasConnected then
      lstEvents.Items.Add('Disconnected');
  end;
end;

end.
