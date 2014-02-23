unit HomeTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Commands, HomeCommunication, HomeCommands, ComCtrls, StdCtrls,
  Protocol, Generics.Collections, DataManager, TypeDefs, AudioFunctions,
  ExtCtrls;

type
  TfrmHomeTest = class(TForm)
    lstEvents: TListBox;
    prgTransfer: TProgressBar;
    Button1: TButton;
    Button2: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    HC: THomeCommunication;

    procedure Start;

    procedure HomeCommunicationStateChanged(Sender: TObject);
    procedure HomeCommunicationBytesTransferred(Sender: TObject; Direction: TTransferDirection; CommandID: Cardinal;
      CommandHeader: TCommandHeader; Transferred: UInt64);

    procedure HomeCommunicationLogInReceived(Sender: TObject; Success: Boolean);
  public
  end;

implementation

{$R *.dfm}

procedure TfrmHomeTest.Button1Click(Sender: TObject);
var
  Cmd: TCommandSubmitStream;
  Cmd2: TCommandSetStreamData;
  Cmd3: TCommandClientStats;
  Cmd4: TCommandTitleChanged;
  Cmd5: TCommandGetServerData;
begin
  {
  Cmd := TCommandSubmitStream.Create;
  Cmd.URL := 'http://abc:123/test778.m3u';
  HC.SendCommand(Cmd);
  }

  {
  Cmd2 := TCommandSetStreamData.Create;
  Cmd2.StreamID := 188090;
  Cmd2.Rating := 2;
  Cmd2.HasRecordingOkay := False;
  Cmd2.RecordingOkay := False;
  Cmd2.TitleRegEx := '111';
  Cmd2.HasIgnoreTitles := False;
  Cmd2.IgnoreTitles := 'asdf';
  HC.SendCommand(Cmd2);
  }

  {
  Cmd3 := TCommandClientStats.Create;
  Cmd3.StatType := csAutoSave;
  HC.SendCommand(Cmd3);
  }

  Cmd4 := TCommandTitleChanged.Create(5, 'dfasdfsa', 'duffy - mercy', 'fsdfsdfas', 'fsdfasdfas', atMPEG, 128, '');
  HC.SendCommand(Cmd4);

  //Cmd5 := TCommandGetServerData.Create;
  //HC.SendCommand(Cmd5);
end;

procedure TfrmHomeTest.Button2Click(Sender: TObject);
begin
  HC.SendTitleChanged(12, '#MUSIK.ROCK (EXTREME) - WWW.RAUTEMUSIK.FM - 24H ROCK ALTERNATIVE METAL AND MORE', 'Def Leppard - Armageddon It',
    'http://87.230.100.94:10000/', 'http://87.230.100.94:10000/', atMPEG, 128, TStringList.Create);
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
  HC := THomeCommunication.Create(nil);
  HC.OnStateChanged := HomeCommunicationStateChanged;
  HC.OnLogInReceived := HomeCommunicationLogInReceived;
  HC.OnBytesTransferred := HomeCommunicationBytesTransferred;
  HC.Connect;
end;

procedure TfrmHomeTest.Timer1Timer(Sender: TObject);
var
  Cmd4: TCommandTitleChanged;
begin
  Cmd4 := TCommandTitleChanged.Create(120, 'dfasdfsa', 'Sie hören "!!!!!! LOL X" mit "!!!!!!! LOL Y"', 'fsdfsdfas', 'fsdfasdfas', atMPEG, 128, '');
  HC.SendCommand(Cmd4);
end;

procedure TfrmHomeTest.HomeCommunicationBytesTransferred(Sender: TObject;
  Direction: TTransferDirection; CommandID: Cardinal;
  CommandHeader: TCommandHeader; Transferred: UInt64);
begin
  {
  prgTransfer.Min := 0;
  prgTransfer.Max := CommandHeader.CommandLength;
  if Transferred > 1 then
    prgTransfer.Position := Transferred - 1;
  prgTransfer.Position := Transferred;
  }
end;

procedure TfrmHomeTest.HomeCommunicationLogInReceived(Sender: TObject;
  Success: Boolean);
begin
  if Success then
    lstEvents.Items.Add('Eingeloggt!')
  else
    lstEvents.Items.Add('NICHT Eingeloggt!');
end;

procedure TfrmHomeTest.HomeCommunicationStateChanged(Sender: TObject);
begin
  if HC.Connected then
    lstEvents.Items.Add('Connected')
  else
  begin
    if HC.WasConnected then
      lstEvents.Items.Add('Disconnected');
  end;
end;

end.
