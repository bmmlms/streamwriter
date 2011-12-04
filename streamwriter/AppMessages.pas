unit AppMessages;

interface

uses
  Windows, SysUtils, MessageBus;

type
  TFileModifyMsg = class(TMessageBase)
  private
    FFilename: string;
  public
    constructor Create(Filename: string);
    property Filename: string read FFilename;
  end;

implementation

{ TDeleteFileMessage }

constructor TFileModifyMsg.Create(Filename: string);
begin
  inherited Create;
  FFilename := Filename;
end;

end.
