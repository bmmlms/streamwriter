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

  TVolumeChangedMsg = class(TMessageBase)
  private
    FVolume: Integer;
  public
    constructor Create(Volume: Integer);
    property Volume: Integer read FVolume;
  end;

implementation

{ TDeleteFileMessage }

constructor TFileModifyMsg.Create(Filename: string);
begin
  inherited Create;
  FFilename := Filename;
end;

{ TVolumeChangedMsg }

constructor TVolumeChangedMsg.Create(Volume: Integer);
begin
  inherited Create;
  FVolume := Volume;
end;

end.
