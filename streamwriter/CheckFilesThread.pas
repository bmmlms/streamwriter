unit CheckFilesThread;

interface

uses
  Windows, SysUtils, Classes;

type
  TCheckFilesThread = class(TThread)
  private
    FSuccess: Boolean;
    FFiles: TStringList;
    FRemoveFiles: TStringList;
  protected
    procedure Execute; override;
  public
    constructor Create(Files: TStringList); overload;
    destructor Destroy; override;

    property Success: Boolean read FSuccess;
    property RemoveFiles: TStringList read FRemoveFiles;
  end;

implementation

{ TCheckFilesThread }

constructor TCheckFilesThread.Create(Files: TStringList);
begin
  inherited Create(True);

  FSuccess := False;
  FFiles := TStringList.Create;
  FFiles.Assign(Files);
  FRemoveFiles := TStringList.Create;

  FreeOnTerminate := True;
end;

destructor TCheckFilesThread.Destroy;
begin
  FFiles.Free;
  FRemoveFiles.Free;
  inherited;
end;

procedure TCheckFilesThread.Execute;
var
  i: Integer;
begin
  inherited;

  for i := 0 to FFiles.Count - 1 do
  begin
    if Terminated then
      Exit;

    if not FileExists(FFiles[i]) then
    begin
      FRemoveFiles.Add(FFiles[i]);
    end;
  end;
  FSuccess := True;
end;

end.
