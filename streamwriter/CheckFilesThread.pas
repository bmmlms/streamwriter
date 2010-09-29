unit CheckFilesThread;

interface

uses
  Windows, SysUtils, Classes, Functions;

type
  TFileEntryAction = (eaNone, eaSize, eaRemove);

  TFileEntry = class
  private
    FAction: TFileEntryAction;
    FSize: UInt64;
    FFilename: string;
    FHash: Cardinal;
  public
    constructor Create(Filename: string; Size: UInt64; Action: TFileEntryAction);
    destructor Destroy; override;

    property Action: TFileEntryAction read FAction write FAction;
    property Size: UInt64 read FSize write FSize;
    property Filename: string read FFilename write FFilename;
    property Hash: Cardinal read FHash write FHash;
  end;

  TCheckFilesThread = class(TThread)
  private
    FSuccess: Boolean;
    FFiles: TList;
  protected
    procedure Execute; override;
  public
    constructor Create(Files: TList); overload;
    destructor Destroy; override;

    property Success: Boolean read FSuccess;
    property Files: TList read FFiles;
  end;

implementation

{ TCheckFilesThread }

constructor TCheckFilesThread.Create(Files: TList);
begin
  inherited Create(True);

  FSuccess := False;
  FFiles := Files.Create;

  FreeOnTerminate := True;
end;

destructor TCheckFilesThread.Destroy;
var
  i: Integer;
begin
  for i := 0 to FFiles.Count - 1 do
    TFileEntry(FFiles[i]).Free;
  FFiles.Free;
  inherited;
end;

procedure TCheckFilesThread.Execute;
var
  i: Integer;
  NewSize: UInt64;
  E: TFileEntry;
begin
  inherited;

  for i := 0 to FFiles.Count - 1 do
  begin
    E := TFileEntry(FFiles[i]);
    if Terminated then
      Exit;

    if not FileExists(E.Filename) then
      E.Action := eaRemove
    else
    begin
      NewSize := GetFileSize(E.Filename);
      if E.Size <> NewSize then
      begin
        E.Action := eaSize;
        E.Size := NewSize;
      end;
    end;
  end;
  FSuccess := True;
end;

{ TFileEntry }

constructor TFileEntry.Create(Filename: string; Size: UInt64;
  Action: TFileEntryAction);
begin
  FFilename := Filename;
  FSize := Size;
  FAction := Action;
  FHash := HashString(LowerCase(ExtractFileName(Filename)));
end;

destructor TFileEntry.Destroy;
begin

  inherited;
end;

end.
