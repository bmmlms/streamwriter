unit AddonSoX;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects;

type
  TAddonSoX = class(TAddonBase)
  private
    FEXEPath: string;
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TAddonSoX }

procedure TAddonSoX.Assign(Source: TAddonBase);
begin
  inherited;

  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonSoX.Copy: TAddonBase;
begin
  Result := TAddonSoX.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonSoX.Create;
begin
  inherited;

  FDownloadName := 'addon_sox';
  FDownloadPackage := 'addon_sox.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_sox\';
  FEXEPath := FFilesDir + 'sox.exe';

  FFilenames.Add('libgomp-1.dll');
  FFilenames.Add('pthreadgc2.dll');
  FFilenames.Add('zlib1.dll');
  FFilenames.Add('sox.exe');
end;

function TAddonSoX.FGetHelp: string;
begin
  Result := _('This addon adds support for applying effects to recorded songs using Sound eXchange (SoX).');
end;

function TAddonSoX.FGetName: string;
begin
  Result := _('Support applying of effects to recorded songs using SoX');
end;

end.
