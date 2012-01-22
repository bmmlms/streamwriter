unit AddonSoX;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects;

type
  TAddonSoX = class(TAddonBase)
  private
    FEXEPath: string;
  protected
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    procedure Initialize; override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TAddonSoX }

procedure TAddonSoX.Assign(Source: TAddonBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonSoX.Copy: TAddonBase;
begin
  Result := TAddonSoX.Create;

  Result.Assign(Self);
end;

constructor TAddonSoX.Create;
begin
  inherited;

  FName := _('Support applying of effects to recorded songs using SoX');
  FHelp := _('This addon adds support for applying effects to recorded songs using Sound eXchange (SoX).');
  FDownloadName := 'addon_sox';
  FDownloadPackage := 'addon_sox.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_sox\';
  FEXEPath := FFilesDir + 'sox.exe';

  FFilenames.Add('libgomp-1.dll');
  FFilenames.Add('pthreadgc2.dll');
  FFilenames.Add('zlib1.dll');
  FFilenames.Add('sox.exe');
end;

procedure TAddonSoX.Initialize;
begin
  inherited;

  FName := _('Support applying of effects to recorded songs using SoX');
  FHelp := _('This addon adds support for applying effects to recorded songs using Sound eXchange (SoX).');
end;

end.
