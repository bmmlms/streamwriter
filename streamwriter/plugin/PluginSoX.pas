unit PluginSoX;

interface

uses
  SysUtils, Windows, Classes, PluginBase, LanguageObjects;

type
  TPluginSoX = class(TPluginBase)
  private
    FEXEPath: string;
  protected
  public
    constructor Create;

    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;

    procedure Initialize; override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TPluginSoX }

procedure TPluginSoX.Assign(Source: TPluginBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TPluginSoX.Copy: TPluginBase;
begin
  Result := TPluginSoX.Create;

  Result.Assign(Self);
end;

constructor TPluginSoX.Create;
begin
  inherited;

  FName := _('Support applying of effects to recorded songs using SoX');
  FHelp := _('This plugin adds support for applying effects to recorded songs using Sound eXchange (SoX).');
  FDownloadName := 'plugin_sox';
  FDownloadPackage := 'plugin_sox.dll';

  FFilesDir := AppGlobals.TempDir + 'plugin_sox\';
  FEXEPath := FFilesDir + '';

  FFilenames.Add('libgomp-1.dll');
  FFilenames.Add('pthreadgc2.dll');
  FFilenames.Add('zlib1.dll');
  FFilenames.Add('sox.exe');
end;

procedure TPluginSoX.Initialize;
begin
  inherited;

  FName := _('Support applying of effects to recorded songs using SoX');
  FHelp := _('This plugin adds support for applying effects to recorded songs using Sound eXchange (SoX).');
end;

end.
