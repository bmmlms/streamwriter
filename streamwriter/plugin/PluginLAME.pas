unit PluginLAME;

interface

uses
  SysUtils, Windows, Classes, PluginBase, LanguageObjects;

type
  TPluginLAME = class(TPluginBase)
  private
    FDLLPath: string;
  protected
  public
    constructor Create;

    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;

    procedure Initialize; override;

    property DLLPath: string read FDLLPath;
  end;

implementation

uses
  AppData;

{ TPluginLAME }

procedure TPluginLAME.Assign(Source: TPluginBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TPluginLAME.Copy: TPluginBase;
begin
  Result := TPluginLAME.Create;

  Result.Assign(Self);
end;

constructor TPluginLAME.Create;
begin
  inherited;

  FName := _('Support encoding of MP3s using LAME');
  FHelp := _('This plugin adds support for encoding of MP3s to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'plugin_lame';
  FDownloadPackage := 'plugin_lame.dll';

  FFilesDir := AppGlobals.TempDir + 'plugin_lame\';
  FDLLPath := FFilesDir + 'lame-enc.dll';

  FFilenames.Add('lame-enc.dll');
end;

procedure TPluginLAME.Initialize;
begin
  inherited;

  FName := _('Support encoding of MP3s using LAME');
  FHelp := _('This plugin adds support for encoding of MP3s to the application which is useful for postprocessing of recorded songs.');
end;

end.
