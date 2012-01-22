unit PluginAudioGenie;

interface

uses
  SysUtils, Windows, Classes, PluginBase, LanguageObjects, Functions, TypeDefs;

type
  TPluginAudioGenie = class(TPluginBase)
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

{ TPluginAudioGenie }

procedure TPluginAudioGenie.Assign(Source: TPluginBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TPluginAudioGenie.Copy: TPluginBase;
begin
  Result := TPluginAudioGenie.Create;

  Result.Assign(Self);
end;

constructor TPluginAudioGenie.Create;
begin
  inherited;

  FName := _('Support setting of tags using AudioGenie');
  FHelp := _('This plugin sets tags to saved songs using AudioGenie.');
  FDownloadName := 'addon_audiogenie';
  FDownloadPackage := 'addon_audiogenie.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_audiogenie\';
  FDLLPath := FFilesDir + 'audiogenie3.dll';

  FFilenames.Add('audiogenie3.dll');
end;

procedure TPluginAudioGenie.Initialize;
begin
  inherited;

  FName := _('Support setting of tags using AudioGenie');
  FHelp := _('This plugin sets tags to saved songs using AudioGenie.');
end;

end.
