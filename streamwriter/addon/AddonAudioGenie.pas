unit AddonAudioGenie;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions, TypeDefs;

type
  TAddonAudioGenie = class(TAddonBase)
  private
    FDLLPath: string;
  protected
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    procedure Initialize; override;

    property DLLPath: string read FDLLPath;
  end;

implementation

uses
  AppData;

{ TAddonAudioGenie }

procedure TAddonAudioGenie.Assign(Source: TAddonBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonAudioGenie.Copy: TAddonBase;
begin
  Result := TAddonAudioGenie.Create;

  Result.Assign(Self);
end;

constructor TAddonAudioGenie.Create;
begin
  inherited;

  FName := _('Support setting of tags using AudioGenie');
  FHelp := _('This addon sets tags to saved songs using AudioGenie.');
  FDownloadName := 'addon_audiogenie';
  FDownloadPackage := 'addon_audiogenie.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_audiogenie\';
  FDLLPath := FFilesDir + 'audiogenie3.dll';

  FFilenames.Add('audiogenie3.dll');
end;

procedure TAddonAudioGenie.Initialize;
begin
  inherited;

  FName := _('Support setting of tags using AudioGenie');
  FHelp := _('This addon sets tags to saved songs using AudioGenie.');
end;

end.