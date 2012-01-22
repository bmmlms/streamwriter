unit PluginOGGEnc;

interface

uses
  SysUtils, Windows, Classes, PluginBase, LanguageObjects, Functions, TypeDefs;

type
  TPluginOGGEnc = class(TPluginBase)
  private
    FEXEPath: string;
  protected
  public
    constructor Create;

    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;

    procedure Initialize; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TPluginOGGEnc }

procedure TPluginOGGEnc.Assign(Source: TPluginBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TPluginOGGEnc.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atOGG;
end;

function TPluginOGGENC.Copy: TPluginBase;
begin
  Result := TPluginOGGENC.Create;

  Result.Assign(Self);
end;

constructor TPluginOGGEnc.Create;
begin
  inherited;

  FName := _('Support encoding of OGG using OggEnc');
  FHelp := _('This plugin adds support for encoding of OGG files to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'addon_oggenc';
  FDownloadPackage := 'addon_oggenc.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_oggenc\';
  FEXEPath := FFilesDir + 'oggenc2.exe';

  FFilenames.Add('oggenc2.exe');
end;

procedure TPluginOGGEnc.Initialize;
begin
  inherited;

  FName := _('Support encoding of OGG using OggEnc');
  FHelp := _('This plugin adds support for encoding of OGG files to the application which is useful for postprocessing of recorded songs.');
end;

function TPluginOGGEnc.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
