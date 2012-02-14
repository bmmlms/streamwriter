unit AddonOGGEnc;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions, TypeDefs;

type
  TAddonOGGEnc = class(TAddonBase)
  private
    FEXEPath: string;
  protected
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    procedure Initialize; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TAddonOGGEnc }

procedure TAddonOGGEnc.Assign(Source: TAddonBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonOGGEnc.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atOGG;
end;

function TAddonOGGEnc.Copy: TAddonBase;
begin
  Result := TAddonOGGEnc.Create;

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonOGGEnc.Create;
begin
  inherited;

  FName := _('Support encoding of OGG using OggEnc');
  FHelp := _('This addon adds support for encoding of OGG files to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'addon_oggenc';
  FDownloadPackage := 'addon_oggenc.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_oggenc\';
  FEXEPath := FFilesDir + 'oggenc2.exe';

  FFilenames.Add('oggenc2.exe');
end;

procedure TAddonOGGEnc.Initialize;
begin
  inherited;

  FName := _('Support encoding of OGG using OggEnc');
  FHelp := _('This addon adds support for encoding of OGG files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonOGGEnc.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := True;
end;

end.
