unit AddonLAME;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions, TypeDefs;

type
  TAddonLAME = class(TAddonBase)
  private
    FDLLPath: string;
  protected
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    procedure Initialize; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

    property DLLPath: string read FDLLPath;
  end;

implementation

uses
  AppData;

{ TAddonLAME }

procedure TAddonLAME.Assign(Source: TAddonBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TAddonLAME.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atMPEG;
end;

function TAddonLAME.Copy: TAddonBase;
begin
  Result := TAddonLAME.Create;

  Result.Assign(Self);
end;

constructor TAddonLAME.Create;
begin
  inherited;

  FName := _('Support encoding of MP3 using LAME');
  FHelp := _('This addon adds support for encoding of MP3 files to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'addon_lame';
  FDownloadPackage := 'addon_lame.dll';

  FFilesDir := AppGlobals.TempDir + 'addon_lame\';
  FDLLPath := FFilesDir + 'lame-enc.dll';

  FFilenames.Add('lame-enc.dll');
end;

procedure TAddonLAME.Initialize;
begin
  inherited;

  FName := _('Support encoding of MP3 using LAME');
  FHelp := _('This addon adds support for encoding of MP3 files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonLAME.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := MsgBox(Handle, _('WARNING:'#13#10'It is not be allowed in some contries to use this addon because it contains lame-enc.dll ' +
                             'that makes use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
end;

end.
