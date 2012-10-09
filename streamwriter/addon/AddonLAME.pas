unit AddonLAME;

interface

uses
  SysUtils, Windows, Classes, AddonBase, LanguageObjects, Functions,
  AudioFunctions;

type
  TAddonLAME = class(TAddonBase)
  private
    FEXEPath: string;
  protected
    function FGetName: string; override;
    function FGetHelp: string; override;
  public
    constructor Create;

    function Copy: TAddonBase; override;
    procedure Assign(Source: TAddonBase); override;

    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

    property EXEPath: string read FEXEPath;
  end;

implementation

uses
  AppData;

{ TAddonLAME }

procedure TAddonLAME.Assign(Source: TAddonBase);
begin
  inherited;

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

  Result.Copied := True;
  Result.Assign(Self);
end;

constructor TAddonLAME.Create;
begin
  inherited;

  FDownloadName := 'addon_lame';
  FDownloadPackage := 'addon_lame.dll';
  FHasInitMessage := True;

  FFilesDir := AppGlobals.TempDir + 'addon_lame\';
  FEXEPath := FFilesDir + 'lame.exe';

  FFilenames.Add('lame.exe');
end;

function TAddonLAME.FGetHelp: string;
begin
  Result := _('This addon adds support for encoding of MP3 files to the application which is useful for postprocessing of recorded songs.');
end;

function TAddonLAME.FGetName: string;
begin
  Result := _('Support encoding of MP3 using LAME');
end;

function TAddonLAME.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := MsgBox(Handle, _('WARNING:'#13#10'It may not be allowed in some contries to use the LAME-addon because it contains lame.exe ' +
                             'that makes use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
end;

end.
