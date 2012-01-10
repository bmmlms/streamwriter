unit PluginLAME;

interface

uses
  SysUtils, Windows, Classes, PluginBase, LanguageObjects, Functions, TypeDefs;

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
    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(AudioType: TAudioTypes): Boolean; override;

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

function TPluginLAME.CanEncode(AudioType: TAudioTypes): Boolean;
begin
  Result := AudioType = atMPEG;
end;

function TPluginLAME.Copy: TPluginBase;
begin
  Result := TPluginLAME.Create;

  Result.Assign(Self);
end;

constructor TPluginLAME.Create;
begin
  inherited;

  FName := _('Support encoding of MP3 using LAME');
  FHelp := _('This plugin adds support for encoding of MP3 files to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'plugin_lame';
  FDownloadPackage := 'plugin_lame.dll';

  FFilesDir := AppGlobals.TempDir + 'plugin_lame\';
  FDLLPath := FFilesDir + 'lame-enc.dll';

  FFilenames.Add('lame-enc.dll');
end;

procedure TPluginLAME.Initialize;
begin
  inherited;

  FName := _('Support encoding of MP3 using LAME');
  FHelp := _('This plugin adds support for encoding of MP3 files to the application which is useful for postprocessing of recorded songs.');
end;

function TPluginLAME.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := MsgBox(Handle, _('WARNING:'#13#10'It is not be allowed in some contries to use this plugin because it contains lame-enc.dll ' +
                             'that makes use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
end;

end.
