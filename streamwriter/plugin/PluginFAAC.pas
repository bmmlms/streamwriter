unit PluginFAAC;

interface

uses
  SysUtils, Windows, Classes, PluginBase, LanguageObjects, Functions;

type
  TPluginFAAC = class(TPluginBase)
  private
  protected
  public
    constructor Create;

    function Copy: TPluginBase; override;
    procedure Assign(Source: TPluginBase); override;

    procedure Initialize; override;
    function ShowInitMessage(Handle: THandle): Boolean; override;

    function CanEncode(FileExtension: string): Boolean; override;
  end;

implementation

uses
  AppData;

{ TPluginFAAC }

procedure TPluginFAAC.Assign(Source: TPluginBase);
begin
  inherited;

  FName := Source.Name;
  FHelp := Source.Help;
  FDownloadName := Source.DownloadName;
  FDownloadPackage := Source.DownloadPackage;
end;

function TPluginFAAC.CanEncode(FileExtension: string): Boolean;
begin
  Result := (LowerCase(FileExtension) = '.aac') or (LowerCase(FileExtension) = '.m4a');
end;

function TPluginFAAC.Copy: TPluginBase;
begin
  Result := TPluginFAAC.Create;

  Result.Assign(Self);
end;

constructor TPluginFAAC.Create;
begin
  inherited;

  FName := _('Support encoding of AAC/M4A using FAAC');
  FHelp := _('This plugin adds support for encoding of AAC/M4A files to the application which is useful for postprocessing of recorded songs.');
  FDownloadName := 'plugin_faac';
  FDownloadPackage := 'plugin_faac.dll';

  FFilesDir := AppGlobals.TempDir + 'plugin_faac\';

  FFilenames.Add('atomicparsley.exe');
  FFilenames.Add('js32.dll');
  FFilenames.Add('mp4box.exe');
end;

procedure TPluginFAAC.Initialize;
begin
  inherited;

  FName := _('Support encoding of MP3s using LAME');
  FHelp := _('This plugin adds support for encoding of MP3s to the application which is useful for postprocessing of recorded songs.');
end;

function TPluginFAAC.ShowInitMessage(Handle: THandle): Boolean;
begin
  Result := MsgBox(Handle, _('WARNING:'#13#10'It is not be allowed in some contries to use this plugin because it contains lame-enc.dll ' +
                             'that make use of some patented technologies. Please make sure you may use these files in your country. ' +
                             'If you are sure you may use these files, press "Yes" to continue.'), _('Warning'), MB_ICONWARNING or MB_YESNO or MB_DEFBUTTON2) = IDYES;
end;

end.
