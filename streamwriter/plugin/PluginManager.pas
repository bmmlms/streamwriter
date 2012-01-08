unit PluginManager;

interface

uses
  Windows, SysUtils, Classes, Generics.Collections, PluginBase, PluginLAME,
  Forms, Functions, LanguageObjects, PluginSoX;

type
  TPluginManager = class
  private
    FShowVersionWarning: Boolean;
    FPlugins: TList<TPluginBase>;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(ClassType: TClass): TPluginBase;
    function EnablePlugin(Owner: TCustomForm; Plugin: TPluginBase; ShowMessage: Boolean): Boolean;

    property ShowVersionWarning: Boolean read FShowVersionWarning;
    property Plugins: TList<TPluginBase> read FPlugins;
  end;

implementation

uses
  DownloadAddons;

{ TPluginManager }

constructor TPluginManager.Create;
var
  i: Integer;
  PB: TPluginBase;
begin
  inherited Create;

  FPlugins := TList<TPluginBase>.Create;

  FPlugins.Add(TPluginLAME.Create);
  FPlugins.Add(TPluginSoX.Create);

  for i := 0 to Plugins.Count - 1 do
    if Plugins[i].ClassType.InheritsFrom(TPluginBase) then
    begin
      PB := TPluginBase(Plugins[i]);
      if (PB.PackageDownloaded) and (not PB.FilesExtracted) then
      begin
        try
          if not PB.VersionOkay then
          begin
            FShowVersionWarning := True;
            Continue;
          end;
          PB.ExtractFiles;
        except
        end;
      end;
    end;
end;

destructor TPluginManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPlugins.Count - 1 do
  begin
    FPlugins[i].Free;
  end;

  FPlugins.Free;

  inherited;
end;

function TPluginManager.EnablePlugin(Owner: TCustomForm;
  Plugin: TPluginBase; ShowMessage: Boolean): Boolean;
var
  i: Integer;
  Res: Integer;
  DA: TfrmDownloadAddons;
begin
  if not Plugin.PackageDownloaded then
  begin
    if ShowMessage then
      Res := MsgBox(Owner.Handle, _('The plugin cannot be activated because needed files have not been downloaded.'#13#10'Do you want to download these files now?'), _('Question'), MB_ICONQUESTION or MB_YESNO or MB_DEFBUTTON1)
    else
      Res := IDYES;

    if Res = IDYES then
    begin
      if not Plugin.ShowInitMessage(Owner.Handle) then
      begin
        Exit(False);
      end;

      DA := TfrmDownloadAddons.Create(Owner, Plugin);
      try
        DA.ShowModal;

        if not DA.Downloaded then
        begin
          if DA.Error then
            MsgBox(Owner.Handle, _('An error occured while downloading the file.'), _('Error'), MB_ICONEXCLAMATION);

          Exit(False);
        end;
      finally
        DA.Free;
      end;
    end else if Res = IDNO then
    begin
      Exit(False);
    end;
  end;

  if not Plugin.ExtractFiles then
  begin
    MsgBox(Owner.Handle, _('The plugin is not ready for use. This might happen when it''s files could not be extracted.'), _('Error'), MB_ICONEXCLAMATION);
    Exit(False);
  end;

  Exit(True);
end;

function TPluginManager.Find(ClassType: TClass): TPluginBase;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FPlugins.Count - 1 do
    if FPlugins[i].ClassType = ClassType then
    begin
      Result := FPlugins[i];
      Break;
    end;
end;

end.
