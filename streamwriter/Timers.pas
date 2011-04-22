unit Timers;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Mask;

type
  TfrmTimers = class(TForm)
    MaskEdit1: TMaskEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmTimers: TfrmTimers;

implementation

{$R *.dfm}

end.
