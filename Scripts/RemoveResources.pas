{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, Windows;

var
  i: Integer;
  H, DLLH: HANDLE;
  Lst: TStringList;
  s: string;
begin
  Lst := TStringList.Create;
  Lst.LoadFromFile(ParamStr(2));

  H := BeginUpdateResource(PChar(ParamStr(1)), False);
  if H = 0 then
    raise Exception.Create('BeginUpdateResource failed');

  for s in Lst do
  begin
    if s.Trim = '' then
      Continue;
    
    UpdateResource(H, RT_RCDATA, PChar(s), MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), nil, 0);
  end;

  if not EndUpdateResource(H, False) then
    raise Exception.Create('EndUpdateResource failed');
end.
