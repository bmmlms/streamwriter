{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, Controls, Graphics, FileUtil, LazFileUtils;

function StreamToHexString(Stream: TMemoryStream): string;
var
  pb: PByte;
  HexStr: string;
begin
  Result := '';
  HexStr := '';
  PB := Stream.Memory;
  while PB < Stream.Memory + Stream.Size do
  begin
    HexStr += IntToHex(PB^, 2);
    if HexStr.Length mod 64 = 0 then
    begin
      Result += HexStr + #13#10;
      HexStr := '';
    end;
    Inc(PB);
  end;
  if HexStr.Length > 0 then
    Result += HexStr;
end;

function Sort(List: TStringList; Index1, Index2: Integer): Integer;
var
  F1, F2: string;
begin
  F1 := ExtractFileNameOnly(List[Index1]).ToUpper.Replace('-', '_', [rfReplaceAll]);
  F2 := ExtractFileNameOnly(List[Index2]).ToUpper.Replace('-', '_', [rfReplaceAll]);
  Result := F1.CompareTo(F2);
end;

var
  Files, SL: TStringList;
  F, ImageName: string;
  Img: TPortableNetworkGraphic;
  Stream: TMemoryStream;
  Imagelist: TImageList;
const
  ImagesPath: string = '..\streamwriter\Res\Images';
begin
  Files := FindAllFiles(ImagesPath, '*.png', True);
  
  Files.CustomSort(@Sort);

  SL := TStringList.Create;
  Img := TPortableNetworkGraphic.Create;
  Stream := TMemoryStream.Create;
  ImageList := TImageList.CreateSize(16, 16);
  // ImageList.RegisterResolutions([8, 10]);

  try
    for F in Files do
    begin
      Img.LoadFromFile(F);

      if Img.Width > 16 then
      begin
        writeln('Skipping %s'.Format([F]));
        continue;
      end;

      ImageName := ExtractFileNameOnly(F).ToUpper.Replace('-', '_', [rfReplaceAll]);
      SL.Add('const %s = %d;'.Format([ImageName, ImageList.AddMultipleResolutions([Img])]));
    end;
    
    SL.SaveToFile('..\Build\ImageConsts.pas');

    ImageList.WriteData(Stream);
    SL.Text := StreamToHexString(Stream);
    SL.SaveToFile('..\Build\ImageListData.txt');
    
    Stream.Clear;
    ImageList.WriteAdvData(Stream);
    SL.Text := StreamToHexString(Stream);
    SL.SaveToFile('..\Build\ImageListAdvData.txt');
  finally
    Stream.free;
    SL.Free;
    Img.free;
    ImageList.Free;
    Files.Free;
  end;
end.
