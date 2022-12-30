{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, Controls, Graphics, FileUtil, regexpr, LazFileUtils, Generics.Collections;

function StreamToHexString(Stream: TMemoryStream): string;
var
  pb: PByte;
  HexStr: string;
begin
  Result := '      ';
  HexStr := '';
  PB := Stream.Memory;
  while PB < Stream.Memory + Stream.Size do
  begin
    HexStr += IntToHex(PB^, 2);
    if HexStr.Length mod 64 = 0 then
    begin
      Result += HexStr + #13#10'      ';
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
  Files, FormFiles, SL: TStringList;
  i, Idx: Integer;
  F, ImageName: string;
  Img: TPortableNetworkGraphic;
  Stream: TMemoryStream;
  Imagelist: TImageList;
  ImageIndices, OldImageIndices: specialize TDictionary<string, Integer>;
  OldToNewIndexMap: specialize TDictionary<Integer, Integer>;
  Expr: TRegExpr;
const
  OldConstsPath: string = '..\Source\Images.pas';
  ConstsPath: string = '..\Build\ImageConsts.pas';
  ImagesPath: string = '..\Resources\Images';
  SrcPath: string = '..';
begin
  ImageIndices := specialize TDictionary<string, Integer>.Create;
  OldImageIndices := specialize TDictionary<string, Integer>.Create;
  OldToNewIndexMap := specialize TDictionary<Integer, Integer>.Create;

  Expr := TRegExpr.Create;
  Expr.Expression := '\s*(\w+) = (\d+);';
  SL := TStringList.Create;
  try
    SL.LoadFromFile(OldConstsPath);
    for F in SL do
    begin
      if Expr.Exec(F) then
        OldImageIndices.Add(Expr.Match[1], Expr.Match[2].ToInteger);
    end;
  finally
    Expr.Free;
    SL.Free;
  end;

  if OldImageIndices.Count = 0 then
    raise Exception.Create('OldImageIndices.Count = 0');

  Files := FindAllFiles(ImagesPath, '*.png', True);
  if Files.Count = 0 then
    raise Exception.Create('Files.Count = 0');

  FormFiles := FindAllFiles(SrcPath, '*.lfm', True);
  if FormFiles.Count = 0 then
    raise Exception.Create('FormFiles.Count = 0');

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
        WriteLn('Skipping %s'.Format([F]));
        Continue;
      end;

      ImageName := ExtractFileNameOnly(F).ToUpper.Replace('-', '_', [rfReplaceAll]);
      Idx := ImageList.AddMultipleResolutions([Img]);

      ImageIndices.Add(ImageName, Idx);

      SL.Add('    %s = %d;'.Format([ImageName, Idx]));
    end;

    if ImageIndices.Count = 0 then
      raise Exception.Create('ImageIndices.Count = 0');

    SL.SaveToFile(ConstsPath);

    ImageList.WriteData(Stream);
    SL.Text := StreamToHexString(Stream);
    SL.SaveToFile('..\Build\ImageListData.txt');
    
    Stream.Clear;
    ImageList.WriteAdvData(Stream);
    SL.Text := StreamToHexString(Stream);
    SL.SaveToFile('..\Build\ImageListAdvData.txt');

    for ImageName in OldImageIndices.Keys do
      if ImageIndices.ContainsKey(ImageName) then
        OldToNewIndexMap.Add(OldImageIndices[ImageName], ImageIndices[ImageName]);

    Expr := TRegExpr.Create;
    Expr.Expression := '(?:\.|\s+)ImageIndex = (\d+)';
    try
      for F in FormFiles do
      begin
        SL.LoadFromFile(F);

        for i := 0 to Pred(SL.Count) do
          if Expr.Exec(SL[i]) then
            if OldToNewIndexMap.ContainsKey(Expr.Match[1].ToInteger) then
            begin
              Idx := OldToNewIndexMap[Expr.Match[1].ToInteger];
              SL[i] := SL[i].Replace(Expr.Match[1], Idx.ToString);
            end else
              SL[i] := SL[i].Replace(Expr.Match[1], '-1');

        SL.SaveToFile(F);
      end;
    finally
      Expr.Free;
    end;
  finally
    Stream.Free;
    SL.Free;
    Img.Free;
    ImageList.Free;
    Files.Free;
    OldImageIndices.Free;
    ImageIndices.Free;
    FormFiles.Free;
    OldToNewIndexMap.Free;
  end;
end.
