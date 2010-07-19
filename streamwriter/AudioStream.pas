{
    ------------------------------------------------------------------------
    streamWriter
    Copyright (c) 2010 Alexander Nottelmann

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
    ------------------------------------------------------------------------
}
unit AudioStream;

interface

uses
  SysUtils, StrUtils, Classes, ExtendedStream, MPEG;

type
  TAudioStreamFile = class(TFileStream)
  private
  protected
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Integer; virtual; abstract;
    procedure SaveToFile(const Filename: string; From, Length: Int64);
  end;

  TMPEGStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Integer; override;
  end;

  TAACStreamFile = class(TAudioStreamFile)
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Integer; override;
  end;

  TAudioStreamMemory = class(TExtendedStream)
  private
  protected
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Integer; virtual; abstract;
  end;

  TMPEGStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Integer; override;
  end;

  TAACStreamMemory = class(TAudioStreamMemory)
  public
    function GetFrame(From: Int64; GetEnd: Boolean): Integer; override;
  end;

implementation

{ TAudioStream }

procedure TAudioStreamFile.SaveToFile(const Filename: string; From, Length: Int64);
var
  Stream: TFileStream;
  OldPos: Int64;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    OldPos := Position;
    Seek(From, soFromBeginning);
    Stream.CopyFrom(Self, Length);
    Seek(OldPos, soFromBeginning);
  finally
    Stream.Free;
  end;
end;

{ TMPEGStream }

function TMPEGStreamFile.GetFrame(From: Int64; GetEnd: Boolean): Integer;
var
  i, OldPos, Offset: Integer;
  Frame: FrameData;
  Len: Integer;
  Buf: array[0..3] of byte;
begin
  OldPos := Position;
  Result := -1;

  if GetEnd then
  begin
    Offset := Size;

    for i := Offset - 4 downto 0 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Len := GetFrameLength(Frame);

        if i + Len <= Size then
        begin
          Result := i + Len;
          Break;
        end;
      end;
    end;
  end else
  begin
    Offset := From;

    for i := Offset to Size - 4 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Result := i;
        Break;
      end;
    end;
  end;
  Position := OldPos;
end;

{ TAACStream }

function TAACStreamFile.GetFrame(From: Int64; GetEnd: Boolean): Integer;
begin
  if GetEnd then
    Result := Size - 1
  else
    Result := From;
end;

{ TMPEGStreamMemory }

function TMPEGStreamMemory.GetFrame(From: Int64; GetEnd: Boolean): Integer;
var
  i, OldPos, Offset: Integer;
  Frame: FrameData;
  Len: Integer;
  Buf: array[0..3] of byte;
begin
  OldPos := Position;
  Result := -1;

  if GetEnd then
  begin
    Offset := Size;

    for i := Offset - 4 downto 0 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Len := GetFrameLength(Frame);

        if i + Len <= Size then
        begin
          Result := i + Len;
          Break;
        end;
      end;
    end;
  end else
  begin
    Offset := From;

    for i := Offset to Size - 4 do
    begin
      Position := i;
      Read(Buf, 4);

      if IsFrameHeader(Buf, 0) then
      begin
        DecodeHeader(Buf, Frame, 0);
        Result := i;
        Break;
      end;
    end;
  end;
  Position := OldPos;
end;

{ TAACStreamMemory }

function TAACStreamMemory.GetFrame(From: Int64; GetEnd: Boolean): Integer;
begin
  if GetEnd then
    Result := Size - 1
  else
    Result := From;
end;

end.
