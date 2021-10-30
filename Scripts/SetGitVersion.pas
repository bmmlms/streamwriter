{$mode objfpc}{$H+}

uses
  SysUtils, Process, laz2_XMLRead, laz2_DOM, laz2_XMLWrite;

function GetGitSHA: string;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
   // Proc.CurrentDirectory := 'E:\Entwicklung\Mistake\streamwriter';
    Proc.Executable := 'git';
    Proc.Parameters.SetStrings(['rev-parse', '--short', 'HEAD']);

    Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes];

    Proc.Execute;

    SetLength(Result, Proc.Output.NumBytesAvailable);
    Proc.Output.Read(Result[1], Proc.Output.NumBytesAvailable);

    Exit(Result.Trim);
  finally
    Proc.Free;
  end;
end;

function GetVersionAttribute(const VersionInfoNode: TDOMElement; const Name: string): string;
var
  VersionNode: TDOMElement;
begin
  VersionNode := TDOMElement(VersionInfoNode.FindNode(Name));
  if Assigned(VersionNode) and VersionNode.hasAttribute('Value') then
    Exit(VersionNode.GetAttribute('Value'))
  else
    Exit('0');
end;

var
  VersionInfoNode: TDOMElement;
  Doc: TXMLDocument;
  ProductVersion: string;
begin
  try
    ReadXMLFile(Doc, ParamStr(1));

    VersionInfoNode := TDOMElement(Doc.DocumentElement.FindNode('ProjectOptions').FindNode('VersionInfo'));

    ProductVersion := '%s.%s.%s.%s-%s'.Format([
      GetVersionAttribute(VersionInfoNode, 'MajorVersionNr'),
      GetVersionAttribute(VersionInfoNode, 'MinorVersionNr'),
      GetVersionAttribute(VersionInfoNode, 'RevisionNr'),
      GetVersionAttribute(VersionInfoNode, 'BuildNr'),
      GetGitSHA]);

    TDOMElement(VersionInfoNode.FindNode('StringTable')).SetAttribute('ProductVersion', ProductVersion);

    WriteXMLFile(Doc, ParamStr(1));
    WriteXMLFile(Doc, 'z:\aaa.xml');
  finally
    Doc.Free;
  end;
end.
