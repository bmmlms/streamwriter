{$mode objfpc}{$H+}

uses
  SysUtils, Process, laz2_XMLRead, laz2_DOM, laz2_XMLWrite;

function GetProcessOutput(Exe: string; Parameters: array of string): string;
var
  Proc: TProcess;
begin
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := Exe;
    Proc.Parameters.SetStrings(Parameters);

    Proc.Options := Proc.Options + [poWaitOnExit, poUsePipes];

    Proc.Execute;

    SetLength(Result, Proc.Output.NumBytesAvailable);
    Proc.Output.Read(Result[1], Proc.Output.NumBytesAvailable);

    Exit(Result.Trim);
  finally
    Proc.Free;
  end;
end;

function GetGitSHA: string;
begin
  Result := GetProcessOutput('git', ['rev-parse', '--short', 'HEAD']);
end;

function GetGitCommitCount: string;
begin
  Result := GetProcessOutput('git', ['rev-list', '--count', 'HEAD']);
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
  VersionInfoNode, RevisionNode: TDOMElement;
  Doc: TXMLDocument;
  ProductVersion: string;
begin
  try
    ReadXMLFile(Doc, ParamStr(1));

    VersionInfoNode := TDOMElement(Doc.DocumentElement.FindNode('ProjectOptions').FindNode('VersionInfo'));

    if (ParamCount = 3) and (ParamStr(3) = 'AddBuildNr') then
    begin
      RevisionNode := Doc.CreateElement('BuildNr');
      RevisionNode.SetAttribute('Value', GetGitCommitCount);
      VersionInfoNode.AppendChild(RevisionNode);
    end;

    ProductVersion := '%s.%s.%s.%s-%s'.Format([
      GetVersionAttribute(VersionInfoNode, 'MajorVersionNr'),
      GetVersionAttribute(VersionInfoNode, 'MinorVersionNr'),
      GetVersionAttribute(VersionInfoNode, 'RevisionNr'),
      GetVersionAttribute(VersionInfoNode, 'BuildNr'),
      GetGitSHA]);

    TDOMElement(VersionInfoNode.FindNode('StringTable')).SetAttribute('ProductVersion', ProductVersion);

    WriteXMLFile(Doc, ParamStr(2));
  finally
    Doc.Free;
  end;
end.
