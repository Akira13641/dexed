unit u_dastworx;
{$I u_defines.inc}

interface

uses
  Classes, SysUtils, process, jsonscanner, fpjson, jsonparser, u_common;

(**
 * Gets the module name and the imports of the source code located in
 * "source". The first line of "import" contains the module name, double quoted.
 * Each following line contain an import.
 *)
procedure getModuleImports(source, imports: TStrings);

(**
 * Gets the module names and the imports of the sources in "files".
 * source. Each line in "import" that contains double quoted text indicates
 * that a new group of import starts.
 *)
procedure getModulesImports(const files: string; results: TStrings);

procedure getHalsteadMetrics(source: TStrings; out jsn: TJSONObject);

procedure getDdocTemplate(source, res: TStrings;caretLine: integer; plusComment: boolean);

implementation

var
  toolname: string;

function getToolName: string;
begin
  if toolname = '' then
    toolname := exeFullName('dastworx' + exeExt);
  exit(toolname);
end;

procedure getModuleImports(source, imports: TStrings);
var
  str: string;
  prc: TProcess;
begin
  str := getToolName;
  if str.isEmpty then
    exit;
  prc := TProcess.Create(nil);
  try
    prc.Executable := str;
    prc.Parameters.Add('-i');
    prc.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    str := source.Text;
    prc.Input.Write(str[1], str.length);
    prc.CloseInput;
    processOutputToStrings(prc, imports);
    while prc.Running do ;
    {$IFDEF DEBUG}
    tryRaiseFromStdErr(prc);
    {$ENDIF}
  finally
    prc.free;
  end;
end;

procedure getModulesImports(const files: string; results: TStrings);
var
  str: string;
  prc: TProcess;
begin
  str := getToolName;
  if str.isEmpty then
    exit;
  prc := TProcess.Create(nil);
  try
    prc.Executable := str;
    prc.Parameters.Add('-f' + files);
    prc.Parameters.Add('-i');
    prc.Options := [poUsePipes {$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    prc.CloseInput;
    processOutputToStrings(prc, results);
    while prc.Running do ;
    {$IFDEF DEBUG}
    tryRaiseFromStdErr(prc);
    {$ENDIF}
  finally
    prc.free;
  end;
end;

procedure getHalsteadMetrics(source: TStrings; out jsn: TJSONObject);
var
  prc: TProcess;
  prs: TJSONParser;
  jps: TJSONData;
  str: string;
  lst: TStringList;
begin
  str := getToolName;
  if str.isEmpty then
    exit;
  prc := TProcess.Create(nil);
  lst := TStringList.create;
  try
    prc.Executable := str;
    prc.Parameters.Add('-H');
    prc.Options := [poUsePipes {$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    str := source.Text;
    prc.Input.Write(str[1], str.length);
    prc.CloseInput;
    processOutputToStrings(prc, lst);
    prs := TJSONParser.Create(lst.Text, [joIgnoreTrailingComma, joUTF8]);
    jps := prs.Parse;
    if jps.isNotNil and (jps.JSONType = jtObject) then
      jsn := TJSONObject(jps.Clone);
    jps.Free;
    while prc.Running do ;
  finally
    prs.Free;
    prc.Free;
    lst.free;
  end;
end;

procedure getDdocTemplate(source, res: TStrings; caretLine: integer; plusComment: boolean);
var
  prc: TProcess;
  str: string;
begin
  str := getToolName;
  if str.isEmpty then
    exit;
  prc := TProcess.Create(nil);
  try
    prc.Executable := str;
    prc.Parameters.Add('-l' + caretLine.ToString);
    if plusComment then
      prc.Parameters.Add('-o');
    prc.Parameters.Add('-K');
    prc.Options := [poUsePipes {$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    str := source.Text;
    prc.Input.Write(str[1], str.length);
    prc.CloseInput;
    processOutputToStrings(prc, res);
  finally
    prc.Free;
  end;
end;

end.

