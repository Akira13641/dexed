unit u_dcd;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, process, forms, strutils, LazFileUtils,
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  u_common, u_writableComponent, u_interfaces, u_observer, u_synmemo,
  u_stringrange, u_projutils, u_semver;

type

  TIntOpenArray = array of integer;

  (**
   * Wrap the dcd-server and dcd-client processes.
   *
   * Projects folders are automatically imported: IProjectObserver.
   * Completion, hints and declaration finder automatically work on the current
   *   document: IDocumentObserver.
   *)
  TDcdWrapper = class(TWritableLfmTextComponent, IProjectObserver, IDocumentObserver)
  private
    fTempLines: TStringList;
    fInputSource: string;
    fImportCache: TStringHashSet;
    fPortNum: Word;
    fCurrentSessionPortNum: Word;
    fServerWasRunning: boolean;
    fAvailable: boolean;
    fServerListening: boolean;
    fDoc: TDexedMemo;
    fProj: ICommonProject;
    fPortAsProcParam: string;
    fVersion: TSemVer;
    fCanRemove: boolean;
    procedure killServer;
    procedure terminateClient; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure waitClient; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure updateServerlistening; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure writeSourceToInput; {$IFNDEF DEBUG}inline;{$ENDIF}
    function checkDcdSocket: boolean;
    function getIfLaunched: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure tryAddTcpParams; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure updateImportPathsFromProject;
    //
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    //
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
  published
    property port: word read fPortNum write fPortNum default 0;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    class procedure relaunch; static;
    class function noDcdPassedAsArg: boolean; static;
    //
    procedure addImportFolders(const folders: TStrings);
    procedure addImportFolder(const folder: string);
    procedure remImportFolder(const folder: string);
    procedure remImportFolders(const folders: TStrings);
    procedure getComplAtCursor(list: TStringList);
    procedure getCallTip(out tips: string);
    procedure getDdocFromCursor(out comment: string);
    procedure getDeclFromCursor(out fname: string; out position: Integer);
    procedure getLocalSymbolUsageFromCursor(var locs: TIntOpenArray);
    //
    property available: boolean read fAvailable;
    property launchedByCe: boolean read getIfLaunched;
  end;

  function DCDWrapper: TDcdWrapper;

implementation

var
  fDcdWrapper: TDcdWrapper = nil;
  fClient: TProcess = nil;
  fServer: TProcess = nil;

const
  clientName = 'dcd-client' + exeExt;
  serverName = 'dcd-server' + exeExt;
  optsname = 'dcdoptions.txt';


{$REGION Standard Comp/Obj------------------------------------------------------}

procedure TDcdWrapper.updateServerlistening;
begin
  fServerListening := AppIsRunning(serverName);
end;

constructor TDcdWrapper.create(aOwner: TComponent);
var
  fname: string;
  i: integer = 0;
  r: TSemVer;
begin
  inherited;

  fVersion.init('v0.0.0');
  fname := getDocPath + optsname;
  if fname.fileExists then
    loadFromFile(fname);
  fCurrentSessionPortNum := fPortNum;
  fPortAsProcParam := '-p' + intToStr(fCurrentSessionPortNum);

  fAvailable := exeInSysPath(clientName) and exeInSysPath(serverName)
    and not noDcdPassedAsArg();
  if not fAvailable then
    exit;

  fClient := TProcess.Create(self);
  fClient.Executable := exeFullName(clientName);
  fClient.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
  fClient.ShowWindow := swoHIDE;

  fServerWasRunning := fServer.IsNotNil() and fServer.Running;
  if not fServerWasRunning then
  begin
    if fServer = nil then
      fServer := TProcess.Create(self);
    fServer.Executable := exeFullName(serverName);
    fServer.Options := [{$IFDEF WINDOWS} poNewConsole{$ENDIF}];
    {$IFNDEF DEBUG}
    fServer.ShowWindow := swoHIDE;
    {$ENDIF}
    if fCurrentSessionPortNum <> 0 then
      fServer.Parameters.AddStrings(['--tcp', fPortAsProcParam]);
  end;
  fTempLines := TStringList.Create;
  fImportCache := TStringHashSet.Create;

  fClient.Parameters.Add('--version');
  fClient.Execute;
  processOutputToStrings(fClient, fTempLines);
  while fClient.Running do ;
  fVersion.init(fTempLines.strictText);
  r.major := 0;
  r.minor := 9;
  r.patch := 10;
  fCanRemove := fVersion > r;

  if fServer.isNotNil then
  begin
    fServer.Execute;
    while true do
    begin
      if (i = 10) or checkDcdSocket then
        break;
      i += 1;
    end;
  end;
  updateServerlistening;

  EntitiesConnector.addObserver(self);
end;

class function TDcdWrapper.noDcdPassedAsArg(): boolean;
var
  i: integer;
begin
  result := false;
  for i := 1 to argc-1 do
    if ParamStr(i) = '-nodcd' then
  begin
    result :=true;
    break;
  end;
end;

class procedure TDcdWrapper.relaunch;
begin
  fDcdWrapper.Free;
  fDcdWrapper := TDcdWrapper.create(nil);
end;

function TDcdWrapper.getIfLaunched: boolean;
begin
  result := fServer.isNotNil;
end;

destructor TDcdWrapper.destroy;
var
  i: integer = 0;
begin
  saveToFile(getDocPath + optsname);
  EntitiesConnector.removeObserver(self);
  fImportCache.Free;
  if fTempLines.isNotNil then
    fTempLines.Free;
  if fServer.isNotNil then
  begin
    if not fServerWasRunning then
    begin
      killServer;
      while true do
      begin
        if (not checkDcdSocket) or (i = 10) then
          break;
        i +=1;
      end;
    end;
    fServer.Terminate(0);
    fServer.Free;
  end;
  fClient.Free;
  inherited;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TDcdWrapper.updateImportPathsFromProject;
var
  i: Integer;
  fold: string;
  folds: TStringList;
begin
  if not assigned(fProj) then
    exit;

  folds := TStringList.Create;
  try
    fold := u_projutils.projectSourcePath(fProj);
    if fold.dirExists then
      folds.Add(fold);
  	for i := 0 to fProj.importsPathCount-1 do
  	begin
    	fold := fProj.importPath(i);
      if fold.dirExists and (folds.IndexOf(fold) = -1) then
        folds.Add(fold);
    end;
    addImportFolders(folds);
  finally
    folds.Free;
  end;
end;

procedure TDcdWrapper.projNew(project: ICommonProject);
begin
  fProj := project;
end;

procedure TDcdWrapper.projChanged(project: ICommonProject);
begin
  if (fProj = nil) or (fProj <> project) then
    exit;
  updateImportPathsFromProject();
end;

procedure TDcdWrapper.projClosing(project: ICommonProject);
begin
  if fProj <> project then
    exit;
  fProj := nil;
end;

procedure TDcdWrapper.projFocused(project: ICommonProject);
begin
  fProj := project;
  updateImportPathsFromProject();
end;

procedure TDcdWrapper.projCompiling(project: ICommonProject);
begin
end;

procedure TDcdWrapper.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TDcdWrapper.docNew(document: TDexedMemo);
begin
  fDoc := document;
end;

procedure TDcdWrapper.docFocused(document: TDexedMemo);
begin
  fDoc := document;
end;

procedure TDcdWrapper.docChanged(document: TDexedMemo);
begin
  if fDoc <> document then exit;
end;

procedure TDcdWrapper.docClosing(document: TDexedMemo);
begin
  if fDoc <> document then exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION DCD things ------------------------------------------------------------}
procedure TDcdWrapper.terminateClient;
begin
  if fClient.Running then
    fClient.Terminate(0);
end;

function TDcdWrapper.checkDcdSocket: boolean;
var
  str: string;
  prt: word = 9166;
  prc: TProcess;
  lst: TStringList;
begin
  sleep(100);
  // nix/osx: the file might exists from a previous session that crashed
  // however the 100 ms might be enough for DCD initialization
  if fCurrentSessionPortNum = 0 then
  begin
    {$IFDEF LINUX}
    str := sysutils.GetEnvironmentVariable('XDG_RUNTIME_DIR');
    if (str + DirectorySeparator + 'dcd.socket').fileExists then
      exit(true);
    str := sysutils.GetEnvironmentVariable('UID');
    if ('/tmp/dcd-' + str + '.socket').fileExists then
      exit(true);
    {$ENDIF}
    {$IFDEF DARWIN}
    str := sysutils.GetEnvironmentVariable('UID');
    if ('/var/tmp/dcd-' + str + '.socket').fileExists then
      exit(true);
    {$ENDIF}
  end;
  result := false;
  if port <> 0 then
    prt := port;
  prc := TProcess.Create(nil);
  try
    prc.Options:= [poUsePipes, poNoConsole];
    prc.Executable := 'netstat';
    prc.Parameters.Add('-o');
    prc.Parameters.Add('-a');
    prc.Parameters.Add('-n');
    prc.Execute;
    lst := TStringList.Create;
    try
      processOutputToStrings(prc,lst);
      while prc.Running do ;
      for str in lst do
      if AnsiContainsText(str, '127.0.0.1:' + intToStr(prt))
      and AnsiContainsText(str, 'TCP')
      and AnsiContainsText(str, 'LISTEN') then
      begin
        result := true;
        break;
      end;
    finally
      lst.Free;
    end;
  finally
    prc.Free;
  end;
  exit(result);
end;

procedure TDcdWrapper.tryAddTcpParams;
begin
  if fCurrentSessionPortNum <> 0 then
  begin
    fClient.Parameters.Add('--tcp');
    fClient.Parameters.Add(fPortAsProcParam);
  end;
end;

procedure TDcdWrapper.killServer;
begin
  if not fAvailable or not fServerListening then
    exit;

  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('--shutdown');
  fClient.Execute;
  while fServer.Running or fClient.Running do
    sleep(50);
end;

procedure TDcdWrapper.waitClient;
begin
  while fClient.Running do
    sleep(5);
end;

procedure TDcdWrapper.writeSourceToInput;
begin
  fInputSource := fDoc.Text;
  fClient.Input.Write(fInputSource[1], fInputSource.length);
  fClient.CloseInput;
end;

procedure TDcdWrapper.addImportFolder(const folder: string);
begin
  if not fAvailable or not fServerListening or fImportCache.contains(folder) then
    exit;

  fImportCache.Add(folder);
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-I' + folder);
  fClient.Execute;
  while fClient.Running do ;
end;

procedure TDcdWrapper.addImportFolders(const folders: TStrings);
var
  i: string;
  c: integer;
begin
  if not fAvailable or not fServerListening then
    exit;

  fClient.Parameters.Clear;
  tryAddTcpParams;
  c := folders.Count;
  for i in folders do
  begin
    if fImportCache.contains(i) then
      continue;
    fImportCache.Add(i);
    fClient.Parameters.Add('-I' + i);
    dec(c);
  end;
  if c <> folders.Count then
  begin
    fClient.Execute;
    while fClient.Running do ;
  end;
end;

procedure TDcdWrapper.remImportFolder(const folder: string);
begin
  if not fCanRemove then
    exit;
  if not fAvailable or not fServerListening or not fImportCache.contains(folder) then
    exit;

  fImportCache.Remove(folder);
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-R' + folder);
  fClient.Execute;
  while fClient.Running do ;
end;

procedure TDcdWrapper.remImportFolders(const folders: TStrings);
var
  i: string;
  c: integer;
begin
  if not fCanRemove then
    exit;
  if not fAvailable or not fServerListening then
    exit;

  fClient.Parameters.Clear;
  tryAddTcpParams;
  c := folders.Count;
  for i in folders do
  begin
    if not fImportCache.contains(i) then
      continue;
    fImportCache.Remove(i);
    fClient.Parameters.Add('-R' + i);
    dec(c);
  end;
  if c <> folders.Count then
  begin
    fClient.Execute;
    while fClient.Running do ;
  end;
end;

procedure TDcdWrapper.getCallTip(out tips: string);
begin
  if not fAvailable or not fServerListening or fDoc.isNil then
    exit;

  terminateClient;
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Execute;
  writeSourceToInput;

  fTempLines.Clear;
  processOutputToStrings(fClient, fTempLines);
  while fClient.Running do ;
  if fTempLines.Count = 0 then
  begin
    updateServerlistening;
    exit;
  end;
  if not (fTempLines[0] = 'calltips') then exit;

  fTempLines.Delete(0);
  tips := fTempLines.Text;
  {$IFDEF WINDOWS}
  tips := tips[1..tips.length-2];
  {$ELSE}
  tips := tips[1..tips.length-1];
  {$ENDIF}
end;

function compareknd(List: TStringList; Index1, Index2: Integer): Integer;
var
  k1, k2: byte;
begin
  k1 := Byte(PtrUint(List.Objects[Index1]));
  k2 := Byte(PtrUint(List.Objects[Index2]));
  if k1 > k2 then
    result := 1
  else if k1 < k2 then
    result := -1
  else
    result := CompareStr(list[Index1], list[Index2]);
end;

procedure TDcdWrapper.getComplAtCursor(list: TStringList);
var
  i: Integer;
  kind: Char;
  item: string;
  kindObj: TObject = nil;
begin
  if not fAvailable or not fServerListening or fDoc.isNil then
    exit;

  terminateClient;
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Execute;
  writeSourceToInput;

  fTempLines.Clear;
  processOutputToStrings(fClient, fTempLines);
  while fClient.Running do ;
  if fTempLines.Count = 0 then
  begin
    updateServerlistening;
    exit;
  end;
  if not (fTempLines[0] = 'identifiers') then exit;

  list.Clear;
  for i := 1 to fTempLines.Count-1 do
  begin
    item := fTempLines[i];
    kind := item[item.length];
    setLength(item, item.length-2);
    case kind of
      'c': kindObj := TObject(PtrUint(dckClass));
      'i': kindObj := TObject(PtrUint(dckInterface));
      's': kindObj := TObject(PtrUint(dckStruct));
      'u': kindObj := TObject(PtrUint(dckUnion));
      'v': kindObj := TObject(PtrUint(dckVariable));
      'm': kindObj := TObject(PtrUint(dckMember));
      'k': kindObj := TObject(PtrUint(dckReserved));
      'f': kindObj := TObject(PtrUint(dckFunction));
      'g': kindObj := TObject(PtrUint(dckEnum));
      'e': kindObj := TObject(PtrUint(dckEnum_member));
      'P': kindObj := TObject(PtrUint(dckPackage));
      'M': kindObj := TObject(PtrUint(dckModule));
      'a': kindObj := TObject(PtrUint(dckArray));
      'A': kindObj := TObject(PtrUint(dckAA));
      'l': kindObj := TObject(PtrUint(dckAlias));
      't': kindObj := TObject(PtrUint(dckTemplate));
      'T': kindObj := TObject(PtrUint(dckMixin));
      'h': kindObj := TObject(PtrUint(dckMember));
      'p': kindObj := TObject(PtrUint(dckMember));
      // internal DCD stuff, Should not to happen...report bug if it does.
      '*', '?': continue;
    end;
    list.AddObject(item, kindObj);
  end;
  //list.CustomSort(@compareknd);
end;

procedure TDcdWrapper.getDdocFromCursor(out comment: string);
var
  i: Integer;
  len: Integer;
  str: string;
begin
  if not fAvailable or not fServerListening or fDoc.isNil then
    exit;

  i := fDoc.MouseBytePosition;
  if i = 0 then exit;

  terminateClient;
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-d');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(i - 1));
  fClient.Execute;
  writeSourceToInput;

  comment := '';
  fTempLines.Clear;
  processOutputToStrings(fClient, fTempLines);
  while fClient.Running do ;
  len := fTempLines.Count-1;
  if len = -1 then
    updateServerlistening;
  for i := 0 to len do
  begin
    str := fTempLines[i];
    with TStringRange.create(str) do while not empty do
    begin
      comment += takeUntil('\').yield;
      if startsWith('\\') then
      begin
        comment += '\';
        popFrontN(2);
      end
      else if startsWith('\n') then
      begin
        comment += LineEnding;
        popFrontN(2);
      end
    end;
    if i <> len then
      comment += LineEnding + LineEnding;
  end;
end;

procedure TDcdWrapper.getDeclFromCursor(out fname: string; out position: Integer);
var
   i: Integer;
   str, loc: string;
begin
  if not fAvailable or not fServerListening or fDoc.isNil then
    exit;

  terminateClient;
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-l');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart));
  fClient.Execute;
  writeSourceToInput;

  fTempLines.Clear;
  processOutputToStrings(fClient, fTempLines);
  while fClient.Running do ;
  if fTempLines.Count > 0 then
  begin
    str := fTempLines[0];
    if str.isNotEmpty then
    begin
      i := Pos(#9, str);
      if i = -1 then
        exit;
      loc := str[i+1..str.length];
      fname := TrimFilename(str[1..i-1]);
      loc := ReplaceStr(loc, LineEnding, '');
      position := strToIntDef(loc, -1);
    end;
  end
  else updateServerlistening;
end;

procedure TDcdWrapper.getLocalSymbolUsageFromCursor(var locs: TIntOpenArray);
var
   i: Integer;
   str: string;
begin
  if not fAvailable or not fServerListening or fDoc.isNil then
    exit;

  terminateClient;
  fClient.Parameters.Clear;
  tryAddTcpParams;
  fClient.Parameters.Add('-u');
  fClient.Parameters.Add('-c');
  fClient.Parameters.Add(intToStr(fDoc.SelStart - 1));
  fClient.Execute;
  writeSourceToInput;

  setLength(locs, 0);
  fTempLines.Clear;
  processOutputToStrings(fClient, fTempLines);
  while fClient.Running do ;
  if fTempLines.Count < 2 then
    exit;
  str := fTempLines[0];
  // symbol is not in current module, too complex for now
  if str[1..5] <> 'stdin' then
    exit;

  setLength(locs, fTempLines.count-1);
  for i:= 1 to fTempLines.count-1 do
    locs[i-1] := StrToIntDef(fTempLines[i], -1);
end;
{$ENDREGION}

function DCDWrapper: TDcdWrapper;
begin
  if fDcdWrapper.isNil then
    fDcdWrapper := TDcdWrapper.create(nil);
  exit(fDcdWrapper);
end;

finalization
  DcdWrapper.Free;
end.
