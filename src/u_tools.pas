unit u_tools;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, LazFileUtils, process, menus, u_processes, controls,
  u_common, u_writableComponent, u_interfaces, u_observer, u_inspectors,
  u_synmemo, u_dialogs;

type

  TToolItems = class;

  TPipeInputKind = (pikNone, pikEditor, pikSelection, pikLine);

  TToolItem = class(TCollectionItem)
  private
    fToolItems: TToolItems;
    fTerminatedFlag: boolean;
    fNextToolAlias: string;
    fProcess: TDexedProcess;
    fExecutable: TFilename;
    fWorkingDir: TPathname;
    fShowWin: TShowWindowOptions;
    fOpts: TProcessOptions;
    fParameters: TStringList;
    fToolAlias: string;
    fQueryParams: boolean;
    fClearMessages: boolean;
    fOutputToNext: boolean;
    fShortcut: TShortcut;
    fMsgs: IMessagesDisplay;
    fSymStringExpander: ISymStringExpander;
    fPipeInputKind: TPipeInputKind;
    fAskConfirmation: boolean;
    procedure setParameters(value: TStringList);
    procedure processOutput(sender: TObject);
    procedure setToolAlias(value: string);
  published
    property toolAlias: string read fToolAlias write setToolAlias;
    property options: TProcessOptions read fOpts write fOpts;
    property executable: TFilename read fExecutable write fExecutable;
    property workingDirectory: TPathname read fWorkingDir write fWorkingDir;
    property parameters: TStringList read fParameters write setParameters;
    property showWindows: TShowWindowOptions read fShowWin write fShowWin;
    property queryParameters: boolean read fQueryParams write fQueryParams;
    property clearMessages: boolean read fClearMessages write fClearMessages;
    property shortcut: TShortcut read fShortcut write fShortcut;
    property nextToolAlias: string read fNextToolAlias write fNextToolAlias;
    property outputToNext: boolean read fOutputToNext write fOutputToNext;
    property pipeInputKind: TPipeInputKind read fPipeInputKind write fPipeInputKind;
    property askConfirmation: boolean read fAskConfirmation write fAskConfirmation;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
    procedure assign(Source: TPersistent); override;
    procedure execute(previous: TToolItem);
    property process: TDexedProcess read fProcess;
  end;

  TToolItems = class(TCollection)
  public
    function findTool(const value: string): TToolItem;
  end;

  TTools = class(TWritableLfmTextComponent, IEditableShortCut, IDocumentObserver)
  private
    fTools: TToolItems;
    fShctCount: Integer;
    fDoc: TDexedMemo;
    fMenu: TMenuItem;
    fReadOnly: boolean;
    function getTool(index: Integer): TToolItem;
    procedure setTools(value: TToolItems);
    //
    procedure executeToolFromMenu(sender: TObject);
    //
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    //
    function scedWantFirst: boolean;
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
    procedure scedSendDone;
  published
    property tools: TToolItems read fTools write setTools;
    property readOnly: boolean read fReadOnly write fReadOnly;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure updateMenu;
    function addTool: TToolItem;
    procedure executeTool(tool: TToolItem); overload;
    procedure executeTool(index: Integer); overload;
    property tool[index: integer]: TToolItem read getTool; default;
  end;

//TODO-crefactor: either set the tools as a service of merge the tools collection& tool editor in a single unit.

var
  CustomTools: TTools;

implementation

uses
  dialogs;

const
  toolsFname = 'tools.txt';

{$REGION TToolItem -----------------------------------------------------------}
function TToolItems.findTool(const value: string): TToolItem;
var
  item: TCollectionItem;
begin
  for item in self do
    if TToolItem(item).toolAlias = value then
      exit(TToolItem(item));
  exit(nil);
end;

constructor TToolItem.create(ACollection: TCollection);
begin
  inherited;
  // TODO-cbugfix: tools are init before symstring, even when order of 'uses' is modified (lpr)
  fSymStringExpander:= getSymStringExpander;
  fMsgs       := getMessageDisplay;
  fToolItems  := TToolItems(ACollection);
  fToolAlias  := format('<tool %d>', [ID]);
  fParameters := TStringList.create;
end;

destructor TToolItem.destroy;
begin
  fParameters.Free;
  u_processes.killProcess(fProcess);
  inherited;
end;

procedure TToolItem.assign(Source: TPersistent);
var
  tool: TToolItem;
begin
  // only used to clone a tool: so don't copy everything.
  if Source is TToolItem then
  begin
    tool := TToolItem(Source);
    toolAlias         := tool.toolAlias;
    queryParameters   := tool.queryParameters;
    clearMessages     := tool.clearMessages;
    options           := tool.options;
    executable        := tool.executable;
    workingDirectory  := tool.workingDirectory;
    showWindows       := tool.showWindows;
    pipeInputKind     := tool.pipeInputKind;
    askConfirmation   := tool.askConfirmation;
    parameters.Assign(tool.parameters);
  end
  else inherited;
end;

procedure TToolItem.setParameters(value: TStringList);
begin
  fParameters.Assign(value);
end;

procedure TToolItem.setToolAlias(value: string);
var
  i: integer = 0;
begin
  while fToolItems.findTool(value).isNotNil do
  begin
    value += intToStr(i);
    i += 1;
  end;
  fToolAlias := value;
end;

procedure TToolItem.execute(previous: TToolItem);
var
  arg: string;
  prm: string;
  inp: string;
  old: string;
const
  confSpec = 'Are you sure you want to execute the "%s" tool ?';
begin
  u_processes.killProcess(fProcess);
  fTerminatedFlag := false;

  if fMsgs = nil then
    fMsgs := getMessageDisplay;
  if fClearMessages then
    fMsgs.clearByContext(amcMisc);
  if fSymStringExpander = nil then
    fSymStringExpander:= getSymStringExpander;

  if askConfirmation and (dlgOkCancel(format(confSpec, [toolAlias])) <> mrOk) then
    exit;

  old := GetCurrentDirUTF8;
  fProcess := TDexedProcess.Create(nil);
  fProcess.OnReadData:= @processOutput;
  fProcess.OnTerminate:= @processOutput;
  fProcess.Options := fOpts;
  fProcess.Executable := exeFullName(fSymStringExpander.expand(fExecutable));
  fProcess.ShowWindow := fShowWin;
  fProcess.CurrentDirectory := fSymStringExpander.expand(fWorkingDir);
  fProcess.XTermProgram:=consoleProgram;
  for prm in fParameters do if not isStringDisabled(prm) then
    fProcess.Parameters.AddText(fSymStringExpander.expand(prm));
  if fQueryParams then
  begin
    prm := '';
    if InputQuery('Parameters', '', prm) then
    begin
      prm := fSymStringExpander.expand(prm);
      arg := StringReplace(fParameters.Text, '<$1>', prm, [rfReplaceAll]);
      if prm.isNotEmpty and (arg = fParameters.Text) then
        fProcess.Parameters.AddText(prm)
      else
        fProcess.Parameters.Text := arg;
    end;
  end;
  ensureNoPipeIfWait(fProcess);
  //
  if fProcess.Executable.fileExists then
  begin
    fProcess.Execute;
    if previous.isNotNil and previous.outputToNext
      and (poUsePipes in previous.Options) and (poUsePipes in Options) then
    begin
      setLength(inp, previous.process.StdoutEx.Size);
      previous.process.StdoutEx.Position:=0;
      previous.process.StdoutEx.Read(inp[1], inp.length);
      fProcess.Input.Write(inp[1], inp.length);
      fProcess.CloseInput;
    end;
  end;
  //
  SetCurrentDirUTF8(old);
end;

procedure TToolItem.processOutput(sender: TObject);
var
  lst: TStringList;
  str: string;
  nxt: TToolItem;
begin
  if ((not fOutputToNext) or fNextToolAlias.isEmpty) and (poUsePipes in options) then
  begin
    lst := TStringList.Create;
    try
      fProcess.getFullLines(lst);
      for str in lst do
        fMsgs.message(str, nil, amcMisc, amkAuto);
    finally
      lst.Free;
    end;
  end;
  if (not fProcess.Running) then
  begin
    if fTerminatedFlag then
      exit;
    fTerminatedFlag := true;
    if fProcess.ExitStatus <> 0 then
    begin
      fMsgs.message(format('error: the tool (%s) has returned the status %s',
        [fProcess.Executable, prettyReturnStatus(fProcess)]), nil, amcMisc, amkErr);
      u_processes.killProcess(fProcess);
      exit;
    end
    else
    begin
      fMsgs.message(format('the tool (%s) has finished normally',
        [fProcess.Executable]), nil, amcMisc, amkBub);
    end;
    if fNextToolAlias.isNotEmpty then
    begin
      nxt := fToolItems.findTool(fNextToolAlias);
      if nxt.isNotNil then
        nxt.execute(self);
    end;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TTools.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fTools := TToolItems.Create(TToolItem);
  fname := getDocPath + toolsFname;
  if fname.fileExists then
    loadFromFile(fname);

  EntitiesConnector.addObserver(self);
end;

destructor TTools.destroy;
begin
  EntitiesConnector.removeObserver(self);

  ForceDirectoriesUTF8(getDocPath);
  saveToFile(getDocPath + toolsFname);
  fTools.Free;
  inherited;
end;
{$ENDREGION}

{$REGION IMainMenuProvider ---------------------------------------------------}
procedure TTools.updateMenu;
var
  mnu: IMainMenu = nil;
  itm: TMenuItem;
  colitm: TToolItem;
  i: integer;
begin
  if fMenu.isNil then
  begin
    mnu := getMainMenu;
    if not assigned(mnu) then
      exit;
    fMenu := mnu.mnuAdd;
    fMenu.Caption:='Custom tools';
  end;
  fMenu.Clear;
  for i := 0 to tools.Count-1 do
  begin
    colitm := tool[i];
    itm := TMenuItem.Create(fMenu);
    itm.ShortCut:= colitm.shortcut;
    itm.Caption := colitm.toolAlias;
    itm.tag := ptrInt(colitm);
    itm.onClick := @executeToolFromMenu;
    fMenu.add(itm);
  end;
end;

procedure TTools.executeToolFromMenu(sender: TObject);
begin
  executeTool(TToolItem(TMenuItem(sender).tag));
end;
{$ENDREGION}

{$REGION IEditableShortCut ---------------------------------------------------}
function TTools.scedWantFirst: boolean;
begin
  result := fTools.Count > 0;
  fShctCount := 0;
end;

function TTools.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
begin
  category  := 'Tools';
  identifier:= tool[fShctCount].toolAlias;
  aShortcut := tool[fShctCount].shortcut;
  //
  fShctCount += 1;
  result := fShctCount < fTools.Count;
end;

procedure TTools.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
var
  i: Integer;
begin
 if category <> 'Tools' then exit;
 //
 for i := 0 to tools.Count-1 do if tool[i].toolAlias = identifier then
 begin
   tool[i].shortcut := aShortcut;
   break;
 end;
end;

procedure TTools.scedSendDone;
begin
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TTools.docNew(document: TDexedMemo);
begin
  fDoc := document;
end;

procedure TTools.docFocused(document: TDexedMemo);
begin
  fDoc := document;
end;

procedure TTools.docChanged(document: TDexedMemo);
begin
end;

procedure TTools.docClosing(document: TDexedMemo);
begin
  if fDoc <> document then exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION Tools things ----------------------------------------------------------}
procedure TTools.setTools(value: TToolItems);
begin
  fTools.Assign(value);
end;

function TTools.getTool(index: Integer): TToolItem;
begin
  result := TToolItem(fTools.Items[index]);
end;

function TTools.addTool: TToolItem;
begin
  result := TToolItem(fTools.Add);
end;

procedure TTools.executeTool(tool: TToolItem);
var
  txt: string = '';
begin
  if tool.isNil then exit;
  //
  tool.execute(nil);
  if (tool.pipeInputKind <> pikNone) and fDoc.isNotNil
    and (poUsePipes in tool.options) and tool.fProcess.Input.isNotNil then
  begin
    case tool.pipeInputKind of
      pikEditor:    txt := fDoc.Text;
      pikLine:      txt := fDoc.LineText;
      pikSelection: txt := fDoc.SelText;
      else ;
    end;
    if txt.isNotEmpty then
      tool.fProcess.Input.Write(txt[1], txt.length);
    tool.fProcess.CloseInput;
  end;
end;

procedure TTools.executeTool(index: Integer);
begin
  if index < 0 then exit;
  if index > fTools.Count-1 then exit;
  //
  executeTool(tool[index]);
end;
{$ENDREGION}

initialization
  CustomTools := TTools.create(nil);
finalization
  CustomTools.Free;
end.
