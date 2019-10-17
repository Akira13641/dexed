unit u_todolist;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls,
  strutils, Graphics, Dialogs, ExtCtrls, Menus, Buttons, ComCtrls,
  u_widget, process, u_common, u_interfaces, u_synmemo, u_processes,
  u_writableComponent, u_observer, u_sharedres,
  u_dsgncontrols;

type

  TTodoColumn = (filename, line, text, priority, assignee, category, status);
  TTodoColumns = set of TTodoColumn;

  TTodoOptions = class(TWritableLfmTextComponent)
  private
    fAutoRefresh: boolean;
    fSingleClick: boolean;
    fColumns: TTodoColumns;
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
    property singleClickSelect: boolean read fSingleClick write fSingleClick;
    property columns: TTodoColumns read fColumns write fColumns;
  public
    procedure AssignTo(target: TPersistent); override;
    procedure Assign(source: TPersistent); override;
  end;

  TTodoContext = (tcNone, tcProject, tcFile);

  // represents a TODO item
  // warning: the props names must be kept in sync with the values set in the tool.
  TTodoItem = class(TCollectionItem)
  private
    fFile: string;
    fLine: string;
    fText: string;
    fPriority: string;
    fAssignee: string;
    fCategory: string;
    fStatus: string;
  published
    property filename: string read fFile write fFile;
    property line: string read fLine write fLine;
    property text: string read fText write fText;
    property assignee: string read fAssignee write fAssignee;
    property category: string read fCategory write fCategory;
    property status: string read fStatus write fStatus;
    property priority: string read fPriority write fPriority;
  end;

  // encapsulates / makes serializable a collection of TODO item.
  // warning: the class name must be kept in sync with the value set in the tool.
  TTodoItems = class(TComponent)
  private
    fItems: TCollection;
    procedure setItems(value: TCollection);
    function getItem(index: Integer): TTodoItem;
    function getCount: integer;
  published
    // warning, "items" must be kept in sync with...
    property items: TCollection read fItems write setItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // str is the output stream of the tool process.
    procedure loadFromTxtStream(str: TMemoryStream);
    property Count: integer read getCount;
    property item[index: integer]: TTodoItem read getItem; default;
  end;

  { TTodoListWidget }

  TTodoListWidget = class(TDexedWidget, IDocumentObserver, IProjectObserver, IEditableOptions)
    btnGo: TDexedToolButton;
    btnRefresh: TDexedToolButton;
    lstItems: TListView;
    lstfilter: TListFilterEdit;
    mnuAutoRefresh: TMenuItem;
    procedure handleListClick(Sender: TObject);
    procedure mnuAutoRefreshClick(Sender: TObject);
    procedure toolbarResize(Sender: TObject);
  private
    fAutoRefresh: Boolean;
    fSingleClick: Boolean;
    fColumns: TTodoColumns;
    fProj: ICommonProject;
    fDoc: TDexedMemo;
    fToolProc: TDexedProcess;
    fTodos: TTodoItems;
    fMsgs: IMessagesDisplay;
    fOptions: TTodoOptions;
    // IDocumentObserver
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    // IProjectObserver
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    // IEditableOptions
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    // TODOlist things
    function getContext: TTodoContext;
    procedure killToolProcess;
    procedure callToolProcess;
    procedure toolTerminated(Sender: TObject);
    procedure procOutputDbg(Sender: TObject);
    procedure clearTodoList;
    procedure fillTodoList;
    procedure lstItemsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lstItemsCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
    procedure btnRefreshClick(Sender: TObject);
    procedure filterItems(Sender: TObject);
    procedure setSingleClick(value: boolean);
    procedure setAutoRefresh(value: boolean);
    procedure setColumns(value: TTodoColumns);
    procedure refreshVisibleColumns;
  protected
    procedure SetVisible(value: boolean); override;
    procedure setToolBarFlat(value: boolean); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    //
    property singleClickSelect: boolean read fSingleClick write setSingleClick;
    property autoRefresh: boolean read fAutoRefresh write setAutoRefresh;
    property columns: TTodoColumns read fColumns write setColumns;
  end;

implementation

{$R *.lfm}

const
  ToolExeName = 'dastworx' + exeExt;
  OptFname = 'todolist.txt';

{$REGION TTodoItems ------------------------------------------------------------}
constructor TTodoItems.Create(aOwner: TComponent);
begin
  inherited;
  fItems := TCollection.Create(TTodoItem);
end;

destructor TTodoItems.Destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TTodoItems.setItems(value: TCollection);
begin
  fItems.Assign(value);
end;

function TTodoItems.getItem(index: Integer): TTodoItem;
begin
  Result := TTodoItem(fItems.Items[index]);
end;

function TTodoItems.getCount: integer;
begin
  Result := fItems.Count;
end;

procedure TTodoItems.loadFromTxtStream(str: TMemoryStream);
var
  bin: TMemoryStream;
begin
  // empty collection ~ length
  if str.Size < 50 then
    exit;
  //
  try
    bin := TMemoryStream.Create;
    try
      str.Position := 0;
      ObjectTextToBinary(str, bin);
      bin.Position := 0;
      bin.ReadComponent(self);
    finally
      bin.Free;
    end;
  except
    fItems.Clear;
  end;
end;
{$ENDREGIOn}

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TTodoListWidget.Create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;

  Case GetIconScaledSize of
    iss16: AssignPng(lstfilter.Glyph, 'FILTER_CLEAR');
    iss24: AssignPng(lstfilter.Glyph, 'FILTER_CLEAR24');
    iss32: AssignPng(lstfilter.Glyph, 'FILTER_CLEAR32');
  end;
  lstfilter.BorderSpacing.Left := scaleX(58, 96);

  columns:= [TTodoColumn.filename .. TTodoColumn.line];
  fOptions := TTodoOptions.Create(self);
  fOptions.autoRefresh := True;
  fOptions.Name := 'todolistOptions';

  fTodos := TTodoItems.Create(self);
  lstItems.OnDblClick := @handleListClick;
  btnRefresh.OnClick := @btnRefreshClick;
  lstItems.OnColumnClick := @lstItemsColumnClick;
  lstItems.OnCompare := @lstItemsCompare;
  fAutoRefresh := True;
  fSingleClick := False;
  mnuAutoRefresh.Checked := True;
  lstfilter.OnChange := @filterItems;
  btnGo.OnClick := @handleListClick;

  fname := getDocPath + OptFname;
  if fname.fileExists then
  begin
    fOptions.loadFromFile(fname);
    fOptions.AssignTo(self);
  end;

  EntitiesConnector.addObserver(self);
end;

destructor TTodoListWidget.Destroy;
begin
  fOptions.saveToFile(getDocPath + OptFname);
  killToolProcess;
  inherited;
end;

procedure TTodoListWidget.SetVisible(value: boolean);
begin
  inherited;
  if value and fAutoRefresh then
    callToolProcess;
  refreshVisibleColumns;
end;

procedure TTodoListWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  lstfilter.Flat:=value;
end;
{$ENDREGION}

{$REGION IEditableOptions ----------------------------------------------------}
procedure TTodoOptions.AssignTo(target: TPersistent);
var
  widg: TTodoListWidget;
begin
  if target is TTodoListWidget then
  begin
    widg := TTodoListWidget(target);
    widg.singleClickSelect := fSingleClick;
    widg.autoRefresh := fAutoRefresh;
    widg.columns := fColumns;
  end
  else
    inherited;
end;

procedure TTodoOptions.Assign(source: TPersistent);
var
  widg: TTodoListWidget;
begin
  if source is TTodoListWidget then
  begin
    widg := TTodoListWidget(source);
    fSingleClick := widg.singleClickSelect;
    fAutoRefresh := widg.autoRefresh;
    fColumns:=widg.columns;
  end
  else
    inherited;
end;

function TTodoListWidget.optionedWantCategory(): string;
begin
  exit('Todo list');
end;

function TTodoListWidget.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TTodoListWidget.optionedWantContainer: TPersistent;
begin
  fOptions.Assign(self);
  exit(fOptions);
end;

procedure TTodoListWidget.optionedEvent(event: TOptionEditorEvent);
begin
  if event <> oeeAccept then
    exit;
  fOptions.AssignTo(self);
end;

function TTodoListWidget.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TTodoListWidget.docNew(document: TDexedMemo);
begin
end;

procedure TTodoListWidget.docFocused(document: TDexedMemo);
begin
  if document.isNil then
    exit;

  // issue 412 :
  // 1. the file name is in a first time "<new document>"
  // 2. document assigned the fDoc var
  // 3. once the filename loaded it exited on next focused
  //    because diff for filename was not tested.
  if fDoc.isNotNil and (document = fDoc) and (fDoc.fileName = document.fileName) then
    exit;
  fDoc := document;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TTodoListWidget.docChanged(document: TDexedMemo);
begin
end;

procedure TTodoListWidget.docClosing(document: TDexedMemo);
begin
  if fDoc <> document then
    exit;
  fDoc := nil;
  if Visible and fAutoRefresh then
    callToolProcess;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TTodoListWidget.projNew(project: ICommonProject);
begin
  fProj := project;
end;

procedure TTodoListWidget.projChanged(project: ICommonProject);
begin
  if fProj <> project then
    exit;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TTodoListWidget.projClosing(project: ICommonProject);
begin
  if fProj <> project then
    exit;
  fProj := nil;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TTodoListWidget.projFocused(project: ICommonProject);
begin
  if project = fProj then
    exit;
  fProj := project;
  if Visible and fAutoRefresh then
    callToolProcess;
end;

procedure TTodoListWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TTodoListWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION Todo list things ------------------------------------------------------}
function TTodoListWidget.getContext: TTodoContext;
begin
  if (fProj = nil) and fDoc.isNil then
    exit(tcNone);
  if (fProj = nil) and fDoc.isNotNil then
    exit(tcFile);
  if (fProj <> nil) and fDoc.isNil then
    exit(tcProject);
  //
  if fProj.isSource(fDoc.fileName) then
    exit(tcProject)
  else
    exit(tcFile);
end;

procedure TTodoListWidget.killToolProcess;
begin
  if fToolProc.isNil then
    exit;
  //
  fToolProc.Terminate(0);
  fToolProc.Free;
  fToolProc := nil;
end;

procedure TTodoListWidget.callToolProcess;
var
  ctxt: TTodoContext;
  i,j: integer;
  nme: string;
  str: string = '';
begin
  clearTodoList;
  if not exeInSysPath(ToolExeName) then
    exit;
  killToolProcess;

  ctxt := getContext;
  case ctxt of
    tcNone: exit;
    tcProject: if (fProj = nil) or (fProj.sourcesCount = 0) then exit;
    tcFile: if fDoc = nil then exit;
  end;

  fToolProc := TDexedProcess.Create(nil);
  fToolProc.Executable := exeFullName(ToolExeName);
  fToolProc.Options := [poUsePipes];
  fToolProc.ShowWindow := swoHIDE;
  fToolProc.CurrentDirectory := Application.ExeName.extractFileDir;
  fToolProc.OnTerminate := @toolTerminated;
  // files passed to the tool argument
  if (ctxt = tcProject) then
  begin
    i := 0;
    j := fProj.sourcesCount-1;
    for i := 0 to j do
    begin
      nme := fProj.sourceAbsolute(i);
      if not hasDlangSyntax(nme.extractFileExt) then
        continue;
      str += nme;
      if i <> j then
        str += PathSeparator;
    end;
  end
  else str := fDoc.fileName;
  fToolProc.Parameters.Add('-f' + str);
  fToolProc.Parameters.Add('-t');

  fToolProc.Execute;
  fToolProc.CloseInput;
end;

procedure TTodoListWidget.procOutputDbg(Sender: TObject);
var
  str: TStringList;
  msg: string;
  ctxt: TTodoContext;
begin
  getMessageDisplay(fMsgs);
  str := TStringList.Create;
  try
    processOutputToStrings(fToolProc, str);
    ctxt := getContext;
    for msg in str do
      case ctxt of
        tcNone: fMsgs.message(msg, nil, amcMisc, amkAuto);
        tcFile: fMsgs.message(msg, fDoc, amcEdit, amkAuto);
        tcProject: fMsgs.message(msg, fProj, amcProj, amkAuto);
      end;
  finally
    str.Free;
  end;
end;

procedure TTodoListWidget.toolTerminated(Sender: TObject);
begin
  fToolProc.StdoutEx.Position := 0;
  fTodos.loadFromTxtStream(fToolProc.StdoutEx);
  fillTodoList;
  fToolProc.OnTerminate := nil;
end;

procedure TTodoListWidget.clearTodoList;
begin
  lstItems.Clear;
  fTodos.items.Clear;
end;

procedure TTodoListWidget.fillTodoList;
var
  i: integer;
  src: TTodoItem;
  trg: TListItem;
  flt: string;
begin
  lstItems.Clear;
  lstItems.Column[1].Visible := False;
  lstItems.Column[2].Visible := False;
  lstItems.Column[3].Visible := False;
  lstItems.Column[4].Visible := False;
  flt := lstfilter.Text;
  for i := 0 to fTodos.Count - 1 do
  begin
    src := fTodos[i];
    trg := lstItems.Items.Add;
    trg.Data := src;
    trg.Caption := src.Text;
    trg.SubItems.Add(src.category);
    trg.SubItems.Add(src.assignee);
    trg.SubItems.Add(src.status);
    trg.SubItems.Add(src.priority);
    trg.SubItems.Add(shortenPath(src.filename, 25));
    //
    if flt.isNotEmpty then
      if flt <> '(filter)' then
        if not AnsiContainsText(src.Text, flt) then
          if not AnsiContainsText(src.category, flt) then
            if not AnsiContainsText(src.assignee, flt) then
              if not AnsiContainsText(src.status, flt) then
                if not AnsiContainsText(src.priority, flt) then
                begin
                  lstItems.Items.Delete(trg.Index);
                  continue;
                end;
    //
    if src.category.isNotEmpty then
      lstItems.Column[1].Visible := True;
    if src.assignee.isNotEmpty then
      lstItems.Column[2].Visible := True;
    if src.status.isNotEmpty then
      lstItems.Column[3].Visible := True;
    if src.priority.isNotEmpty then
      lstItems.Column[4].Visible := True;
  end;
end;

procedure TTodoListWidget.handleListClick(Sender: TObject);
var
  itm: TTodoItem;
  fname, ln: string;
begin
  if lstItems.Selected.isNil or lstItems.Selected.Data.isNil then
    exit;

  // the collection will be cleared if a file is opened
  // docFocused->callToolProcess->fTodos....clear
  // so line and filename must be copied
  itm := TTodoItem(lstItems.Selected.Data);
  fname := itm.filename;
  ln := itm.line;
  getMultiDocHandler.openDocument(fname);

  if fDoc.isNil then
    exit;

  fDoc.setFocus;
  fDoc.CaretY := StrToInt(ln);
  fDoc.SelectLine;
end;

procedure TTodoListWidget.mnuAutoRefreshClick(Sender: TObject);
begin
  autoRefresh := mnuAutoRefresh.Checked;
  fOptions.autoRefresh := autoRefresh;
end;

procedure TTodoListWidget.toolbarResize(Sender: TObject);
begin
  lstfilter.Width := toolbar.Width - lstfilter.Left - lstfilter.BorderSpacing.Around;
end;

procedure TTodoListWidget.lstItemsColumnClick(Sender: TObject; Column: TListColumn);
var
  curr: TListItem;
begin
  if lstItems.Selected.isNil then
    exit;
  lstItems.BeginUpdate;
  curr := lstItems.Selected;
  //
  if lstItems.SortDirection = sdAscending then
    lstItems.SortDirection := sdDescending
  else
    lstItems.SortDirection := sdAscending;
  lstItems.SortColumn := Column.Index;
  lstItems.Selected := nil;
  lstItems.Selected := curr;
  lstItems.EndUpdate;
end;

procedure TTodoListWidget.lstItemsCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
var
  txt1: string = '';
  txt2: string = '';
  col: Integer;
begin
  col := lstItems.SortColumn;
  if col = 0 then
  begin
    txt1 := item1.Caption;
    txt2 := item2.Caption;
  end
  else
  begin
    col -= 1;
    if col < item1.SubItems.Count then
      txt1 := item1.SubItems[col];
    if col < item2.SubItems.Count then
      txt2 := item2.SubItems[col];
  end;
  Compare := AnsiCompareStr(txt1, txt2);
  if lstItems.SortDirection = sdDescending then
    Compare := -Compare;
end;

procedure TTodoListWidget.btnRefreshClick(Sender: TObject);
begin
  callToolProcess;
end;

procedure TTodoListWidget.filterItems(Sender: TObject);
begin
  fillTodoList;
end;

procedure TTodoListWidget.setSingleClick(value: boolean);
begin
  fSingleClick := value;
  if fSingleClick then
  begin
    lstItems.OnClick := @handleListClick;
    lstItems.OnDblClick := nil;
  end
  else
  begin
    lstItems.OnClick := nil;
    lstItems.OnDblClick := @handleListClick;
  end;
end;

procedure TTodoListWidget.setAutoRefresh(value: boolean);
begin
  fAutoRefresh := value;
  mnuAutoRefresh.Checked := value;
  if fAutoRefresh then
    callToolProcess;
end;

procedure TTodoListWidget.setColumns(value: TTodoColumns);
begin
  fColumns := value;
  refreshVisibleColumns;
end;

procedure TTodoListWidget.refreshVisibleColumns;
begin
  if lstItems.isNil then exit;
  if lstItems.Columns.isNil then exit;
  if lstItems.ColumnCount <> 6 then exit;
  //
  lstItems.Column[1].Visible := TTodoColumn.category in fColumns ;
  lstItems.Column[2].Visible := TTodoColumn.assignee in fColumns ;
  lstItems.Column[3].Visible := TTodoColumn.status in fColumns ;
  lstItems.Column[4].Visible := TTodoColumn.priority in fColumns ;
  lstItems.Column[5].Visible := TTodoColumn.filename in fColumns ;
end;
{$ENDREGION}

end.
