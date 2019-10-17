unit u_search;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, actnList, Buttons, SynEdit, SynEditSearch, SynEditTypes,
  u_common, u_mru, u_widget, u_synmemo, u_interfaces, u_observer,
  u_writableComponent, u_dialogs, u_sharedres, u_dsgncontrols,
  SynEditTextBuffer;

type

  // TSearchWidget persistents settings
  TSearchOptions = class(TWritableLfmTextComponent)
  private
    fPrompt: boolean;
    fFromCur: boolean;
    fRegex: boolean;
    fCaseSens:boolean;
    fBackWard: boolean;
    fWholeWord: boolean;
    fMrSearches: TStringList;
    fMrReplacements: TStringList;
    procedure cleanIvnalidHistoryItems;
    procedure setMrSearches(value: TStringList);
    procedure setMrReplacements(value: TStringList);
  protected
    procedure afterLoad; override;
    procedure beforeSave; override;
  published
    property prompt: boolean read fPrompt write fPrompt;
    property fromCursor: boolean read fFromCur write fFromCur;
    property regex: boolean read fRegex write fRegex;
    property caseSensistive: boolean read fCaseSens write fCaseSens;
    property backward: boolean read fBackWard write fBackWard;
    property wholeWord: boolean read fWholeWord write fWholeWord;
    property recentSearches: TStringList read fMrSearches write setMrSearches;
    property recentReplacements: TStringList read fMrReplacements write setMrReplacements;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure assignTo(target: TPersistent); override;
  end;

  TSearchScope = (scDoc, scProj, scOpened);

  TSearchWidget = class(TDexedWidget, IDocumentObserver, IProjectObserver)
    btnAllScope: TBitBtn;
    btnFind: TBitBtn;
    btnFindAll: TBitBtn;
    btnReplace: TBitBtn;
    btnReplaceAll: TBitBtn;
    cbToFind: TComboBox;
    cbReplaceWth: TComboBox;
    chkEnableRep: TCheckBox;
    chkPrompt: TCheckBox;
    chkRegex: TCheckBox;
    chkWWord: TCheckBox;
    chkBack: TCheckBox;
    chkFromCur: TCheckBox;
    chkCaseSens: TCheckBox;
    FlowPanel1: TFlowPanel;
    grpOpts: TGroupBox;
    imgList: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnAllScopeClick(Sender: TObject);
    procedure cbReplaceWthChange(Sender: TObject);
    procedure cbReplaceWthKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbToFindChange(Sender: TObject);
    procedure cbToFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure chkEnableRepChange(Sender: TObject);
  private
    fDoc: TDexedMemo;
    fToFind: string;
    fReplaceWth: string;
    fActReplaceNext: TAction;
    fActFindNext: TAction;
    fActReplaceAll: TAction;
    fActFindAll: TAction;
    fSearchMru, fReplaceMru: TMruList;
    fCancelAll: boolean;
    fHasSearched: boolean;
    fHasRestarted: boolean;
    fProj: ICommonProject;
    fFindScope: TSearchScope;
    function getOptions: TSynSearchOptions;
    procedure actReplaceAllExecute(sender: TObject);
    procedure replaceEvent(Sender: TObject; const ASearch, AReplace:
      string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);

    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);

    procedure docNew(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);

    function findAll(const filename: string; lines: TStrings;
        showNoResult: boolean = true): integer;
  protected
    procedure updateImperative; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure actFindNextExecute(sender: TObject);
    procedure actReplaceNextExecute(sender: TObject);
    procedure actFindAllExecute(sender: TObject);
  end;

implementation
{$R *.lfm}

const
  OptsFname = 'search.txt';
  FindScopeStr: array[TSearchScope] of string = ('Document', 'Project', 'Opened docs');

{$REGION TSearchOptions ------------------------------------------------------}
constructor TSearchOptions.create(aOwner: TComponent);
begin
  inherited;
  fMrReplacements := TStringList.Create;
  fMrSearches := TStringList.Create;
end;

destructor TSearchOptions.destroy;
begin
  fMrSearches.Free;
  fMrReplacements.Free;
  inherited;
end;

procedure TSearchOptions.assign(source: TPersistent);
var
  widg: TSearchWidget;
begin
  if source is TSearchWidget then
  begin
    widg := TSearchWidget(source);
    fMrSearches.Assign(widg.fSearchMru);
    fMrReplacements.Assign(widg.fReplaceMru);
    fPrompt     := widg.chkPrompt.Checked;
    fBackWard   := widg.chkBack.Checked;
    fCaseSens   := widg.chkCaseSens.Checked;
    fRegex      := widg.chkRegex.Checked;
    fFromCur    := widg.chkFromCur.Checked;
    fWholeWord  := widg.chkWWord.Checked;
  end
  else inherited;
end;

procedure TSearchOptions.assignTo(target: TPersistent);
var
  widg: TSearchWidget;
begin
  if target is TSearchWidget then
  begin
    widg := TSearchWidget(target);
    widg.cbToFind.Items.Assign(fMrSearches);
    widg.fSearchMru.Assign(fMrSearches);
    widg.cbReplaceWth.Items.Assign(fMrReplacements);
    widg.fReplaceMru.Assign(fMrReplacements);
    widg.chkPrompt.Checked  := fPrompt;
    widg.chkBack.Checked    := fBackWard;
    widg.chkCaseSens.Checked:= fCaseSens;
    widg.chkRegex.Checked   := fRegex;
    widg.chkFromCur.Checked := fFromCur;
    widg.chkWWord.Checked   := fWholeWord;
  end
  else inherited;
end;

procedure TSearchOptions.setMrSearches(value: TStringList);
begin
  fMrSearches.Assign(value);
end;

procedure TSearchOptions.cleanIvnalidHistoryItems;
var
  i: integer;
begin
  for i := fMrReplacements.Count-1 downto 0 do
    if fMrReplacements[i].length > 128 then
      fMrReplacements.Delete(i);
  for i := fMrSearches.Count-1 downto 0 do
      if fMrSearches[i].length > 128 then
        fMrSearches.Delete(i);
end;

procedure TSearchOptions.setMrReplacements(value: TStringList);
begin
  fMrReplacements.Assign(value);
end;

procedure TSearchOptions.afterLoad;
begin
  cleanIvnalidHistoryItems;
end;

procedure TSearchOptions.beforeSave;
begin
  cleanIvnalidHistoryItems;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TSearchWidget.Create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  toolbarVisible:=false;
  fActFindNext := TAction.Create(self);
  fActFindNext.Caption := 'Find';
  fActFindNext.OnExecute := @actFindNextExecute;
  fActFindAll := TAction.Create(self);
  fActFindAll.Caption := 'Find all';
  fActFindAll.OnExecute := @actFindAllExecute;
  fActReplaceNext := TAction.Create(self);
  fActReplaceNext.Caption := 'Replace';
  fActReplaceNext.OnExecute := @actReplaceNextExecute;
  fActReplaceAll := TAction.Create(self);
  fActReplaceAll.Caption := 'Replace all';
  fActReplaceAll.OnExecute := @actReplaceAllExecute;

  fSearchMru := TMruList.Create;
  fReplaceMru:= TMruList.Create;

  fname := getDocPath + OptsFname;
  if fname.fileExists then with TSearchOptions.create(nil) do
  try
    loadFromFile(fname);
    assignTo(self);
  finally
    free;
  end;

  btnFind.Action := fActFindNext;
  btnReplace.Action := fActReplaceNext;
  btnReplaceAll.Action := fActReplaceAll;
  btnFindAll.Action := fActFindAll;
  case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnAllScope, 'DOCUMENT');
      AssignPng(btnFind, 'FIND');
      AssignPng(btnFindAll, 'FIND');
      AssignPng(btnReplace, 'TEXT_REPLACE');
      AssignPng(btnReplaceAll, 'TEXT_REPLACE');
    end;
    iss24:
    begin
      AssignPng(btnAllScope, 'DOCUMENT24');
      AssignPng(btnFind, 'FIND24');
      AssignPng(btnFindAll, 'FIND24');
      AssignPng(btnReplace, 'TEXT_REPLACE24');
      AssignPng(btnReplaceAll, 'TEXT_REPLACE24');
    end;
    iss32:
    begin
      AssignPng(btnAllScope, 'DOCUMENT32');
      AssignPng(btnFind, 'FIND32');
      AssignPng(btnFindAll, 'FIND32');
      AssignPng(btnReplace, 'TEXT_REPLACE32');
      AssignPng(btnReplaceAll, 'TEXT_REPLACE32');
    end;
  end;
  btnAllScope.Caption:= FindScopeStr[fFindScope];
  updateImperative;

  EntitiesConnector.addObserver(self);
end;

destructor TSearchWidget.Destroy;
begin
  with TSearchOptions.create(nil) do
  try
    assign(self);
    saveToFile(getDocPath + OptsFname);
  finally
    free;
  end;

  EntitiesConnector.removeObserver(self);
  fSearchMru.Free;
  fReplaceMru.Free;
  inherited;
end;
{$ENDREGION}

{$REGION IContextualActions---------------------------------------------------}
function TSearchWidget.getOptions: TSynSearchOptions;
begin
  result := [];
  if chkRegex.Checked     then result += [ssoRegExpr];
  if chkWWord.Checked     then result += [ssoWholeWord];
  if chkBack.Checked      then result += [ssoBackwards];
  if chkCaseSens.Checked  then result += [ssoMatchCase];
  if chkPrompt.Checked    then result += [ssoPrompt];
end;

function dlgReplaceAll: TModalResult;
const
  Btns = [mbYes, mbNo, mbYesToAll, mbNoToAll];
begin
  exit( MessageDlg('dexed', 'Replace this match ?', mtConfirmation, Btns, ''));
end;

procedure TSearchWidget.replaceEvent(Sender: TObject; const ASearch, AReplace:
  string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  case dlgReplaceAll of
    mrYes: ReplaceAction := raReplace;
    mrNo: ReplaceAction := raSkip;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrCancel, mrClose, mrNoToAll:
      begin
        ReplaceAction := raCancel;
        fCancelAll := true;
      end;
  end;
end;

procedure TSearchWidget.actFindAllExecute(sender: TObject);
var
  i: integer;
  c: TSynEditStringList;
  f: string;
  s: integer = 0;
  m: IMessagesDisplay;
  h: IMultiDocHandler;
begin
  if (fDoc.isNil and (fFindScope <> scProj)) or
     ((fProj = nil) and (fFindScope = scProj)) then
      exit;

  fSearchMru.Insert(0,fToFind);
  cbToFind.Items.Assign(fSearchMru);

  case fFindScope of
    scDoc:
    begin
      findAll(fDoc.fileName, fDoc.Lines, true);
    end;

    scProj:
    begin
      c := TSynEditStringList.Create;
      try
        for i := 0 to fProj.sourcesCount-1 do
        begin
          f := fProj.sourceAbsolute(i);
          c.LoadFromFile(f);
          s += findAll(f, c, false);
        end;
        if s = 0 then
        begin
          m := getMessageDisplay;
          m.message(format('0 result for the pattern <%s>', [fToFind]),
            nil, amcMisc, amkInf);
        end;
      finally
        c.Free;
      end;
    end;

    scOpened:
    begin
      c := TSynEditStringList.Create;
      h := getMultiDocHandler;
      try
        for i := 0 to h.documentCount-1 do
        begin
          f := h.getDocument(i).fileName;
          s += findAll(f, h.getDocument(i).Lines, false);
        end;
        if s = 0 then
        begin
          m := getMessageDisplay;
          m.message(format('0 result for the pattern <%s>', [fToFind]),
            nil, amcMisc, amkInf);
        end;
      finally
        c.Free;
      end;
    end;
  end;
end;

function TSearchWidget.findAll(const filename: string; lines: TStrings;
  showNoResult: boolean = true): integer;
var
  search: TSynEditSearch;
  options: TSynSearchOptions;
  start, stop: TPoint;
  startf, stopf: TPoint;
  msgs: IMessagesDisplay;
  msg: string;
  fmt: string;
  i: integer;
  res: array of TPoint = nil;
begin
  result := 0;
  search := TSynEditSearch.Create;
  try
    options := getOptions;
    search.Sensitive := ssoMatchCase in options;
    search.Whole := ssoWholeWord in options;
    search.RegularExpressions:= ssoRegExpr in options;
    search.Pattern:=fToFind;
    start := Point(1,1);
    stop := Point(high(integer), lines.Count);
    while search.FindNextOne(lines, start, stop, startf, stopf) do
    begin
      setLength(res, length(res) + 1);
      res[high(res)].X := startf.X;
      res[high(res)].Y := startf.Y;
      start := stopf;
    end;
    result := length(res);
    msgs := getMessageDisplay;
    if (not showNoResult and (result > 0)) or showNoResult then
    begin
      msg := format('%d result(s) for the pattern <%s> in %s',
        [length(res), fToFind, filename]);
      msgs.message(msg, nil, amcMisc, amkInf);
    end;
    fmt := fileName + '(%d,%d): "%s"';
    for i := 0 to high(res) do
    begin
      msg := format(fmt, [res[i].Y, res[i].X, Trim(lines[res[i].Y-1])]);
      msgs.message(msg, nil, amcMisc, amkInf);
    end;
  finally
    search.free;
  end;
end;

procedure TSearchWidget.actFindNextExecute(sender: TObject);
begin
  if fDoc.isNil then
    exit;

  fSearchMru.Insert(0, fToFind);
  cbToFind.Items.Assign(fSearchMru);

  if not chkFromCur.Checked then
  begin
    if chkBack.Checked then
      fDoc.CaretXY := Point(high(Integer), high(Integer))
    else
    begin
      if not fHasRestarted then
        fDoc.CaretXY := Point(1,1);
      fHasRestarted := true;
    end;
  end
  else if fHasSearched then
  begin
    if chkBack.Checked then
      fDoc.CaretX := fDoc.CaretX - 1
    else
      fDoc.CaretX := fDoc.CaretX + 1;
  end;
  if fDoc.SearchReplace(fToFind, '', getOptions) = 0 then
    dlgOkInfo('the expression cannot be found')
  else
  begin
    fHasSearched := true;
    fHasRestarted := false;
    chkFromCur.Checked := true;
  end;
  updateImperative;
end;

procedure TSearchWidget.actReplaceNextExecute(sender: TObject);
begin
  if fDoc.isNil then
    exit;

  fSearchMru.Insert(0, fToFind);
  fReplaceMru.Insert(0, fReplaceWth);
  cbToFind.Items.Assign(fSearchMru);
  cbReplaceWth.Items.Assign(fReplaceMru);

  if chkPrompt.Checked then
    fDoc.OnReplaceText := @replaceEvent;
  if not chkFromCur.Checked then
  begin
    if chkBack.Checked then
      fDoc.CaretXY := Point(high(Integer), high(Integer))
    else
      fDoc.CaretXY := Point(0,0);
  end
  else if fHasSearched then
  begin
    if chkBack.Checked then
      fDoc.CaretX := fDoc.CaretX - 1
    else
      fDoc.CaretX := fDoc.CaretX + fToFind.length;
  end;
  if fDoc.SearchReplace(fToFind, fReplaceWth, getOptions + [ssoReplace]) <> 0 then
    fHasSearched := true;
  fDoc.OnReplaceText := nil;
  updateImperative;
end;

procedure TSearchWidget.actReplaceAllExecute(sender: TObject);
var
  opts: TSynSearchOptions;
begin
  if fDoc.isNil then
    exit;

  cbReplaceWth.Items.Assign(fReplaceMru);
  opts := getOptions + [ssoReplace];
  opts -= [ssoBackwards];

  fSearchMru.Insert(0, fToFind);
  fReplaceMru.Insert(0, fReplaceWth);
  if chkPrompt.Checked then fDoc.OnReplaceText := @replaceEvent;
  fDoc.CaretXY := Point(0,0);
  while(true) do
  begin
    if fDoc.SearchReplace(fToFind, fReplaceWth, opts) = 0
      then break;
    if fCancelAll then
    begin
      fCancelAll := false;
      break;
    end;
  end;
  fDoc.OnReplaceText := nil;
  updateImperative;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TSearchWidget.projNew(project: ICommonProject);
begin
  fProj := project;
  updateImperative;
end;

procedure TSearchWidget.projChanged(project: ICommonProject);
begin
end;

procedure TSearchWidget.projClosing(project: ICommonProject);
begin
  if fProj = project then
    fProj := nil;
  updateImperative;
end;

procedure TSearchWidget.projFocused(project: ICommonProject);
begin
  fProj := project;
  updateImperative;
end;

procedure TSearchWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TSearchWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TSearchWidget.docNew(document: TDexedMemo);
begin
  fDoc := document;
  updateImperative;
end;

procedure TSearchWidget.docClosing(document: TDexedMemo);
begin
  if fDoc = document then fDoc := nil;
  updateImperative;
end;

procedure TSearchWidget.docFocused(document: TDexedMemo);
begin
  if fDoc = document then exit;
  fDoc := document;
  updateImperative;
end;

procedure TSearchWidget.docChanged(document: TDexedMemo);
begin
end;
{$ENDREGION}

{$REGION Misc. -----------------------------------------------------------------}
procedure TSearchWidget.cbToFindChange(Sender: TObject);
begin
  if Updating then exit;
  fToFind := cbToFind.Text;
  fHasSearched := false;
  updateImperative;
end;

procedure TSearchWidget.cbToFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> 13 then
    exit;
  actFindNextExecute(nil);
end;

procedure TSearchWidget.chkEnableRepChange(Sender: TObject);
begin
  if Updating then exit;
  updateImperative;
end;

procedure TSearchWidget.cbReplaceWthChange(Sender: TObject);
begin
  if Updating then exit;
  fReplaceWth := cbReplaceWth.Text;
  fHasSearched := false;
  updateImperative;
end;

procedure TSearchWidget.cbReplaceWthKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key <> 13 then
    exit;
  actReplaceNextExecute(nil);
end;

procedure TSearchWidget.btnAllScopeClick(Sender: TObject);
begin
  case fFindScope of
    scDoc: fFindScope := scProj;
    scProj: fFindScope := scOpened;
    scOpened: fFindScope := scDoc;
  end;
  btnAllScope.Caption:= FindScopeStr[fFindScope];
  if fFindScope <> scDoc then
  begin
    case GetIconScaledSize of
      iss16: AssignPng(btnAllScope, 'DOCUMENT_ALL');
      iss24: AssignPng(btnAllScope, 'DOCUMENT_ALL24');
      iss32: AssignPng(btnAllScope, 'DOCUMENT_ALL32');
    end;
    if fFindScope = scProj then
      btnAllScope.Hint := 'find in all the project sources'
    else
      btnAllScope.Hint := 'find in all the documents currently opened';
  end
  else
  begin
    case GetIconScaledSize of
      iss16: AssignPng(btnAllScope, 'DOCUMENT');
      iss24: AssignPng(btnAllScope, 'DOCUMENT24');
      iss32: AssignPng(btnAllScope, 'DOCUMENT32');
    end;
    btnAllScope.Hint := 'find in the selected source';
  end;
  updateImperative;
end;

procedure TSearchWidget.updateImperative;
var
  canAll: boolean;
  hasTxt: boolean;
begin
  canAll := ((fDoc.isNotNil and (fFindScope <> scProj)) or
            ((fFindScope = scProj) and (fProj <> nil)));
  hasTxt := fToFind.isNotEmpty and not fToFind.isBlank;
  btnFind.Enabled := fDoc.isNotNil and hasTxt;
  btnFindAll.Enabled := canAll and hasTxt;
  btnReplace.Enabled := fDoc.isNotNil and chkEnableRep.Checked and fToFind.isNotEmpty;
  btnReplaceAll.Enabled := btnReplace.Enabled;
  cbReplaceWth.Enabled := fDoc.isNotNil and chkEnableRep.Checked;
  cbToFind.Enabled := canAll;
end;
{$ENDREGION}

end.
