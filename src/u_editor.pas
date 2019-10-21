unit u_editor;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, lcltype, Graphics, SynEditKeyCmds,
  ComCtrls, SynEditHighlighter, ExtCtrls, Menus, SynMacroRecorder, dialogs, LazFileUtils,
  SynPluginSyncroEdit, SynEdit, SynHighlighterMulti, AnchorDocking, u_dialogs,
  u_widget, u_interfaces, u_synmemo, u_dlang, u_common, u_dcd, u_observer,
  u_sharedres, u_controls, u_writableComponent, u_dsgncontrols, LMessages;

type

  TEditorWidget = class;

  TPagesOptions = class(TWritableLfmTextComponent, IEditableOptions, IEditableShortCut)
  private
    fEditorWidget: TEditorWidget;
    fPageButtons: TPageControlButtons;
    fPageOptions: TPageControlOptions;
    fMoveLeft: TShortCut;
    fMoveRight: TShortCut;
    fNextPage: TShortCut;
    fPrevPage: TShortCut;
    fDetectModuleName: boolean;
    fShCount: integer;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    function scedWantFirst: boolean;
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
    procedure scedSendDone;
  published
    property pageButtons: TPageControlButtons read fPageButtons write fPageButtons;
    property pageOptions: TPageControlOptions read fPageOptions write fPageOptions;
    property nextPage: TShortCut read fNextPage write fNextPage;
    property previousPage: TShortCut read fPrevPage write fPrevPage;
    property moveLeft: TShortCut read fMoveLeft write fMoveLeft;
    property moveRight: TShortCut read fMoveRight write fMoveRight;
    property detectModuleName: boolean read fDetectModuleName write fDetectModuleName default true;
  public
    procedure assign(source: TPersistent); override;
    procedure assignTo(target: TPersistent); override;
    constructor construct(editorWidg: TEditorWidget);
    destructor Destroy; override;
  end;

  { TEditorWidget }
  TEditorWidget = class(TDexedWidget, IDocumentObserver, IMultiDocHandler, IProjectObserver)
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    mnuEdTabWidth2: TMenuItem;
    mnuEdTabWidth3: TMenuItem;
    mnuEdTabWidth4: TMenuItem;
    mnuEdTabWidth5: TMenuItem;
    mnuEdTabWidth6: TMenuItem;
    mnuEdTabWidth7: TMenuItem;
    mnuEdTabWidth8: TMenuItem;
    mnuEdShowSpec: TMenuItem;
    mnuEdSetSpaces: TMenuItem;
    mnuEdSetTabs: TMenuItem;
    mnuedGotoline: TMenuItem;
    mnuedPrevWarn: TMenuItem;
    mnuedNextWarn: TMenuItem;
    mnuedDdocTmp: TMenuItem;
    mnuedPrevProtGrp: TMenuItem;
    mnuedNextProtGrp: TMenuItem;
    MenuItem2: TMenuItem;
    mnuedSortLines: TMenuItem;
    mnuedNextCarea: TMenuItem;
    mnuedPrevCarea: TMenuItem;
    mnuedLowcase: TMenuItem;
    mnuedUpcase: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mnuedRename: TMenuItem;
    mnuedInvAllNone: TMenuItem;
    mnuedComm: TMenuItem;
    mnuedPrev: TMenuItem;
    mnuedNext: TMenuItem;
    mnuedCallTip: TMenuItem;
    mnuedDdoc: TMenuItem;
    mnuedCopy: TMenuItem;
    mnuedCut: TMenuItem;
    mnuedPaste: TMenuItem;
    MenuItem4: TMenuItem;
    mnuedUndo: TMenuItem;
    mnuedRedo: TMenuItem;
    MenuItem7: TMenuItem;
    mnuedJum2Decl: TMenuItem;
    macRecorder: TSynMacroRecorder;
    editorStatus: TStatusBar;
    mnuEditor: TPopupMenu;
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure mnuedDdocTmpClick(Sender: TObject);
    procedure mnuedGotolineClick(Sender: TObject);
    procedure mnuedNextWarnClick(Sender: TObject);
    procedure mnuedPrevProtGrpClick(Sender: TObject);
    procedure mnuedNextProtGrpClick(Sender: TObject);
    procedure mnuedNextCareaClick(Sender: TObject);
    procedure mnuedPrevCareaClick(Sender: TObject);
    procedure mnuedLowcaseClick(Sender: TObject);
    procedure mnuedPrevWarnClick(Sender: TObject);
    procedure mnuEdSetSpacesClick(Sender: TObject);
    procedure mnuEdSetTabsClick(Sender: TObject);
    procedure mnuEdShowSpecClick(Sender: TObject);
    procedure mnuedSortLinesClick(Sender: TObject);
    procedure mnuEdTabWidth2Click(Sender: TObject);
    procedure mnuedUpcaseClick(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure mnuedRenameClick(Sender: TObject);
    procedure mnuedInvAllNoneClick(Sender: TObject);
    procedure mnuedCommClick(Sender: TObject);
    procedure mnuedPrevClick(Sender: TObject);
    procedure mnuedNextClick(Sender: TObject);
    procedure mnuedCallTipClick(Sender: TObject);
    procedure mnuedCopyClick(Sender: TObject);
    procedure mnuedCutClick(Sender: TObject);
    procedure mnuedDdocClick(Sender: TObject);
    procedure mnuEditorPopup(Sender: TObject);
    procedure mnuedPasteClick(Sender: TObject);
    procedure mnuedUndoClick(Sender: TObject);
    procedure mnuedRedoClick(Sender: TObject);
    procedure mnuedJum2DeclClick(Sender: TObject);
  protected
    procedure updateDelayed; override;
    procedure updateImperative; override;
    procedure setToolBarFlat(value: boolean); override;
  private
    fDetectModuleName: boolean;
    fOptions: TPagesOptions;
    pageControl: TDexedPageControl;
    fKeyChanged: boolean;
    fDoc: TDexedMemo;
    fProj: ICommonProject;
    fTokList: TLexTokenList;
    fLastCommand: TSynEditorCommand;
    procedure PageControlButtonClick(sender: TObject; button: TPageControlButton);
    procedure PageControlChanged(Sender: TObject);
    procedure PageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure updateStatusBar;
    procedure updatePageCaption(page: TDexedPage);
    procedure pageBtnAddCLick(Sender: TObject);
    procedure pageCloseBtnClick(Sender: TObject);
    procedure memoKeyPress(Sender: TObject; var Key: char);
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoCtrlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure getSymbolLoc;
    procedure focusedEditorChanged;
    procedure memoCmdProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure setDetectModuleName(value: boolean);
    //
    procedure docNew(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    //
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    //
    function SingleServiceName: string;
    function documentCount: Integer;
    function getDocument(index: Integer): TDexedMemo;
    function findDocument(const fname: string): TDexedMemo;
    procedure openDocument(const fname: string);
    function closeDocument(index: Integer;promptOnChanged: boolean = true): boolean;
    function closeDocument(doc: TDexedMemo;promptOnChanged: boolean = true): boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function closeQuery: boolean; override;
  end;

implementation
{$R *.lfm}

uses
  u_lcldragdrop;
const
  optname = 'editorpages.txt';

{$REGION TPagesOptions -------------------------------------------------------}
constructor TPagesOptions.construct(editorWidg: TEditorWidget);
var
  fname: string;
begin
  fEditorWidget := editorWidg;
  inherited create(editorWidg);
  EntitiesConnector.addObserver(self);
  //
  fDetectModuleName := true;
  fname := getDocPath + optname;
  if fname.fileExists then
  begin
    loadFromFile(fname);
    assignTo(fEditorWidget);
  end
  else assign(fEditorWidget);
end;

destructor TPagesOptions.Destroy;
begin
  saveToFile(getDocPath + optname);
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TPagesOptions.assign(source: TPersistent);
begin
  if source = fEditorWidget then
  begin
    fPageButtons := fEditorWidget.pageControl.buttons;
    fPageOptions := fEditorWidget.pageControl.options;
    fDetectModuleName:= fEditorWidget.fDetectModuleName;
  end
  else inherited;
end;

procedure TPagesOptions.assignTo(target: TPersistent);
begin
  if target = fEditorWidget then
  begin
    fEditorWidget.pageControl.buttons := fPageButtons;
    fEditorWidget.pageControl.options := fPageOptions;
    fEditorWidget.setDetectModuleName(fDetectModuleName);
  end
  else inherited;
end;

function TPagesOptions.optionedWantCategory(): string;
begin
  exit('Editor pages')
end;

function TPagesOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TPagesOptions.optionedWantContainer: TPersistent;
begin
  exit(self);
end;

procedure TPagesOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept: assignTo(fEditorWidget);
    oeeCancel: assign(fEditorWidget);
    else ;
  end;
end;

function TPagesOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;

function TPagesOptions.scedWantFirst: boolean;
begin
  fShCount := 0;
  exit(true);
end;

function TPagesOptions.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
begin
  category := 'Editor pages';
  case fShCount of
    0: begin identifier := 'Select next page'; aShortcut:= fNextPage; end;
    1: begin identifier := 'Select previous page'; aShortcut:= fPrevPage; end;
    2: begin identifier := 'Move page left'; aShortcut:= fMoveLeft; end;
    3: begin identifier := 'Move page right'; aShortcut:= fMoveRight; end;
  end;
  fShCount += 1;
  result := fShCount <> 4;
end;

procedure TPagesOptions.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
begin
  case identifier of
    'Select next page': fNextPage := aShortcut;
    'Select previous page': fPrevPage := aShortcut;
    'Move page left': fMoveLeft := aShortcut;
    'Move page right': fMoveRight:= aShortcut;
  end;
end;

procedure TPagesOptions.scedSendDone;
begin
  fShCount := 0;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TEditorWidget.create(aOwner: TComponent);
var
  s: TIconScaledSize;
begin
  inherited;
  toolbarVisible:=false;
  fDetectModuleName:=true;

  pageControl := TDexedPageControl.Create(self);
  pageControl.Parent := Content;
  pageControl.align := alClient;
  pageControl.onChanged:= @PageControlChanged;
  pageControl.onChanging:=@PageControlChanging;
  pageControl.closeButton.OnClick:=@pageCloseBtnClick;
  pageControl.addButton.OnClick:=@pageBtnAddCLick;
  pageControl.OnDragDrop:= @ddHandler.DragDrop;
  pageControl.OnDragOver:= @ddHandler.DragOver;
  pageControl.onButtonClick:= @PageControlButtonClick;

  s := GetIconScaledSize;

  case s of
    iss16:
    begin
      AssignPng(pageControl.moveLeftButton, 'GO_PREVIOUS');
      AssignPng(pageControl.moveRightButton, 'GO_NEXT');
      AssignPng(pageControl.addButton, 'DOCUMENT_ADD');
      AssignPng(pageControl.closeButton, 'DOCUMENT_DELETE');
      AssignPng(pageControl.splitButton, 'SPLITTER');

      AssignPng(mnuedCopy.Bitmap, 'COPY');
      AssignPng(mnuedCut.Bitmap, 'CUT');
      AssignPng(mnuedPaste.Bitmap, 'PASTE');
      AssignPng(mnuedUndo.Bitmap, 'ARROW_UNDO');
      AssignPng(mnuedRedo.Bitmap, 'ARROW_REDO');
      AssignPng(mnuedJum2Decl.Bitmap, 'ARROW_SHOE');
      AssignPng(mnuedCopy.Bitmap, 'COPY');
      AssignPng(mnuedNext.Bitmap, 'GO_NEXT');
      AssignPng(mnuedPrev.Bitmap, 'GO_PREVIOUS');
      AssignPng(mnuedRename.Bitmap, 'PENCIL');
      AssignPng(mnuedUpcase.Bitmap, 'CASE');
      AssignPng(mnuedLowcase.Bitmap, 'CASE');
      AssignPng(mnuedNextCarea.Bitmap, 'GO_NEXT');
      AssignPng(mnuedPrevCarea.Bitmap, 'GO_PREVIOUS');
      AssignPng(mnuedSortLines.Bitmap, 'SORT_AZ');
      AssignPng(mnuedNextProtGrp.Bitmap, 'GO_NEXT');
      AssignPng(mnuedPrevProtGrp.Bitmap, 'GO_PREVIOUS');
      AssignPng(mnuedNextWarn.Bitmap, 'GO_NEXT');
      AssignPng(mnuedPrevWarn.Bitmap, 'GO_PREVIOUS');
    end;
    iss24:
    begin
      AssignPng(pageControl.moveLeftButton, 'GO_PREVIOUS24');
      AssignPng(pageControl.moveRightButton, 'GO_NEXT24');
      AssignPng(pageControl.addButton, 'DOCUMENT_ADD24');
      AssignPng(pageControl.closeButton, 'DOCUMENT_DELETE24');
      AssignPng(pageControl.splitButton, 'SPLITTER24');

      AssignPng(mnuedCopy.Bitmap, 'COPY24');
      AssignPng(mnuedCut.Bitmap, 'CUT24');
      AssignPng(mnuedPaste.Bitmap, 'PASTE24');
      AssignPng(mnuedUndo.Bitmap, 'ARROW_UNDO24');
      AssignPng(mnuedRedo.Bitmap, 'ARROW_REDO24');
      //AssignPng(mnuedJum2Decl.Bitmap, 'ARROW_SHOE24');
      AssignPng(mnuedCopy.Bitmap, 'COPY24');
      AssignPng(mnuedNext.Bitmap, 'GO_NEXT24');
      AssignPng(mnuedPrev.Bitmap, 'GO_PREVIOUS24');
      AssignPng(mnuedRename.Bitmap, 'PENCIL24');
      AssignPng(mnuedUpcase.Bitmap, 'CASE24');
      AssignPng(mnuedLowcase.Bitmap, 'CASE24');
      AssignPng(mnuedNextCarea.Bitmap, 'GO_NEXT24');
      AssignPng(mnuedPrevCarea.Bitmap, 'GO_PREVIOUS24');
      AssignPng(mnuedSortLines.Bitmap, 'SORT_AZ24');
      AssignPng(mnuedNextProtGrp.Bitmap, 'GO_NEXT24');
      AssignPng(mnuedPrevProtGrp.Bitmap, 'GO_PREVIOUS24');
      AssignPng(mnuedNextWarn.Bitmap, 'GO_NEXT24');
      AssignPng(mnuedPrevWarn.Bitmap, 'GO_PREVIOUS24');
    end;
    iss32:
    begin
      AssignPng(pageControl.moveLeftButton, 'GO_PREVIOUS32');
      AssignPng(pageControl.moveRightButton, 'GO_NEXT32');
      AssignPng(pageControl.addButton, 'DOCUMENT_ADD32');
      AssignPng(pageControl.closeButton, 'DOCUMENT_DELETE32');
      AssignPng(pageControl.splitButton, 'SPLITTER32');

      AssignPng(mnuedCopy.Bitmap, 'COPY32');
      AssignPng(mnuedCut.Bitmap, 'CUT32');
      AssignPng(mnuedPaste.Bitmap, 'PASTE32');
      AssignPng(mnuedUndo.Bitmap, 'ARROW_UNDO32');
      AssignPng(mnuedRedo.Bitmap, 'ARROW_REDO32');
      //AssignPng(mnuedJum2Decl.Bitmap, 'ARROW_SHOE32');
      AssignPng(mnuedCopy.Bitmap, 'COPY32');
      AssignPng(mnuedNext.Bitmap, 'GO_NEXT32');
      AssignPng(mnuedPrev.Bitmap, 'GO_PREVIOUS32');
      AssignPng(mnuedRename.Bitmap, 'PENCIL32');
      AssignPng(mnuedUpcase.Bitmap, 'CASE32');
      AssignPng(mnuedLowcase.Bitmap, 'CASE32');
      AssignPng(mnuedNextCarea.Bitmap, 'GO_NEXT32');
      AssignPng(mnuedPrevCarea.Bitmap, 'GO_PREVIOUS32');
      AssignPng(mnuedSortLines.Bitmap, 'SORT_AZ32');
      AssignPng(mnuedNextProtGrp.Bitmap, 'GO_NEXT32');
      AssignPng(mnuedPrevProtGrp.Bitmap, 'GO_PREVIOUS32');
      AssignPng(mnuedNextWarn.Bitmap, 'GO_NEXT32');
      AssignPng(mnuedPrevWarn.Bitmap, 'GO_PREVIOUS32');
    end;
  end;

  editorStatus.Panels[0].Width := ScaleX(115, 96);
  editorStatus.Panels[1].Width := ScaleX(85, 96);
  editorStatus.Panels[2].Width := ScaleX(125, 96);
  editorStatus.Panels[3].Width := ScaleX(105, 96);
  editorStatus.Panels[4].Width := ScaleX(2000, 96);

  fTokList := TLexTokenList.Create;

  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);

  fOptions:= TPagesOptions.construct(self);
end;

destructor TEditorWidget.destroy;
var
  i: integer;
begin
  EntitiesConnector.removeObserver(self);
  for i := PageControl.PageCount-1 downto 0 do
    if PageControl.Pages[i].ControlCount > 0 then
      if (PageControl.Pages[i].Controls[0] is TDexedMemo) then
        PageControl.Pages[i].Controls[0].Free;
  fTokList.Free;
  fOptions.Free;
  inherited;
end;

function TEditorWidget.closeQuery: boolean;
begin
  result := inherited and (Parent.isNil  or
    // already set as dockable but not docked
    // necessary because it has te closed programmatically during loading sequence
    // otherwise the layout reloading has no effect.
    Parent is TAnchorDockHostSite and Parent.Parent.isNil);
end;

procedure TEditorWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  if value then
  begin
    fOptions.pageOptions:= fOptions.pageOptions + [poFlatButtons];
    pageControl.options := pageControl.options + [poFlatButtons]
  end
  else
  begin
    fOptions.pageOptions:= fOptions.pageOptions - [poFlatButtons];
    pageControl.options := pageControl.options - [poFlatButtons];
  end;
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TEditorWidget.docNew(document: TDexedMemo);
var
  pge: TDexedPage;
begin
  pge := pageControl.addPage;
  //
  document.Align := alClient;
  document.Parent := pge;
  //
  document.OnKeyDown := @memoKeyDown;
  document.OnKeyUp := @memoKeyUp;
  document.OnKeyPress := @memoKeyPress;
  document.OnMouseDown := @memoMouseDown;
  document.OnMouseMove := @memoMouseMove;
  document.OnClickLink := @memoCtrlClick;
  document.OnCommandProcessed:= @memoCmdProcessed;
  //
  fDoc := document;
  fDoc.Visible:=true;
  fDoc.setFocus;
  focusedEditorChanged;
  updateImperative;
end;

procedure TEditorWidget.docClosing(document: TDexedMemo);
begin
  if document.isNil then
    exit;
  document.Parent := nil;
  if document = fDoc then
    fDoc := nil;
  pageControl.deletePage(pageControl.pageIndex);
  updateImperative;
end;

procedure TEditorWidget.docFocused(document: TDexedMemo);
begin
  if fDoc.isNotNil and pageControl.currentPage.isNotNil and
    (pageControl.currentPage.Caption = '<new document>') then
      updatePageCaption(pageControl.currentPage);
  if document = fDoc then exit;
  fDoc := document;
  fDoc.Visible:=true;
  focusedEditorChanged;
  updateImperative;
end;

procedure TEditorWidget.docChanged(document: TDexedMemo);
begin
  if fDoc <> document then
    exit;
  fKeyChanged := true;
  beginDelayedUpdate;
end;
{$ENDREGION}

{$REGION ICommonProject ------------------------------------------------------}
procedure TEditorWidget.projNew(project: ICommonProject);
begin
end;

procedure TEditorWidget.projChanged(project: ICommonProject);
begin
end;

procedure TEditorWidget.projClosing(project: ICommonProject);
begin
  if fProj = project then
    fProj := nil;
end;

procedure TEditorWidget.projFocused(project: ICommonProject);
begin
  fProj := project;
end;

procedure TEditorWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TEditorWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION IMultiDocHandler ----------------------------------------------------}
function TEditorWidget.SingleServiceName: string;
begin
  exit('IMultiDocHandler');
end;

function TEditorWidget.documentCount: Integer;
begin
  exit(PageControl.PageCount);
end;

function TEditorWidget.getDocument(index: Integer): TDexedMemo;
begin
  exit(TDexedMemo(pageControl.Pages[index].Controls[0]));
end;

function TEditorWidget.findDocument(const fname: string): TDexedMemo;
var
  i: Integer;
begin
  for i := 0 to PageControl.PageCount-1 do
  begin
    result := getDocument(i);
    if SameFileName(result.fileName, TrimFilename(fname)) then
      exit;
  end;
  result := nil;
end;

procedure TEditorWidget.openDocument(const fname: string);
var
  doc: TDexedMemo;
begin
  showWidget;
  doc := findDocument(fname);
  if doc.isNotNil then
  begin
    PageControl.currentPage := TDexedPage(doc.Parent);
    exit;
  end;
  doc := TDexedMemo.Create(nil);
  fDoc.loadFromFile(TrimFilename(fname));
  if assigned(fProj) and (fProj.filename = fDoc.fileName) then
  begin
    if fProj.getFormat = pfDEXED then
      fDoc.Highlighter := LfmSyn
    else
      fDoc.Highlighter := JsSyn;
  end;
end;

function TEditorWidget.closeDocument(index: Integer; promptOnChanged: boolean = true): boolean;
var
  doc: TDexedMemo;
begin
  doc := getDocument(index);
  if doc.isNil then
    exit(false);
  if promptOnChanged and (doc.modified or (doc.fileName = doc.tempFilename)) and
    (dlgFileChangeClose(doc.fileName, UnsavedFile) = mrCancel) then
      exit(false);
  showWidget;
  doc.disableFileDateCheck:=true;
  pageControl.pageIndex:=index;
  doc.Free;
  result := true;
end;

function TEditorWidget.closeDocument(doc: TDexedMemo;promptOnChanged: boolean = true): boolean;
var
  page: TDexedPage = nil;
begin
  page := TDexedPage(doc.Parent);
  if page.isNil then
    exit(false);
  exit(closeDocument(page.index, promptOnChanged));
end;
{$ENDREGION}

{$REGION PageControl/Editor things ---------------------------------------------}
procedure TEditorWidget.pageCloseBtnClick(Sender: TObject);
begin
  closeDocument(PageControl.PageIndex);
  PageControlButtonClick(pageControl, pbClose);
end;

procedure TEditorWidget.pageBtnAddCLick(Sender: TObject);
begin
  TDexedMemo.Create(nil);
  pageControl.currentPage.Caption:='<new document>';
end;

procedure TEditorWidget.setDetectModuleName(value: boolean);
var
  i: integer;
begin
  if fDetectModuleName = value then
    exit;
  fDetectModuleName:=value;
  for i:= 0 to pageControl.pageCount-1 do
    updatePageCaption(pageControl.pages[i]);
end;

procedure TEditorWidget.focusedEditorChanged;
begin
  if fDoc.isNil then exit;
  //
  macRecorder.Editor:= fDoc;
  fDoc.PopupMenu := mnuEditor;
  fDoc.hideCallTips;
  fDoc.hideDDocs;
  if (pageControl.currentPage.Caption = '') or
    (pageControl.currentPage.Caption ='<new document>') then
  begin
    fKeyChanged := true;
    beginDelayedUpdate;
  end;
end;

procedure TEditorWidget.PageControlChanged(Sender: TObject);
begin
  if fDoc.isNil then exit;
  fDoc.hideCallTips;
  fDoc.hideDDocs;
end;

procedure TEditorWidget.PageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if fDoc.isNil then exit;
  fDoc.hideCallTips;
  fDoc.hideDDocs;
end;

procedure TEditorWidget.memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_CLEAR,VK_RETURN,VK_BACK :
      fKeyChanged := true;
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
      if Shift = [] then
        updateImperative;
  end;
  if fKeyChanged then
    beginDelayedUpdate;
end;

procedure TEditorWidget.memoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case fLastCommand of
    ecSelectionStart..ecSelectionEnd: updateImperative;
    ecLeft..ecHalfWordRight: updateImperative;
  end;
end;

procedure TEditorWidget.memoCmdProcessed(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
begin
  fLastCommand := Command;
  //
  case Command of
    ecJumpToDeclaration:
      getSymbolLoc;
    ecRecordMacro:
    begin
      if macRecorder.State = msStopped then
        macRecorder.RecordMacro(fDoc)
      else
        macRecorder.Stop;
      updateImperative;
    end;
    ecPlayMacro:
    begin
      macRecorder.Stop;
      macRecorder.PlaybackMacro(fDoc);
      updateImperative;
    end;
  end;
end;

procedure TEditorWidget.memoKeyPress(Sender: TObject; var Key: char);
begin
  fKeyChanged := true;
  beginDelayedUpdate;
end;

procedure TEditorWidget.memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  updateImperative;
end;

procedure TEditorWidget.memoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then exit;
  beginDelayedUpdate;
end;

procedure TEditorWidget.memoCtrlClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  getSymbolLoc;
end;

procedure TEditorWidget.getSymbolLoc;
var
  page: TDexedPage;
  srcpos, i, sum, linelen: Integer;
  fname: string;
  len: byte;
begin
  if not DcdWrapper.available then exit;
  //
  DcdWrapper.getDeclFromCursor(fname, srcpos);
  if (fname <> fDoc.fileName) and fname.fileExists then
  begin
    page := pageControl.splitPage;
    if assigned(page) then
    begin
      fDoc := TDexedMemo(page.Controls[0]);
      if fDoc.fileName <> fname then
        openDocument(fname);
    end
    else openDocument(fname);
  end;
  if srcpos <> -1 then
  begin
    sum := 0;
    len := getLineEndingLength(fDoc.fileName);
    for i := 0 to fDoc.Lines.Count-1 do
    begin
      linelen := fDoc.Lines[i].length;
      if sum + linelen + len > srcpos then
      begin
        fDoc.CaretY := i + 1;
        fDoc.CaretX := srcpos - sum + len;
        fDoc.SelectWord;
        fDoc.EnsureCursorPosVisible;
        break;
      end;
      sum += linelen;
      sum += len;
    end;
  end;
end;

procedure TEditorWidget.updateStatusBar;
const
  modstr: array[boolean] of string = ('...', 'MODIFIED');
begin
  if fDoc = nil then
  begin
    editorStatus.Panels[0].Text := '';
    editorStatus.Panels[1].Text := '';
    editorStatus.Panels[2].Text := '';
    editorStatus.Panels[3].Text := '';
    editorStatus.Panels[4].Text := '';
  end else
  begin
    editorStatus.Panels[0].Text := format('%d : %d | %d', [fDoc.CaretY, fDoc.CaretX, fDoc.SelEnd - fDoc.SelStart]);
    editorStatus.Panels[1].Text := modstr[fDoc.modified];
    if macRecorder.State = msRecording then
      editorStatus.Panels[2].Text := 'recording macro'
    else if macRecorder.IsEmpty then
      editorStatus.Panels[2].Text := 'no macro'
    else
      editorStatus.Panels[2].Text := 'macro ready';
    if fDoc.ReadOnly then
    begin
      editorStatus.Panels[3].Width:= 120;
      editorStatus.Panels[3].Text := '(read-only)';
    end else
      editorStatus.Panels[3].Width:= 0;
    editorStatus.Panels[4].Text := fDoc.fileName;
  end;
end;

procedure TEditorWidget.updatePageCaption(page: TDexedPage);
var
  txt: string = '<new document>';
  dc1: TDexedMemo = nil;
  dc2: TDexedMemo = nil;
begin
  if page.isNil or (page.ControlCount = 0) then
    exit;
  if pageControl.splitPage.isNotNil and
    (page <> pageControl.splitPage) then
  begin
    txt := '';
    dc1 := TDexedMemo(page.Controls[0]);
    dc2 := TDexedMemo(pageControl.splitPage.Controls[0]);
    if dc1.isNotNil and dc2.isNotNil then
      txt := dc1.pageCaption(fDetectModuleName) + ' - ' +
        dc2.pageCaption(fDetectModuleName);
  end
  else
    txt := TDexedMemo(page.Controls[0]).pageCaption(fDetectModuleName);
  page.Caption := txt;
end;

procedure TEditorWidget.PageControlButtonClick(sender: TObject; button: TPageControlButton);
var
  i: integer;
begin
  if ((button = pbClose) and (pageControl.currentPage = pageControl.splitPage))
    or (button = pbSplit) then
  begin
    for i:= 0 to pageControl.pageCount-1 do
      updatePageCaption(pageControl.pages[i]);
  end;
end;

procedure TEditorWidget.updateImperative;
begin
  updateStatusBar;
  if fDoc.isNotNil then
    updatePageCaption(pageControl.currentPage);
end;

procedure TEditorWidget.updateDelayed;
begin
  if fDoc = nil then
    exit;
  updateStatusBar;
  if not fKeyChanged then
    exit;
  if pageControl.currentPage.isNotNil then
    updatePageCaption(pageControl.currentPage);
end;
{$ENDREGION}

{$REGION Editor context menu ---------------------------------------------------}
procedure TEditorWidget.mnuedCopyClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecCopy, '', nil);
end;

procedure TEditorWidget.mnuedCallTipClick(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  mnuEditor.Close;
  fDoc.hideDDocs;
  if not fDoc.IsDSource and not fDoc.alwaysAdvancedFeatures then
      exit;
  fDoc.showCallTips;
end;

procedure TEditorWidget.mnuedCommClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecCommentSelection, '', nil);
end;

procedure TEditorWidget.mnuedPrevClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecPreviousLocation, '', nil);
end;

procedure TEditorWidget.mnuedNextClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecNextLocation, '', nil);
end;

procedure TEditorWidget.mnuedInvAllNoneClick(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  if not fDoc.IsDSource and not fDoc.alwaysAdvancedFeatures then
    exit;
  fDoc.CommandProcessor(ecSwapVersionAllNone, '', nil);
end;

procedure TEditorWidget.MenuItem5Click(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  if not fDoc.IsDSource and not fDoc.alwaysAdvancedFeatures then
    exit;
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      fTokList.Clear;
      lex(fDoc.Text, fTokList, nil);
      fTokList.saveToFile(FileName.normalizePath);
      fTokList.Clear;
    end;
  finally
    free;
  end;
end;

procedure TEditorWidget.mnuedUpcaseClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecUpperCaseWordOrSel, #0, nil);
end;

procedure TEditorWidget.mnuedLowcaseClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecLowerCaseWordOrSel, #0, nil);
end;

procedure TEditorWidget.mnuedPrevWarnClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.previousWarning;
end;

procedure TEditorWidget.mnuEdSetSpacesClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.Options := fDoc.Options + [eoTabsToSpaces];
end;

procedure TEditorWidget.mnuEdSetTabsClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.Options := fDoc.Options - [eoTabsToSpaces];
end;

procedure TEditorWidget.mnuEdShowSpecClick(Sender: TObject);
begin
  if fDoc.isNil then
    exit;
  if mnuEdShowSpec.Checked then
    fDoc.Options := fDoc.Options + [eoShowSpecialChars]
  else
    fDoc.Options := fDoc.Options - [eoShowSpecialChars];
end;

procedure TEditorWidget.mnuedSortLinesClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecSortLines, #0, nil);
end;

procedure TEditorWidget.mnuEdTabWidth2Click(Sender: TObject);
begin
  if fDoc.isNil then
    exit;

  fDoc.TabWidth:= TMenuItem(sender).Tag;
  if not (eoTabsToSpaces in fDoc.options) then
  begin
    fDoc.BlockIndent := 0;
    fDoc.BlockTabIndent := 1;
  end
  else
  begin
    fDoc.BlockIndent := fDoc.TabWidth;
    fDoc.BlockTabIndent := 0;
  end;
end;

procedure TEditorWidget.mnuedNextCareaClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.nextChangedArea;
end;

procedure TEditorWidget.mnuedPrevProtGrpClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.previousProtectionGroup;
end;

procedure TEditorWidget.mnuedDdocTmpClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.insertDdocTemplate;
end;

procedure TEditorWidget.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  sh: TShortCut;
begin
  Handled := false;
  sh := KeyToShortCut(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
  if sh = fOptions.fNextPage then
  begin
    pageControl.pageIndex:= (pageControl.pageIndex + 1) mod pageControl.pageCount;
    Handled := true;
  end
  else if sh = fOptions.fPrevPage then
  begin
    if pageControl.pageIndex - 1 < 0 then
      pageControl.pageIndex:= pageControl.pageCount - 1
    else
      pageControl.pageIndex:= pageControl.pageIndex - 1;
    Handled := true;
  end
  else if sh = fOptions.fMoveLeft then
  begin
    pageControl.movePageLeft;
    Handled := true;
  end
  else if sh = fOptions.fMoveRight then
  begin
    pageControl.movePageRight;
    Handled := true;
  end;
end;

procedure TEditorWidget.FormShow(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.Visible:=true;
end;

procedure TEditorWidget.mnuedGotolineClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.gotoLinePrompt;
end;

procedure TEditorWidget.mnuedNextWarnClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.nextWarning;
end;

procedure TEditorWidget.mnuedNextProtGrpClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.nextProtectionGroup;
end;

procedure TEditorWidget.mnuedPrevCareaClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.previousChangedArea;
end;

procedure TEditorWidget.MenuItem8Click(Sender: TObject);
var
  str: TStringList;
begin
  if fDoc.isNil then
    exit;
  if not fDoc.IsDSource and not fDoc.alwaysAdvancedFeatures then
    exit;
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      str := TStringList.Create;
      fTokList.Clear;
      lex(fDoc.Text, fTokList, nil, [lxoNoComments]);
      getImports(fTOkList, str);
      str.SaveToFile(filename.normalizePath);
      fTokList.Clear;
      str.Free;
    end;
  finally
    free;
  end;
end;

procedure TEditorWidget.MenuItem6Click(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecCommentIdentifier, '', nil);
end;

procedure TEditorWidget.mnuedRenameClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.CommandProcessor(ecRenameIdentifier, '', nil);
end;

procedure TEditorWidget.mnuedCutClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecCut, '', nil);
end;

procedure TEditorWidget.mnuedDdocClick(Sender: TObject);
begin
  if fDoc.isNil then exit;
  mnuEditor.Close;
  fDoc.hideCallTips;
  if not fDoc.IsDSource and not fDoc.alwaysAdvancedFeatures then
    exit;
  fDoc.showDDocs;
end;

procedure TEditorWidget.mnuedPasteClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecPaste, '', nil);
end;

procedure TEditorWidget.mnuedUndoClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecUndo, '', nil);
end;

procedure TEditorWidget.mnuedRedoClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    fDoc.ExecuteCommand(ecRedo, '', nil);
end;

procedure TEditorWidget.mnuedJum2DeclClick(Sender: TObject);
begin
  if fDoc.isNotNil then
    getSymbolLoc;
end;

procedure TEditorWidget.mnuEditorPopup(Sender: TObject);
begin
  if fDoc.isNil then
    exit;

  mnuedCut.Enabled:=fDoc.SelAvail;
  mnuedPaste.Enabled:=fDoc.CanPaste;
  mnuedCopy.Enabled:=fDoc.SelAvail;
  mnuedUndo.Enabled:=fDoc.CanUndo;
  mnuedRedo.Enabled:=fDoc.CanRedo;
  mnuedJum2Decl.Enabled:=fDoc.isDSource;

  mnuEdSetSpaces.Checked:= eoTabsToSpaces in fDoc.Options;
  mnuEdSetTabs.Checked:= not (eoTabsToSpaces in fDoc.Options);
  mnuEdShowSpec.Checked:=eoShowSpecialChars in fDoc.Options;
  case fDoc.TabWidth of
    2: mnuEdTabWidth2.Checked:=true;
    3: mnuEdTabWidth3.Checked:=true;
    4: mnuEdTabWidth4.Checked:=true;
    5: mnuEdTabWidth5.Checked:=true;
    6: mnuEdTabWidth6.Checked:=true;
    7: mnuEdTabWidth7.Checked:=true;
    8: mnuEdTabWidth8.Checked:=true;
  end;
end;
{$ENDREGION}
end.
