unit u_synmemo;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, controls,lcltype, Forms, graphics, ExtCtrls, crc, process,
  SynEdit, SynPluginSyncroEdit, SynCompletion, SynEditKeyCmds, LazSynEditText,
  SynHighlighterLFM, SynEditHighlighter, SynEditMouseCmds, SynEditFoldedView,
  SynEditMarks, SynEditTypes, SynHighlighterJScript, SynBeautifier, dialogs,
  md5, Spin, LCLIntf, LazFileUtils, LMessages, SynHighlighterCpp, math,
  //SynEditMarkupFoldColoring,
  Clipbrd, fpjson, jsonparser, LazUTF8, LazUTF8Classes, Buttons, StdCtrls,
  LGHelpers, LGVector, u_common,  u_writableComponent, u_d2syn, u_txtsyn, u_dialogs, u_dastworx,
  u_sharedres, u_dlang, u_stringrange, u_dbgitf, u_observer, u_diff,
  u_processes;

type

  TDexedMemo = class;

  TIdentifierMatchOption = (
    caseSensitive = longInt(ssoMatchCase),
    wholeWord = longInt(ssoWholeWord)
  );

  TBraceAutoCloseStyle = (
    autoCloseNever,
    autoCloseAtEof,
    autoCloseAlways,
    autoCloseLexically,
    autoCloseOnNewLineEof,
    autoCloseOnNewLineAlways,
    autoCloseOnNewLineLexically
  );

  TAutoClosedPair = (
    autoCloseSingleQuote,
    autoCloseDoubleQuote,
    autoCloseBackTick,
    autoCloseSquareBracket
  );

  TAutoClosePairs = set of TAutoClosedPair;

const

  autoClosePair2Char: array[TAutoClosedPair] of char = (#39, '"', '`', ']');

type

  TIdentifierMatchOptions = set of TIdentifierMatchOption;

  // Simple THintWindow descendant allowing the font size to be in sync with the editor.
  TEditorHintWindow = class(THintWindow)
  public
    class var FontSize: Integer;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
  end;

  // Specialized to allow displaying call tips, actual param in bold
  TEditorCallTipWindow = class(TEditorHintWindow)
  strict private
    fIndexOfExpectedArg: integer;
  public
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    procedure Paint; override;
    property indexOfExpectedArg: integer write fIndexOfExpectedArg;
  end;

  // Stores the state of a particular source code folding.
  TFoldCache = class(TCollectionItem)
  private
    fCollapsed: boolean;
    fLineIndex: Integer;
    fNestedIndex: Integer;
  published
    property isCollapsed: boolean read fCollapsed   write fCollapsed;
    property lineIndex: Integer   read fLineIndex   write fLineIndex;
    property nestedIndex: Integer read fNestedIndex write fNestedIndex;
  end;

  // Stores the state of a document between two cessions.
  TSynMemoCache = class(TWritableLfmTextComponent)
  private
    fMemo: TDexedMemo;
    fFolds: TCollection;
    fCaretPosition: Integer;
    fSelectionEnd: Integer;
    fFontSize: Integer;
    fSourceFilename: string;
    procedure setFolds(someFolds: TCollection);
  published
    property caretPosition: Integer read fCaretPosition write fCaretPosition;
    property sourceFilename: string read fSourceFilename write fSourceFilename;
    property folds: TCollection read fFolds write setFolds;
    property selectionEnd: Integer read fSelectionEnd write fSelectionEnd;
    property fontSize: Integer read fFontSize write fFontSize;
  public
    constructor create(aComponent: TComponent); override;
    destructor destroy; override;
    procedure beforeSave; override;
    procedure afterLoad; override;
    procedure save;
    procedure load;
  end;

  // Caret positions buffer allowing to jump fast to the most recent locations.
  // Replaces the bookmarks.
  TSynMemoPositions = class
  private
    fPos: Integer;
    fMax: Integer;
    fList: specialize TGLiteVector<PtrInt>;
    fMemo: TCustomSynEdit;
  public
    constructor create(memo: TCustomSynEdit);
    destructor destroy; override;
    procedure store;
    procedure back;
    procedure next;
  end;

  PDscannerResult = ^TDscannerResult;
  TDscannerResult = record
    warning: string;
    line, column: integer;
  end;

  TDscannerResults = class
  private
    fList: specialize TGLiteVector<PDscannerResult>;
    function getItem(const Index: PtrInt): PDscannerResult; {$IFNDEF DEBUG}inline;{$ENDIF}
    function getCount: PtrInt; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    constructor create;
    destructor destroy; override;
    procedure clear;
    procedure push(const warning: string; line, column: integer);
    property count: PtrInt read getCount;
    property Item[const Index: PtrInt]: PDscannerResult read getItem; default;
  end;

  TSortDialog = class;

  TGutterIcon = (
    giBreakSet    = 0,          // breakpoint set here
    giBulletGreen = 1,
    giBulletBlack = 2,
    giBreakReached= 3,          // break point reached
    giStep        = 4,          // step / signal / pause
    giWatch       = 5,          // watch point reached
    giWarn        = 6           // Dscanner result with text hint
  );

  const debugTimeGutterIcons = [giBreakReached, giStep, giWatch];

type

  //TODO-cGDB: add a system allowing to define watch points

  // Partial read-only editor displayed as scroll hint
  TScrollMemo = class(TPanel)
  private
    fMemo: TSynEdit;
    fD2Hl: TSynD2Syn;
    fTxtHl: TSynTxtSyn;
    fCppHl: TSynCppSyn;
    fSource: TDexedMemo;
    procedure updateFromSource;
  protected
    procedure SetVisible(Value: Boolean); override;
  public
    constructor construct(editor: TDexedMemo);
    procedure goToLine(value: integer);
  end;

  { TDexedMemo }

  TDexedMemo = class(TSynEdit, IDebugObserver)
  private
    //fIndentGuideMarkup: TSynEditMarkupFoldColors;
    fLifeTimeManager: TObject;
    fIdentDialShown: boolean;
    fScrollMemo: TScrollMemo;
    fFilename: string;
    fDastWorxExename: string;
    fModified: boolean;
    fFileDate: double;
    fCacheLoaded: boolean;
    fIsDSource: boolean;
    fFocusForInput: boolean;
    fIdentifier: string;
    fTempFileName: string;
    fMultiDocSubject: TObject;
    fDefaultFontSize: Integer;
    fPositions: TSynMemoPositions;
    fMousePos: TPoint;
    fCallTipWin: TEditorCallTipWindow;
    fDDocWin: TEditorHintWindow;
    fDDocDelay: Integer;
    fAutoDotDelay: Integer;
    fDscannerDelay: Integer;
    fDDocTimer: TIdleTimer;
    fAutoDotTimer: TIdleTimer;
    fDscannerTimer: TIdleTimer;
    fCanShowHint: boolean;
    fCanAutoDot: boolean;
    fOldMousePos: TPoint;
    fSyncEdit: TSynPluginSyncroEdit;
    fCompletion: TSynCompletion;
    fD2Highlighter: TSynD2Syn;
    fTxtHighlighter: TSynTxtSyn;
    fCppHighlighter: TSynCppSyn;
    fImages: TImageList;
    fMatchSelectionOpts: TSynSearchOptions;
    fMatchIdentOpts: TSynSearchOptions;
    fMatchOpts: TIdentifierMatchOptions;
    fCallTipStrings: TStringList;
    fOverrideColMode: boolean;
    fAutoCloseCurlyBrace: TBraceAutoCloseStyle;
    fSmartDdocNewline: boolean;
    fLexToks: TLexTokenList;
    fDisableFileDateCheck: boolean;
    fDetectIndentMode: boolean;
    fPhobosDocRoot: string;
    fAlwaysAdvancedFeatures: boolean;
    fIsProjectDescription: boolean;
    fAutoClosedPairs: TAutoClosePairs;
    fSortDialog: TSortDialog;
    fModuleTokFound: boolean;
    fHasModuleDeclaration: boolean;
    fLastCompletion: string;
    fDebugger: IDebugger;
    fInsertPlusDdoc: boolean;
    fAutoCallCompletion: boolean;
    fCloseCompletionCharsWithSpace: TSysCharSet;
    fCloseCompletionChars: TSysCharSet;
    fCompletionMenuAutoClose: boolean;
    fTransparentGutter: boolean;
    fDscanner: TDexedProcess;
    fDscannerResults: TDscannerResults;
    fCanDscan: boolean;
    fKnowsDscanner: boolean;
    fDscannerEnabled: boolean;
    fScrollPreview: boolean;
    fDiffDialogWillClose: boolean;
    procedure showHintEvent(Sender: TObject; HintInfo: PHintInfo);
    procedure setGutterTransparent(value: boolean);
    procedure decCallTipsLvl;
    procedure setMatchOpts(value: TIdentifierMatchOptions);
    function getMouseBytePosition: Integer;
    procedure changeNotify(Sender: TObject);
    procedure highlightCurrentIdentifier;
    procedure saveCache;
    procedure loadCache;
    class procedure cleanCache; static;
    procedure setDefaultFontSize(value: Integer);
    procedure DDocTimerEvent(sender: TObject);
    procedure AutoDotTimerEvent(sender: TObject);
    procedure dscannerTimerEvent(sender: TObject);
    procedure dscannerTerminate(sender: TObject);
    procedure removeDscannerWarnings;
    function getDscannerWarning(line: integer): string;
    function lineHasDscannerWarning(line: integer): boolean;
    procedure InitHintWins;
    function getIfTemp: boolean;
    procedure setDDocDelay(value: Integer);
    procedure setAutoDotDelay(value: Integer);
    procedure completionExecute(sender: TObject);
    procedure completionDeleteKey(sender: TObject);
    procedure getCompletionList;
    procedure completionFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function completionItemPaint(const AKey: string; ACanvas: TCanvas;X, Y: integer;
      Selected: boolean; Index: integer): boolean;
    procedure completionCodeCompletion(var value: string; SourceValue: string;
      var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
    procedure showCallTipsString(const tips: string; indexOfExpected: integer);
    function lexCanCloseBrace: boolean;
    function canInsertLeadingDdocSymbol: char;
    procedure handleStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure goToChangedArea(next: boolean);
    procedure goToProtectionGroup(next: boolean);
    procedure goToWarning(next: boolean);
    procedure autoClosePair(value: TAutoClosedPair);
    procedure setSelectionOrWordCase(upper: boolean);
    procedure sortSelectedLines(descending, caseSensitive: boolean);
    procedure tokFoundForCaption(const token: PLexToken; out stop: boolean);
    procedure addGutterIcon(line: integer; value: TGutterIcon);
    procedure removeGutterIcon(line: integer; value: TGutterIcon);
    procedure patchClipboardIndentation;
    procedure gotoWordEdge(right: boolean);
    //
    procedure gutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
    procedure removeDebugTimeMarks;
    function  isGutterIconSet(line: integer; value: TGutterIcon): boolean;
    function  findBreakPoint(line: integer): boolean;
    procedure debugStart(debugger: IDebugger);
    procedure debugStop;
    procedure debugContinue;
    function debugQueryBpCount: integer;
    procedure debugQueryBreakPoint(const line: integer; out fname: string; out kind: TBreakPointKind);
    procedure debugBreak(const fname: string; line: integer; reason: TDebugBreakReason);
    function breakPointsCount: integer;
    procedure tryToPatchMixedIndentation;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand; var AChar: TUTF8Char;
      Data: pointer); override;
    procedure MouseLeave; override;
    procedure SetVisible(Value: Boolean); override;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure SetHighlighter(const Value: TSynCustomHighlighter); override;
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure setFocus; override;
    procedure showPage;
    //
    function pageCaption(checkModule: boolean): string;
    procedure checkFileDate;
    procedure loadFromFile(const fname: string);
    procedure saveToFile(const fname: string);
    procedure save;
    procedure saveTempFile;
    //
    function indentationMode(out numTabs, numSpaces: integer): TIndentationMode;
    procedure forceIndentation(m: TIndentationMode; w: integer);
    procedure addBreakPoint(line: integer);
    procedure removeBreakPoint(line: integer);
    procedure curlyBraceCloseAndIndent;
    procedure insertLeadingDDocSymbol(c: char);
    procedure commentSelection;
    procedure commentIdentifier;
    procedure renameIdentifier;
    procedure invertVersionAllNone;
    procedure showCallTips(findOpenParen: boolean = true);
    procedure hideCallTips;
    procedure showDDocs;
    procedure hideDDocs;
    procedure ShowPhobosDoc;
    procedure previousChangedArea;
    procedure nextChangedArea;
    procedure previousProtectionGroup;
    procedure nextProtectionGroup;
    procedure previousWarning;
    procedure nextWarning;
    procedure sortLines;
    procedure addCurLineBreakPoint;
    procedure removeCurLineBreakPoint;
    procedure toggleCurLineBreakpoint;
    procedure insertDdocTemplate;
    procedure gotoLinePrompt;
    procedure showWarningForLine(line: integer);
    procedure showCurLineWarning;
    function implementMain: THasMain;
    procedure replaceUndoableContent(const value: string);
    procedure setDscannerOptions(dsEnabled: boolean; dsDelay: integer);
    //
    property IdentifierMatchOptions: TIdentifierMatchOptions read fMatchOpts write setMatchOpts;
    property Identifier: string read fIdentifier;
    property fileName: string read fFilename;
    property modified: boolean read fModified;
    property tempFilename: string read fTempFileName;
    //
    property completionMenu: TSynCompletion read fCompletion;
    property syncroEdit: TSynPluginSyncroEdit read fSyncEdit;
    property isDSource: boolean read fIsDSource;
    property isTemporary: boolean read getIfTemp;
    property TextView;
    //
    property transparentGutter: boolean read fTransparentGutter write setGutterTransparent;
    property isProjectDescription: boolean read fIsProjectDescription write fIsProjectDescription;
    property alwaysAdvancedFeatures: boolean read fAlwaysAdvancedFeatures write fAlwaysAdvancedFeatures;
    property phobosDocRoot: string read fPhobosDocRoot write fPhobosDocRoot;
    property detectIndentMode: boolean read fDetectIndentMode write fDetectIndentMode;
    property disableFileDateCheck: boolean read fDisableFileDateCheck write fDisableFileDateCheck;
    property MouseBytePosition: Integer read getMouseBytePosition;
    property D2Highlighter: TSynD2Syn read fD2Highlighter;
    property TxtHighlighter: TSynTxtSyn read fTxtHighlighter;
    property CppHighlighter: TSynCppSyn read fCppHighlighter;
    property defaultFontSize: Integer read fDefaultFontSize write setDefaultFontSize;
    property ddocDelay: Integer read fDDocDelay write setDDocDelay;
    property autoDotDelay: Integer read fAutoDotDelay write setAutoDotDelay;
    property autoCloseCurlyBrace: TBraceAutoCloseStyle read fAutoCloseCurlyBrace write fAutoCloseCurlyBrace;
    property autoClosedPairs: TAutoClosePairs read fAutoClosedPairs write fAutoClosedPairs;
    property smartDdocNewline: boolean read fSmartDdocNewline write fSmartDdocNewline;
    property insertPlusDdoc: boolean read fInsertPlusDdoc write fInsertPlusDdoc;
    property autoCallCompletion: boolean read fAutoCallCompletion write fAutoCallCompletion;
    property closeCompletionCharsWithSpace: TSysCharSet read fCloseCompletionCharsWithSpace write fCloseCompletionCharsWithSpace;
    property closeCompletionChars: TSysCharSet read fCloseCompletionChars write fCloseCompletionChars;
    property completionMenuAutoClose: boolean read fCompletionMenuAutoClose write fCompletionMenuAutoClose;
    property scrollPreview: boolean read fScrollPreview write fScrollPreview;
  end;

  TSortDialog = class(TForm)
  private
    class var fDescending: boolean;
    class var fCaseSensitive: boolean;
    fEditor: TDexedMemo;
    fCanUndo: boolean;
    procedure btnApplyClick(sender: TObject);
    procedure btnUndoClick(sender: TObject);
    procedure chkCaseSensClick(sender: TObject);
    procedure chkDescClick(sender: TObject);
  public
    constructor construct(editor: TDexedMemo);
  end;

  TMixedIndentationDialog = class(TForm)
  private
    class var fSpacesPerTab: integer;
    procedure spinSpacesPerTabChange(sender: TObject);
  public
    constructor construct(numSpaces, numTabs: integer);
  end;

  procedure SetDefaultDexedKeystrokes(ed: TSynEdit);

  function CustomStringToCommand(const Ident: string; var Int: Longint): Boolean;
  function CustomCommandToSstring(Int: Longint; var Ident: string): Boolean;

const
  ecCompletionMenu      = ecUserFirst + 1;
  ecJumpToDeclaration   = ecUserFirst + 2;
  ecPreviousLocation    = ecUserFirst + 3;
  ecNextLocation        = ecUserFirst + 4;
  ecRecordMacro         = ecUserFirst + 5;
  ecPlayMacro           = ecUserFirst + 6;
  ecShowDdoc            = ecUserFirst + 7;
  ecShowCallTips        = ecUserFirst + 8;
  ecCurlyBraceClose     = ecUserFirst + 9;
  ecCommentSelection    = ecUserFirst + 10;
  ecSwapVersionAllNone  = ecUserFirst + 11;
  ecRenameIdentifier    = ecUserFirst + 12;
  ecCommentIdentifier   = ecUserFirst + 13;
  ecShowPhobosDoc       = ecUserFirst + 14;
  ecPreviousChangedArea = ecUserFirst + 15;
  ecNextChangedArea     = ecUserFirst + 16;
  ecUpperCaseWordOrSel  = ecUserFirst + 17;
  ecLowerCaseWordOrSel  = ecUserFirst + 18;
  ecSortLines           = ecUserFirst + 19;
  ecPrevProtGrp         = ecUserFirst + 20;
  ecNextProtGrp         = ecUserFirst + 21;
  ecAddBreakpoint       = ecUserFirst + 22;
  ecRemoveBreakpoint    = ecUserFirst + 23;
  ecToggleBreakpoint    = ecUserFirst + 24;
  ecInsertDdocTemplate  = ecUserFirst + 25;
  ecNextWarning         = ecUserFirst + 26;
  ecPrevWarning         = ecUserFirst + 27;
  ecGotoLine            = ecUserFirst + 28;
  ecShowCurlineWarning  = ecUserFirst + 29;
  ecLeftWordEdge        = ecUserFirst + 30;
  ecRightWordEdge       = ecUserFirst + 31;
  ecSelLeftWordEdge     = ecUserFirst + 32;
  ecSelRightWordEdge    = ecUserFirst + 33;
var
  D2Syn: TSynD2Syn;     // used as model to set the options when no editor exists.
  TxtSyn: TSynTxtSyn;   // used as model to set the options when no editor exists.
  LfmSyn: TSynLfmSyn;   // used to highlight the native projects.
  JsSyn: TSynJScriptSyn;// used to highlight the DUB JSON projects.


implementation

uses
  u_interfaces, u_dcd, SynEditHighlighterFoldBase, u_lcldragdrop;

const
  DcdCompletionKindStrings: array[TDCDCompletionKind] of string = (
    ' (class)            ',
    ' (interface)        ',
    ' (struct)           ',
    ' (union)            ',
    ' (variable)         ',
    ' (member)           ',
    ' (reserved word)    ',
    ' (function)         ',
    ' (enum)             ',
    ' (enum member)      ',
    ' (package)          ',
    ' (module)           ',
    ' (array)            ',
    ' (associative array)',
    ' (alias)            ',
    ' (template)         ',
    ' (mixin)            '
  );

function TEditorHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect;
begin
  Font.Size:= FontSize;
  result := inherited CalcHintRect(MaxWidth, AHint, AData);
end;

function TEditorCallTipWindow.CalcHintRect(MaxWidth: Integer; const AHint: String; AData: Pointer): TRect;
begin
  //Font.Style := Font.Style + [fsBold];
  result := inherited CalcHintRect(MaxWidth, AHint, AData);
  //Font.Style := Font.Style - [fsBold];
end;

procedure TEditorCallTipWindow.Paint;
//var
  //s: string;
  //a: string;
  //i: integer = 0;
  //x: integer = 0;
  //o: integer = 0;
  //r: TStringRange = (ptr:nil; pos:0; len: 0);
  //f: TFontStyles;
begin
  //s := caption;
  //caption := '';
  inherited Paint;
  //if s.isEmpty then
  //  exit;
  //f := canvas.Font.Style;
  //r.init(s);
  //// func decl (TODO skip template params)
  //a := r.takeUntil('(').yield + '(';
  //o := x;
  //x += canvas.TextWidth(a);
  //canvas.TextOut(o, 0, a);
  //r.popFront;
  //// func args
  //while not r.empty do
  //begin
  //  a := r.takeUntil(',').yield;
  //  if not r.empty then
  //  begin
  //    r.popFrontN(2);
  //    a += ', ';
  //  end;
  //  o := x;
  //  if fIndexOfExpectedArg = i then
  //    canvas.Font.Style := canvas.Font.Style + [fsBold]
  //  else
  //    canvas.Font.Style := canvas.Font.Style - [fsBold];
  //  x += canvas.TextWidth(a);
  //  canvas.TextOut(o, 0, a);
  //  canvas.Font.Style := f;
  //  i += 1;
  //end;
end;

{$REGION TSortDialog -----------------------------------------------------------}
constructor TSortDialog.construct(editor: TDexedMemo);
var
  pnl: TPanel;
begin
  inherited Create(nil);
  BorderStyle:= bsToolWindow;
  fEditor := editor;

  width := 150;
  Height:= 95;
  FormStyle:= fsStayOnTop;
  BorderStyle:= bsToolWindow;
  Position:= poScreenCenter;
  ShowHint:=true;

  with TCheckBox.Create(self) do
  begin
    parent := self;
    BorderSpacing.Around:=2;
    OnClick:=@chkCaseSensClick;
    Caption:='case sensitive';
    checked := fCaseSensitive;
    align := alTop;
  end;

  with TCheckBox.Create(self) do
  begin
    parent := self;
    BorderSpacing.Around:=2;
    OnClick:=@chkDescClick;
    Caption:='descending';
    Checked:= fDescending;
    align := alTop;
  end;

  pnl := TPanel.Create(self);
  pnl.Parent := self;
  pnl.Align:=alBottom;
  pnl.Caption:='';
  pnl.Height:= 32;
  pnl.BevelOuter:=bvLowered;

  with TSpeedButton.Create(self) do
  begin
    parent := pnl;
    BorderSpacing.Around:=2;
    OnClick:=@btnUndoClick;
    align := alRight;
    width := 28;
    Hint := 'undo changes';
    AssignPng(Glyph, 'ARROW_UNDO');
  end;

  with TSpeedButton.Create(self) do
  begin
    parent := pnl;
    BorderSpacing.Around:=2;
    OnClick:=@btnApplyClick;
    align := alRight;
    width := 28;
    Hint := 'apply sorting';
    AssignPng(Glyph, 'ACCEPT');
  end;
end;

procedure TSortDialog.btnApplyClick(sender: TObject);
begin
  fEditor.sortSelectedLines(fDescending, fCaseSensitive);
  fCanUndo:= true;
end;

procedure TSortDialog.btnUndoClick(sender: TObject);
begin
  if fCanUndo then
    fEditor.undo;
  fCanUndo:= false;
end;

procedure TSortDialog.chkCaseSensClick(sender: TObject);
begin
  fCaseSensitive := TCheckBox(sender).checked;
end;

procedure TSortDialog.chkDescClick(sender: TObject);
begin
  fDescending := TCheckBox(sender).checked;
end;
{$ENDREGION}

{$REGION TMixedIndentationDialog}
constructor TMixedIndentationDialog.construct(numSpaces, numTabs: integer);
var
  pn: TPanel;
begin
  inherited create(nil);
  BorderStyle:= bsToolWindow;
  caption := 'Indentation converter';
  Position:= TPosition.poMainFormCenter;
  fSpacesPerTab := 4;
  with TButton.Create(self) do
  begin
    Align:= alBottom;
    parent := self;
    caption := 'Do nothing';
    AutoSize:= true;
    ModalResult:= 1;
    BorderSpacing.Around:=4;
  end;
  pn := TPanel.Create(self);
  pn.Align:= alTop;
  pn.parent := self;
  pn.Caption:='';
  pn.AutoSize:=true;
  pn.BevelInner:= bvNone;
  pn.BevelOuter:= bvNone;
  pn.BorderSpacing.Around:=4;
  pn.ParentColor:=true;
  with TSpinEdit.Create(self) do
  begin
    value := fSpacesPerTab;
    Align:= alLeft;
    parent := pn;
    MinValue:= 1;
    MaxValue:= 8;
    AutoSize:= true;
    OnChange:= @spinSpacesPerTabChange;
    hint := 'defines how many spaces per TAB will be used';
    ShowHint:=true;
  end;
  with TLabel.Create(self) do
  begin
    parent := pn;
    AutoSize:=true;
    Caption:= 'Spaces per TAB';
    Align:= alClient;
    Layout:= TTextLayout.tlCenter;
    BorderSpacing.Left:= 4;
  end;
  with TButton.Create(self) do
  begin
    Align:= alTop;
    parent := self;
    caption := 'Always use tabs';
    AutoSize:= true;
    ModalResult:= 10;
    BorderSpacing.Around:=4;
  end;
  with TButton.Create(self) do
  begin
    Align:= alTop;
    parent := self;
    caption := 'Always use spaces';
    AutoSize:= true;
    ModalResult:= 11;
    BorderSpacing.Around:=4;
  end;
  with TLabel.Create(self) do
  begin
    Align := alTop;
    parent  := self;
    AutoSize :=true;
    BorderSpacing.Around:=8;
    caption := format('this document is%s- indented %d times by TAB%s- indented %d times by (n)spaces',
      [#13#10, numTabs, #13#10, numSpaces]);
  end;
  width := ScaleX(280, 96);
  height := ScaleY(200, 96);
end;

procedure TMixedIndentationDialog.spinSpacesPerTabChange(sender: TObject);
begin
  self.fSpacesPerTab:= TSpinEdit(sender).Value;
end;
{$ENDREGION}

{$REGION TSynMemoCache -------------------------------------------------------}
constructor TSynMemoCache.create(aComponent: TComponent);
begin
  inherited create(nil);
  if (aComponent is TDexedMemo) then
  	fMemo := TDexedMemo(aComponent);
  fFolds := TCollection.Create(TFoldCache);
end;

destructor TSynMemoCache.destroy;
begin
  fFolds.Free;
  inherited;
end;

procedure TSynMemoCache.setFolds(someFolds: TCollection);
begin
  fFolds.Assign(someFolds);
end;

procedure TSynMemoCache.beforeSave;
var
  i, start, prev: Integer;
  itm : TFoldCache;
begin
  if fMemo.isNil then
    exit;

  fCaretPosition := fMemo.SelStart;
  fSourceFilename := fMemo.fileName;
  fSelectionEnd := fMemo.SelEnd;
  fFontSize := fMemo.Font.Size;
  TEditorHintWindow.FontSize := fMemo.Font.Size;

  prev := fMemo.Lines.Count-1;
  for i := fMemo.Lines.Count-1 downto 0 do
  begin
    // - CollapsedLineForFoldAtLine() does not handle the sub-folding.
    // - TextView visibility is increased so this is not the standard way of getting the infos.
    start := fMemo.TextView.CollapsedLineForFoldAtLine(i);
    if start = -1 then
      continue;
    if start = prev then
      continue;
    prev := start;
    itm := TFoldCache(fFolds.Add);
    itm.isCollapsed := true;
    itm.fLineIndex := start;
  end;
end;

procedure TSynMemoCache.afterLoad;
var
  i: integer;
  itm : TFoldCache;
begin
  if fMemo.isNil then
    exit;

  if fFontSize > 0 then
    fMemo.Font.Size := fFontSize;

  // Currently collisions are not handled.
  if fMemo.fileName <> fSourceFilename then
    exit;

  for i := 0 to fFolds.Count-1 do
  begin
    itm := TFoldCache(fFolds.Items[i]);
    if not itm.isCollapsed then
      continue;
    fMemo.TextView.FoldAtLine(itm.lineIndex-1);
  end;

  fMemo.SelStart := fCaretPosition;
  fMemo.SelEnd := fSelectionEnd;
end;

{$IFDEF DEBUG}{$R-}{$ENDIF}
procedure TSynMemoCache.save;
var
  fname: string;
  tempn: string;
  chksm: Cardinal;
begin
  tempn := fMemo.fileName;
  if (tempn = fMemo.tempFilename) or (not tempn.fileExists) then
    exit;

  fname := getDocPath + 'editorcache' + DirectorySeparator;
  ForceDirectories(fname);
  chksm := crc32(0, nil, 0);
  chksm := crc32(chksm, @tempn[1], tempn.length);
  fname := fname + format('%.8X.txt', [chksm]);
  saveToFile(fname);
end;

procedure TSynMemoCache.load;
var
  fname: string;
  tempn: string;
  chksm: Cardinal;
begin
  tempn := fMemo.fileName;
  if not tempn.fileExists then
    exit;

  fname := getDocPath + 'editorcache' + DirectorySeparator;
  chksm := crc32(0, nil, 0);
  chksm := crc32(chksm, @tempn[1], tempn.length);
  fname := fname + format('%.8X.txt', [chksm]);

  if not fname.fileExists then
    exit;
  loadFromFile(fname);
end;
{$IFDEF DEBUG}{$R+}{$ENDIF}
{$ENDREGION}

{$REGION TSynMemoPositions ---------------------------------------------------}
constructor TSynMemoPositions.create(memo: TCustomSynEdit);
begin
  fMax  := 40;
  fMemo := memo;
  fPos  := -1;
end;

destructor TSynMemoPositions.destroy;
begin
  inherited Destroy;
end;

procedure TSynMemoPositions.back;
begin
  Inc(fPos);
  {$HINTS OFF}
  if fPos < fList.Count then
    fMemo.CaretY := fList[fPos]
  {$HINTS ON}
  else Dec(fPos);
end;

procedure TSynMemoPositions.next;
begin
  Dec(fPos);
  {$HINTS OFF}
  if fPos > -1 then
    fMemo.CaretY := fList[fPos]
  {$HINTS ON}
  else Inc(fPos);
end;

procedure TSynMemoPositions.store;
var
  delta: NativeInt;
const
  thresh = 6;
begin
  fPos := 0;
  {$PUSH}
  {$HINTS OFF}{$WARNINGS OFF}
  if fList.Count > 0 then
  begin
    delta := fMemo.CaretY - fList[fPos];
    if (delta > -thresh) and (delta < thresh) then exit;
  end;
  fList.Insert(0, fMemo.CaretY);
  {$POP}
  while fList.Count > fMax do
    fList.DeleteItem(fList.Count-1);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION TScrollMemo ---------------------------------------------------------}
constructor TScrollMemo.construct(editor: TDexedMemo);
begin
  inherited create(editor);
  visible := false;

  BevelOuter:= bvNone;
  BevelInner:= bvNone;
  parent := Application.MainForm;
  width := scaleX(475, 96);
  height := scaleY(275, 96);

  fMemo:= TSynEdit.Create(self);
  fMemo.Parent := self;
  fMemo.Align:= alCLient;
  fMemo.ReadOnly:=true;
  fMemo.ScrollBars:=ssNone;
  fMemo.MouseActions.Clear;
  fMemo.Keystrokes.Clear;
  fMemo.CaptureMouseButtons:=[];
  fMemo.Options:=fMemo.Options+[eoNoCaret];

  fD2Hl:= TSynD2Syn.create(self);
  fTxtHl:= TSynTxtSyn.create(self);
  fCppHl:= TSynCppSyn.create(self);
  fSource:= editor;
  updateFromSource();
end;

procedure TScrollMemo.updateFromSource;
begin
  fMemo.Font.Assign(fSource.Font);
  fMemo.Lines := fSource.Lines;
  width := fSource.Width div 2;
  if fSource.Highlighter.isNotNil then
  begin
    fMemo.Color:= fSource.Color;
    fMemo.LineHighlightColor.Assign(fSource.LineHighlightColor);
    fMemo.SelectedColor.Assign(fSource.SelectedColor);
    if fMemo.Highlighter.isNil then
    begin
      fD2Hl.Assign(fSource.Highlighter);
      fTxtHl.Assign(fSource.Highlighter);
      fCppHl.Assign(fSource.Highlighter);
    end;
    if fSource.Highlighter is TSynD2Syn then
      fMemo.Highlighter := fD2Hl
    else if fSource.Highlighter is TSynCppSyn then
      fMemo.Highlighter := fCppHl
    else  if fSource.Highlighter is TSynD2Syn then
      fMemo.Highlighter := fTxtHl;
  end;
end;

procedure TScrollMemo.SetVisible(Value: Boolean);
var
  o: boolean;
begin
  o := Visible;
  inherited;
  if (o <> value) and value then
    updateFromSource;
end;

procedure TScrollMemo.goToLine(value: integer);
begin
  if fMemo.PaintLock <> 0 then
    exit;
  if value > fMemo.Lines.Count then
    value := fMemo.Lines.Count
  else if value < 1 then
    value := 1;
  fMemo.CaretY := value;
  fMemo.CaretX := 1;
  fMemo.SelectLine(true);
end;
{$ENDREGION}

{$REGION TDexedMemo ------------------------------------------------------------}

{$REGION Standard Obj and Comp -------------------------------------------------}
constructor TDexedMemo.Create(aOwner: TComponent);
var
  z: TIconScaledSize;
  i: ILifetimeManager;
begin
  inherited;

  fScrollMemo := TScrollMemo.construct(self);

  i := getLifeTimeManager();
  if (i <> nil) then
    fLifeTimeManager := i.asObject;

  OnShowHint:= @showHintEvent;
  OnStatusChange:= @handleStatusChanged;
  fDefaultFontSize := 10;
  Font.Size:=10;
  SetDefaultDexedKeystrokes(Self); // not called in inherited if owner = nil !
  fLexToks:= TLexTokenList.Create;
  fSmartDdocNewline := true;

  OnDragDrop:= @ddHandler.DragDrop;
  OnDragOver:= @ddHandler.DragOver;

  ShowHint := false;
  InitHintWins;
  fDDocDelay := 200;
  fDDocTimer := TIdleTimer.Create(self);
  fDDocTimer.AutoEnabled:=true;
  fDDocTimer.Interval := fDDocDelay;
  fDDocTimer.OnTimer := @DDocTimerEvent;

  fAutoDotDelay := 100;
  fAutoDotTimer := TIdleTimer.Create(self);
  fAutoDotTimer.AutoEnabled:=true;
  fAutoDotTimer.Interval := fAutoDotDelay;
  fAutoDotTimer.OnTimer := @AutoDotTimerEvent;

  fDscannerDelay := 500;
  fDscannerTimer := TIdleTimer.Create(self);
  fDscannerTimer.AutoEnabled:=true;
  fDscannerTimer.Interval := fDscannerDelay;
  fDscannerTimer.OnTimer := @dscannerTimerEvent;
  fDscanner := TDexedProcess.create(self);
  fDscanner.Executable:= exeFullName('dscanner' + exeExt);
  fDscanner.Options:=[poUsePipes];
  fDscanner.ShowWindow:=swoHIDE;
  fDscanner.OnTerminate:=@dscannerTerminate;
  fDscanner.Parameters.add('-S');
  fDscanner.Parameters.add('stdin');
  fDscannerResults:= TDscannerResults.create;
  fKnowsDscanner := fDscanner.Executable.fileExists;

  Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf := 5;
  Gutter.LineNumberPart.MarkupInfo.Foreground := clWindowText;
  Gutter.LineNumberPart.MarkupInfo.Background := clBtnFace;
  Gutter.SeparatorPart.LineOffset := 0;
  Gutter.SeparatorPart.LineWidth := 1;
  Gutter.OnGutterClick:= @gutterClick;
  BracketMatchColor.Foreground:=clRed;

  fSyncEdit := TSynPluginSyncroEdit.Create(self);
  fSyncEdit.Editor := self;
  fSyncEdit.CaseSensitive := true;

  fCompletion := TSyncompletion.create(nil);
  fCompletion.ShowSizeDrag := true;
  fCompletion.Editor := Self;
  fCompletion.OnExecute:= @completionExecute;
  fCompletion.OnCodeCompletion:=@completionCodeCompletion;
  fCompletion.OnPaintItem:= @completionItemPaint;
  fCompletion.OnKeyDelete:= @completionDeleteKey;
  fCompletion.TheForm.OnKeyDown:= @completionFormKeyDown;
  fCompletion.CaseSensitive:=true;
  TStringList(fCompletion.ItemList).CaseSensitive:=true;
  fCompletion.LongLineHintType:=sclpNone;
  fCompletion.TheForm.ShowInTaskBar:=stNever;
  fCompletion.ShortCut:=0;
  fCompletion.LinesInWindow:=15;
  fCompletion.Width:= 250;
  fCallTipStrings:= TStringList.Create;

  MouseLinkColor.Style:= [fsUnderline];
  with MouseActions.Add do begin
    Command := emcMouseLink;
    shift := [ssCtrl];
    ShiftMask := [ssCtrl];
  end;

  fD2Highlighter := TSynD2Syn.create(self);
  fTxtHighlighter := TSynTxtSyn.Create(self);
  fCppHighlighter := TSynCppSyn.Create(self);
  Highlighter := fD2Highlighter;

  fTempFileName := GetTempDir(false) + 'temp_' + uniqueObjStr(self) + '.d';
  fFilename := '<new document>';
  fModified := false;
  TextBuffer.AddNotifyHandler(senrUndoRedoAdded, @changeNotify);

  Gutter.MarksPart.AutoSize:=false;
  Gutter.MarksPart.Width := ScaleX(20,96);
  fImages := TImageList.Create(self);
  z := GetIconScaledSize;
  case z of
    iss16:
    begin
      fImages.Width:= 16;
      fImages.Height:= 16;
      fImages.AddResourceName(HINSTANCE, 'BREAK_SET');
      fImages.AddResourceName(HINSTANCE, 'BULLET_GREEN');
      fImages.AddResourceName(HINSTANCE, 'BULLET_BLACK');
      fImages.AddResourceName(HINSTANCE, 'BREAK_REACHED');
      fImages.AddResourceName(HINSTANCE, 'STEP');
      fImages.AddResourceName(HINSTANCE, 'CAMERA_GO');
      fImages.AddResourceName(HINSTANCE, 'WARNING');
      AssignPng(fSyncEdit.GutterGlyph, 'LINK_EDIT');
    end;
    iss24:
    begin
      fImages.Width:= 24;
      fImages.Height:= 24;
      fImages.AddResourceName(HINSTANCE, 'BREAK_SET24');
      fImages.AddResourceName(HINSTANCE, 'BULLET_GREEN24');
      fImages.AddResourceName(HINSTANCE, 'BULLET_BLACK24');
      fImages.AddResourceName(HINSTANCE, 'BREAK_REACHED24');
      fImages.AddResourceName(HINSTANCE, 'STEP24');
      fImages.AddResourceName(HINSTANCE, 'CAMERA_GO24');
      fImages.AddResourceName(HINSTANCE, 'WARNING24');
      AssignPng(fSyncEdit.GutterGlyph, 'LINK_EDIT24');
    end;
    iss32:
    begin
      fImages.Width:= 32;
      fImages.Height:= 32;
      fImages.AddResourceName(HINSTANCE, 'BREAK_SET32');
      fImages.AddResourceName(HINSTANCE, 'BULLET_GREEN32');
      fImages.AddResourceName(HINSTANCE, 'BULLET_BLACK32');
      fImages.AddResourceName(HINSTANCE, 'BREAK_REACHED32');
      fImages.AddResourceName(HINSTANCE, 'STEP32');
      fImages.AddResourceName(HINSTANCE, 'CAMERA_GO32');
      fImages.AddResourceName(HINSTANCE, 'WARNING32');
      AssignPng(fSyncEdit.GutterGlyph, 'LINK_EDIT32');
    end;
  end;

  fPositions := TSynMemoPositions.create(self);
  fMultiDocSubject := TMultiDocSubject.create;

  HighlightAllColor.Foreground := clNone;
  HighlightAllColor.Background := clSilver;
  HighlightAllColor.BackAlpha  := 70;
  IdentifierMatchOptions:= [caseSensitive];

  LineHighlightColor.Background := color - $080808;
  LineHighlightColor.Foreground := clNone;

  //fIndentGuideMarkup:= TSynEditMarkupFoldColors.Create(self);
  //MarkupManager.AddMarkUp(fIndentGuideMarkup);

  fAutoCloseCurlyBrace:= autoCloseOnNewLineLexically;
  fAutoClosedPairs:= [autoCloseSquareBracket];

  fDastWorxExename:= exeFullName('dastworx' + exeExt);

  fDebugger := EntitiesConnector.getSingleService('IDebugger') as IDebugger;

  subjDocNew(TMultiDocSubject(fMultiDocSubject), self);
  EntitiesConnector.addObserver(self);
end;

procedure TDexedMemo.WMKillFocus(var Message: TLMKillFocus);
begin
  if eoAutoHideCursor in options2 then
    inherited MouseMove([], 0, 0);
end;

destructor TDexedMemo.destroy;
begin
  saveCache;

  //fIndentGuideMarkup.Free;
  EntitiesConnector.removeObserver(self);
  subjDocClosing(TMultiDocSubject(fMultiDocSubject), self);
  fMultiDocSubject.Free;
  fPositions.Free;
  fCompletion.Free;
  fCallTipStrings.Free;
  fLexToks.Clear;
  fLexToks.Free;
  fSortDialog.Free;
  fDscannerResults.Free;

  if fTempFileName.fileExists then
    sysutils.DeleteFile(fTempFileName);

  inherited;
end;

procedure TDexedMemo.setGutterTransparent(value: boolean);
begin
  fTransparentGutter:=value;
  if fTransparentGutter then
  begin
    Gutter.LineNumberPart.MarkupInfo.Background:= Color;
    Gutter.SeparatorPart.MarkupInfo.Background:= Color;
    Gutter.MarksPart.MarkupInfo.Background:= Color;
    Gutter.ChangesPart.MarkupInfo.Background:= Color;
    Gutter.CodeFoldPart.MarkupInfo.Background:= Color;
    Gutter.Color:=Color;
  end
  else
  begin
    Gutter.LineNumberPart.MarkupInfo.Background:= clBtnFace;
    Gutter.SeparatorPart.MarkupInfo.Background:= clBtnFace;
    Gutter.MarksPart.MarkupInfo.Background:= clBtnFace;
    Gutter.ChangesPart.MarkupInfo.Background:= clBtnFace;
    Gutter.CodeFoldPart.MarkupInfo.Background:= clBtnFace;
    Gutter.Color:=clBtnFace;
  end;
end;

procedure TDexedMemo.setDefaultFontSize(value: Integer);
var
  old: Integer;
begin
  old := Font.Size;
  if value < 5 then
    value := 5;
  fDefaultFontSize:= value;
  if Font.Size = old then
    Font.Size := fDefaultFontSize;
end;

procedure TDexedMemo.setFocus;
begin
  inherited;
  highlightCurrentIdentifier;
  subjDocFocused(TMultiDocSubject(fMultiDocSubject), self);
end;

procedure TDexedMemo.showPage;
begin
  getMultiDocHandler.openDocument(fileName);
end;

procedure TDexedMemo.DoEnter;
begin
  inherited;
  checkFileDate;
  if not fFocusForInput then
    subjDocFocused(TMultiDocSubject(fMultiDocSubject), self);
  fFocusForInput := true;
  fScrollMemo.Visible:=false;
  tryToPatchMixedIndentation;
end;

procedure TDexedMemo.DoExit;
begin
  inherited;
  fFocusForInput := false;
  hideDDocs;
  hideCallTips;
  fScrollMemo.Visible:=false;
  if fCompletion.IsActive then
    fCompletion.Deactivate;
end;

procedure TDexedMemo.SetVisible(Value: Boolean);
begin
  inherited;
  if Value then
  begin
    setFocus;
    if not fCacheLoaded then
      loadCache;
    fCacheLoaded := true;
  end
  else
  begin
    hideDDocs;
    hideCallTips;
    fScrollMemo.Visible:=false;
    if fCompletion.IsActive then
      fCompletion.Deactivate;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION Custom editor commands and shortcuts ----------------------------------}
procedure SetDefaultDexedKeystrokes(ed: TSynEdit);
begin
  with ed do
  begin
    Keystrokes.Clear;

    AddKey(ecUp, VK_UP, [], 0, []);
    AddKey(ecSelUp, VK_UP, [ssShift], 0, []);
    AddKey(ecScrollUp, VK_UP, [ssCtrl], 0, []);
    AddKey(ecDown, VK_DOWN, [], 0, []);
    AddKey(ecSelDown, VK_DOWN, [ssShift], 0, []);
    AddKey(ecScrollDown, VK_DOWN, [ssCtrl], 0, []);
    AddKey(ecLeft, VK_LEFT, [], 0, []);
    AddKey(ecSelLeft, VK_LEFT, [ssShift], 0, []);
    AddKey(ecWordLeft, VK_LEFT, [ssCtrl], 0, []);
    AddKey(ecWordEndLeft, VK_LEFT, [ssCtrl,ssAlt], 0, []);
    AddKey(ecWordEndRight, VK_RIGHT, [ssCtrl,ssAlt], 0, []);
    AddKey(ecSelWordLeft, VK_LEFT, [ssShift,ssCtrl], 0, []);
    AddKey(ecRight, VK_RIGHT, [], 0, []);
    AddKey(ecSelRight, VK_RIGHT, [ssShift], 0, []);
    AddKey(ecWordRight, VK_RIGHT, [ssCtrl], 0, []);
    AddKey(ecSelWordRight, VK_RIGHT, [ssShift,ssCtrl], 0, []);
    AddKey(ecPageDown, VK_NEXT, [], 0, []);
    AddKey(ecSelPageDown, VK_NEXT, [ssShift], 0, []);
    AddKey(ecPageBottom, VK_NEXT, [ssCtrl], 0, []);
    AddKey(ecSelPageBottom, VK_NEXT, [ssShift,ssCtrl], 0, []);
    AddKey(ecPageUp, VK_PRIOR, [], 0, []);
    AddKey(ecSelPageUp, VK_PRIOR, [ssShift], 0, []);
    AddKey(ecPageTop, VK_PRIOR, [ssCtrl], 0, []);
    AddKey(ecSelPageTop, VK_PRIOR, [ssShift,ssCtrl], 0, []);
    AddKey(ecLineStart, VK_HOME, [], 0, []);
    AddKey(ecSelLineStart, VK_HOME, [ssShift], 0, []);
    AddKey(ecEditorTop, VK_HOME, [ssCtrl], 0, []);
    AddKey(ecSelEditorTop, VK_HOME, [ssShift,ssCtrl], 0, []);
    AddKey(ecLineEnd, VK_END, [], 0, []);
    AddKey(ecSelLineEnd, VK_END, [ssShift], 0, []);
    AddKey(ecEditorBottom, VK_END, [ssCtrl], 0, []);
    AddKey(ecSelEditorBottom, VK_END, [ssShift,ssCtrl], 0, []);
    AddKey(ecToggleMode, VK_INSERT, [], 0, []);
    AddKey(ecDeleteChar, VK_DELETE, [], 0, []);
    AddKey(ecDeleteLastChar, VK_BACK, [], 0, []);
    AddKey(ecDeleteLastWord, VK_BACK, [ssCtrl], 0, []);
    AddKey(ecLineBreak, VK_RETURN, [], 0, []);
    AddKey(ecSelectAll, ord('A'), [ssCtrl], 0, []);
    AddKey(ecCopy, ord('C'), [ssCtrl], 0, []);
    AddKey(ecBlockIndent, ord('I'), [ssCtrl,ssShift], 0, []);
    AddKey(ecInsertLine, ord('N'), [ssCtrl], 0, []);
    AddKey(ecDeleteWord, ord('T'), [ssCtrl], 0, []);
    AddKey(ecBlockUnindent, ord('U'), [ssCtrl,ssShift], 0, []);
    AddKey(ecPaste, ord('V'), [ssCtrl], 0, []);
    AddKey(ecCut, ord('X'), [ssCtrl], 0, []);
    AddKey(ecDeleteLine, ord('Y'), [ssCtrl], 0, []);
    AddKey(ecDeleteEOL, ord('Y'), [ssCtrl,ssShift], 0, []);
    AddKey(ecUndo, ord('Z'), [ssCtrl], 0, []);
    AddKey(ecRedo, ord('Z'), [ssCtrl,ssShift], 0, []);
    AddKey(ecFoldLevel1, ord('1'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel2, ord('2'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel3, ord('3'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel4, ord('4'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel5, ord('5'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel6, ord('6'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel7, ord('7'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel8, ord('8'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel9, ord('9'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldLevel0, ord('0'), [ssAlt,ssShift], 0, []);
    AddKey(ecFoldCurrent, ord('-'), [ssAlt,ssShift], 0, []);
    AddKey(ecUnFoldCurrent, ord('+'), [ssAlt,ssShift], 0, []);
    AddKey(EcToggleMarkupWord, ord('M'), [ssAlt], 0, []);
    AddKey(ecNormalSelect, ord('N'), [ssCtrl,ssShift], 0, []);
    AddKey(ecColumnSelect, ord('C'), [ssCtrl,ssShift], 0, []);
    AddKey(ecLineSelect, ord('L'), [ssCtrl,ssShift], 0, []);
    AddKey(ecTab, VK_TAB, [], 0, []);
    AddKey(ecShiftTab, VK_TAB, [ssShift], 0, []);
    AddKey(ecMatchBracket, ord('B'), [ssCtrl,ssShift], 0, []);
    AddKey(ecColSelUp, VK_UP,    [ssAlt, ssShift], 0, []);
    AddKey(ecColSelDown, VK_DOWN,  [ssAlt, ssShift], 0, []);
    AddKey(ecColSelLeft, VK_LEFT, [ssAlt, ssShift], 0, []);
    AddKey(ecColSelRight, VK_RIGHT, [ssAlt, ssShift], 0, []);
    AddKey(ecColSelPageDown, VK_NEXT, [ssAlt, ssShift], 0, []);
    AddKey(ecColSelPageBottom, VK_NEXT, [ssAlt, ssShift,ssCtrl], 0, []);
    AddKey(ecColSelPageUp, VK_PRIOR, [ssAlt, ssShift], 0, []);
    AddKey(ecColSelPageTop, VK_PRIOR, [ssAlt, ssShift,ssCtrl], 0, []);
    AddKey(ecColSelLineStart, VK_HOME, [ssAlt, ssShift], 0, []);
    AddKey(ecColSelLineEnd, VK_END, [ssAlt, ssShift], 0, []);
    AddKey(ecColSelEditorTop, VK_HOME, [ssAlt, ssShift,ssCtrl], 0, []);
    AddKey(ecColSelEditorBottom, VK_END, [ssAlt, ssShift,ssCtrl], 0, []);
    AddKey(ecSynPSyncroEdStart, ord('E'), [ssCtrl], 0, []);
    AddKey(ecSynPSyncroEdEscape, ord('E'), [ssCtrl, ssShift], 0, []);
    AddKey(ecCompletionMenu, ord(' '), [ssCtrl], 0, []);
    AddKey(ecJumpToDeclaration, VK_UP, [ssCtrl,ssShift], 0, []);
    AddKey(ecPreviousLocation, 0, [], 0, []);
    AddKey(ecNextLocation, 0, [], 0, []);
    AddKey(ecRecordMacro, ord('R'), [ssCtrl,ssShift], 0, []);
    AddKey(ecPlayMacro, ord('P'), [ssCtrl,ssShift], 0, []);
    AddKey(ecShowDdoc, 0, [], 0, []);
    AddKey(ecShowCallTips, 0, [], 0, []);
    AddKey(ecCurlyBraceClose, 0, [], 0, []);
    AddKey(ecCommentSelection, ord('/'), [ssCtrl], 0, []);
    AddKey(ecSwapVersionAllNone, 0, [], 0, []);
    AddKey(ecRenameIdentifier, VK_F2, [], 0, []);
    AddKey(ecCommentIdentifier, 0, [], 0, []);
    AddKey(ecShowPhobosDoc, VK_F1, [], 0, []);
    AddKey(ecPreviousChangedArea, VK_UP, [ssAlt], 0, []);
    AddKey(ecNextChangedArea, VK_DOWN, [ssAlt], 0, []);
    AddKey(ecLowerCaseWordOrSel, 0, [], 0, []);
    AddKey(ecUpperCaseWordOrSel, 0, [], 0, []);
    AddKey(ecSortLines, 0, [], 0, []);
    AddKey(ecPrevProtGrp, 0, [], 0, []);
    AddKey(ecNextProtGrp, 0, [], 0, []);
    AddKey(ecAddBreakpoint, 0, [], 0, []);
    AddKey(ecRemoveBreakpoint, 0, [], 0, []);
    AddKey(ecToggleBreakpoint, 0, [], 0, []);
    AddKey(ecInsertDdocTemplate, 0, [], 0, []);
    AddKey(ecPrevWarning, 0, [], 0, []);
    AddKey(ecNextWarning, 0, [], 0, []);
    AddKey(ecGotoLine, 0, [], 0, []);
    AddKey(ecShowCurlineWarning, 0, [], 0, []);
    AddKey(ecLeftWordEdge, 0, [], 0, []);
    AddKey(ecRightWordEdge, 0, [], 0, []);
    AddKey(ecSelLeftWordEdge, 0, [], 0, []);
    AddKey(ecSelRightWordEdge, 0, [], 0, []);
    AddKey(ecSmartWordLeft, 0, [], 0, []);
    AddKey(ecSmartWordRight, 0, [], 0, []);
  end;
end;

function CustomStringToCommand(const Ident: string; var Int: Longint): Boolean;
begin
  case Ident of
    'ecCompletionMenu':     begin Int := ecCompletionMenu; exit(true); end;
    'ecJumpToDeclaration':  begin Int := ecJumpToDeclaration; exit(true); end;
    'ecPreviousLocation':   begin Int := ecPreviousLocation; exit(true); end;
    'ecNextLocation':       begin Int := ecNextLocation; exit(true); end;
    'ecRecordMacro':        begin Int := ecRecordMacro; exit(true); end;
    'ecPlayMacro':          begin Int := ecPlayMacro; exit(true); end;
    'ecShowDdoc':           begin Int := ecShowDdoc; exit(true); end;
    'ecShowCallTips':       begin Int := ecShowCallTips; exit(true); end;
    'ecCurlyBraceClose':    begin Int := ecCurlyBraceClose; exit(true); end;
    'ecCommentSelection':   begin Int := ecCommentSelection; exit(true); end;
    'ecSwapVersionAllNone': begin Int := ecSwapVersionAllNone; exit(true); end;
    'ecRenameIdentifier':   begin Int := ecRenameIdentifier; exit(true); end;
    'ecCommentIdentifier':  begin Int := ecCommentIdentifier; exit(true); end;
    'ecShowPhobosDoc':      begin Int := ecShowPhobosDoc; exit(true); end;
    'ecNextChangedArea':    begin Int := ecNextChangedArea; exit(true); end;
    'ecPreviousChangedArea':begin Int := ecPreviousChangedArea; exit(true); end;
    'ecUpperCaseWordOrSel': begin Int := ecUpperCaseWordOrSel; exit(true); end;
    'ecLowerCaseWordOrSel': begin Int := ecLowerCaseWordOrSel; exit(true); end;
    'ecSortLines':          begin Int := ecSortLines; exit(true); end;
    'ecNextProtGrp':        begin Int := ecNextProtGrp; exit(true); end;
    'ecPrevProtGrp':        begin Int := ecPrevProtGrp; exit(true); end;
    'ecAddBreakpoint':      begin Int := ecAddBreakpoint; exit(true); end;
    'ecRemoveBreakpoint':   begin Int := ecRemoveBreakpoint; exit(true); end;
    'ecToggleBreakpoint':   begin Int := ecToggleBreakpoint; exit(true); end;
    'ecInsertDdocTemplate': begin Int := ecInsertDdocTemplate; exit(true); end;
    'ecPrevWarning':        begin Int := ecPrevWarning; exit(true); end;
    'ecNextWarning':        begin Int := ecNextWarning; exit(true); end;
    'ecGotoLine':           begin Int := ecGotoLine; exit(true); end;
    'ecShowCurlineWarning': begin Int := ecShowCurlineWarning; exit(true); end;
    'ecLeftWordEdge':       begin Int := ecLeftWordEdge; exit(true); end;
    'ecRightWordEdge':      begin Int := ecRightWordEdge; exit(true); end;
    'ecSelLeftWordEdge':    begin Int := ecSelLeftWordEdge; exit(true); end;
    'ecSelRightWordEdge':   begin Int := ecSelRightWordEdge; exit(true); end;
    else exit(false);
  end;
end;

function CustomCommandToSstring(Int: Longint; var Ident: string): Boolean;
begin
  case Int of
    ecCompletionMenu:     begin Ident := 'ecCompletionMenu'; exit(true); end;
    ecJumpToDeclaration:  begin Ident := 'ecJumpToDeclaration'; exit(true); end;
    ecPreviousLocation:   begin Ident := 'ecPreviousLocation'; exit(true); end;
    ecNextLocation:       begin Ident := 'ecNextLocation'; exit(true); end;
    ecRecordMacro:        begin Ident := 'ecRecordMacro'; exit(true); end;
    ecPlayMacro:          begin Ident := 'ecPlayMacro'; exit(true); end;
    ecShowDdoc:           begin Ident := 'ecShowDdoc'; exit(true); end;
    ecShowCallTips:       begin Ident := 'ecShowCallTips'; exit(true); end;
    ecCurlyBraceClose:    begin Ident := 'ecCurlyBraceClose'; exit(true); end;
    ecCommentSelection:   begin Ident := 'ecCommentSelection'; exit(true); end;
    ecSwapVersionAllNone: begin Ident := 'ecSwapVersionAllNone'; exit(true); end;
    ecRenameIdentifier:   begin Ident := 'ecRenameIdentifier'; exit(true); end;
    ecCommentIdentifier:  begin Ident := 'ecCommentIdentifier'; exit(true); end;
    ecShowPhobosDoc:      begin Ident := 'ecShowPhobosDoc'; exit(true); end;
    ecNextChangedArea:    begin Ident := 'ecNextChangedArea'; exit(true); end;
    ecPreviousChangedArea:begin Ident := 'ecPreviousChangedArea'; exit(true); end;
    ecUpperCaseWordOrSel: begin Ident := 'ecUpperCaseWordOrSel'; exit(true); end;
    ecLowerCaseWordOrSel: begin Ident := 'ecLowerCaseWordOrSel'; exit(true); end;
    ecSortLines:          begin Ident := 'ecSortLines'; exit(true); end;
    ecNextProtGrp:        begin Ident := 'ecNextProtGrp'; exit(true); end;
    ecPrevProtGrp:        begin Ident := 'ecPrevProtGrp'; exit(true); end;
    ecAddBreakpoint:      begin Ident := 'ecAddBreakpoint'; exit(true); end;
    ecRemoveBreakpoint:   begin Ident := 'ecRemoveBreakpoint'; exit(true); end;
    ecToggleBreakpoint:   begin Ident := 'ecToggleBreakpoint'; exit(true); end;
    ecInsertDdocTemplate: begin Ident := 'ecInsertDdocTemplate'; exit(true); end;
    ecPrevWarning:        begin Ident := 'ecPrevWarning'; exit(true); end;
    ecNextWarning:        begin Ident := 'ecNextWarning'; exit(true); end;
    ecGotoLine:           begin Ident := 'ecGotoLine'; exit(true); end;
    ecShowCurlineWarning: begin Ident := 'ecShowCurlineWarning'; exit(true); end;
    ecLeftWordEdge:       begin Ident := 'ecLeftWordEdge'; exit(true); end;
    ecRightWordEdge:      begin Ident := 'ecRightWordEdge'; exit(true); end;
    ecSelLeftWordEdge:    begin Ident := 'ecSelLeftWordEdge'; exit(true); end;
    ecSelRightWordEdge:   begin Ident := 'ecSelRightWordEdge'; exit(true); end;
    else exit(false);
  end;
end;

procedure TDexedMemo.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: TUTF8Char; Data: pointer);
begin
  FBlockSelection.AutoExtend := False;
  FBlockSelection.StickyAutoExtend := False;
  case Command of
    5: Command := ecLeftWordEdge;
    6: Command := ecRightWordEdge;
    105:
    begin
      Command := ecSelLeftWordEdge;
      FBlockSelection.ActiveSelectionMode := smNormal;
      FBlockSelection.AutoExtend := true;
      FBlockSelection.StickyAutoExtend := True;
    end;
    106:
    begin
      Command := ecSelRightWordEdge;
      FBlockSelection.ActiveSelectionMode := smNormal;
      FBlockSelection.AutoExtend := true;
      FBlockSelection.StickyAutoExtend := True;
    end;
  end;
  inherited;
  case Command of
    ecCut: if not SelAvail then
    begin
      SelectLine(true);
      ExecuteCommand(ecCut, #0, nil);
      Clipboard.AsText := TrimLeft(Clipboard.AsText);
    end;
    ecCopy: if not SelAvail then
    begin
      SelectLine(false);
      ExecuteCommand(ecCopy, #0, nil);
      SelEnd:=SelStart;
    end;
    ecPaste: patchClipboardIndentation;
    ecCompletionMenu:
    begin
      fCanAutoDot:=false;
      if not fIsDSource and not alwaysAdvancedFeatures then
        exit;
      fCompletion.Execute(GetWordAtRowCol(LogicalCaretXY),
        ClientToScreen(point(CaretXPix, CaretYPix + LineHeight)));
    end;
    ecPreviousLocation:
      fPositions.back;
    ecNextLocation:
      fPositions.next;
    ecShowDdoc:
    begin
      hideCallTips;
      hideDDocs;
      if not fIsDSource and not alwaysAdvancedFeatures then
        exit;
      showDDocs;
    end;
    ecShowCallTips:
    begin
      hideCallTips;
      hideDDocs;
      if not fIsDSource and not alwaysAdvancedFeatures then
        exit;
      showCallTips(true);
    end;
    ecCurlyBraceClose:
      curlyBraceCloseAndIndent;
    ecCommentSelection:
      commentSelection;
    ecSwapVersionAllNone:
      invertVersionAllNone;
    ecRenameIdentifier:
      renameIdentifier;
    ecCommentIdentifier:
      commentIdentifier;
    ecShowPhobosDoc:
      ShowPhobosDoc;
    ecNextChangedArea:
      goToChangedArea(true);
    ecPreviousChangedArea:
      goToChangedArea(false);
    ecUpperCaseWordOrSel:
      setSelectionOrWordCase(true);
    ecLowerCaseWordOrSel:
      setSelectionOrWordCase(false);
    ecSortLines:
      sortLines;
    ecPrevProtGrp:
      previousProtectionGroup;
    ecNextProtGrp:
      nextProtectionGroup;
    ecAddBreakpoint:
      addCurLineBreakPoint;
    ecRemoveBreakpoint:
      removeCurLineBreakPoint;
    ecToggleBreakpoint:
      toggleCurLineBreakpoint;
    ecInsertDdocTemplate:
      insertDdocTemplate;
    ecPrevWarning:
      goToWarning(false);
    ecNextWarning:
      goToWarning(true);
    ecGotoLine:
      gotoLinePrompt;
    ecShowCurlineWarning:
      showCurLineWarning;
    ecLeftWordEdge, ecSelLeftWordEdge:
      gotoWordEdge(false);
    ecRightWordEdge, ecSelRightWordEdge:
      gotoWordEdge(true);
  end;
  if fOverrideColMode and not SelAvail then
  begin
    fOverrideColMode := false;
    Options := Options - [eoScrollPastEol];
  end;
end;

function TDexedMemo.indentationMode(out numTabs, numSpaces: integer): TIndentationMode;
  function checkLine(index: integer): TIndentationMode;
  var
    u: string;
    b: array[0..15] of char = '                ';
  begin
    result := imNone;
    u := Lines[index];
    if (u.length > 0) and (u[1] = #9) then
      result := imTabs
    else if (u.length >= TabWidth) and u.StartsWith(b[0..TabWidth-1]) then
      result := imSpaces;
  end;
var
  i: integer;
  t: integer = 0;
  s: integer = 0;
begin
  for i:= 0 to lines.count-1 do
  begin
    result := checkLine(i);
    t += byte(result = imTabs);
    s += byte(result = imSpaces);
  end;
  if (t <> 0) and (s <> 0) then
    result := imMixed
  else if t = 0 then
    result := imSpaces
  else if s = 0 then
    result := imTabs
  else
    result := imNone;
  numTabs:= t;
  numSpaces:= s;
end;

procedure TDexedMemo.forceIndentation(m: TIndentationMode; w: integer);
var
  i: integer;
begin
  assert(w > 0);
  lines.BeginUpdate;
  for i:= 0 to lines.Count-1 do
  case m of
    imTabs:
    begin
      lines[i] := leadingSpacesToTabs(lines[i], TabWidth);
      fModified:=true;
    end;
    imSpaces:
    begin
      lines[i] := leadingTabsToSpaces(lines[i], TabWidth);
      fModified:=true;
    end;
    else ;
  end;
  lines.EndUpdate;
end;

procedure TDexedMemo.insertLeadingDDocSymbol(c: char);
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  BeginUndoBlock;
  if ((CaretX-1) and 1) = 0 then
    ExecuteCommand(ecChar, ' ', nil);
  ExecuteCommand(ecChar, c, nil);
  EndUndoBlock;
end;

procedure TDexedMemo.curlyBraceCloseAndIndent;
var
  i: integer;
  beg: string = '';
  numTabs: integer = 0;
  numSpac: integer = 0;
  numSkip: integer = 0;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;

  i := CaretY - 1;
  while true do
  begin
    if i < 0 then
      break;
    beg := Lines[i];
    if (Pos('}', beg) <> 0) then
    begin
      numSkip += 1;
    end
    else if (Pos('{', beg) <> 0) then
    begin
      numSkip -= 1;
    end;
    if numSkip < 0 then
      break
    else
      i -= 1;
  end;

  for i:= 1 to beg.length do
  begin
    case beg[i] of
      #9: numTabs += 1;
      ' ': numSpac += 1;
      else break;
    end;
  end;
  numTabs += numSpac div TabWidth;

  BeginUndoBlock;

  CommandProcessor(ecInsertLine, '', nil);
  CommandProcessor(ecDown, '', nil);

  CommandProcessor(ecInsertLine, '', nil);
  CommandProcessor(ecDown, '', nil);
  while CaretX <> 1 do CommandProcessor(ecLeft, '' , nil);
  for i:= 0 to numTabs-1 do CommandProcessor(ecTab, '', nil);
  CommandProcessor(ecChar, '}', nil);

  CommandProcessor(ecUp, '', nil);
  while CaretX <> 1 do CommandProcessor(ecLeft, '' , nil);
  for i:= 0 to numTabs do CommandProcessor(ecTab, '', nil);

  EndUndoBlock;
end;

procedure TDexedMemo.commentSelection;
  procedure commentHere;
  begin
    ExecuteCommand(ecChar, '/', nil);
    ExecuteCommand(ecChar, '/', nil);
  end;
  procedure unCommentHere;
  begin
    ExecuteCommand(ecLineTextStart, '', nil);
    ExecuteCommand(ecDeleteChar, '', nil);
    ExecuteCommand(ecDeleteChar, '', nil);
  end;
var
  i, j, dx, lx, numUndo: integer;
  line: string;
  mustUndo: boolean = false;
  pt, cp: TPoint;
begin
  if not SelAvail then
  begin
    i := CaretX;
    line := TrimLeft(LineText);
    mustUndo := (line.length > 1) and (line[1..2] = '//');
    BeginUndoBlock;
    ExecuteCommand(ecLineTextStart, '', nil);
    if not mustUndo then
    begin
      commentHere;
      CaretX:= i+2;
    end
    else
    begin
      unCommentHere;
      CaretX:= i-2;
    end;
    EndUndoBlock;
  end else
  begin
    mustUndo := false;
    pt.X:= high(pt.X);
    cp := CaretXY;
    numUndo := 0;
    for i := BlockBegin.Y-1 to BlockEnd.Y-1 do
    begin
      line := TrimLeft(Lines[i]);
      dx := Lines[i].length - line.length;
      lx := 0;
      for j := 1 to dx do
        if Lines[i][j] = #9 then
          lx += TabWidth
        else
          lx += 1;
      if (lx + 1 < pt.X) and not line.isEmpty then
        pt.X:= lx + 1;
      if (line.length > 1) and (line[1..2] = '//') then
        numUndo += 1;
    end;
    if numUndo = 0 then
      mustUndo := false
    else if numUndo = BlockEnd.Y + 1 - BlockBegin.Y then
      mustUndo := true;
    BeginUndoBlock;
    for i := BlockBegin.Y to BlockEnd.Y do
    begin
      pt.Y:= i;
      ExecuteCommand(ecGotoXY, '', @pt);
      while CaretX < pt.X do
        ExecuteCommand(ecChar, ' ', nil);
      if not mustUndo then
      begin
        commentHere;
      end
      else
        unCommentHere;
    end;
    if not mustUndo then
      cp.X += 2
    else
      cp.X -= 2;
    CaretXY := cp;
    EndUndoBlock;
  end;
end;

procedure TDexedMemo.commentIdentifier;
var
  str: string;
  x, x0, x1: integer;
  comBeg: boolean = false;
  comEnd: boolean = false;
  comment:boolean = true;
  attrib: TSynHighlighterAttributes;
begin
  if not GetHighlighterAttriAtRowColEx(CaretXY, str, x0, x, attrib) then
    exit;
  if str.isEmpty then
    exit;

  str := LineText;
  x := LogicalCaretXY.X;

  ExecuteCommand(ecWordEndRight, #0, nil);
  x1 := LogicalCaretXY.X;
  while true do
  begin
    if (str[x1] in ['*', '+']) and (x1 < str.length) and (str[x1+1] = '/') then
    begin
      comEnd:=true;
      break;
    end;
    if not isBlank(str[x1]) then
      break;
    ExecuteCommand(ecRight, #0, nil);
    x1 += 1;
    if x1 = str.length then
      break;
  end;

  LogicalCaretXY := point(x, LogicalCaretXY.Y);
  ExecuteCommand(ecWordLeft, #0, nil);
  x0 := LogicalCaretXY.X - 1;
  if (x0 > 1) then while true do
  begin
    if (x0 > 1) and (str[x0] in ['*', '+']) and (str[x0-1] = '/') then
    begin
      x0 -= 1;
      comBeg:=true;
      break;
    end;
    if not isBlank(str[x0]) then
      break;
    ExecuteCommand(ecLeft, #0, nil);
    x0 -= 1;
    if x0 = 1 then
      break;
  end;

  comment := not comBeg and not comEnd;
  LogicalCaretXY := point(x, LogicalCaretXY.Y);
  if comment then
  begin
    BeginUndoBlock;
    ExecuteCommand(ecWordLeft, '', nil);
    ExecuteCommand(ecChar, '/', nil);
    ExecuteCommand(ecChar, '*', nil);
    ExecuteCommand(ecWordEndRight, '', nil);
    ExecuteCommand(ecChar, '*', nil);
    ExecuteCommand(ecChar, '/', nil);
    EndUndoBlock;
  end else
  begin
    BeginUndoBlock;
    LogicalCaretXY := point(x1, LogicalCaretXY.Y);
    ExecuteCommand(ecDeleteChar, '', nil);
    ExecuteCommand(ecDeleteChar, '', nil);
    LogicalCaretXY := point(x0, LogicalCaretXY.Y);
    ExecuteCommand(ecDeleteChar, '', nil);
    ExecuteCommand(ecDeleteChar, '', nil);
    EndUndoBlock;
  end;
end;

procedure TDexedMemo.invertVersionAllNone;
var
  i: integer;
  c: char;
  tok, tok1, tok2: PLexToken;
  cp, st, nd: TPoint;
  sel: boolean;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  fLexToks.Clear;
  lex(lines.Text, fLexToks, nil, [lxoNoComments]);
  cp := CaretXY;
  if SelAvail then
  begin
    sel := true;
    st := BlockBegin;
    nd := BlockEnd;
  end else
  begin
    sel := false;
    st := Point(0,0);
    nd := Point(0,0);
  end;
  for i := fLexToks.Count-1 downto 2 do
  begin
    tok := PLexToken(fLexToks[i]);

    if sel and ((tok^.position.Y < st.Y)
      or (tok^.position.Y > nd.Y)) then
        continue;
    if ((tok^.Data <> 'all') and (tok^.Data <> 'none'))
      or (tok^.kind <> ltkIdentifier) or (i < 2) then
        continue;

    tok1 := PLexToken(fLexToks[i-2]);
    tok2 := PLexToken(fLexToks[i-1]);

    if  ((tok1^.kind = ltkKeyword) and (tok1^.data = 'version')
      and (tok2^.kind = ltkSymbol) and (tok2^.data = '(')) then
    begin
      BeginUndoBlock;
      LogicalCaretXY := tok^.position;
      CaretX:=CaretX+1;
      case tok^.Data of
        'all':
        begin
          for c in 'all'  do ExecuteCommand(ecDeleteChar, '', nil);
          for c in 'none' do ExecuteCommand(ecChar, c, nil);
        end;
        'none':
        begin
          for c in 'none' do ExecuteCommand(ecDeleteChar, '', nil);
          for c in 'all'  do ExecuteCommand(ecChar, c, nil);
        end;
      end;
      EndUndoBlock;
    end;
  end;
  CaretXY := cp;
end;

procedure TDexedMemo.renameIdentifier;
var
  locs: TIntOpenArray = nil;
  old, idt, line: string;
  i, j, loc: integer;
  p: TPoint;
  c: char;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  if not DcdWrapper.available then
    exit;
  p := CaretXY;
  line := lineText;
  if (CaretX = 1) or not (line[LogicalCaretXY.X] in IdentChars) or
    not (line[LogicalCaretXY.X-1] in IdentChars)  then exit;
  old := GetWordAtRowCol(LogicalCaretXY);
  DcdWrapper.getLocalSymbolUsageFromCursor(locs);
  if length(locs) = 0 then
  begin
    dlgOkInfo('Unknown, ambiguous or non-local symbol for "'+ old +'"');
    exit;
  end;

  idt := 'new identifier for "' + old + '"';
  idt := InputBox('Local identifier renaming', idt, old);
  if idt.isEmpty or idt.isBlank then
    exit;

  for i:= high(locs) downto 0 do
  begin
    loc := locs[i];
    if loc = -1 then
      continue;
    BeginUndoBlock;
    SelStart := loc + 1;
    for j in [0..old.length-1] do
      ExecuteCommand(ecDeleteChar, '', nil);
    for c in idt do
      ExecuteCommand(ecChar, c, nil);
    EndUndoBlock;
    CaretXY := p;
  end;
end;

procedure TDexedMemo.ShowPhobosDoc;
var
  str: string;
  pth: string;
  rac: string;
  idt: string = '';
  pos: integer;
  len: integer;
  sum: integer;
  edt: TSynEdit;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  i: integer;
  linelen: integer;
  procedure errorMessage(const msg: string);
  begin
    // parameter for doc racine is a folder
    if rac[rac.length] in ['/','\'] then
      rac += 'index.html'
    else if rac.dirExists then
      rac += DirectorySeparator + 'index.html'
    else
      rac += '/' + 'index.html';

    if dlgYesNo(format('%s.%sOpen the documentation index anyway ?',
      [msg, LineEnding])) = mrYes then
        OpenURL(rac);
  end;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  if fPhobosDocRoot.dirExists then
    rac := 'file://' + fPhobosDocRoot
  else
    rac := fPhobosDocRoot;
  if rac.isEmpty then
    rac := 'https://dlang.org/phobos/index.html';
  DcdWrapper.getDeclFromCursor(str, pos);
  if not str.fileExists then
  begin
    errorMessage('The source where the symbol is declared is not existing');
    exit;
  end;
  // verify that the decl is in phobos
  pth := str;
  while true do
  begin
    if pth.extractFilePath = pth then
    begin
      errorMessage('The source where the symbol is declared does not seem' +
        ' to be part of Phobos or the D runtime');
      exit;
    end;
    pth := pth.extractFilePath;
    setLength(pth,pth.length-1);
    if (pth.extractFilename = 'std') or (pth.extractFilename = 'core')
      or (pth.extractFilename = 'etc') then
        break;
  end;
  // get the declaration name
  if pos <> -1 then
  begin
    edt := TSynEdit.Create(nil);
    edt.Lines.LoadFromFile(str);
    sum := 0;
    len := getLineEndingLength(str);
    for i := 0 to edt.Lines.Count-1 do
    begin
      linelen := edt.Lines[i].length;
      if sum + linelen + len > pos then
      begin
        edt.CaretY := i + 1;
        edt.CaretX := pos - sum + len;
        edt.SelectWord;
        idt := '.html#.' + edt.SelText;
        break;
      end;
      sum += linelen;
      sum += len;
    end;
    edt.Free;
  end;
  // guess the htm file + anchor
  rng.init(str);
  while true do
  begin
    if rng.empty then
    begin
      errorMessage('Failed to determine the matching HTML file');
      exit;
    end;
    rng.popUntil(DirectorySeparator);
    if not rng.empty then
      rng.popFront;
    if rng.startsWith('std' + DirectorySeparator) or rng.startsWith('core' + DirectorySeparator)
      or rng.startsWith('etc' + DirectorySeparator) then
        break;
  end;
  pth := rac;
  while not rng.empty do
  begin
    pth += rng.takeUntil([DirectorySeparator, '.']).yield;
    if rng.startsWith('.d') then
      break;
    pth += '_';
    rng.popFront;
  end;
  pth += idt;
  {$IFDEF WINDOWS}
  if fPhobosDocRoot.dirExists then
    for i:= 1 to pth.length do
      if pth[i] = '\' then
        pth[i] := '/';
  {$ENDIF}
  OpenURL(pth);
end;

procedure TDexedMemo.nextChangedArea;
begin
  goToChangedArea(true);
end;

procedure TDexedMemo.previousChangedArea;
begin
  goToChangedArea(false);
end;

procedure TDexedMemo.previousWarning;
begin
  goToWarning(false);
end;

procedure TDexedMemo.nextWarning;
begin
  goToWarning(true);
end;

procedure TDexedMemo.gotoLinePrompt;
var
  d: string;
  v: string;
  i: integer;
begin
  d := caretY.ToString;
  v := InputBox('Goto line', 'line number', d);
  if v.isNotEmpty and not v.Equals(d) then
  begin
    i := v.toIntNoExcept;
    if i <> -1 then
    begin
      if i < 1 then
        i := 1
      else if i > lines.Count then
        i := lines.Count;
      CaretY:= i;
      EnsureCursorPosVisible;
    end;
  end;
end;

procedure TDexedMemo.showWarningForLine(line: integer);
var
  s: string;
  p: TPoint;
  c: TPoint = (x: 0; y: 0);
begin
  s := getDscannerWarning(line);
  if s.isNotEmpty then
  begin
    p.x := 0;
    p.y := line;
    p := RowColumnToPixels(p);
    c := ClientToScreen(c);
    p.Offset(c);
    s := 'Warning(s):' + LineEnding + s;
    fDDocWin.FontSize := Font.Size;
    fDDocWin.HintRect := fDDocWin.CalcHintRect(0, s, nil);
    fDDocWin.OffsetHintRect(p, Font.Size);
    fDDocWin.ActivateHint(fDDocWin.HintRect, s);
  end;
end;

procedure TDexedMemo.showCurLineWarning;
begin
  showWarningForLine(CaretY);
end;

procedure TDexedMemo.goToChangedArea(next: boolean);
var
  i: integer;
  s: TSynLineState;
  d: integer;
  b: integer = 0;
  p: TPoint;
begin
  i := CaretY - 1;
  s := GetLineState(i);
  case next of
    true: begin d := 1; b := lines.count-1; end;
    false:d := -1;
  end;
  if i = b then
    exit;
  // exit the current area if it's modified
  while s <> slsNone do
  begin
    s := GetLineState(i);
    if i = b then
      exit;
    i += d;
  end;
  // find next modified area
  while s = slsNone do
  begin
    s := GetLineState(i);
    if i = b then
      break;
    i += d;
  end;
  // goto area beg/end
  if (s <> slsNone) and (i <> CaretY + 1) then
  begin
    p.X:= 1;
    p.Y:= i + 1 - d;
    ExecuteCommand(ecGotoXY, #0, @p);
  end;
end;

procedure TDexedMemo.goToProtectionGroup(next: boolean);
var
  i: integer;
  tk0, tk1: PLexToken;
  tk: PLexToken = nil;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  fLexToks.Clear;
  lex(Lines.Text, fLexToks, nil, [lxoNoComments, lxoNoWhites]);
  for i:=0 to fLexToks.Count-2 do
  begin
    tk0 := fLexToks[i];
    tk1 := fLexToks[i+1];
    if not next then
    begin
      if tk0^.position.Y >= caretY then
        break;
    end
    else if tk0^.position.Y <= caretY then
      continue;
    if tk0^.kind = ltkKeyword then
    case tk0^.Data of
      'public','private','protected','package','export':
        if (tk1^.kind = ltkSymbol) and (tk1^.Data[1] in ['{',':']) then
        begin
          tk := tk0;
          if next then
            break;
        end;
    end;
  end;
  if assigned(tk) then
    ExecuteCommand(ecGotoXY, #0, @tk^.position);
end;

function TDscannerResults.getCount: PtrInt;
begin
  Result := fList.Count;
end;

function TDscannerResults.getItem(const Index: PtrInt): PDscannerResult;
begin
  Result := fList[Index];
end;

procedure TDexedMemo.goToWarning(next: boolean);
var
  i: integer;
  j: integer = -1;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  if not next then
  begin
    for i:= 0 to fDscannerResults.count-1 do
    begin
      j := i - 1;
      if fDscannerResults.item[i]^.line >= caretY then
        break;
    end;
    if j <> -1 then
    begin
      CaretY:= fDscannerResults.item[j]^.line;
      EnsureCursorPosVisible;
    end;
  end
  else
  begin
    for i:= fDscannerResults.count-1 downto 0 do
    begin
      j := i + 1;
      if fDscannerResults.item[i]^.line <= caretY then
        break;
    end;
    if (j <> -1) and (j < fDscannerResults.count) then
    begin
      CaretY:= fDscannerResults.item[j]^.line;
      EnsureCursorPosVisible;
    end;
  end;
end;

procedure TDexedMemo.previousProtectionGroup;
begin
  goToProtectionGroup(false);
end;

procedure TDexedMemo.nextProtectionGroup;
begin
  goToProtectionGroup(true);
end;

function TDexedMemo.implementMain: THasMain;
var
  res: char = '0';
  prc: TProcess;
  src: string;
begin
  if fDastWorxExename.length = 0 then
    exit(mainDefaultBehavior);
  src := Lines.Text;
  prc := TProcess.Create(nil);
  try
    prc.Executable:= fDastWorxExename;
    prc.Parameters.Add('-m');
    prc.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    prc.ShowWindow := swoHIDE;
    prc.Execute;
    prc.Input.Write(src[1], src.length);
    prc.CloseInput;
    prc.Output.Read(res, 1);
    while prc.Running do
      sleep(1);
  finally
    prc.Free;
  end;
  case res = '1' of
    false:result := mainNo;
    true: result := mainYes;
  end;
end;

procedure TDexedMemo.autoClosePair(value: TAutoClosedPair);
var
  i, p: integer;
  tk0, tk1: PLexToken;
  str: string;
const
  dontCloseIfContiguousTo = [ltkIdentifier, ltkNumber, ltkString, ltkChar, ltkComment];
begin
  if fLexToks.Count < 3 then
    exit;

  p := selStart;
  if value in [autoCloseBackTick, autoCloseDoubleQuote, autoCloseSingleQuote] then
  begin
    for i := 0 to fLexToks.Count-1 do
    begin
      tk0 := fLexToks[i];
      // opening char is stuck to something, assume this thing is
      // what has to be between the pair, so don't close.
      if (tk0^.offset+2 = p) and (tk0^.kind in dontCloseIfContiguousTo) then
        exit;
      if i < fLexToks.Count-1 then
      begin
        tk1 := fLexToks[i+1];
        // inside a strings lit., char lit., comments, don't put the mess.
        if (tk0^.offset+1 <= p) and (p < tk1^.offset+2) and
           (tk0^.kind in [ltkString, ltkChar, ltkComment]) then
             exit;
        // since the source is scanned before inserting the opening char,
        // in single line comments, p can be > to next tok offset
        if (tk0^.offset+1 <= p) and (p > tk1^.offset+2) and
           (tk0^.kind = ltkComment) and (tk0^.Data[1] = '/') then
             exit;
      end
      // at the EOF an illegal tok is likely something that has to be closed so
      // dont auto insert the pair.
      else if (tk0^.offset+1 <= p) and (tk0^.kind = ltkIllegal) then
        exit;
    end;
  end

  else if value = autoCloseSquareBracket then
  begin
    for i:=0 to fLexToks.Count-2 do
    begin
      tk0 := fLexToks[i];
      tk1 := fLexToks[i+1];
      // opening char is stuck to something, assume this thing is
      // what has to be between the pair, so don't close.
      if (tk0^.offset+2 = p) and (tk0^.kind in dontCloseIfContiguousTo) then
        exit;
      if (tk0^.offset+1 <= p) and (p < tk1^.offset+2) and
        (tk0^.kind = ltkComment) then
          exit;
    end;
    tk0 := fLexToks[fLexToks.Count-1];
    str := lineText;
    i := LogicalCaretXY.X;
    if (i <= str.length) and (lineText[i] = ']') then
      exit;
  end;

  BeginUndoBlock;
  ExecuteCommand(ecChar, autoClosePair2Char[value], nil);
  ExecuteCommand(ecLeft, #0, nil);
  EndUndoBlock;
end;

procedure TDexedMemo.setSelectionOrWordCase(upper: boolean);
var
  i: integer;
  txt: string;
begin
  if SelAvail then
  begin
    BeginUndoBlock;
    case upper of
      false: txt := UTF8LowerString(SelText);
      true:  txt := UTF8UpperString(SelText);
    end;
    ExecuteCommand(ecBlockDelete, #0, nil);
    for i:= 1 to txt.length do
    case txt[i] of
      #13: continue;
      #10: ExecuteCommand(ecLineBreak, #0, nil);
      else ExecuteCommand(ecChar, txt[i], nil);
    end;
    EndUndoBlock;
  end else
  begin
    txt := GetWordAtRowCol(LogicalCaretXY);
    if txt.isBlank then
      exit;
    BeginUndoBlock;
    ExecuteCommand(ecWordLeft, #0, nil);
    case upper of
      false: txt := UTF8LowerString(txt);
      true:  txt := UTF8UpperString(txt);
    end;
    ExecuteCommand(ecDeleteWord, #0, nil);
    for i:= 1 to txt.length do
      ExecuteCommand(ecChar, txt[i], nil);
    EndUndoBlock;
  end;
end;

procedure TDexedMemo.sortSelectedLines(descending, caseSensitive: boolean);
var
  i,j: integer;
  lne: string;
  lst: TStringListUTF8;
  pt0: TPoint;
begin
  if BlockEnd.Y - BlockBegin.Y < 1 then
    exit;
  lst := TStringListUTF8.Create;
  try
    BeginUndoBlock;
    for i:= BlockBegin.Y-1 to BlockEnd.Y-1 do
      lst.Add(lines[i]);
    pt0 := BlockBegin;
    pt0.X:=1;
    ExecuteCommand(ecGotoXY, #0, @pt0);
    lst.CaseSensitive:=caseSensitive;
    if not caseSensitive then
      lst.Sorted:=true;
    case descending of
      false: for i:= 0 to lst.Count-1 do
        begin
          ExecuteCommand(ecDeleteLine, #0, nil);
          ExecuteCommand(ecInsertLine, #0, nil);
          lne := lst[i];
          for j := 1 to lne.length do
            ExecuteCommand(ecChar, lne[j], nil);
          ExecuteCommand(ecDown, #0, nil);
        end;
      true: for i:= lst.Count-1 downto 0 do
        begin
          ExecuteCommand(ecDeleteLine, #0, nil);
          ExecuteCommand(ecInsertLine, #0, nil);
          lne := lst[i];
          for j := 1 to lne.length do
            ExecuteCommand(ecChar, lne[j], nil);
          ExecuteCommand(ecDown, #0, nil);
        end;
    end;
    EndUndoBlock;
  finally
    lst.Free;
  end;
end;

procedure TDexedMemo.sortLines;
begin
  if not assigned(fSortDialog) then
    fSortDialog := TSortDialog.construct(self);
  fSortDialog.Show;
end;

procedure TDexedMemo.addCurLineBreakPoint;
begin
  if not findBreakPoint(CaretY) then
    addBreakPoint(CaretY);
end;

procedure TDexedMemo.removeCurLineBreakPoint;
begin
  if findBreakPoint(CaretY) then
    removeBreakPoint(CaretY);
end;

procedure TDexedMemo.toggleCurLineBreakpoint;
begin
  if not findBreakPoint(CaretY) then
    addBreakPoint(CaretY)
  else
    removeBreakPoint(CaretY);
end;

procedure TDexedMemo.insertDdocTemplate;
var
  d: TStringList;
  i: integer;
  j: integer;
  k: integer;
  s: string;
  p: TPoint;
begin
  d := TStringList.Create;
  try
    getDdocTemplate(lines, d, CaretY, fInsertPlusDdoc);
    if d.Text.isNotEmpty then
    begin
      BeginUndoBlock;
      ExecuteCommand(ecLineStart, #0, nil);
      k := CaretX;
      p.y:= CaretY -1 ;
      p.x:= 1 ;
      ExecuteCommand(ecGotoXY, #0, @p);
      for i := 0 to d.Count-1 do
      begin
        s := d[i];
        ExecuteCommand(ecLineBreak, #0, nil);
        while caretX < k do
          ExecuteCommand(ecTab, #0, nil);
        for j := 1 to s.length do
          ExecuteCommand(ecChar, s[j], nil);
      end;
      EndUndoBlock;
    end;
  finally
    d.Free;
  end;
end;

procedure TDexedMemo.gotoWordEdge(right: boolean);
var
  s: string;
  c: char;
  p: TPoint;
const
  w: TSysCharSet = [' ', #9];
  i: TSysCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
begin
  s := LineText;
  p := PhysicalToLogicalPos(CaretXY);
  if (p.x <= 1) and not right then
  begin
    ExecuteCommand(ecLeft, #0, nil);
    exit;
  end;
  if (p.x > s.length-1) and right then
  begin
    ExecuteCommand(ecRight, #0, nil);
    exit;
  end;
  if right then
    c := s[p.x]
  else
    c := s[min(p.x-1, s.length)];
  if not right then
  begin
    if c in w then
      ExecuteCommand(ecWordEndLeft, #0, nil)
    else if c in i then
      ExecuteCommand(ecWordLeft, #0, nil)
    else
      ExecuteCommand(ecLeft, #0, nil);
  end
  else
  begin
    if c in w then
      ExecuteCommand(ecWordRight, #0, nil)
    else if c in i then
      ExecuteCommand(ecWordEndRight, #0, nil)
    else
      ExecuteCommand(ecRight, #0, nil);
  end;
end;
{$ENDREGION}

{$REGION DDoc & CallTip --------------------------------------------------------}
procedure TDexedMemo.InitHintWins;
begin
  if fCallTipWin.isNil then
  begin
    fCallTipWin := TEditorCallTipWindow.Create(self);
    fCallTipWin.Color := clInfoBk + $01010100;
    fCallTipWin.Font.Color:= clInfoText;
  end;
  if fDDocWin.isNil then
  begin
    fDDocWin := TEditorHintWindow.Create(self);
    fDDocWin.Color := clInfoBk + $01010100;
    fDDocWin.Font.Color:= clInfoText;
  end;
end;

procedure TDexedMemo.showCallTips(findOpenParen: boolean = true);
var
  str, lne: string;
  i, x: integer;
  j: integer = 0;
  n: integer = 0;
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  if not fCallTipWin.Visible then
    fCallTipStrings.Clear;
  str := LineText[1..CaretX];
  x := CaretX;
  i := min(x, str.length);
  if findOpenParen then while true do
  begin
    if i = 1 then
      break;
    if str[i] = ',' then
      j += 1;
    if str[i] = ')' then
      n += 1;
    if str[i-1] = '(' then
    begin
      if n = 0 then
      begin
        LogicalCaretXY := Point(i, CaretY);
        break;
      end
      else n -= 1;
    end;
    if str[i] = #9 then
      i -= TabWidth
    else
      i -= 1;
    if i <= 0 then
      break;
  end;

  if i > 0 then
  begin
    DcdWrapper.getCallTip(str);
    i := fCallTipStrings.Count;
    if (fCallTipStrings.Count <> 0) and str.isNotEmpty then
      fCallTipStrings.Insert(0, '---');
    fCallTipStrings.Insert(0, str);
    i := fCallTipStrings.Count - i;
    // overload count to delete on ')'
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
    fCallTipStrings.Objects[0] := TObject(pointer(i));
    {$POP}
    str := '';
    for lne in fCallTipStrings do
      if lne.isNotEmpty then
        str += lne + LineEnding;
    if str.isNotEmpty then
    begin
      {$IFDEF WINDOWS}
      str := str[1..str.length-2];
      {$ELSE}
      str := str[1..str.length-1];
      {$ENDIF}
      showCallTipsString(str, j);
    end;
  end;

  if findOpenParen then
    CaretX:=x;
end;

procedure TDexedMemo.showCallTipsString(const tips: string; indexOfExpected: integer);
var
  pnt: TPoint;
begin
  if (not fIsDSource and not alwaysAdvancedFeatures) or tips.isEmpty then
    exit;

  pnt := ClientToScreen(point(CaretXPix, CaretYPix));
  fCallTipWin.indexOfExpectedArg:=indexOfExpected;
  fCallTipWin.FontSize := Font.Size;
  fCallTipWin.HintRect := fCallTipWin.CalcHintRect(0, tips, nil);
  fCallTipWin.OffsetHintRect(pnt, Font.Size * 2);

  // see procedure THintWindow.ActivateHint(const AHint: String);
  // caused a regression in call tips stacking
  fCallTipWin.Caption:= tips;

  fCallTipWin.ActivateHint(tips);
end;

procedure TDexedMemo.hideCallTips;
begin
  if not fCallTipWin.Visible then
    exit;
  fCallTipStrings.Clear;
  fCallTipWin.Hide;
end;

procedure TDexedMemo.decCallTipsLvl;
var
  i: integer;
begin
  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  i := integer(pointer(fCallTipStrings.Objects[0]));
  {$POP}
  for i in [0..i-1] do
    fCallTipStrings.Delete(0);
  if fCallTipStrings.Count = 0 then
    hideCallTips
  else
    showCallTipsString(fCallTipStrings.Text, 0);
end;

procedure TDexedMemo.showDDocs;
var
  str: string;
begin
  fCanShowHint := false;
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  DcdWrapper.getDdocFromCursor(str);

  if str.isNotEmpty then
  begin
    fDDocWin.FontSize := Font.Size;
    fDDocWin.HintRect := fDDocWin.CalcHintRect(0, str, nil);
    fDDocWin.OffsetHintRect(mouse.CursorPos, Font.Size);
    fDDocWin.ActivateHint(fDDocWin.HintRect, str);
  end;
end;

procedure TDexedMemo.hideDDocs;
begin
  fCanShowHint := false;
  fDDocWin.Hide;
end;

procedure TDexedMemo.setDDocDelay(value: Integer);
begin
  fDDocDelay:=value;
  fDDocTimer.Interval:=fDDocDelay;
end;

procedure TDexedMemo.DDocTimerEvent(sender: TObject);
begin
  if (not Visible) or (not isDSource) or (not fCanShowHint) then
    exit;

  showDDocs;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION Completion ------------------------------------------------------------}
procedure TDexedMemo.completionExecute(sender: TObject);
begin
  if not fIsDSource and not alwaysAdvancedFeatures then
    exit;
  hideDDocs;
  hideCallTips;
  fCompletion.TheForm.Font.Size := Font.Size;
  fCompletion.TheForm.BackgroundColor:= self.Color;
  fCompletion.TheForm.TextColor:= fD2Highlighter.identifiers.Foreground;
  getCompletionList;
end;

procedure TDexedMemo.completionDeleteKey(sender: TObject);
begin
  if CaretX > 0 then
  begin

    // these "stinky" lines are necessary because
    // the proc is called before deletion is effective
    // i.e the deleted char is still there...
    // related issues #400 and #398
    caretX := caretX - 1;
    ExecuteCommand(ecDeleteChar, #0, nil);
    caretX := caretX + 1;

    getCompletionList();
    if fCompletion.TheForm.ItemList.Count = 0 then
    begin
      fCompletion.TheForm.Close;
      exit;
    end;
  end;
  if fCompletionMenuAutoClose and (fCompletion.CurrentString.length < 2) then
    fCompletion.TheForm.Close;
end;

procedure TDexedMemo.getCompletionList;
var
  i: integer;
  o: TObject;
begin
  if not DcdWrapper.available then
    exit;

  fCompletion.Position := 0;
  fCompletion.ItemList.Clear;
  DcdWrapper.getComplAtCursor(TStringList(fCompletion.ItemList));
  if fLastCompletion.isNotEmpty then
  begin
    i := fCompletion.ItemList.IndexOf(fLastCompletion);
    if i <> -1 then
    begin
      o := fCompletion.ItemList.Objects[i];
      fCompletion.ItemList.Delete(i);
      fCompletion.ItemList.InsertObject(0, fLastCompletion, o);
    end
    else fLastCompletion:= '';
  end;
end;

procedure TDexedMemo.completionCodeCompletion(var value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
begin
  if KeyChar <> '' then
  begin
    if KeyChar[1] = ' ' then
      value := sourceValue + KeyChar[1]
    else
    begin
      fLastCompletion := value;
      if KeyChar[1] in fCloseCompletionCharsWithSpace then
        value += ' ' + KeyChar[1]
      else if KeyChar[1] in fCloseCompletionChars then
        value += KeyChar[1];
    end;
  end;
end;

procedure TDexedMemo.completionFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if char(key) = #9 then
    key := 13;
end;

function TDexedMemo.completionItemPaint(const AKey: string; ACanvas: TCanvas;X, Y: integer;
  Selected: boolean; Index: integer): boolean;
var
  dck: TDCDCompletionKind;
  knd: string;
  len: Integer;
begin
  result := true;
  // empty items can be produced if completion list is too long
  if aKey.isEmpty then
    exit;
  {$PUSH} {$Warnings OFF} {$Hints OFF}
  dck := TDCDCompletionKind(PtrUInt(fCompletion.ItemList.Objects[index]));
  knd := DcdCompletionKindStrings[dck];
  {$POP}
  ACanvas.Font.Style := [fsBold];
  len := ACanvas.TextExtent(aKey).cx;
  ACanvas.TextOut(2 + X , Y, aKey);
  case dck of
    dckALias, dckClass, dckStruct, dckUnion, dckEnum, dckInterface:
      ACanvas.Font.Color:= clMaroon;
    dckMember, dckEnum_member, dckVariable, dckArray, dckAA:
      ACanvas.Font.Color:= clGray;
    dckReserved:
      ACanvas.Font.Color:= clNavy;
    dckFunction:
      ACanvas.Font.Color:= clGreen;
    dckPackage, dckModule:
      ACanvas.Font.Color:= clBlue;
    dckTemplate, dckMixin:
      ACanvas.Font.Color:= clTeal;
  end;
  ACanvas.Font.Style := [fsItalic];
  ACanvas.TextOut(2 + X + len + 2, Y, knd);
end;

procedure TDexedMemo.AutoDotTimerEvent(sender: TObject);
begin
  if (not fCanAutoDot) or (fAutoDotDelay = 0) then
    exit;

  fCanAutoDot := false;
  fCompletion.Execute('', ClientToScreen(point(CaretXPix, CaretYPix + LineHeight)));
end;

procedure TDexedMemo.setAutoDotDelay(value: Integer);
begin
  fAutoDotDelay:=value;
  fAutoDotTimer.Interval:=fAutoDotDelay;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION Dscanner --------------------------------------------------------------}
constructor TDscannerResults.create;
begin
  inherited Create;
end;

destructor TDscannerResults.destroy;
begin
  clear;
  inherited Destroy;
end;

procedure TDscannerResults.clear;
var
  I: PtrInt;
begin
  for I := FList.Count - 1 downto 0 do
    Dispose(fList[i]);
  fList.Clear;
end;

procedure TDscannerResults.push(const warning: string; line, column: integer);
var
  r: PDscannerResult;
begin
  r := new(PDscannerResult);
  r^.column:=column;
  r^.warning:=warning;
  r^.line:=line;
  fList.Add(r);
end;

procedure TDexedMemo.setDscannerOptions(dsEnabled: boolean; dsDelay: integer);
begin
  fDscannerTimer.Interval:=dsDelay;
  fDscannerEnabled := dsEnabled;
  if not dsEnabled then
    removeDscannerWarnings
  else
    dscannerTimerEvent(nil);
end;

procedure TDexedMemo.dscannerTimerEvent(sender: TObject);
var
  s: string;
begin
  if not fDscannerEnabled or not fKnowsDscanner or not isDSource
    or not fCanDscan then
      exit;

  if fDscanner.Running then
  begin
    fDscanner.Terminate(0);
    sleep(1);
  end;
  removeDscannerWarnings;
  fCanDscan := false;
  fDScanner.execute;
  s := Lines.strictText;
  if s.length > 0 then
    fDscanner.Input.Write(s[1], s.length);
  fDscanner.CloseInput;
end;

procedure TDexedMemo.dscannerTerminate(sender: TObject);
  procedure processLine(const lne: string);
  var
    r: TStringRange = (ptr:nil; pos:0; len: 0);
    line: integer;
    column: integer;
  begin
    if lne.isBlank then
      exit;
    r.init(lne);
    line := r.popUntil('(')^.popFront^.takeWhile(['0'..'9']).yield.toIntNoExcept();
    column := r.popFront^.takeWhile(['0'..'9']).yield.toIntNoExcept();
    r.popUntil(':');
    r.popFront;
    fDscannerResults.push(r.takeUntil(#0).yield, line, column);

    addGutterIcon(line, giWarn);

  end;
var
  i: integer;
  s: string;
  m: TStringList;
begin
  m := TStringList.Create;
  try
    fDscanner.getFullLines(m);
    for i := 0 to m.Count-1 do
    begin
      s := m[i];
      processLine(s);
    end;
  finally
    m.free;
  end;
end;

procedure TDexedMemo.removeDscannerWarnings;
var
  i: integer;
  n: TSynEditMark;
begin
  IncPaintLock;
  fDscannerResults.clear;
  for i:= Marks.Count-1 downto 0 do
    if marks.Items[i].ImageIndex = longint(giWarn) then
  begin
    n := marks.Items[i];
    marks.Delete(i);
    FreeAndNil(n);
  end;
  DecPaintLock;
  repaint;
end;

function TDexedMemo.getDscannerWarning(line: integer): string;
const
  spec = '@column %d: %s' + LineEnding;
var
  i: integer;
begin
  result := '';
  for i := 0 to fDscannerResults.count-1 do
    if fDscannerResults[i]^.line = line then
      result += format(spec, [fDscannerResults[i]^.column, fDscannerResults[i]^.warning]);
end;

function TDexedMemo.lineHasDscannerWarning(line: integer): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to fDscannerResults.count-1 do
    if fDscannerResults[i]^.line = line then
      exit(true);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION memo things -----------------------------------------------------------}
procedure TDexedMemo.handleStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
begin
  if scOptions in Changes then
  begin
    if fSmartDdocNewline and not (eoAutoIndent in Options) then
      Options := Options + [eoAutoIndent];
    if Beautifier.isNotNil and (Beautifier is TSynBeautifier) then
    begin
      if not (eoTabsToSpaces in Options) and not (eoSpacesToTabs in Options) then
        TSynBeautifier(Beautifier).IndentType := sbitConvertToTabOnly
      else if eoSpacesToTabs in options then
        TSynBeautifier(Beautifier).IndentType := sbitConvertToTabOnly
      else
        TSynBeautifier(Beautifier).IndentType := sbitSpace;
    end;
  end;
end;

function TDexedMemo.pageCaption(checkModule: boolean): string;
begin
  result := '';
  fHasModuleDeclaration := false;
  if checkModule and isDSource then
  begin
    fLexToks.Clear;
    lex(Lines.Text, fLexToks, @tokFoundForCaption, [lxoNoComments]);
    if fHasModuleDeclaration then
      result := getModuleName(fLexToks);
  end;
  if result.length = 0 then
  begin
    if fFilename.length > 0 then
      result := fFilename.extractFileName
    else
      result := '<new document>';
  end;
end;

procedure TDexedMemo.tokFoundForCaption(const token: PLexToken; out stop: boolean);
begin
  if token^.kind = ltkKeyword then
  begin
    if token^.data = 'module' then
      fModuleTokFound := true
    else
      // "module" is always the first KW
      stop := true;
  end
  else if fModuleTokFound and (token^.kind = ltkSymbol) and (token^.data = ';') then
  begin
    stop := true;
    fModuleTokFound := false;
    fHasModuleDeclaration := true;
  end;
end;

function TDexedMemo.canInsertLeadingDdocSymbol: char;
var
  i: integer;
  p: TPoint;
  tk1: PLexToken = nil;
  tk2: PLexToken = nil;
begin
  // note: never use SelStart here. SelStart is updated too early
  // and matches to the future position, e.g the one after auto-indentation.
  result := #0;
  p := CaretXY;
  for i := 0 to fLexToks.Count-1 do
  begin
    tk1 := fLexToks[i];
    if (i <> fLexToks.Count-1) then
    begin
      tk2 := fLexToks[i+1];
      if (tk1^.position < p) and (tk1^.kind in [ltkComment, ltkIllegal])
        and (p <= tk2^.position) and (tk1^.Data[1] in ['*','+']) then
      begin
        exit(tk1^.Data[1]);
      end
      else if (tk1^.position > p) then
        exit;
    end
    else if (tk1^.position < p) and (tk1^.kind in [ltkComment, ltkIllegal])
      and (tk1^.Data[1] in ['*','+']) then
        exit(tk1^.Data[1]);
  end;
end;

function TDexedMemo.lexCanCloseBrace: boolean;
var
  i: integer;
  p: integer;
  c: integer = 0;
  tok: PLexToken = nil;
  ton: PLexToken = nil;
  bet: boolean;
begin
  p := SelStart;
  for i := 0 to fLexToks.Count-1 do
  begin
    tok := fLexToks[i];
    if (i <> fLexToks.Count-1) then
    begin
      ton := fLexToks[i+1];
      bet := (tok^.offset + 1 <= p) and (p < ton^.offset + 2);
    end else
      bet := false;
    if bet and (tok^.kind = ltkComment) then
      exit(false);
    c += byte((tok^.kind = TLexTokenKind.ltkSymbol) and (((tok^.Data = '{')) or (tok^.Data = 'q{')));
    c -= byte((tok^.kind = TLexTokenKind.ltkSymbol) and (tok^.Data = '}'));
    if bet and (c = 0) then
      exit(false);
  end;
  if (tok <> nil) and (tok^.kind = ltkIllegal) then
    result := false
  else
    result := c > 0;
end;

procedure TDexedMemo.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  inherited;
  fIsDSource := Highlighter = fD2Highlighter;
end;

procedure TDexedMemo.highlightCurrentIdentifier;
var
  str: string;
  i: integer;
begin
  fIdentifier := GetWordAtRowCol(LogicalCaretXY);
  if (fIdentifier.length > 2) and (not SelAvail) then
    SetHighlightSearch(fIdentifier, fMatchIdentOpts)
  else if SelAvail and (BlockBegin.Y = BlockEnd.Y) then
  begin
    str := SelText;
    for i := 1 to str.length do
    begin
      if not (str[i] in [' ', #10, #13]) then
      begin
        SetHighlightSearch(str, fMatchSelectionOpts);
        break;
      end;
      if i = str.length then
        SetHighlightSearch('', []);
    end;
  end
  else SetHighlightSearch('', []);
end;

procedure TDexedMemo.setMatchOpts(value: TIdentifierMatchOptions);
begin
  fMatchOpts:= value;
  fMatchIdentOpts := TSynSearchOptions(fMatchOpts);
  fMatchSelectionOpts:= TSynSearchOptions(fMatchOpts - [wholeWord]);
end;

procedure TDexedMemo.changeNotify(Sender: TObject);
begin
  highlightCurrentIdentifier;
  fModified := true;
  fPositions.store;
  subjDocChanged(TMultiDocSubject(fMultiDocSubject), self);
end;

procedure TDexedMemo.loadFromFile(const fname: string);
var
  e: string;
  c: boolean;
begin
  e := fname.extractFileExt;
  fIsDsource := hasDlangSyntax(e);
  c := hasCppSyntax(e);
  if not fIsDsource then
  begin
    if c then
      Highlighter := CppHighlighter
    else
      Highlighter := TxtSyn;
  end;
  fFilename := fname;
  if not FilenameIsAbsolute(fFilename) then
    fFilename := ExpandFileName(fFilename);
  Lines.LoadFromFile(fname);
  FileAge(fFilename, fFileDate);
  fModified := false;
  if Showing then
  begin
    setFocus;
    loadCache;
    fCacheLoaded := true;
  end;

  tryToPatchMixedIndentation();
  subjDocChanged(TMultiDocSubject(fMultiDocSubject), self);
  fCanDscan := true;
end;

procedure TDexedMemo.saveToFile(const fname: string);
var
  ext: string;
begin
  if fname.fileExists and not FileIsWritable(fname) then
  begin
    getMessageDisplay.message('The file is read-only, save your changes in a copy',
      self, amcEdit, amkWarn);
    exit;
  end;
  Lines.SaveToFile(fname);
  fFilename := fname;
  ext := fname.extractFileExt;
  fIsDsource := hasDlangSyntax(ext);
  if fIsDsource then
    Highlighter := fD2Highlighter
  else if not isProjectDescription then
  begin
    if hasCppSyntax(ext) then
      Highlighter := CppHighlighter
    else
      Highlighter := TxtHighlighter;
  end;
  FileAge(fFilename, fFileDate);
  fModified := false;
  if fFilename <> fTempFileName then
  begin
    if fTempFileName.fileExists then
      sysutils.DeleteFile(fTempFileName);
    subjDocChanged(TMultiDocSubject(fMultiDocSubject), self);
  end;
end;

procedure TDexedMemo.save;
begin
  if fFilename.fileExists and not FileIsWritable(fFilename) then
  begin
    getMessageDisplay.message('The file is read-only, save your changes in a copy',
      self, amcEdit, amkWarn);
    exit;
  end;
  Lines.SaveToFile(fFilename);
  FileAge(fFilename, fFileDate);
  fModified := false;
  if fFilename <> fTempFileName then
    subjDocChanged(TMultiDocSubject(fMultiDocSubject), self);
end;

procedure TDexedMemo.saveTempFile;
begin
  saveToFile(fTempFileName);
  fModified := false;
end;

function TDexedMemo.getIfTemp: boolean;
begin
  exit(fFilename = fTempFileName);
end;

procedure TDexedMemo.saveCache;
var
  cache: TSynMemoCache;
begin
  cache := TSynMemoCache.create(self);
  try
    cache.save;
  finally
    cache.free;
  end;
end;

procedure TDexedMemo.loadCache;
var
  cache: TSynMemoCache;
begin
  cache := TSynMemoCache.create(self);
  try
    cache.load;
  finally
    cache.free;
  end;
end;

class procedure TDexedMemo.cleanCache;
var
  lst: TStringList;
  today, t: TDateTime;
  fname: string;
  y, m, d: word;
begin
  lst := TStringList.Create;
  try
    listFiles(lst, getDocPath + 'editorcache' + DirectorySeparator);
    today := date();
    for fname in lst do if FileAge(fname, t) then
    begin
      DecodeDate(t, y, m, d);
      IncAMonth(y, m, d, 3);
      if EncodeDate(y, m, d) <= today then
        sysutils.DeleteFile(fname);
    end;
  finally
    lst.free;
  end;
end;

procedure TDexedMemo.replaceUndoableContent(const value: string);
var
  b: TPoint;
  e: TPoint;
  p: TPoint;
begin
  p := CaretXY;
  b := point(1,1);
  e := Point(length(Lines[lines.Count-1])+1,lines.Count);
  TextBetweenPoints[b,e] := value;
  CaretXY := p;
  EnsureCursorPosVisible;
  fModified := true;
end;

procedure TDexedMemo.checkFileDate;
var
  mr: TModalResult;
  newDate: double;
  newMd5: TMDDigest;
  curMd5: TMDDigest;
  str: TStringList;
  txt: string;
begin
  if fDiffDialogWillClose or fDisableFileDateCheck then
    exit;
  if fFilename.isNotEmpty and not fFilename.fileExists and
    (fFilename <> '<new document>') then
  begin
    // cant use a dialog: dialog closed -> doc focused -> warn again, etc
    getMessageDisplay.message(fFilename + ' does not exist anymore', self, amcEdit, amkWarn);
  end;
  if (fFilename = fTempFileName) or fDisableFileDateCheck
    or not FileAge(fFilename, newDate) or (fFileDate = newDate) then
      exit;
  if (fFileDate <> 0.0) then
  begin
    str := TStringList.Create;
    try
      str.LoadFromFile(fFilename);
      txt := str.strictText;
      newMd5 := MD5String(txt);
      txt := lines.strictText;
      curMd5 := MD5String(txt);
      if not MDMatch(curMd5, newMd5) then
      begin
        lines.SaveToFile(tempFilename);
        fDiffDialogWillClose := true;
        With TDiffViewer.construct(self, fTempFileName, fFilename) do
        try
          mr := ShowModal;
          case mr of
            mrOK:
            begin
              replaceUndoableContent(str.strictText);
              fModified := false;
              fFileDate := newDate;
            end;
            mrIgnore: fFileDate := newDate;
            mrCancel:;
          end;
        finally
          free;
          fDiffDialogWillClose := false;
        end;
      end;
    finally
      str.Free;
    end;
  end
  else fFileDate := newDate;
end;

function TDexedMemo.getMouseBytePosition: Integer;
var
  i, len, llen: Integer;
begin
  result := 0;
  if fMousePos.y-1 > Lines.Count-1 then exit;
  llen := Lines[fMousePos.y-1].length;
  if fMousePos.X > llen  then exit;
  len := getSysLineEndLen;
  for i:= 0 to fMousePos.y-2 do
    result += Lines[i].length + len;
  result += fMousePos.x;
end;

procedure TDexedMemo.patchClipboardIndentation;
var
  lst: TStringList;
  i: integer;
begin
  //TODO: Check for changes made to option eoSpacesToTabs
  if not (eoTabsToSpaces in Options) then
    exit;

  lst := TStringList.Create;
  lst.Text:=clipboard.asText;
  try
    for i := 0 to lst.count-1 do
    begin
      lst[i] := leadingTabsToSpaces(lst[i], TabWidth);
    end;
    clipboard.asText := lst.strictText;
  finally
    lst.free;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION user input ------------------------------------------------------------}
procedure TDexedMemo.KeyDown(var Key: Word; Shift: TShiftState);
var
  line: string;
  ddc: char;
  lxd: boolean;
begin
  case Key of
    VK_BACK:
    begin
      fCanDscan:=true;
      if fCallTipWin.Visible and (CaretX > 1)
        and (LineText[LogicalCaretXY.X-1] = '(') then
          decCallTipsLvl;
    end;
    VK_RETURN:
    begin
      fCanDscan:=true;
      line := LineText;
      if [ssCtrl] <> Shift then
      begin
        case fAutoCloseCurlyBrace of
          autoCloseOnNewLineAlways: if (CaretX > 1) and (line[LogicalCaretXY.X - 1] = '{') then
          begin
            Key := 0;
            curlyBraceCloseAndIndent;
          end;
          autoCloseOnNewLineEof: if (CaretX > 1) and (line[LogicalCaretXY.X - 1] = '{') then
          if (CaretY = Lines.Count) and (CaretX = line.length+1) then
          begin
            Key := 0;
            curlyBraceCloseAndIndent;
          end;
          else ;
        end;
        if (fAutoCloseCurlyBrace = autoCloseOnNewLineLexically) or
          fSmartDdocNewline then
        begin
          lxd := false;
          if (LogicalCaretXY.X - 1 >= line.length)
              or isBlank(line[LogicalCaretXY.X .. line.length]) then
          begin
            lxd := true;
            fLexToks.Clear;
            lex(lines.Text, fLexToks);
            if lexCanCloseBrace then
            begin
              Key := 0;
              curlyBraceCloseAndIndent;
              lxd := false;
            end;
          end;
          if (fSmartDdocNewline) then
          begin
            if not lxd then
            begin
              fLexToks.Clear;
              lex(lines.Text, fLexToks);
            end;
            ddc := canInsertLeadingDdocSymbol;
            if ddc in ['*', '+'] then
            begin
              inherited;
              insertLeadingDDocSymbol(ddc);
              fCanShowHint:=false;
              fDDocWin.Hide;
              exit;
            end;
          end;
        end;
      end
      else shift := [];
    end;
  end;
  inherited;
  highlightCurrentIdentifier;
  if fCompletion.IsActive then
    fCompletion.CurrentString:= GetWordAtRowCol(LogicalCaretXY);
  case Key of
    VK_BROWSER_BACK: fPositions.back;
    VK_BROWSER_FORWARD: fPositions.next;
    VK_ESCAPE:
      begin
        hideCallTips;
        hideDDocs;
      end;
  end;
  if not (Shift = [ssCtrl]) then exit;
  case Key of
    VK_ADD: if Font.Size < 50 then Font.Size := Font.Size + 1;
    VK_SUBTRACT: if Font.Size > 3 then Font.Size := Font.Size - 1;
    VK_DECIMAL: Font.Size := fDefaultFontSize;
  end;
  fCanShowHint:=false;
  fDDocWin.Hide;
end;

procedure TDexedMemo.KeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_PRIOR, VK_NEXT, VK_UP: fPositions.store;
    VK_OEM_PERIOD, VK_DECIMAL: fCanAutoDot := true;
  end;
  inherited;
  if fAutoCallCompletion and fIsDSource and (not fCompletion.IsActive) and
    (Key < $80) and (char(Key) in ['a'..'z', 'A'..'Z']) then
  begin
    fCompletion.Execute(GetWordAtRowCol(LogicalCaretXY),
      ClientToScreen(point(CaretXPix, CaretYPix + LineHeight)));
  end;
end;

procedure TDexedMemo.UTF8KeyPress(var Key: TUTF8Char);
var
  c: AnsiChar;

procedure reLex();
begin
  fLexToks.Clear;
  lex(Lines.Text, fLexToks);
end;

begin
  c := Key[1];

  // scan source before insertion if pair auto closing is allowed otherwise the
  // tokens following the cursor are wrong after the "inherited" call.
  case c of
    #39: if autoCloseSingleQuote in fAutoClosedPairs then
      reLex();
    '"': if autoCloseDoubleQuote in fAutoClosedPairs then
      reLex();
    '`': if autoCloseBackTick in fAutoClosedPairs then
      reLex();
    '[': if autoCloseSquareBracket in fAutoClosedPairs then
      reLex();
  end;

  inherited;

  fCanDscan := true;
  case c of
    #39: if autoCloseSingleQuote in fAutoClosedPairs then
      autoClosePair(autoCloseSingleQuote);
    ',':
    begin
      if not fCallTipWin.Visible then
        showCallTips(true);
    end;
    '"': if autoCloseDoubleQuote in fAutoClosedPairs then
      autoClosePair(autoCloseDoubleQuote);
    '`': if autoCloseBackTick in fAutoClosedPairs then
      autoClosePair(autoCloseBackTick);
    '[': if autoCloseSquareBracket in fAutoClosedPairs then
      autoClosePair(autoCloseSquareBracket);
    '(': showCallTips(false);
    ')': if fCallTipWin.Visible then decCallTipsLvl;
    '{': if GetKeyShiftState <> [ssShift] then
    begin
        case fAutoCloseCurlyBrace of
          autoCloseAlways:
            curlyBraceCloseAndIndent;
          autoCloseAtEof:
            if (CaretY = Lines.Count) and (CaretX = LineText.length+1) then
              curlyBraceCloseAndIndent;
          autoCloseLexically:
          begin
            fLexToks.Clear;
            lex(lines.Text, fLexToks);
            if lexCanCloseBrace then
              curlyBraceCloseAndIndent;
          end;
          else ;
        end;
    end;
  end;
  if fCompletion.IsActive then
    fCompletion.CurrentString:=GetWordAtRowCol(LogicalCaretXY);
end;

procedure TDexedMemo.MouseLeave;
begin
  inherited;
  hideDDocs;
  hideCallTips;
  fScrollMemo.Visible:=false;
end;

procedure TDexedMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  hideDDocs;
  hideCallTips;
  inherited;
  dx := X - fOldMousePos.x;
  dy := Y - fOldMousePos.y;
  fCanShowHint:=false;
  if (shift = []) then if
    ((dx < 0) and (dx > -5) or (dx > 0) and (dx < 5)) or
      ((dy < 0) and (dy > -5) or (dy > 0) and (dy < 5)) then
        fCanShowHint:=true;
  fOldMousePos := Point(X, Y);
  fMousePos := PixelsToRowColumn(fOldMousePos);
  if ssLeft in Shift then
    highlightCurrentIdentifier;

  if fScrollPreview then
  begin
    if (x > width - 40) and (x < width - 20) then
    begin;
      fScrollMemo.Visible:=true;
      fScrollMemo.goToLine(trunc((lines.Count / Height) * Y));
      fScrollMemo.left := mouse.CursorPos.x - fScrollMemo.Width - 10;
      fScrollMemo.Top:= Y - 5;
    end
    else
    begin
      fScrollMemo.Visible:=false;
    end;
  end;

end;

procedure TDexedMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
  inherited;
  highlightCurrentIdentifier;
  fCanShowHint := false;
  hideCallTips;
  hideDDocs;
  if (emAltSetsColumnMode in MouseOptions) and not (eoScrollPastEol in Options)
    and (ssLeft in shift) and (ssAlt in Shift) then
  begin
    fOverrideColMode := true;
    Options := Options + [eoScrollPastEol];
  end;
end;

procedure TDexedMemo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
var
  pt: TPoint;
begin
  inherited;
  if fScrollPreview and fScrollMemo.Visible and (button = mbLeft) then
  begin
    pt := Mouse.CursorPos;
    pt.x:= pt.x - 40;
    CaretY := fScrollMemo.fMemo.CaretY;
    EnsureCursorPosVisible;
    fScrollMemo.Visible:=false;
    mouse.CursorPos := pt;
    fPositions.store;
  end;
  case Button of
    mbMiddle: if (Shift = [ssCtrl]) then
      Font.Size := fDefaultFontSize;
    mbExtra1: fPositions.back;
    mbExtra2: fPositions.next;
    mbLeft:   fPositions.store;
    else ;
  end;
  if fOverrideColMode and not SelAvail then
  begin
    fOverrideColMode := false;
    Options := Options - [eoScrollPastEol];
  end;
end;

function TDexedMemo.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  fCanShowHint:=false;
  fDDocTimer.Enabled:=false;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION debugging/breakpoints -----------------------------------------------------------}
function TDexedMemo.breakPointsCount: integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to marks.count-1 do
    result += byte(marks[i].ImageIndex = integer(giBreakSet));
end;

procedure TDexedMemo.tryToPatchMixedIndentation;
var
  s: integer;
  t: integer;
begin
  if fLifeTimeManager.isNotNil and not fIdentDialShown and (lines.Count <> 0) and
    ((fLifeTimeManager as ILifetimeManager).getLifetimeStatus = lfsLoaded)
      then
  begin
    fIdentDialShown := true;
    case indentationMode(t, s) of
      imTabs:
        if detectIndentMode then
          Options:= Options - [eoTabsToSpaces];
      imSpaces:
        if detectIndentMode then
          Options:= Options + [eoTabsToSpaces];
      imMixed:
        if (isDSource or alwaysAdvancedFeatures) and
          (dlgYesNo('Mixed indentation style detected in, "' + fFilename +
          '", do you wish to convert to a single mode ?') = mrYes) then
        with TMixedIndentationDialog.construct(s, t) do
        try
        case ShowModal of
          10:
          begin
            forceIndentation(imTabs, TMixedIndentationDialog.fSpacesPerTab);
            Options:= Options - [eoTabsToSpaces];
          end;
          11:
          begin
            forceIndentation(imSpaces, TMixedIndentationDialog.fSpacesPerTab);
            Options:= Options + [eoTabsToSpaces];
          end;
        end;
        finally
          free;
        end;
      else ;
    end;
    if not (eoTabsToSpaces in Options) then
    begin
      BlockIndent := 0;
      BlockTabIndent := 1;
    end
    else
    begin
      BlockIndent := TabWidth;
      BlockTabIndent := 0;
    end;
  end;
end;

procedure TDexedMemo.addBreakPoint(line: integer);
begin
  if findBreakPoint(line) then
    exit;
  addGutterIcon(line, giBreakSet);
  if assigned(fDebugger) then
    fDebugger.addBreakPoint(fFilename, line, bpkBreak);
end;

procedure TDexedMemo.removeBreakPoint(line: integer);
var
  break2step: boolean;
begin
  if not findBreakPoint(line) then
    exit;
  break2step := isGutterIconSet(line, giBreakReached);
  removeGutterIcon(line, giBreakSet);
  if fDscannerEnabled and lineHasDscannerWarning(line) then
    addGutterIcon(line, giWarn);
  if assigned(fDebugger) then
  begin
    fDebugger.removeBreakPoint(fFilename, line);
    if break2step and fDebugger.running then
      addGutterIcon(line, giStep);
  end;
end;

procedure TDexedMemo.showHintEvent(Sender: TObject; HintInfo: PHintInfo);
var
  p: TPoint;
begin
  //if cursor <> crDefault then
  //  exit;
  p := ScreenToClient(mouse.CursorPos);
  if p.x > Gutter.MarksPart.Width then
    exit;
  p := self.PixelsToRowColumn(p);
  showWarningForLine(p.y);
end;

procedure TDexedMemo.removeDebugTimeMarks;
var
  i: integer;
begin
  IncPaintLock;
  for i:= marks.Count-1 downto 0 do
    Marks.Items[i].Visible := not (TGutterIcon(Marks.Items[i].ImageIndex) in debugTimeGutterIcons);
  DecPaintLock;
end;

function TDexedMemo.isGutterIconSet(line: integer; value: TGutterIcon): boolean;
var
  m: TSynEditMarkLine = nil;
  i: integer;
begin
  result := false;
  if line <= lines.count then
    m := marks.Line[line];
  if m.isNotNil then
    for i := 0 to m.count - 1 do
      if (m[i].ImageIndex = integer(value)) then
        exit(true);
end;

function TDexedMemo.findBreakPoint(line: integer): boolean;
begin
  result := isGutterIconSet(line, giBreakSet);
end;

procedure TDexedMemo.gutterClick(Sender: TObject; X, Y, Line: integer; mark: TSynEditMark);
begin
  if findBreakPoint(line) then
    removeBreakPoint(line)
  else
    addBreakPoint(line);
  CaretY := Line;
  EnsureCursorPosVisible;
end;

procedure TDexedMemo.addGutterIcon(line: integer; value: TGutterIcon);
var
  m: TSynEditMarkLine;
  n: TSynEditMark;
  i: integer;
  s: boolean = false;
begin
  m := Marks.Line[line];
  if m.isNotNil then
    for i := 0 to m.Count-1 do
  begin
    s := m.Items[i].ImageIndex = longint(value);
    m.Items[i].Visible := s;
  end;
  if not s then
  begin
    n:= TSynEditMark.Create(self);
    n.Line := line;
    n.ImageList := fImages;
    n.ImageIndex := longint(value);
    n.Visible := true;
    Marks.Add(n);
  end;
end;

procedure TDexedMemo.removeGutterIcon(line: integer; value: TGutterIcon);
var
  m: TSynEditMarkLine;
  n: TSynEditMark;
  i: integer;
begin
  m := Marks.Line[line];
  if m.isNotNil then
    for i := m.Count-1 downto 0 do
  begin
    n := m.Items[i];
    if n.ImageIndex = longint(value) then
    begin
      m.Delete(i);
      FreeAndNil(n);
    end;
  end;
  Repaint;
end;

procedure TDexedMemo.debugStart(debugger: IDebugger);
var
  i: integer;
  m: TSynEditMark;
begin
  fDebugger := debugger;
  fDebugger.removeBreakPoints(fileName);
  for i := 0 to marks.count - 1 do
  begin
    m := marks[i];
    if m.ImageIndex = integer(giBreakSet) then
      fDebugger.addBreakPoint(filename, m.line, bpkBreak);
  end;
end;

procedure TDexedMemo.debugStop;
begin
  removeDebugTimeMarks;
end;

procedure TDexedMemo.debugContinue;
begin
  removeDebugTimeMarks;
end;

function TDexedMemo.debugQueryBpCount: integer;
begin
  exit(breakPointsCount());
end;

procedure TDexedMemo.debugQueryBreakPoint(const line: integer; out fname: string; out kind: TBreakPointKind);
begin
  if findBreakPoint(line) then
  begin
    fname:= fFilename;
    kind := bpkBreak;
  end
  else kind := bpkNone;
end;

procedure TDexedMemo.debugBreak(const fname: string; line: integer;
  reason: TDebugBreakReason);
begin
  if fname <> fFilename then
    exit;
  showPage;
  caretY := line;
  EnsureCursorPosVisible;
  removeDebugTimeMarks;
  removeDscannerWarnings;
  case reason of
    dbBreakPoint: addGutterIcon(line, giBreakReached);
    dbStep, dbSignal: addGutterIcon(line, giStep);
    dbWatch: addGutterIcon(line, giWatch);
    else ;
  end;
end;
{$ENDREGION --------------------------------------------------------------------}

{$ENDREGION --------------------------------------------------------------------}

initialization
  D2Syn := TSynD2Syn.create(nil);
  LfmSyn := TSynLFMSyn.Create(nil);
  TxtSyn := TSynTxtSyn.create(nil);
  JsSyn := TSynJScriptSyn.Create(nil);
  //
  LfmSyn.KeyAttri.Foreground := clNavy;
  LfmSyn.KeyAttri.Style := [fsBold];
  LfmSyn.NumberAttri.Foreground := clMaroon;
  LfmSyn.StringAttri.Foreground := clBlue;
  LfmSyn.SymbolAttribute.Foreground:= clPurple;
  LfmSyn.SymbolAttribute.Style := [fsBold];
  //
  JsSyn.KeyAttri.Foreground := clNavy;
  JsSyn.KeyAttri.Style := [fsBold];
  JsSyn.NumberAttri.Foreground := clMaroon;
  JsSyn.StringAttri.Foreground := clBlue;
  JsSyn.SymbolAttribute.Foreground:= clPurple;
  JsSyn.SymbolAttribute.Style := [fsBold];
  //
  TEditorHintWindow.FontSize := 10;
  //
  RegisterKeyCmdIdentProcs(@CustomStringToCommand, @CustomCommandToSstring);
finalization
  D2Syn.Free;
  LfmSyn.Free;
  TxtSyn.Free;
  JsSyn.Free;
  //
  TDexedMemo.cleanCache;
end.
