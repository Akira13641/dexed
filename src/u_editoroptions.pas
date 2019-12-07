unit u_editoroptions;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Graphics, SynEdit, SynEditMouseCmds, SynEditMiscClasses,
  SynEditKeyCmds, Menus, LCLProc,
  u_interfaces, u_observer, u_common, u_writableComponent, u_synmemo,
  u_d2syn, u_txtsyn;

type

  (**
   * Container for the editor and highlither options.
   * The base class is also used to backup the settings
   * to allow a to preview and restore the settings when rejected.
   *
   * note: when adding a new property, the default value must be set in
   * the constructor according to the default value of the member binded
   * to the property.
   *)
  TEditorOptionsBase = class(TWritableLfmTextComponent)
  private
    // note this is how a TComponent can be edited in
    // a basic TTIGrid: in the ctor create the component
    // but expose it as a published TPersistent.
    fD2Syn: TPersistent;
    fTxtSyn: TPersistent;
    //
    fShortCuts: TCollection;
    //
    fDetectIndentationMode: boolean;
    fCurrLineAttribs: TSynSelectedColor;
    fSelAttribs: TSynSelectedColor;
    fFoldedColor: TSynSelectedColor;
    fMouseLinkAttribs: TSynSelectedColor;
    fBracketMatchAttribs: TSynSelectedColor;
    fIdentifierMarkup: TSynSelectedColor;
    fFont: TFont;
    //
    fResetFontSize: boolean;
    fIdentiMatchOpts: TIdentifierMatchOptions;
    fLineNumEvery: Integer;
    fDDocDelay: Integer;
    fAutoDotDelay: Integer;
    fTabWidth: Integer;
    fBlockIdent: Integer;
    fLineSpacing: Integer;
    fCharSpacing: Integer;
    fRightEdge: Integer;
    fBackground: TColor;
    fRightEdgeColor: TColor;
    fOptions1: TSynEditorOptions;
    fOptions2: TSynEditorOptions2;
    fMouseOptions: TSynEditorMouseOptions;
    fCompletionMenuCaseCare: boolean;
    fCompletionMenuAutoClose: boolean;
    fCompletionMenuWidth: integer;
    fCompletionMenuLines: Byte;
    fAutoCloseCurlyBrace: TBraceAutoCloseStyle;
    fPhobosDocRoot: TPathname;
    fAlwaysAdvancedFeatures: boolean;
    fAutoClosedPairs: TAutoClosePairs;
    fSmartDdocNewline: boolean;
    fInsertPlusDdoc: boolean;
    fAutoCallCompletion: boolean;
    fCloseCompletionCharsWithSpace: AnsiString;
    fCloseCompletionChars: AnsiString;
    fTransparentGutter: boolean;
    fDscannerDelay: integer;
    fDscannerEnabled: boolean;
    fScrollPreview: boolean;
    //
    procedure setPhobosDocRoot(value: TPathname);
    procedure setFont(value: TFont);
    procedure setSelCol(value: TSynSelectedColor);
    procedure setFoldedColor(value: TSynSelectedColor);
    procedure setMouseLinkColor(value: TSynSelectedColor);
    procedure setBracketMatchColor(value: TSynSelectedColor);
    procedure setIdentifierMarkup(value: TSynSelectedColor);
    procedure setCurrLineAttribs(value: TSynSelectedColor);
    procedure setD2Syn(value: TPersistent);
    procedure setTxtSyn(value: TPersistent);
    procedure setShortcuts(value: TCollection);
    procedure setDDocDelay(value: Integer);
    procedure setDscannerDelay(value: Integer);
    procedure setAutoDotDelay(value: Integer);
    procedure setCompletionMenuLines(value: byte);
    procedure setLineNumEvery(value: integer);
  published
    property alwaysAdvancedFeatures: boolean read fAlwaysAdvancedFeatures write fAlwaysAdvancedFeatures;
    property autoCallCompletion: boolean read fAutoCallCompletion write fAutoCallCompletion;
    property autoCloseCurlyBrace: TBraceAutoCloseStyle read fAutoCloseCurlyBrace write fAutoCloseCurlyBrace default TBraceAutoCloseStyle.autoCloseNever;
    property autoClosedPairs: TAutoClosePairs read fAutoClosedPairs write fAutoClosedPairs default[];
    property autoDotDelay: integer read fAutoDotDelay write SetautoDotDelay;
    property background: TColor read fBackground write fBackground default clWhite;
    property blockIndentation: Integer read fBlockIdent write fBlockIdent default 4;
    property bracketMatch: TSynSelectedColor read fBracketMatchAttribs write setBracketMatchColor;
    property characterSpacing: Integer read fCharSpacing write fCharSpacing default 0;
    property closeCompletionCharsWithSpace: AnsiString read fCloseCompletionCharsWithSpace write fCloseCompletionCharsWithSpace;
    property closeCompletionChars: AnsiString read fCloseCompletionChars write fCloseCompletionChars;
    property completionMenuAutoClose: boolean read fCompletionMenuAutoClose write fCompletionMenuAutoClose;
    property completionMenuCaseCare: boolean read fCompletionMenuCaseCare write fCompletionMenuCaseCare;
    property completionMenuLines: byte read fCompletionMenuLines write setCompletionMenuLines;
    property completionMenuWidth: integer read fCompletionMenuWidth write fCompletionMenuWidth;
    property currentLine: TSynSelectedColor read fCurrLineAttribs write setCurrLineAttribs;
    property ddocDelay: Integer read fDDocDelay write setDDocDelay;
    property dscannerDelay: integer read fDscannerDelay write setDscannerDelay;
    property dscannerEnabled: boolean read fDscannerEnabled write fDscannerEnabled;
    property detectIndentMode: boolean read fDetectIndentationMode write fDetectIndentationMode;
    property folding: TSynSelectedColor read fFoldedColor write setFoldedColor;
    property font: TFont read fFont write setFont;
    property highlighterDlang: TPersistent read fD2Syn write setD2Syn;
    property highlighterGeneric: TPersistent read fTxtSyn write setTxtSyn;
    property identifierMatch: TSynSelectedColor read fIdentifierMarkup write SetIdentifierMarkup;
    property identifierMatchOptions: TIdentifierMatchOptions read fIdentiMatchOpts write fIdentiMatchOpts default [caseSensitive];
    property lineNumberEvery: integer read fLineNumEvery write setLineNumEvery default 5;
    property lineSpacing: Integer read fLineSpacing write fLineSpacing default 0;
    property mouseLink: TSynSelectedColor read fMouseLinkAttribs write setMouseLinkColor;
    property mouseOptions: TSynEditorMouseOptions read fMouseOptions write fMouseOptions;
    property options1: TSynEditorOptions read fOptions1 write fOptions1;
    property options2: TSynEditorOptions2 read fOptions2 write fOptions2;
    property phobosDocRoot: TPathname read fPhobosDocRoot write setPhobosDocRoot;
    property plusDdoc: boolean read fInsertPlusDdoc write fInsertPlusDdoc;
    property resetFontSize: boolean read fResetFontSize write fResetFontSize default true;
    property rightEdge: Integer read fRightEdge write fRightEdge default 80;
    property rightEdgeColor: TColor read fRightEdgeColor write fRightEdgeColor default clSilver;
    property selection: TSynSelectedColor read fSelAttribs write setSelCol;
    property shortcuts: TCollection read fShortCuts write setShortcuts;
    property scrollPreview: boolean read fScrollPreview write fScrollPreview;
    property smartDdocNewline: boolean read fSmartDdocNewline write fSmartDdocNewline;
    property tabulationWidth: Integer read fTabWidth write fTabWidth default 4;
    property transparentGutter: boolean read fTransparentGutter write fTransparentGutter default false;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure Assign(source: TPersistent); override;
  end;

  (**
   * Manages and exposes all the editor and highligther options to an TOptionsEditor.
   * It's also responsible to give the current options to a new editor.
   *)
  TEditorOptions = class(TEditorOptionsBase, IEditableOptions, IDocumentObserver, IEditableShortCut)
  private
    fBackup: TEditorOptionsBase;
    fShortcutCount: Integer;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
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
    //
    procedure applyChangeToEditor(anEditor: TDexedMemo);
  protected
    procedure afterLoad; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure applyChangesFromSelf;
  end;

var
  EditorOptions: TEditorOptions;

implementation

const
  edoptFname = 'editor.txt';

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TEditorOptionsBase.Create(AOwner: TComponent);
var
  i: integer;
  shc: TPersistentShortcut;
  ed: TSynEdit;
begin
  inherited;
  //
  fPhobosDocRoot := 'https://dlang.org/phobos/';
  fFont := TFont.Create;
  {$IFDEF WINDOWS}
  fFont.Name := 'Courier New';
  {$ELSE}
  fFont.Name := 'DejaVu Sans Mono';
  {$ENDIF}
  fFont.Quality := fqProof;
  fFont.Pitch := fpDefault;
  fFont.Size := 9;
  fResetFontSize:=true;
  //
  fD2Syn := TSynD2Syn.Create(self);
  fD2Syn.Assign(D2Syn);
  fTxtSyn := TSynTxtSyn.Create(self);
  fTxtSyn.Assign(TxtSyn);
  //
  fDDocDelay:=200;
  fAutoDotDelay:=100;
  fCurrLineAttribs := TSynSelectedColor.Create;
  fSelAttribs := TSynSelectedColor.Create;
  fFoldedColor := TSynSelectedColor.Create;
  fMouseLinkAttribs := TSynSelectedColor.Create;
  fBracketMatchAttribs := TSynSelectedColor.Create;
  fIdentifierMarkup := TSynSelectedColor.Create;
  fCompletionMenuCaseCare := true;
  //
  // note: default values come from TSynEditFoldedView ctor.
  fFoldedColor.Background := clNone;
  fFoldedColor.Foreground := clDkGray;
  fFoldedColor.FrameColor := clDkGray;
  //
  fSelAttribs.Background := 13421772;
  fSelAttribs.Foreground := clNone;
  //
  fMouseLinkAttribs.Style := [fsUnderline, fsBold];
  fMouseLinkAttribs.StyleMask := [];
  fMouseLinkAttribs.Foreground := clNone;
  fMouseLinkAttribs.Background := clNone;
  //
  fBracketMatchAttribs.Foreground := clRed;
  fBracketMatchAttribs.Background := clNone;
  //
  fIdentifierMarkup.Foreground:= clNone;
  fIdentifierMarkup.Background:= clSilver;
  fIdentifierMarkup.BackAlpha:=70;
  fIdentiMatchOpts := [caseSensitive];
  //
  fAutoCloseCurlyBrace:= autoCloseOnNewLineLexically;
  fAutoClosedPairs:= [];
  //
  fCompletionMenuWidth:= 320;
  fCompletionMenuLines:= 15;
  //
  fLineNumEvery := 5;
  rightEdge := 80;
  tabulationWidth := 4;
  blockIndentation := 4;
  fBackground := clWhite;
  fRightEdgeColor := clSilver;
  fSmartDdocNewline:=true;
  fScrollPreview:=true;
  //
  fDscannerEnabled:=true;
  fDscannerDelay:= 2200;
  //
  fCurrLineAttribs.Background := 15789545;
  fCurrLineAttribs.Foreground := clNone;
  //
  fCloseCompletionCharsWithSpace := '*+-/^=~><%';
  fCloseCompletionChars:= '.,;)}]!';
  //
  options1 :=
    [eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoTabsToSpaces, eoTrimTrailingSpaces,
    eoDragDropEditing, eoShowCtrlMouseLinks, eoEnhanceHomeKey, eoTabIndent];
  options2 :=
    [eoEnhanceEndKey, eoFoldedCopyPaste, eoOverwriteBlock];
  //
  mouseOptions := MouseOptions +
    [emAltSetsColumnMode, emDragDropEditing, emCtrlWheelZoom, emShowCtrlMouseLinks];
  //
  fShortCuts := TCollection.Create(TPersistentShortcut);
  ed := TSynEdit.Create(nil);
  try
    // note: cant use a TDexedMemo because it'd be added to the EntitiesConnector
    SetDefaultDexedKeystrokes(ed);
    for i:= 0 to ed.Keystrokes.Count-1 do
    begin
      shc := TPersistentShortcut(fShortCuts.Add);
      shc.actionName:= EditorCommandToCodeString(ed.Keystrokes[i].Command);
      shc.shortcut  := ed.Keystrokes[i].ShortCut;
    end;
  finally
    ed.free;
  end;
end;

destructor TEditorOptionsBase.Destroy;
begin
  fFont.Free;
  fCurrLineAttribs.Free;
  fSelAttribs.Free;
  fShortCuts.Free;
  fFoldedColor.Free;
  fMouseLinkAttribs.Free;
  fBracketMatchAttribs.Free;
  fIdentifierMarkup.Free;
  inherited;
end;

procedure TEditorOptionsBase.Assign(source: TPersistent);
var
  srcopt: TEditorOptionsBase;
begin
  if (source is TEditorOptionsBase) then
  begin
    srcopt := TEditorOptionsBase(source);
    //
    fDscannerDelay:=srcopt.fDscannerDelay;
    fDscannerEnabled:=srcopt.dscannerEnabled;
    fTransparentGutter:=srcopt.fTransparentGutter;
    fAlwaysAdvancedFeatures:=srcopt.fAlwaysAdvancedFeatures;
    fResetFontSize:=srcopt.fResetFontSize;
    fAutoCloseCurlyBrace := srcopt.fAutoCloseCurlyBrace;
    fAutoClosedPairs := srcopt.fAutoClosedPairs;
    fCompletionMenuWidth:=srcopt.fCompletionMenuWidth;
    fCompletionMenuLines:=srcopt.fCompletionMenuLines;
    fCompletionMenuCaseCare:=srcopt.fCompletionMenuCaseCare;
    fCompletionMenuAutoClose:= srcopt.fCompletionMenuAutoClose;
    fAutoDotDelay:=srcopt.fAutoDotDelay;
    fDDocDelay:=srcopt.fDDocDelay;
    fFont.Assign(srcopt.fFont);
    fSelAttribs.Assign(srcopt.fSelAttribs);
    fFoldedColor.Assign(srcopt.fFoldedColor);
    fMouseLinkAttribs.Assign(srcopt.fMouseLinkAttribs);
    fBracketMatchAttribs.Assign(srcopt.fBracketMatchAttribs);
    fCurrLineAttribs.Assign(srcopt.fCurrLineAttribs);
    fD2Syn.Assign(srcopt.fD2Syn);
    fTxtSyn.Assign(srcopt.fTxtSyn);
    background := srcopt.background;
    lineNumberEvery := srcopt.lineNumberEvery;
    identifierMatchOptions:=srcopt.identifierMatchOptions;
    detectIndentMode:=srcopt.detectIndentMode;
    fPhobosDocRoot:=srcopt.fPhobosDocRoot;
    fInsertPlusDdoc:= srcopt.fInsertPlusDdoc;
    fAutoCallCompletion:= srcopt.fAutoCallCompletion;
    fCloseCompletionChars:=srcopt.fCloseCompletionChars;
    fCloseCompletionCharsWithSpace:=srcopt.fCloseCompletionCharsWithSpace;
    fScrollPreview:=srcopt.fScrollPreview;

    fSmartDdocNewline:=srcopt.fSmartDdocNewline;
    if fSmartDdocNewline then
      fOptions1 += [eoAutoIndent];

    tabulationWidth := srcopt.tabulationWidth;
    blockIndentation := srcopt.blockIndentation;
    lineSpacing := srcopt.lineSpacing;
    characterSpacing := srcopt.characterSpacing;
    options1 := srcopt.options1;
    options2 := srcopt.options2;
    mouseOptions := srcopt.mouseOptions;
    rightEdge := srcopt.rightEdge;
    rightEdgeColor := srcopt.rightEdgeColor;
    fShortCuts.Assign(srcopt.fShortCuts);
  end
  else
    inherited;
end;

procedure TEditorOptionsBase.setDDocDelay(value: Integer);
begin
  if value > 2000 then value := 2000
  else if value < 20 then value := 20;
  fDDocDelay:=value;
end;

procedure TEditorOptionsBase.setDscannerDelay(value: Integer);
begin
  if value > 10000 then value := 10000
  else if value < 500 then value := 500;
  fDscannerDelay:=value;
end;

procedure TEditorOptionsBase.setAutoDotDelay(value: Integer);
begin
  if value > 2000 then value := 2000
  else if value < 0 then value := 0;
  fAutoDotDelay:=value;
end;

procedure TEditorOptionsBase.setCompletionMenuLines(value: byte);
begin
  if value < 5 then value := 5
  else if value > 64 then value := 64;
  fCompletionMenuLines := value;
end;

procedure TEditorOptionsBase.setLineNumEvery(value: integer);
begin
  if value < 1 then value := 1
  else if value > 10 then value := 10;
  fLineNumEvery := value;
end;

procedure TEditorOptionsBase.setShortcuts(value: TCollection);
begin
  fShortCuts.Assign(value);
end;

procedure TEditorOptionsBase.setFont(value: TFont);
begin
  fFont.Assign(value);
end;

procedure TEditorOptionsBase.setPhobosDocRoot(value: TPathname);
begin
  if not DirectoryExists(value)  and (value <> 'https://dlang.org/phobos/') then
    value := 'https://dlang.org/phobos/';
  if (value[length(value)] <> DirectorySeparator) and DirectoryExists(value) then
    value += DirectorySeparator;
  fPhobosDocRoot:=value;
end;

procedure TEditorOptionsBase.setSelCol(value: TSynSelectedColor);
begin
  fSelAttribs.Assign(value);
end;

procedure TEditorOptionsBase.setFoldedColor(value: TSynSelectedColor);
begin
  fFoldedColor.Assign(value);
end;

procedure TEditorOptionsBase.setMouseLinkColor(value: TSynSelectedColor);
begin
  fMouseLinkAttribs.Assign(value);
end;

procedure TEditorOptionsBase.setBracketMatchColor(value: TSynSelectedColor);
begin
  fBracketMatchAttribs.Assign(value);
end;

procedure TEditorOptionsBase.SetIdentifierMarkup(value: TSynSelectedColor);
begin
  fIdentifierMarkup.Assign(value);
end;

procedure TEditorOptionsBase.setCurrLineAttribs(value: TSynSelectedColor);
begin
  fCurrLineAttribs.Assign(value);
end;

procedure TEditorOptionsBase.setD2Syn(value: TPersistent);
begin
  D2Syn.Assign(value);
end;

procedure TEditorOptionsBase.setTxtSyn(value: TPersistent);
begin
  TxtSyn.Assign(value);
end;

constructor TEditorOptions.Create(AOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fBackup := TEditorOptionsBase.Create(self);
  EntitiesConnector.addObserver(self);
  //
  fname := getDocPath + edoptFname;
  if fileExists(fname) then
    loadFromFile(fname);
end;

destructor TEditorOptions.Destroy;
begin
  saveToFile(getDocPath + edoptFname);
  //
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TEditorOptions.afterLoad;
var
  ed: TSynEdit;
  shc: TPersistentShortcut;
  i,j: integer;
  exists: boolean;
begin
  inherited;
  D2Syn.Assign(fD2Syn);
  TxtSyn.Assign(fTxtSyn);
  //
  ed := TSynEdit.Create(nil);
  try
    SetDefaultDexedKeystrokes(ed);
    // new version with more shortcuts
    for i:= 0 to ed.Keystrokes.Count-1 do
    begin
      exists := false;
      for j := 0 to fShortcuts.count-1 do
      begin
        if TPersistentShortcut(fShortCuts.Items[j]).actionName <>
          EditorCommandToCodeString(ed.Keystrokes.Items[i].Command) then
            continue;
        exists := true;
        break;
      end;
      if exists then
        continue;
      shc := TPersistentShortcut(fShortCuts.Add);
      shc.actionName := EditorCommandToCodeString(ed.Keystrokes.Items[i].Command);
      shc.shortcut := ed.Keystrokes.Items[i].ShortCut;
    end;
    // new version wih less shortcuts
    for j := fShortcuts.count-1 downto 0 do
    begin
      exists := false;
      for i:= 0 to ed.Keystrokes.Count-1 do
      begin
        if TPersistentShortcut(fShortCuts.Items[j]).actionName <>
          EditorCommandToCodeString(ed.Keystrokes.Items[i].Command) then
            continue;
        exists := true;
        break;
      end;
      if exists then
        continue;
      fShortCuts.Delete(j);
    end;
  finally
    ed.free;
  end;
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TEditorOptions.docNew(document: TDexedMemo);
begin
  //apply...des not modify font size to preserve current zoom
  // when called after the options are edited
  applyChangeToEditor(document);
  // must be set manually for a new doc
  document.Font.Size:=self.font.Size;
end;

procedure TEditorOptions.docFocused(document: TDexedMemo);
begin
end;

procedure TEditorOptions.docChanged(document: TDexedMemo);
begin
end;

procedure TEditorOptions.docClosing(document: TDexedMemo);
begin
  fCompletionMenuWidth := document.completionMenu.TheForm.Width;
  fCompletionMenuLines := document.completionMenu.LinesInWindow;
end;
{$ENDREGION}

{$REGION IEditableShortCut ---------------------------------------------------}
function TEditorOptions.scedWantFirst: boolean;
begin
  result := fShortCuts.Count > 0;
  fShortcutCount := 0;
end;

function TEditorOptions.scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
var
  shrct: TPersistentShortcut;
begin
  shrct     := TPersistentShortcut(fShortCuts.Items[fShortcutCount]);
  category  := 'Code editor';
  identifier:= shrct.actionName;
  // SynEdit shortcuts start with 'ec'
  if identifier.length > 2 then
    identifier := identifier[3..identifier.length];
  aShortcut := shrct.shortcut;
  //
  fShortcutCount += 1;
  result := fShortcutCount < fShortCuts.Count;
end;

procedure TEditorOptions.scedSendItem(const category, identifier: string; aShortcut: TShortcut);
var
  i: Integer;
  shc: TPersistentShortcut;
begin
  if category <> 'Code editor' then exit;
  //
  for i:= 0 to fShortCuts.Count-1 do
  begin
    shc := TPersistentShortcut(fShortCuts.Items[i]);
    if shc.actionName.length > 2 then
    begin
      if shc.actionName[3..shc.actionName.length] <> identifier then
        continue;
    end else if shc.actionName <> identifier then
      continue;
    shc.shortcut:= aShortcut;
    break;
  end;
  // note: shortcut modifications are not reversible,
  // they are sent from another option editor.
end;

procedure TEditorOptions.scedSendDone;
begin
  applyChangesFromSelf;
end;

{$ENDREGION}

{$REGION IEditableOptions ----------------------------------------------------}
function TEditorOptions.optionedWantCategory(): string;
begin
  exit('Editor');
end;

function TEditorOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TEditorOptions.optionedWantContainer: TPersistent;
begin
  fD2Syn := D2Syn;
  fTxtSyn := TxtSyn;
  fBackup.Assign(self);
  fBackup.fD2Syn.Assign(D2Syn);
  fBackup.fTxtSyn.Assign(TxtSyn);
  exit(self);
end;

procedure TEditorOptions.optionedEvent(event: TOptionEditorEvent);
begin
  // restores
  if event = oeeCancel then
  begin
    self.Assign(fBackup);
    D2Syn.Assign(fBackup.fD2Syn);
    TxtSyn.Assign(fBackup.fTxtSyn);
  end;
  // apply, if change/accept event
  // to get a live preview
  if event <> oeeSelectCat then
    applyChangesFromSelf;
  // new backup values based on accepted values.
  if event = oeeAccept then
  begin
    fBackup.Assign(self);
    fBackup.fD2Syn.Assign(D2Syn);
    fBackup.fTxtSyn.Assign(TxtSyn);
  end;
end;

function TEditorOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION IEditableOptions ----------------------------------------------------}
procedure TEditorOptions.applyChangesFromSelf;
var
  multied: IMultiDocHandler;
  i: Integer;
begin
  multied := getMultiDocHandler;
  for i := 0 to multied.documentCount - 1 do
    applyChangeToEditor(multied.document[i]);
end;

procedure TEditorOptions.applyChangeToEditor(anEditor: TDexedMemo);
var
  i, j: Integer;
  shc: TPersistentShortcut;
  kst: TSynEditKeyStroke;
  dup: boolean;
  savedSize: integer;
  cs: TSysCharSet;
  c: char;
begin
  anEditor.D2Highlighter.Assign(D2Syn);
  anEditor.TxtHighlighter.Assign(TxtSyn);

  anEditor.autoDotDelay:=fAutoDotDelay;
  anEditor.ddocDelay:=fDDocDelay;

  savedSize := anEditor.Font.Size;
  anEditor.defaultFontSize := font.Size;
  anEditor.Font.Assign(font);
  if not fResetFontSize then
    anEditor.Font.Size := savedSize;

  anEditor.CppHighlighter.AsmAttri := anEditor.D2Highlighter.inlineAsm;
  anEditor.CppHighlighter.CommentAttri := anEditor.D2Highlighter.comments;
  anEditor.CppHighlighter.DirecAttri := anEditor.D2Highlighter.ddoc;
  anEditor.CppHighlighter.IdentifierAttri := anEditor.D2Highlighter.identifiers;
  anEditor.CppHighlighter.KeyAttri := anEditor.D2Highlighter.keywords;
  anEditor.CppHighlighter.SpaceAttri := anEditor.D2Highlighter.whites;
  anEditor.CppHighlighter.StringAttri := anEditor.D2Highlighter.strings;
  anEditor.CppHighlighter.SymbolAttri := anEditor.D2Highlighter.symbols;
  anEditor.CppHighlighter.NumberAttri := anEditor.D2Highlighter.numbers;

  anEditor.completionMenu.TheForm.Font.Assign(font);
  anEditor.autoCloseCurlyBrace            := fAutoCloseCurlyBrace;
  anEditor.autoClosedPairs                := fAutoClosedPairs;
  anEditor.completionMenu.TheForm.Width   := fCompletionMenuWidth;
  anEditor.completionMenu.LinesInWindow   := fCompletionMenuLines;
  anEditor.completionMenu.CaseSensitive   := fCompletionMenuCaseCare;
  TStringList(anEditor.completionMenu.ItemList).CaseSensitive := fCompletionMenuCaseCare;

  anEditor.Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf:=fLineNumEvery;

  anEditor.SelectedColor.Assign(fSelAttribs);
  anEditor.FoldedCodeColor.Assign(fFoldedColor);
  anEditor.MouseLinkColor.Assign(fMouseLinkAttribs);
  anEditor.BracketMatchColor.Assign(fBracketMatchAttribs);
  anEditor.HighlightAllColor.Assign(fIdentifierMarkup);
  anEditor.LineHighlightColor.Assign(fCurrLineAttribs);
  anEditor.TabWidth := tabulationWidth;
  anEditor.ExtraLineSpacing := lineSpacing;
  anEditor.ExtraCharSpacing := characterSpacing;
  anEditor.Options := options1;
  anEditor.Options2 := options2;
  anEditor.MouseOptions := mouseOptions;
  anEditor.Color := background;
  anEditor.RightEdge := rightEdge;
  anEditor.RightEdgeColor := rightEdgeColor;
  anEditor.IdentifierMatchOptions:= identifierMatchOptions;
  anEditor.detectIndentMode := detectIndentMode;
  anEditor.phobosDocRoot:=fPhobosDocRoot;
  anEditor.alwaysAdvancedFeatures:=fAlwaysAdvancedFeatures;
  anEditor.smartDdocNewline:= fSmartDdocNewline;
  anEditor.insertPlusDdoc:= fInsertPlusDdoc;
  anEditor.autoCallCompletion:= fAutoCallCompletion;
  anEditor.completionMenuAutoClose:=fCompletionMenuAutoClose;
  anEditor.transparentGutter:=fTransparentGutter;
  anEditor.setDscannerOptions(fDscannerEnabled, fDscannerDelay);
  anEditor.scrollPreview:=fScrollPreview;

  if not (eoTabsToSpaces in options1) then
  begin
    anEditor.BlockIndent := 0;
    anEditor.BlockTabIndent := 1;
  end
  else
  begin
    anEditor.BlockIndent := blockIndentation;
    anEditor.BlockTabIndent := 0;
  end;

  cs := [];
  for c in fCloseCompletionCharsWithSpace do
    include(cs, c);
  anEditor.closeCompletionCharsWithSpace:=cs;

  cs := [];
  for c in fCloseCompletionChars do
    include(cs, c);
  anEditor.closeCompletionChars:=cs;

  for i := 0 to anEditor.Keystrokes.Count-1 do
  begin
    kst := anEditor.Keystrokes.Items[i];
    kst.ShortCut:= 0;
    kst.ShortCut2:= 0;
  end;

  for i := 0 to anEditor.Keystrokes.Count-1 do
  begin
    kst := anEditor.Keystrokes.Items[i];
    for j := 0 to fShortCuts.Count-1 do
    begin
      dup := false;
      shc := TPersistentShortcut(fShortCuts.Items[j]);
      if shc.actionName = EditorCommandToCodeString(kst.Command) then
      begin
        if not dup then
        try
          kst.shortCut := shc.shortcut;
        except
          kst.shortCut := 0;
        end;
        break;
      end;
    end;
  end;
end;
{$ENDREGION}

initialization
  EditorOptions := TEditorOptions.Create(nil);

finalization
  EditorOptions.Free;
end.
