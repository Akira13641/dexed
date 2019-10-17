unit u_d2synpresets;
{$I u_defines.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, SynEditHighlighter, SynEditTypes, SynEdit, RTTIGrids, Buttons,
  u_interfaces, u_common, u_writableComponent, u_d2syn, u_observer,
  u_editoroptions, u_sharedres, u_txtsyn;

type

  TAttribHelper = class helper for TSynHighlighterAttributes
    procedure define(
      fore: TColor;
      Stl: TFontStyles = [];
      bck: TColor = clNone;
      frCol: TColor = clNone;
      frStyle: TSynLineStyle = slsSolid;
      frEdges: TSynFrameEdges = sfeNone;
      stlMsk: TFontStyles = []);
  end;

  (**
   * persistent class used to store a highlighter preset.
   *)
  TD2SynPreset = class(TCollectionItem)
  private
    fBackground: TColor;
    fBracketMatchAttribs: TSynSelectedColor;
    fCurrLineAttribs: TSynSelectedColor;
    fFoldedColor: TSynSelectedColor;
    fIdentifierMarkup: TSynSelectedColor;
    fMouseLinkAttribs: TSynSelectedColor;
    fSelAttribs: TSynSelectedColor;
    fd2syn: TPersistent;
    fName: string;
    fIsHardcoded: boolean;
    procedure setBracketMatchColor(value: TSynSelectedColor);
    procedure setCurrLineAttribs(value: TSynSelectedColor);
    procedure setFoldedColor(value: TSynSelectedColor);
    procedure setIdentifierMarkup(value: TSynSelectedColor);
    procedure setMouseLinkColor(value: TSynSelectedColor);
    procedure setSelCol(value: TSynSelectedColor);
    procedure setD2syn(value: TPersistent);
    function getHl: TSynD2Syn;
  published
    property name: string read fName write fName stored true;
    property highlighter: TPersistent read fd2syn write setD2Syn stored true;
    property background: TColor read fBackground write fBackground stored true;
    property bracketMatch: TSynSelectedColor read fBracketMatchAttribs write setBracketMatchColor stored true;
    property currentLine: TSynSelectedColor read fCurrLineAttribs write setCurrLineAttribs stored true;
    property folding: TSynSelectedColor read fFoldedColor write setFoldedColor stored true;
    property identifierMatch: TSynSelectedColor read fIdentifierMarkup write setIdentifierMarkup stored true;
    property mouseLink: TSynSelectedColor read fMouseLinkAttribs write setMouseLinkColor stored true;
    property selection: TSynSelectedColor read fSelAttribs write setSelCol stored true;
    property isHardCoded: boolean read fIsHardcoded stored false;
  public
    constructor Create(ACollection: TCollection); override;
    destructor destroy; override;
    procedure assignToOptions;
    procedure assignFromOptions;
    procedure Assign(source: TPersistent); override;
  end;

  TD2SynPresets = class(TWritableLfmTextComponent)
  private
    fCollection: TCollection;
    procedure setCollection(value: TCollection);
    function getPreset(index: integer): TD2SynPreset;
  published
    property presets: TCollection read fCollection write setCollection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function addPreset: TD2SynPreset;
    function insertPreset(index: integer): TD2SynPreset;
    function count: integer;
    property preset[index: integer]: TD2SynPreset read getPreset ; default;
  end;

  (**
   * UI for loading highlighter presets in the options editor.
   *)
  TD2SynPresetsLoaderForm = class(TWinControl, IEditableOptions)
  private
    fPresets: TD2SynPresets;
    fList: TComboBox;
    fEditor: TSynEdit;
    fPropEd: TTIPropertyGrid;
    fBackup: TD2SynPreset;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    procedure lstBoxSelChange(Sender: TObject);
    procedure btnAddClick(sender: TObject);
    procedure btnDelClick(sender: TObject);
    procedure btnCloneClick(sender: TObject);
    procedure propEdModified(sender: TObject);
    procedure updateList;
    procedure updateEditor;
  protected
    procedure SetVisible(Value: Boolean); override;
  private
    fCloneBtn: TBitBtn;
    fAddBtn: TBitBtn;
    fDelBtn: TBitBtn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

const
  optfname = 'highlighterPresets.txt';

var
  presetsLoaderForm: TD2SynPresetsLoaderForm;

{$REGION TD2SynPreset -------------------------------------------------------}
constructor TD2SynPreset.create(ACollection: TCollection);
begin
  inherited Create(ACOllection);
  fBracketMatchAttribs:= TSynSelectedColor.Create;
  fCurrLineAttribs:= TSynSelectedColor.Create;
  fFoldedColor:= TSynSelectedColor.Create;
  fIdentifierMarkup:= TSynSelectedColor.Create;
  fMouseLinkAttribs:= TSynSelectedColor.Create;
  fSelAttribs:= TSynSelectedColor.Create;
  fd2syn := TSynD2Syn.create(nil);
end;

destructor TD2SynPreset.destroy;
begin
  fBracketMatchAttribs.free;
  fCurrLineAttribs.free;
  fFoldedColor.free;
  fIdentifierMarkup.free;
  fMouseLinkAttribs.free;
  fSelAttribs.free;
  fd2syn.Free;
  inherited;
end;

procedure TD2SynPreset.setD2syn(value: TPersistent);
begin
  fd2syn.Assign(value);
end;

function TD2SynPreset.getHl: TSynD2Syn;
begin
  exit(TSynD2Syn(fd2syn));
end;

procedure TD2SynPreset.setBracketMatchColor(value: TSynSelectedColor);
begin
  fBracketMatchAttribs.Assign(value);
end;

procedure TD2SynPreset.setCurrLineAttribs(value: TSynSelectedColor);
begin
  fCurrLineAttribs.Assign(value);
end;

procedure TD2SynPreset.setFoldedColor(value: TSynSelectedColor);
begin
  fFoldedColor.Assign(value);
end;

procedure TD2SynPreset.setIdentifierMarkup(value: TSynSelectedColor);
begin
  fIdentifierMarkup.Assign(value);
end;

procedure TD2SynPreset.setMouseLinkColor(value: TSynSelectedColor);
begin
  fMouseLinkAttribs.Assign(value);
end;

procedure TD2SynPreset.setSelCol(value: TSynSelectedColor);
begin
  fSelAttribs.Assign(value);
end;

procedure TD2SynPreset.assignToOptions;
begin
  EditorOptions.background:=background;
  EditorOptions.highlighterDlang.Assign(highlighter);
  EditorOptions.bracketMatch.Assign(bracketMatch);
  EditorOptions.currentLine.Assign(currentLine);
  EditorOptions.folding.Assign(folding);
  EditorOptions.identifierMatch.Assign(identifierMatch);
  EditorOptions.mouseLink.Assign(mouseLink);
  EditorOptions.selection.Assign(selection);
  TSynTxtSyn(EditorOptions.highlighterGeneric).whites.Assign(getHl.whites);
  TSynTxtSyn(EditorOptions.highlighterGeneric).text.Assign(getHl.identifiers);
  TSynTxtSyn(EditorOptions.highlighterGeneric).symbols.Assign(getHl.symbols);
  EditorOptions.applyChangesFromSelf;
end;

procedure TD2SynPreset.assignFromOptions;
begin
  background:=EditorOptions.background;
  highlighter.Assign(EditorOptions.highlighterDlang);
  bracketMatch.Assign(EditorOptions.bracketMatch);
  currentLine.Assign(EditorOptions.currentLine);
  folding.Assign(EditorOptions.folding);
  identifierMatch.Assign(EditorOptions.identifierMatch);
  mouseLink.Assign(EditorOptions.mouseLink);
  selection.Assign(EditorOptions.selection);
end;

procedure TD2SynPreset.Assign(source: TPersistent);
var
  src: TD2SynPreset;
begin
  if source is TD2SynPreset then
  begin
    src := TD2SynPreset(source);
    background:=src.background;
    highlighter.Assign(src.highlighter);
    bracketMatch.Assign(src.bracketMatch);
    currentLine.Assign(src.currentLine);
    folding.Assign(src.folding);
    identifierMatch.Assign(src.identifierMatch);
    mouseLink.Assign(src.mouseLink);
    selection.Assign(src.selection);
  end else
    inherited;
end;
{$ENDREGION}

{$REGION TD2SynPresets ------------------------------------------------------}
constructor TD2SynPresets.Create(AOwner: TComponent);
begin
  inherited;
  fCollection := TCollection.Create(TD2SynPreset);
end;

destructor TD2SynPresets.Destroy;
begin
  fCollection.Free;
  inherited;
end;

procedure TD2SynPresets.setCollection(value: TCollection);
begin
  fCollection.Assign(value);
end;

function TD2SynPresets.addPreset: TD2SynPreset;
begin
  exit(TD2SynPreset(fCollection.Add));
end;

function TD2SynPresets.insertPreset(index: integer): TD2SynPreset;
begin
  exit(TD2SynPreset(fCollection.Insert(index)));
end;

function TD2SynPresets.count: integer;
begin
  exit(fCollection.Count);
end;

function TD2SynPresets.getPreset(index: integer): TD2SynPreset;
begin
  exit(TD2SynPreset(fCollection.Items[index]));
end;
{$ENDREGION}

{$REGION TD2SynPresetsLoaderForm --------------------------------------------}
procedure TAttribHelper.define(fore: TColor;Stl: TFontStyles = [];
  bck: TColor = clNone; frCol: TColor = clNone; frStyle: TSynLineStyle = slsSolid;
  frEdges: TSynFrameEdges = sfeNone; stlMsk: TFontStyles = []);
begin
  Background:=bck;
  Foreground:=fore;
  FrameColor:=frCol;
  FrameStyle:=frStyle;
  FrameEdges:=frEdges;
  Style:=stl;
  StyleMask:=stlMsk;
end;

procedure TD2SynPresetsLoaderForm.SetVisible(Value: Boolean);
var
  firstTime: boolean;
begin
  inherited;
  fPropEd.DefaultItemHeight:= scaleY(22, 96);
  // extracted from the ctor : lazarus 1.8 regression
  firstTime := fList.ItemIndex = -1;
  if value and (firstTime or (fList.Items.Count >= 0)) then
  begin
    fEditor.Font.Assign(EditorOptions.font);
    fEditor.Font.Size:=12;
    fEditor.Font.Name:=EditorOptions.font.Name;
    fEditor.Height:= scaleY(160,96);
    if firstTime then
      fList.ItemIndex := 0;
    lstBoxSelChange(nil);
    case GetIconScaledSize of
      iss16:
      begin
        AssignPng(fAddBtn, 'DOCUMENT_ADD');
        AssignPng(fDelBtn, 'DOCUMENT_DELETE');
        AssignPng(fCloneBtn, 'DOCUMENT_PLUS');
      end;
      iss24:
      begin
        AssignPng(fAddBtn, 'DOCUMENT_ADD24');
        AssignPng(fDelBtn, 'DOCUMENT_DELETE24');
        AssignPng(fCloneBtn, 'DOCUMENT_PLUS24');
      end;
      iss32:
      begin
        AssignPng(fAddBtn, 'DOCUMENT_ADD32');
        AssignPng(fDelBtn, 'DOCUMENT_DELETE32');
        AssignPng(fCloneBtn, 'DOCUMENT_PLUS32');
      end;
    end;
  end;
end;

constructor TD2SynPresetsLoaderForm.Create(AOwner: TComponent);
var
  fname: string;
  pnl: TPanel;
begin
  inherited;
  fBackup:= TD2SynPreset.Create(nil);
  fPresets:= TD2SynPresets.Create(self);
  fname := getDocPath + optfname;
  if fname.fileExists then
    fPresets.loadFromFile(fname);

  with fPresets.insertPreset(0) do
  begin
    fIsHardcoded := true;
    fName :='bright';
    fBackground := clWhite;
    getHl.whites.define(clNone);
    getHl.numbers.define($000079F2);
    getHl.symbols.define(clMaroon);
    getHl.identifiers.define(clBlack);
    getHl.comments.define(clGreen,[fsItalic]);
    getHl.strings.define(clBlue);
    getHl.keywords.define(clNavy,[fsBold]);
    getHl.ddoc.define(clTeal);
    getHl.inlineAsm.define(clGray,[fsBold]);
    getHl.special.define(clNavy,[fsBold]);
    getHl.errors.define(clBlack,[],clNone,clRed,slsWaved,sfeBottom,[]);
    getHl.attributes.define(clNavy,[fsBold]);
    //
    folding.Background := clNone;
    folding.Foreground := clDkGray;
    folding.FrameColor := clDkGray;
    //
    mouseLink.Style := [fsUnderline, fsBold];
    mouseLink.StyleMask := [];
    mouseLink.Foreground := clNone;
    mouseLink.Background := clNone;
    //
    bracketMatch.Foreground := clRed;
    bracketMatch.Background := clNone;
    //
    identifierMatch.Foreground:= clNone;
    identifierMatch.Background:= clSilver;
    identifierMatch.BackAlpha:=70;
    identifierMatch.BackPriority:= 10;
    //
    selection.Background:= 15984598;
    selection.Foreground:= clNone;
    //
    currentLine.Background:= 15789545;
    currentLine.Foreground:= clNone;
    //
    getHl.types.Background:=clNone;
    getHl.types.Foreground:=clBlack;
    getHl.types.Style:=[fsBold];
    getHl.phobosStyleType := True;
  end;
  with fPresets.insertPreset(1) do
  begin
    fIsHardcoded := true;
    fName :='dark';
    getHl.whites.FrameEdges := sfeNone;
    getHl.numbers.Foreground := 16761218;
    getHl.numbers.FrameEdges := sfeNone;
    getHl.numbers.Style := [];
    getHl.symbols.Foreground := clYellow;
    getHl.symbols.FrameEdges := sfeNone;
    getHl.identifiers.Foreground := 14807024;
    getHl.identifiers.FrameEdges := sfeNone;
    getHl.comments.Foreground := 13092807;
    getHl.comments.FrameEdges := sfeNone;
    getHl.strings.Foreground := 5157104;
    getHl.strings.FrameEdges := sfeNone;
    getHl.keywords.Foreground := 9684887;
    getHl.keywords.FrameEdges := sfeNone;
    getHl.ddoc.Foreground := 14671730;
    getHl.ddoc.FrameEdges := sfeNone;
    getHl.inlineAsm.Foreground := 15500491;
    getHl.inlineAsm.FrameEdges := sfeNone;
    getHl.special.Foreground := 9684887;
    getHl.special.FrameEdges := sfeNone;
    getHl.errors.Foreground := 14807024;
    getHl.attributes.Foreground := 9684887;
    getHl.attributes.FrameEdges := sfeNone;
    getHl.types.Background:=clNone;
    getHl.types.Foreground:=14807024;
    getHl.types.Style:=[fsBold];
    background := 4210752;
    bracketMatch.Background := clNone;
    bracketMatch.Foreground := clFuchsia;
    currentLine.Background := 6184542;
    currentLine.Foreground := clNone;
    folding.Background := 7303023;
    folding.Foreground := clYellow;
    identifierMatch.Background := 7697781;
    identifierMatch.BackPriority:= 2;
    identifierMatch.Foreground := clNone;
    selection.Background := 10132122;
    selection.Foreground := clNone;
    selection.BackPriority:= 2;
    getHl.phobosStyleType := True;
  end;
  with fPresets.insertPreset(2) do
  begin
    fIsHardcoded := true;
    fName :='Mustard';
    getHl.whites.FrameEdges := sfeNone;
    getHl.numbers.FrameEdges := sfeNone;
    getHl.symbols.Foreground := 3487083;
    getHl.symbols.FrameEdges := sfeNone;
    getHl.identifiers.Foreground := 1975089;
    getHl.identifiers.FrameEdges := sfeNone;
    getHl.comments.Foreground := 5206404;
    getHl.comments.FrameEdges := sfeNone;
    getHl.strings.Foreground := 6056852;
    getHl.strings.FrameEdges := sfeNone;
    getHl.keywords.Foreground := 3226202;
    getHl.keywords.FrameEdges := sfeNone;
    getHl.ddoc.Foreground := 6259092;
    getHl.ddoc.FrameEdges := sfeNone;
    getHl.inlineAsm.Foreground := 3379344;
    getHl.inlineAsm.FrameEdges := sfeNone;
    getHl.special.Foreground := 3226202;
    getHl.special.FrameEdges := sfeNone;
    getHl.errors.Foreground := 1975089;
    getHl.attributes.Foreground := 3226202;
    getHl.attributes.FrameEdges := sfeNone;
    getHl.types.Background:=clNone;
    getHl.types.Foreground:=1975089;
    getHl.types.Style:=[fsBold];
    background := 9818842;
    currentLine.Background := 9030871;
    currentLine.Foreground := clNone;
    folding.Background := clNone;
    folding.Foreground := clYellow;
    folding.FrameColor := clYellow;
    identifierMatch.Background := 10278890;
    identifierMatch.BackPriority:= 2;
    identifierMatch.Foreground := clNone;
    selection.Background := 8448232;
    selection.Foreground := clNone;
    selection.BackPriority := 1;
    getHl.phobosStyleType := True;
  end;
  with fPresets.insertPreset(3) do
  begin
    fIsHardcoded := true;
    name := 'Mars bright';
    getHl.identifiers.Foreground := clBlack;
    getHl.identifiers.FrameEdges := sfeNone;
    getHl.numbers.Foreground := 7763655;
    getHl.comments.Foreground := clMedGray;
    getHl.strings.Foreground := 3750276;
    getHl.keywords.Foreground := 2631874;
    getHl.ddoc.Foreground := 7105644;
    getHl.special.Foreground := 2631874;
    getHl.attributes.Foreground := 2631874;
    getHl.types.Background:=clNone;
    getHl.types.Foreground:=clBlack;
    getHl.types.Style:=[fsBold];
    background := 16579836;
    bracketMatch.Background := 12698077;
    bracketMatch.Foreground := clNone;
    currentLine.Background := 15263976;
    currentLine.Foreground := clNone;
    folding.Background := clNone;
    folding.Foreground := clNone;
    folding.FrameColor := clBlack;
    identifierMatch.Background := clNone;
    identifierMatch.Foreground := clNone;
    identifierMatch.FrameColor := clGray;
    identifierMatch.BackPriority := 10;
    mouseLink.Background := clNone;
    mouseLink.Foreground := clNone;
    mouseLink.FrameColor := 3166415;
    mouseLink.FrameEdges := sfeBottom;
    selection.Background := $C3E1E1;
    selection.Foreground := clNone;
    selection.BackPriority := 10;
    getHl.phobosStyleType := True;
  end;
  with fPresets.insertPreset(4) do
  begin
    fIsHardcoded := true;
    name := 'Mars dark';
    getHl.numbers.Foreground := 7763655;
    getHl.symbols.Foreground := 5460961;
    getHl.identifiers.Foreground := clCream;
    getHl.comments.Foreground := 5095359;
    getHl.strings.Foreground := 10790107;
    getHl.keywords.Foreground := 4539883;
    getHl.ddoc.Foreground := 10540501;
    getHl.inlineAsm.Foreground := 12303291;
    getHl.special.Foreground := 2631874;
    getHl.errors.Foreground := clCream;
    getHl.attributes.Foreground := 2631874;
    getHl.types.Background:=clNone;
    getHl.types.Foreground:=clCream;
    getHl.types.Style:=[fsBold];
    background := 5263440;
    bracketMatch.Background := 9276865;
    bracketMatch.Foreground := clNone;
    currentLine.Background := 4013373;
    currentLine.Foreground := clNone;
    folding.Background := clNone;
    folding.Foreground := clNone;
    folding.FrameColor := clBlack;
    identifierMatch.Background := 6381928;
    identifierMatch.Foreground := clNone;
    identifierMatch.BackPriority := 10;
    mouseLink.Background := clNone;
    mouseLink.Foreground := clNone;
    mouseLink.FrameColor := clRed;
    mouseLink.FrameEdges := sfeBottom;
    selection.Background := 12837345;
    selection.Foreground := clNone;
    getHl.phobosStyleType := True;
  end;
  with fPresets.insertPreset(5) do
  begin
    fIsHardcoded := true;
    name := 'Soft dust';
    getHl.phobosStyleType := True;
    getHl.foldKinds := [fkBrackets, fkRegion];
    getHl.whites.FrameEdges := sfeNone;
    getHl.numbers.Foreground := 8618785;
    getHl.numbers.FrameEdges := sfeNone;
    getHl.symbols.Foreground := 5120546;
    getHl.symbols.FrameEdges := sfeNone;
    getHl.identifiers.FrameEdges := sfeNone;
    getHl.comments.FrameEdges := sfeNone;
    getHl.comments.Style := [];
    getHl.strings.Foreground := 7171346;
    getHl.strings.FrameEdges := sfeNone;
    getHl.keywords.Foreground := 6498601;
    getHl.keywords.FrameEdges := sfeNone;
    getHl.ddoc.Foreground := clGreen;
    getHl.ddoc.FrameEdges := sfeNone;
    getHl.ddoc.Style := [fsBold];
    getHl.inlineAsm.FrameEdges := sfeNone;
    getHl.special.FrameEdges := sfeNone;
    getHl.attributes.FrameEdges := sfeNone;
    background := 15395049;
    bracketMatch.Background := clNone;
    bracketMatch.Foreground := clRed;
    currentLine.Background := 14801617;
    currentLine.Foreground := clNone;
    folding.Background := clNone;
    folding.Foreground := clGray;
    folding.FrameColor := clGray;
    identifierMatch.Background := clSilver;
    identifierMatch.Foreground := clNone;
    identifierMatch.BackPriority := 10;
    identifierMatch.BackAlpha := 70;
    mouseLink.Background := clNone;
    mouseLink.Foreground := clNone;
    mouseLink.Style := [fsBold, fsUnderline];
    selection.Background := $9DAABC;
    selection.Foreground := clNone;
  end;

  fEditor := TSynEdit.Create(self);
  fEditor.Parent:= self;
  fEditor.Height:= ScaleY(200,96);
  fEditor.Align:= alTop;
  fEditor.ReadOnly:=true;
  fEditor.Font.Assign(EditorOptions.font);
  fEditor.Font.Size:=12;
  fEditor.Font.Name:=EditorOptions.font.Name;
  fEditor.BorderSpacing.Around:= 4;
  fEditor.ScrollBars:= ssAutoBoth;
  fEditor.Options:= fEditor.Options - [eoScrollPastEof, eoScrollPastEol];
  fEditor.SetHighlightSearch('writeln',[]);
  fEditor.lines.Add('module preview;');
  fEditor.lines.Add('import std.stdio;');
  fEditor.lines.Add('/// ddoc comment');
  fEditor.lines.Add('@safe void main(string[] args)');
  fEditor.lines.Add('{');
  fEditor.lines.Add('    // writeln is the current identifier');
  fEditor.lines.Add('    writeln("this is a string");');
  fEditor.lines.Add('    writeln(__DATE__);');
  fEditor.lines.Add('    int number = 0xDEADBEEF;');
  fEditor.lines.Add('    asm{ xor RAX, RAX; }');
  fEditor.lines.Add('    int error = 12G;');
  fEditor.lines.Add('    alias fun = () => {};');
  fEditor.lines.Add('}');
  pnl := TPanel.Create(self);
  pnl.Parent := self;
  pnl.AutoSize := true;
  pnl.BevelOuter:= bvNone;
  pnl.BevelInner:= bvNone;
  pnl.Align:=alTop;
  pnl.BorderSpacing.Around:= 2;
  pnl.Height:=30;

  fList := TComboBox.Create(self);
  fList.Align:= alClient;
  fList.BorderSpacing.Around:= 2;
  fList.Parent := pnl;
  fList.Style:=csDropDownList;
  fList.OnSelect:= @lstBoxSelChange;
  fList.AutoSize := true;
  updateList;

  fAddBtn := TBitBtn.Create(self);
  fAddBtn.Parent := pnl;
  fAddBtn.AutoSize := true;
  fAddBtn.Width:= ScaleX(28,96);
  fAddBtn.Align:= alRight;
  fAddBtn.OnClick:=@btnAddClick;
  fAddBtn.Hint:='add preset';

  fDelBtn := TBitBtn.Create(self);
  fDelBtn.Parent := pnl;
  fDelBtn.AutoSize := true;
  fDelBtn.Width:= ScaleX(28,96);
  fDelBtn.Align:= alRight;
  fDelBtn.OnClick:=@btnDelClick;
  fDelBtn.Hint:='delete preset';

  fCloneBtn := TBitBtn.Create(self);
  fCloneBtn.Parent := pnl;
  fCloneBtn.AutoSize := true;
  fCloneBtn.Width:= ScaleX(28,96);
  fCloneBtn.Align:= alRight;
  fCloneBtn.OnClick:=@btnCloneClick;
  fCloneBtn.Hint:='clone preset';

  fPropEd := TTIPropertyGrid.Create(self);
  fPropEd.Parent := self;
  fPropEd.Align:= alClient;
  fPropEd.DefaultValueFont.Color := clGreen;
  fPropEd.OnModified:=@propEdModified;
  fPropEd.CheckboxForBoolean:=true;
  fPropEd.PropertyEditorHook.AddHandlerModified(@propEdModified);

  fList.ItemIndex := 0;
  EntitiesConnector.addObserver(self);
end;

destructor TD2SynPresetsLoaderForm.Destroy;
var
  i: integer;
begin
  for i:= fPresets.count-1 downto 0 do
    if fPresets.preset[i].isHardCoded then
      fPresets.fCollection.Delete(i);
  fPresets.saveToFile(getDocPath + optfname);
  fBackup.Free;
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TD2SynPresetsLoaderForm.optionedWantCategory(): string;
begin
  exit('Highlighter presets');
end;

function TD2SynPresetsLoaderForm.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TD2SynPresetsLoaderForm.optionedWantContainer: TPersistent;
begin
  exit(self);
end;

procedure TD2SynPresetsLoaderForm.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept:
    begin
      fPresets[fList.ItemIndex].assignToOptions;
      fBackup.assignFromOptions;
    end;
    oeeCancel: fBackup.assignToOptions;
    oeeSelectCat: fBackup.assignFromOptions;
  end;
end;

function TD2SynPresetsLoaderForm.optionedOptionsModified: boolean;
begin
  exit(false);
end;

procedure TD2SynPresetsLoaderForm.lstBoxSelChange(Sender: TObject);
begin
  if fList.ItemIndex <> -1 then
  begin
    fPropEd.TIObject := fPresets[fList.ItemIndex];
    fPropEd.SplitterX:= (fPropEd.Width - 20) div 2;
    fPropEd.PreferredSplitterX:= fPropEd.SplitterX;
    fEditor.Highlighter := fPresets[fList.ItemIndex].getHl;
    updateEditor;
  end else
    fPropEd.TIObject := nil;
end;

procedure TD2SynPresetsLoaderForm.btnAddClick(sender: TObject);
var
  prs: TD2SynPreset;
begin
  prs := fPresets.addPreset;
  prs.name := format('preset %d', [fPresets.count]);
  updateList;
  fList.ItemIndex := prs.Index;
  lstBoxSelChange(nil);
end;

procedure TD2SynPresetsLoaderForm.btnDelClick(sender: TObject);
var
  o: integer;
begin
  if (fPresets.preset[fList.ItemIndex].isHardCoded) then
    exit;
  o := fList.ItemIndex;
  fPropEd.TIObject := nil;
  fPresets.fCollection.Delete(fList.ItemIndex);
  if o >= fPresets.count then
    o -= 1 ;
  fList.ItemIndex:=o;
  updateList;
  lstBoxSelChange(nil);
end;

procedure TD2SynPresetsLoaderForm.btnCloneClick(sender: TObject);
var
  old: TD2SynPreset;
begin
  if fList.ItemIndex = -1 then
    exit;
  old := fPresets[fList.ItemIndex];
  btnAddClick(nil);
  fPresets[fList.ItemIndex].Assign(old);
  updateEditor;
end;

procedure TD2SynPresetsLoaderForm.propEdModified(sender: TObject);
begin
  updateEditor;
end;

procedure TD2SynPresetsLoaderForm.updateList;
var
  i, j: integer;
begin
  fList.OnChange:=nil;
  j := fList.ItemIndex;
  fList.Clear;
  for i:= 0 to fPresets.count-1 do
    fList.AddItem(fPresets[i].name, fPresets[i]);
  if (j <> -1) and (j < fPresets.count) then
    fList.ItemIndex := j;
  fList.OnChange:=@lstBoxSelChange;
end;

procedure TD2SynPresetsLoaderForm.updateEditor;
var
  p: TD2SynPreset;
begin
  if fList.ItemIndex = -1 then
    exit;
  p := fPresets[fList.ItemIndex];
  fEditor.Color := p.background;
  fEditor.SelectedColor := p.selection;
  fEditor.HighlightAllColor := p.identifierMatch;
  fEditor.LineHighlightColor := p.currentLine;
  fEditor.FoldedCodeColor := p.folding;
  fEditor.MouseLinkColor := p.mouseLink;
  fEditor.BracketMatchColor := p.bracketMatch;
  fList.Items[fList.ItemIndex] := fPresets[fList.ItemIndex].name;
end;
{$ENDREGION}

initialization
  presetsLoaderForm:= TD2SynPresetsLoaderForm.Create(nil);
finalization
  presetsLoaderForm.Free;
end.

