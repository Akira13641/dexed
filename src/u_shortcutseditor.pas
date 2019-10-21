unit u_shortcutseditor;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Menus, Graphics,
  ExtCtrls, LCLProc, ComCtrls, Buttons, LCLType, PropEdits, RTTIGrids,
  strutils, u_sharedres, u_observer, u_interfaces, u_common,
  u_writableComponent, u_dialogs, EditBtn;

type

  TShortcutItem = class(TCollectionItem)
  private
    fIdentifier: string;
    fData: TShortcut;
    fDeclarator: IEditableShortCut;
    property declarator: IEditableShortCut read fDeclarator write fDeclarator;
  published
    property identifier: string read fIdentifier write fIdentifier;
    property data: TShortcut read fData write fData;
  public
    function combination: string;
    procedure assign(source: TPersistent); override;
  end;

  TShortCutCollection = class(TWritableLfmTextComponent)
  private
    fItems: TCollection;
    procedure setItems(value: TCollection);
    function getCount: Integer;
    function getItem(index: Integer): TShortcutItem;
  published
    property items: TCollection read fItems write setItems;
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    //
    function findIdentifier(const identifier: string): boolean;
    function findShortcut(aShortcut: Word): TShortcutItem;
    //
    property count: Integer read getCount;
    property item[index: Integer]: TShortcutItem read getItem; default;
  end;

  TEditableShortcut = class(TPersistent)
  public
    value: TShortCut;
  published
    property shortcut: TShortCut read value write value;
  end;

  { TShortcutEditor }

  TShortcutEditor = class(TFrame, IEditableOptions)
    btnClear: TSpeedButton;
    btnEdit: TSpeedButton;
    Panel1: TPanel;
    fltItems: TTreeFilterEdit;
    Panel2: TPanel;
    propedit: TTIPropertyGrid;
    tree: TTreeView;
    procedure btnClearClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    function fltItemsFilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure shortcutCatcherExit(Sender: TObject);
    procedure shortcutCatcherMouseLeave(Sender: TObject);
    procedure propeditModified(Sender: TObject);
    procedure treeSelectionChanged(Sender: TObject);
  private
    fObservers: TEditableShortCutSubject;
    fShortcuts: TShortCutCollection;
    fBackup: TShortCutCollection;
    fHasChanged: boolean;
    propvalue: TEditableShortcut;
    fHasScaled: boolean;
    //
    procedure updateScaling;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    function findCategory(const aName: string; aData: Pointer): TTreeNode;
    function findCategory(const aShortcutItem: TShortcutItem): string;
    function sortCategories(Cat1, Cat2: TTreeNode): integer;
    procedure receiveShortcuts;
    procedure updateEditCtrls;
    procedure sendShortcuts;
    function anItemIsSelected: boolean;
  public
    constructor create(TheOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

var
  ShortcutEditor: TShortcutEditor;

{$REGION TShortCutCollection ---------------------------------------------------}
function TShortcutItem.combination: string;
begin
  result := ShortCutToText(fData);
end;

procedure TShortcutItem.assign(source: TPersistent);
var
  src: TShortcutItem;
begin
  if source is TShortcutItem then
  begin
    src := TShortcutItem(source);
    fData:= src.fData;
    fIdentifier:= src.fIdentifier;
    fDeclarator := src.fDeclarator;
  end
  else inherited;
end;

constructor TShortCutCollection.create(AOwner: TComponent);
begin
  inherited;
  fItems := TCollection.Create(TShortcutItem);
end;

destructor TShortCutCollection.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TShortCutCollection.assign(source: TPersistent);
begin
  if source is TShortCutCollection then
    fItems.Assign(TShortCutCollection(source).fItems)
  else
    inherited;
end;

procedure TShortCutCollection.setItems(value: TCollection);
begin
  fItems.Assign(value);
end;

function TShortCutCollection.getCount: Integer;
begin
  exit(fItems.Count);
end;

function TShortCutCollection.getItem(index: Integer): TShortcutItem;
begin
  exit(TShortcutItem(fItems.Items[index]));
end;

function TShortCutCollection.findIdentifier(const identifier: string): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to count-1 do
    if item[i].identifier = identifier then
      exit(true);
end;

function TShortCutCollection.findShortcut(aShortcut: Word): TShortcutItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to count-1 do
    if item[i].data = aShortcut then
      exit(item[i]);
end;
{$ENDREGION}

{$REGION Standard Comp/Object things -------------------------------------------}
constructor TShortcutEditor.create(TheOwner: TComponent);
begin
  inherited;
  propvalue := TEditableShortcut.Create;
  fObservers := TEditableShortCutSubject.create;
  fShortcuts := TShortCutCollection.create(self);
  fBackup := TShortCutCollection.create(self);
  EntitiesConnector.addObserver(self);
  propedit.TIObject := propvalue;
  propedit.PropertyEditorHook.AddHandlerModified(@propeditModified);
end;

destructor TShortcutEditor.destroy;
begin
  propvalue.Free;
  fObservers.Free;
  inherited;
end;

procedure TShortcutEditor.updateScaling;
begin
  if fHasScaled then
    exit;
  fHasScaled := true;
  case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnClear, 'CLEAN');
      AssignPng(btnEdit, 'SHORTCUTS');
      AssignPng(fltItems.Glyph, 'FILTER_CLEAR')
    end;
    iss24:
    begin
      AssignPng(btnClear, 'CLEAN24');
      AssignPng(btnEdit, 'SHORTCUTS24');
      AssignPng(fltItems.Glyph, 'FILTER_CLEAR24')
    end;
    iss32:
    begin
      AssignPng(btnClear, 'CLEAN32');
      AssignPng(btnEdit, 'SHORTCUTS32');
      AssignPng(fltItems.Glyph, 'FILTER_CLEAR32')
    end;
  end;
  panel2.Height:=scaleY(30, 96);
  propedit.DefaultItemHeight:= scaleY(26, 96);
  propedit.BuildPropertyList();
end;

function TShortcutEditor.anItemIsSelected: boolean;
begin
  result := true;
  if tree.Selected.isNil or (tree.Selected.Level = 0) or tree.Selected.Data.isNil then
    result := false;
end;
{$ENDREGION}

{$REGION IEditableOptions ----------------------------------------------------}
function TShortcutEditor.optionedWantCategory(): string;
begin
  exit('Shortcuts');
end;

function TShortcutEditor.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TShortcutEditor.optionedWantContainer: TPersistent;
begin
  updateScaling;
  receiveShortcuts;
  exit(self);
end;

procedure TShortcutEditor.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeSelectCat:
    begin
      receiveShortcuts;
      fltItems.Text:='';
    end;
    oeeCancel:
    begin
      fShortcuts.assign(fBackup);
      sendShortcuts;
      fHasChanged := false;
    end;
    oeeAccept:
    begin
      fBackup.assign(fShortcuts);
      sendShortcuts;
      fHasChanged := false;
    end;
    else ;
  end;
end;

function TShortcutEditor.optionedOptionsModified: boolean;
begin
  exit(fHasChanged);
end;
{$ENDREGION}

{$REGION shortcut editor things ------------------------------------------------}
procedure TShortcutEditor.treeSelectionChanged(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TShortcutEditor.shortcutCatcherExit(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TShortcutEditor.shortcutCatcherMouseLeave(Sender: TObject);
begin
  updateEditCtrls;
end;

procedure TShortcutEditor.propeditModified(Sender: TObject);
var
  i: integer;
  j: integer;
  s: TShortCut;
  d: TShortcutItem = nil;
  t: string;
  o: TTreeNode;
const
  m1 = 'warning, "%s" is already assigned in the "%s" category and it is not guaranteed to work properly';
  m2 = 'warning, "%s" is already assigned in the same category to "%s". The new shortcut will be ignored';
begin
  if not anItemIsSelected then
    exit;
  s := propvalue.value;
  t := shortCutToText(s);
  if t.isEmpty then
    exit;

  // warn but accept a dup if already in another category
  for i:= 0 to tree.Items.Count-1 do
  begin
    if tree.Items[i] = tree.Selected.Parent then
      continue;
    for j := 0 to Tree.Items[i].Count-1 do
    begin
      o := Tree.Items[i].Items[j];
      if o.Data.isNil then
        continue;
      if TShortcutItem(o.Data).data = s then
      begin
        dlgOkInfo(format(m1, [t, Tree.Items[i].Text]));
        break;
      end;
    end;
  end;

  // warn and discard a dup if already in the same cat.
  for i:= 0 to tree.Selected.Parent.Count-1 do
  begin
    if i = tree.Selected.Index then
      continue;
    o := tree.Selected.Parent.Items[i];
    if o.Data.isNil then
      continue;
    if TShortcutItem(o.Data).data = s then
      d := TShortcutItem(o.Data);
  end;
  if d.isNotNil then
    dlgOkInfo(format(m2,[t, d.identifier]))
  else if TShortcutItem(tree.Selected.Data).data <> s then
  begin
    TShortcutItem(tree.Selected.Data).data := s;
    fHasChanged := true;
  end;
  updateEditCtrls;
end;

procedure TShortcutEditor.btnClearClick(Sender: TObject);
begin
  if not anItemIsSelected then
    exit;
  if TShortcutItem(tree.Selected.Data).data <> 0 then
  begin
    TShortcutItem(tree.Selected.Data).data := 0;
    fHasChanged := true;
  end;
  updateEditCtrls;
end;

procedure TShortcutEditor.btnEditClick(Sender: TObject);
begin
  if not anItemIsSelected then
    exit;
  propedit.Rows[0].Editor.Edit;
end;

function TShortcutEditor.fltItemsFilterItem(Item: TObject; out Done: Boolean): Boolean;
var
  shc: TShortcutItem;
begin
  if fltItems.Filter.isBlank then
  begin
    result := true;
    done := true;
  end
  else
  begin
    result := false;
    done := false;
    // see TTreeFilterEdit: they pass TObject(TTreeNode.Data) and not a TTreeNode
    if Pointer(item).isNil then
      exit;
    shc := TShortcutItem(item);
    result := AnsiContainsText(shc.combination, fltItems.Filter);
    done := true;
  end;
end;

procedure TShortcutEditor.updateEditCtrls;
var
  shc: TShortcutItem;
begin
  if not anItemIsSelected then
    exit;
  shc := TShortcutItem(tree.Selected.Data);
  if propvalue.value <> shc.data then
  begin
    propvalue.value := shc.data;
    propedit.BuildPropertyList;
  end;
end;

function TShortcutEditor.findCategory(const aName: string; aData: Pointer): TTreeNode;
var
  i: integer;
begin
  result := nil;
  for i:= 0 to tree.Items.Count-1 do
    if tree.Items[i].Text = aName then
      if tree.Items[i].Data = aData then
        exit(tree.Items[i]);
end;

function TShortcutEditor.findCategory(const aShortcutItem: TShortcutItem): string;
var
  i, j: integer;
begin
  result := '';
  for i := 0 to tree.Items.Count-1 do
    for j:= 0 to tree.Items.Item[i].Count-1 do
      if tree.Items.Item[i].Items[j].Data = Pointer(aShortcutItem) then
        exit(tree.Items.Item[i].Text);
end;

function TShortcutEditor.sortCategories(Cat1, Cat2: TTreeNode): integer;
begin
  result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TShortcutEditor.receiveShortcuts;
var
  i: Integer;
  obs: IEditableShortCut;
  cat: string;
  sht: word;
  idt: string;
  itm: TShortcutItem;
procedure addItem();
var
  prt: TTreeNode;
begin
  // root category
  if cat.isEmpty or idt.isEmpty then
    exit;
  prt := findCategory(cat, obs);
  if prt.isNil then
    prt := tree.Items.AddObject(nil, cat, obs);
  // item as child
  itm := TShortcutItem(fShortcuts.items.Add);
  itm.identifier := idt;
  itm.data:= sht;
  itm.declarator := obs;
  tree.Items.AddChildObject(prt, idt, itm);
  cat := '';
  idt := '';
end;
begin
  tree.Items.Clear;
  fShortcuts.items.Clear;
  fBackup.items.Clear;
  cat := '';
  idt := '';
  for i:= 0 to fObservers.observersCount-1 do
  begin
    obs := fObservers.observers[i] as IEditableShortCut;
    if obs.scedWantFirst then
    begin
      while obs.scedWantNext(cat, idt, sht) do
        addItem();
      addItem();
    end;
  end;
  tree.Items.SortTopLevelNodes(@sortCategories);
  fBackup.Assign(fShortcuts);
end;

procedure TShortcutEditor.sendShortcuts;
var
  i: integer;
  shc: TShortcutItem;
  decl: IEditableShortCut = nil;
  cat: string;
begin
  for i := 0 to fShortcuts.count-1 do
  begin
    shc := fShortcuts[i];
    decl:= shc.declarator;
    if decl = nil then
      continue;
    cat := findCategory(shc);
    if cat.isEmpty then
      continue;
    decl.scedSendItem(cat, shc.identifier, shc.data);
    if i = fShortcuts.count-1 then
      decl.scedSendDone
    // fShortcuts is always sorted by declarator, cf. receiveShortcuts()
    else if decl <> fShortcuts[i+1].declarator then
      decl.scedSendDone;
  end;
end;
{$ENDREGION}

initialization
  ShortcutEditor := TShortcutEditor.Create(nil);
finalization
  ShortcutEditor.Free;
end.

