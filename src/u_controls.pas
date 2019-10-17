unit u_controls;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, buttons, Graphics,
  Menus, Clipbrd;

type
  TPageControlButton = (pbClose, pbMoveLeft, pbMoveRight, pbAdd, pbSplit);
  TPageControlButtons = set of TPageControlButton;

const
  DexedPageControlDefaultButtons = [pbClose, pbMoveLeft, pbMoveRight, pbAdd, pbSplit];

type

  // Used instead of a TTabSheet since only the caption is interesting
  TDexedPage = class(TCustomControl)
  private
    function getIndex: integer;
  protected
    procedure realSetText(const Value: TCaption); override;
  public
    property index: integer read getIndex;
  end;

  TPageControlOption = (poPageHistory, poBottomHeader, poFlatButtons);
  TPageControlOptions = set of TPageControlOption;

const

  defPagesOpt = [poPageHistory];

type

  TPageControlButtonClick = procedure(sender: TObject; button: TPageControlButton) of object;

  (**
   * Minimalist page-control
   *
   * - get rid of the framed aspect of the default LCL one
   * - no published props, no need for design time support
   * - add/close/move left and right speed buttons
   * - a particular tab can be set to reside on a split view
   *)
  TDexedPageControl = class(TWinControl)
  private
    fHeader: TWinControl;
    fTabs: TTabControl;
    fCloseBtn: TSpeedButton;
    fMoveLeftBtn: TSpeedButton;
    fMoveRightBtn: TSpeedButton;
    fAddBtn: TSpeedButton;
    fSplitBtn: TSpeedButton;
    fContent: TPanel;
    fPages: TFPList;
    fPagesHistory: TFPList;
    fPageIndex: integer;
    fSplittedPageIndex: integer;
    fButtons: TPageControlButtons;
    fOnChanged: TNotifyEvent;
    fOnChanging: TTabChangingEvent;
    fSplitter: TSplitter;
    fOldSplitPos: integer;
    fOptions: TPageControlOptions;
    fOnDragDrop: TDragDropEvent;
    fOnDragOver: TDragOverEvent;
    fPageControlButtonClick: TPageControlButtonClick;

    procedure btnCloseClick(sender: TObject);
    procedure btnMoveLeftClick(sender: TObject);
    procedure btnMoveRightClick(sender: TObject);
    procedure btnAddClick(sender: TObject);
    procedure btnSplitClick(sender: TObject);
    procedure btnClickEvent(button: TPageControlButton); inline;

    procedure tabsChanging(sender: TObject; var AllowChange: Boolean);
    procedure tabsChanged(sender: TObject);
    procedure hidePage(index: integer);
    procedure showPage(index: integer);
    procedure setPageIndex(index: integer);
    procedure setButtons(value: TPageControlButtons);
    procedure setCurrentPage(value: TDexedPage);
    function getCurrentPage: TDexedPage;
    function getPageCount: integer;
    function getPage(index: integer): TDexedPage;
    function getSplitPage: TDexedPage;

    procedure changedNotify;
    procedure updateButtonsState;

    procedure setOnDragOver(value: TDragOverEvent);
    procedure setOnDragDrop(value: TDragDropEvent);
    procedure setPagesOptions(value: TPageControlOptions);
    procedure setHeaderPosition(bottom: boolean);

  public
    constructor Create(aowner: TComponent); override;
    destructor Destroy; override;

    function  addPage: TDexedPage;
    procedure deletePage(index: integer);
    function  getPageIndex(page: TDexedPage): integer;
    procedure movePageRight;
    procedure movePageLeft;

    property splitPage: TDexedPage read getSplitPage;
    property currentPage: TDexedPage read getCurrentPage write setCurrentPage;
    property pageIndex: integer read fPageIndex write setPageIndex;
    property pageCount: integer read getPageCount;
    property pages[index: integer]: TDexedPage read getPage; default;

    property buttons: TPageControlButtons read fButtons write setButtons;
    property closeButton: TSpeedButton read fCloseBtn;
    property moveLeftButton: TSpeedButton read fMoveLeftBtn;
    property moveRightButton: TSpeedButton read fMoveRightBtn;
    property addButton: TSpeedButton read fAddBtn;
    property splitButton: TSpeedButton read fSplitBtn;
    property onButtonClick: TPageControlButtonClick read fPageControlButtonClick write fPageControlButtonClick;

    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
    property onChanging: TTabChangingEvent read fOnChanging write fOnChanging;

    property options: TPageControlOptions read fOptions write setPagesOptions default defPagesOpt;

    property OnDragOver read fOnDragOver write setOnDragOver;
    property OnDragDrop read fOnDragDrop write setOnDragDrop;
  end;

  (**
   * A context menu dedicated to TlistView with read-only content.
   * Contains items allowing to copy a column, a line, a cell, etc.
   *)
  TListViewCopyMenu = class(TPopupMenu)
  private
    fList: TListView;
    function getColumnIndex: integer;
    function getLine(item: TListItem): string;
    function getLineAsList(item: TListItem): string;
    procedure copyCell(sender: TObject);
    procedure copyLine(sender: TObject);
    procedure copyLineAsList(sender: TObject);
    procedure copyColumn(sender: TObject);
    procedure copyAll(sender: TObject);
    procedure copyAllAsList(sender: TObject);
    procedure copyAllAsMarkdown(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation

function TDexedPage.getIndex: integer;
var
  ctrl: TDexedPageControl;
  i: integer;
begin
  ctrl := TDexedPageControl(owner);
  for i := 0 to ctrl.pageCount-1 do
      if ctrl.pages[i] = self then
        exit(i);
  exit(-1);
end;

procedure TDexedPage.RealSetText(const Value: TCaption);
var
  i: integer;
  ctrl: TDexedPageControl;
begin
  inherited;
  ctrl :=  TDexedPageControl(owner);
  i := ctrl.getPageIndex(self);
  ctrl.fTabs.BeginUpdate;
  if i <> -1 then
    ctrl.fTabs.Tabs[i] := value;
  ctrl.fTabs.EndUpdate;
end;

constructor TDexedPageControl.Create(aowner: TComponent);
begin
  inherited;

  fHeader := TWinControl.Create(self);
  fHeader.Parent:= self;
  fHeader.Align := alTop;
  fHeader.Height:= ScaleY(32, 96);

  fSplittedPageIndex:=-1;

  fTabs := TTabControl.Create(self);
  fTabs.Parent:= fHeader;
  fTabs.Align := alClient;
  fTabs.Options:=[];
  fTabs.OnChange:=@tabsChanged;
  fTabs.OnChanging:=@tabsChanging;
  {$IFDEF WINDOWS}
  fTabs.BorderSpacing.Top:= 8;
  {$ENDIF}

  fMoveLeftBtn:= TSpeedButton.Create(self);
  fMoveLeftBtn.Parent := fHeader;
  fMoveLeftBtn.Align:= alRight;
  fMoveLeftBtn.Width:= ScaleX(28,96);
  fMoveLeftBtn.BorderSpacing.Around:= 2;
  fMoveLeftBtn.ShowCaption:=false;
  fMoveLeftBtn.OnClick:=@btnMoveLeftClick;
  fMoveLeftBtn.Hint:='move current page to the left';
  fMoveLeftBtn.AutoSize:=true;

  fMoveRightBtn:= TSpeedButton.Create(self);
  fMoveRightBtn.Parent := fHeader;
  fMoveRightBtn.Align:= alRight;
  fMoveRightBtn.Width:= ScaleX(28,96);
  fMoveRightBtn.BorderSpacing.Around:= 2;
  fMoveRightBtn.ShowCaption:=false;
  fMoveRightBtn.OnClick:=@btnMoveRightClick;
  fMoveRightBtn.Hint:='move current page to the right';
  fMoveRightBtn.AutoSize:=true;

  fAddBtn:= TSpeedButton.Create(self);
  fAddBtn.Parent := fHeader;
  fAddBtn.Align:= alRight;
  fAddBtn.Width:= ScaleX(28,96);
  fAddBtn.BorderSpacing.Around:= 2;
  fAddBtn.ShowCaption:=false;
  fAddBtn.OnClick:=@btnAddClick;
  fAddBtn.Hint:='add a new page';
  fAddBtn.AutoSize:=true;

  fCloseBtn := TSpeedButton.Create(self);
  fCloseBtn.Parent := fHeader;
  fCloseBtn.Align:= alRight;
  fCloseBtn.Width:= ScaleX(28,96);
  fCloseBtn.BorderSpacing.Around:= 2;
  fCloseBtn.ShowCaption:=false;
  fCloseBtn.OnClick:=@btnCloseClick;
  fCloseBtn.Hint:='close current page';
  fCloseBtn.AutoSize:=true;

  fSplitBtn := TSpeedButton.Create(self);
  fSplitBtn.Parent := fHeader;
  fSplitBtn.Align:= alRight;
  fSplitBtn.Width:= ScaleX(28,96);
  fSplitBtn.BorderSpacing.Around:= 2;
  fSplitBtn.ShowCaption:=false;
  fSplitBtn.OnClick:=@btnSplitClick;
  fSplitBtn.Hint:= 'pin or un-pin the page to the right';
  fSplitBtn.AutoSize:=true;

  fContent := TPanel.Create(self);
  fContent.Parent := self;
  fContent.Align  := alClient;
  fContent.BevelInner:= bvNone;
  fContent.BevelOuter:= bvNone;
  fContent.BorderStyle:=bsNone;
  fContent.BorderSpacing.Top:=3;
  fContent.AutoSize:=true;

  fSplitter := TSplitter.Create(self);
  fSplitter.Parent := fContent;
  fSplitter.Visible:= false;
  fSplitter.Align := alLeft;
  fSplitter.Width := 6;

  fPages := TFPList.Create;
  fPagesHistory := TFPList.Create;
  fPageIndex := -1;

  setPagesOptions(defPagesOpt);

  fButtons:= DexedPageControlDefaultButtons;
  updateButtonsState;
end;

destructor TDexedPageControl.Destroy;
begin
  while fPages.Count > 0 do
    deletePage(fPages.Count-1);
  fPages.Free;
  fPagesHistory.Free;
  inherited;
end;

procedure TDexedPageControl.setOnDragOver(value: TDragOverEvent);
begin
  if fOnDragOver = value then
    exit;
  fOnDragOver:=value;
  fContent.OnDragOver:=value;
  fTabs.OnDragOver:=value;
end;

procedure TDexedPageControl.setOnDragDrop(value: TDragDropEvent);
begin
  if fOnDragDrop = value then
    exit;
  fOnDragDrop:=value;
  fContent.OnDragDrop:=value;
  fTabs.OnDragDrop:=value;
end;

procedure TDexedPageControl.setPagesOptions(value: TPageControlOptions);
var
  flat: boolean;
begin
  if fOptions = value then
    exit;
  fOptions := value;
  fPagesHistory.Clear;
  setHeaderPosition(poBottomHeader in fOptions);
  flat := poFlatButtons in fOptions;
  fAddBtn.Flat:= flat;
  fCloseBtn.Flat:= flat;
  fMoveLeftBtn.Flat:=flat;
  fMoveRightBtn.Flat:=flat;
  fSplitBtn.Flat:=flat;
end;

procedure TDexedPageControl.setHeaderPosition(bottom: boolean);
begin
  if bottom then
  begin
    fTabs.TabPosition:= tpBottom;
    fHeader.Align:= alBottom;
  end else
  begin
    fTabs.TabPosition:= tpTop;
    fHeader.Align:= alTop;
  end;
end;

procedure TDexedPageControl.changedNotify;
begin
  updateButtonsState;
  if assigned(fOnChanged) then
    fOnChanged(self);
end;

procedure TDexedPageControl.tabsChanged(sender: TObject);
begin
  if fTabs.TabIndex < fPages.Count then
    setPageIndex(fTabs.TabIndex);
end;

procedure TDexedPageControl.tabsChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if assigned(fOnChanging) then fOnChanging(self, AllowChange);
end;

procedure TDexedPageControl.hidePage(index: integer);
var
  pge: TDexedPage;
begin
  if (index < 0) or (index > fPages.Count-1) then
    exit;

  pge := TDexedPage(fPages[index]);
  pge.Visible:=false;
end;

procedure TDexedPageControl.showPage(index: integer);
var
  pge: TDexedPage;
  ctl: TControl;
begin
  if (index < 0) or (index > fPages.Count-1) then
    exit;

  pge := TDexedPage(fPages[index]);
  if (fSplittedPageIndex = -1) or (index = fSplittedPageIndex) then
    pge.Align:=alClient;
  pge.Visible:=true;
  pge.Repaint;
  for ctl in pge.GetEnumeratorControls do
    ctl.Visible:=true;
end;

procedure TDexedPageControl.setPageIndex(index: integer);
var
  leftp, rightp: TDexedPage;
begin
  if (fPageIndex <> fSplittedPageIndex) then
    fOldSplitPos := fSplitter.Left;
  if (index > fPages.Count-1) then
    index := fPages.Count-1;
  if (index < 0) then
      exit;

  if (fSplittedPageIndex = -1) or (index = fSplittedPageIndex) then
  begin
    hidePage(fPageIndex);
    fPageIndex := index;
    showPage(fPageIndex);
    fSplitter.Visible:= false;
  end
  else if (fSplittedPageIndex <> -1)  then
  begin
    hidePage(fPageIndex);
    fPageIndex := index;

    fSplitter.Visible:= true;

    rightp := getPage(fSplittedPageIndex);
    rightp.Align := alClient;
    showPage(fSplittedPageIndex);

    leftp := getPage(fPageIndex);
    leftp.Align := alLeft;
    if fOldSplitPos = 0 then
      leftp.Width:= (fContent.Width - fSplitter.Width) div 2
    else
      leftp.Width:= fOldSplitPos;
    showPage(fPageIndex);
  end;

  if fTabs.TabIndex <> fPageIndex then
    fTabs.TabIndex:= fPageIndex;

  changedNotify;
end;

function TDexedPageControl.addPage: TDexedPage;
var
  pge: TDexedPage;
begin

  if poPageHistory in fOptions then
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
    fPagesHistory.Insert(0, Pointer(PtrUint(fPageIndex)));
    {$POP}

  pge := TDexedPage.Create(self);
  pge.Parent := fContent;
  pge.Align:= alClient;

  fPages.Add(pge);
  fTabs.Tabs.Add(format('', [fPages.Count]));
  setPageIndex(fTabs.Tabs.Count-1);

  result := pge;
end;

procedure TDexedPageControl.deletePage(index: integer);
begin
  if (index > fPages.Count-1) or (index < 0) then
    exit;

  if index = fSplittedPageIndex then
    fSplittedPageIndex := -1
  else if index < fSplittedPageIndex then
    fSplittedPageIndex -= 1;

  TDexedPage(fPages[index]).Free;
  if fPageIndex >= fPages.Count then
    fPageIndex -= 1;

  fPages.Delete(index);
  fTabs.Tabs.Delete(index);

  if (poPageHistory in fOptions) and (fPagesHistory.Count > 0) then
  begin
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
    fPageIndex := Integer(fPagesHistory[0]);
    fPagesHistory.Delete(0);
    {$POP}
  end;

  if fPages.Count = 0 then
    fPageIndex:=-1;

  updateButtonsState;
  setPageIndex(fPageIndex);
end;

function TDexedPageControl.getPageIndex(page: TDexedPage): integer;
begin
  exit(fPages.IndexOf(page));
end;

function TDexedPageControl.getCurrentPage: TDexedPage;
begin
  if (fPageIndex < 0) or (fPageIndex > fPages.Count-1) then
    exit(nil)
  else
    exit(TDexedPage(fPages[fPageIndex]));
end;

procedure TDexedPageControl.setCurrentPage(value: TDexedPage);
begin
  setPageIndex(getPageIndex(value));
end;

function TDexedPageControl.getPageCount: integer;
begin
  exit(fPages.Count);
end;

function TDexedPageControl.getPage(index: integer): TDexedPage;
begin
  exit(TDexedPage(fPages[index]));
end;

function TDexedPageControl.getSplitPage: TDexedPage;
begin
  if fSplittedPageIndex = -1 then
    exit(nil)
  else
    exit(getPage(fSplittedPageIndex));
end;

procedure TDexedPageControl.movePageRight;
var
  i: integer;
begin
  if fPageIndex = fPages.Count-1 then
    exit;

  fPages.Exchange(fPageIndex, fPageIndex + 1);
  fTabs.Tabs.Exchange(fPageIndex, fPageIndex + 1);

  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  for i := 0 to fPagesHistory.Count-1 do
  begin
    if PtrInt(fPagesHistory[i]) = fPageIndex then
      fPagesHistory[i] := Pointer(PtrInt(fPageIndex+1))
    else if PtrInt(fPagesHistory[i]) = fPageIndex+1 then
      fPagesHistory[i] := Pointer(PtrInt(fPageIndex))
  end;
  {$POP}

  if fPageIndex = fSplittedPageIndex then
    fSplittedPageIndex += 1;
  setPageIndex(fPageIndex+1);
end;

procedure TDexedPageControl.movePageLeft;
var
  i: integer;
begin
  if fPageIndex <= 0 then
    exit;

  fPages.Exchange(fPageIndex, fPageIndex - 1);
  fTabs.Tabs.Exchange(fPageIndex, fPageIndex - 1);

  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
  for i := 0 to fPagesHistory.Count-1 do
  begin
    if PtrInt(fPagesHistory[i]) = fPageIndex then
      fPagesHistory[i] := Pointer(PtrInt(fPageIndex-1))
    else if PtrInt(fPagesHistory[i]) = fPageIndex-1 then
      fPagesHistory[i] := Pointer(PtrInt(fPageIndex))
  end;
  {$POP}

  if fPageIndex = fSplittedPageIndex then
    fSplittedPageIndex -= 1;
  setPageIndex(fPageIndex-1);
end;

procedure TDexedPageControl.btnClickEvent(button: TPageControlButton);
begin
  if assigned(fPageControlButtonClick) then
    fPageControlButtonClick(self, button);
end;

procedure TDexedPageControl.btnCloseClick(sender: TObject);
begin
  btnClickEvent(pbClose);
  deletePage(fPageIndex);
end;

procedure TDexedPageControl.btnMoveLeftClick(sender: TObject);
begin
  movePageLeft;
  btnClickEvent(pbMoveLeft);
end;

procedure TDexedPageControl.btnMoveRightClick(sender: TObject);
begin
  movePageRight;
  btnClickEvent(pbMoveRight);
end;

procedure TDexedPageControl.btnAddClick(sender: TObject);
begin
  addPage;
  btnClickEvent(pbAdd);
end;

procedure TDexedPageControl.btnSplitClick(sender: TObject);
begin
  if fPageIndex = fSplittedPageIndex then
    fSplittedPageIndex := -1
  else
  begin
    if (fSplittedPageIndex <> -1) then
      hidePage(fSplittedPageIndex);
    fSplittedPageIndex:= fPageIndex;
  end;
  setPageIndex(fPageIndex);
  btnClickEvent(pbSplit);
end;

procedure TDexedPageControl.setButtons(value: TPageControlButtons);
begin
  if fButtons = value then
    exit;

  fButtons := value;
  updateButtonsState;
  fHeader.ReAlign;
end;

procedure TDexedPageControl.updateButtonsState;
begin
  fHeader.DisableAlign;
  fMoveLeftBtn.Visible := pbMoveLeft in fButtons;
  fMoveRightBtn.Visible := pbMoveRight in fButtons;
  fAddBtn.Visible := pbAdd in fButtons;
  fCloseBtn.Visible := pbClose in fButtons;
  fSplitBtn.Visible := pbSplit in fButtons;
  fSplitBtn.Enabled := fPages.Count > 0;
  fHeader.EnableAlign;
  fCloseBtn.Enabled := fPageIndex <> -1;
  fMoveLeftBtn.Enabled := fPageIndex > 0;
  fMoveRightBtn.Enabled := fPageIndex < fPages.Count-1;
end;

constructor TListViewCopyMenu.create(aOwner: TComponent);
var
  itm: TMenuItem;
begin
  inherited create(aOwner);

  if not (aOwner is TListView) then
  begin
    {$IFDEF DEBUG}
    raise Exception.Create(self.ClassName + ' owner must be a TListView');
    {$ENDIF}
    exit;
  end;
  fList := TListView(aOwner);
  fList.PopupMenu := self;

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy cell';
  itm.OnClick := @copyCell;
  items.Add(itm);

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy line';
  itm.OnClick := @copyLine;
  items.Add(itm);

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy line as list';
  itm.OnClick := @copyLineAsList;
  items.Add(itm);

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy column';
  itm.OnClick := @copyColumn;
  items.Add(itm);

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy all';
  itm.OnClick := @copyAll;
  items.Add(itm);

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy all as list';
  itm.OnClick := @copyAllAsList;
  items.Add(itm);

  itm := TMenuItem.Create(self);
  itm.Caption := 'copy all as markdown table';
  itm.OnClick := @copyAllAsMarkdown;
  items.Add(itm);
end;

function TListViewCopyMenu.getColumnIndex: integer;
var
  i: integer;
  w: integer = 0;
  o: integer = 0;
  p: TPoint;
begin
  result:= 0;

  p := fList.ScreenToControl(PopupPoint);

  for i := 0 to fList.ColumnCount - 1 do
  begin
    o := w;
    w += fList.Column[i].Width;
    if (o < p.x) and (p.x < w) then
      exit(i);
  end;
end;

function TListViewCopyMenu.getLine(item: TListItem): string;
var
  c: integer;
  i: integer;
begin
  result := '';
  if not assigned(item) then
    exit;

  result := item.Caption;
  c := item.SubItems.count-1;
  for i := 0 to c do
  begin
    if i <> c + 1 then
      result += ' | ';
    result += item.SubItems[i];
  end;
end;

function TListViewCopyMenu.getLineAsList(item: TListItem): string;
var
  i: integer;
begin
  result := '';
  if not assigned(item) then
    exit;

  result := item.Caption;
  for i := 0 to item.SubItems.count-1 do
    result += LineEnding + item.SubItems[i];
end;

procedure TListViewCopyMenu.copyCell(sender: TObject);
var
  s: string = '';
  c: integer;
begin
  if not assigned(fList) or not assigned(fList.Selected) then
    exit;

  c := getColumnIndex;
  if c = 0 then
    s := fList.Selected.Caption
  else
    s := fList.Selected.SubItems[c-1];

  clipboard.AsText := s;
end;

procedure TListViewCopyMenu.copyLine(sender: TObject);
begin
  if not assigned(fList) or not assigned(fList.Selected) then
    exit;

  Clipboard.AsText := getLine(fList.Selected);
end;

procedure TListViewCopyMenu.copyLineAsList(sender: TObject);
begin
  if not assigned(fList) or not assigned(fList.Selected) then
    exit;

  Clipboard.AsText := getLineAsList(fList.Selected);
end;

procedure TListViewCopyMenu.copyColumn(sender: TObject);
var
  s: string = '';
  c: integer;
  i: integer;
begin
  if not assigned(fList) or not assigned(fList.Selected) then
    exit;

  c := getColumnIndex;
  if c = 0 then
  begin
    for i := 0 to fList.Items.Count - 1 do
      s += fList.Items[i].Caption + LineEnding;
  end
  else
  begin
    for i := 0 to fList.Items.Count - 1 do
      s += fList.Items[i].SubItems[c-1] + LineEnding;
  end;

  clipboard.AsText := s;
end;

procedure TListViewCopyMenu.copyAll(sender: TObject);
var
  s: string = '';
  c: integer;
  i: integer;
begin
  c := fList.Items.Count - 1;
  for i := 0 to c do
  begin
    s += getLine(fList.Items[i]);
    if i <> c then
      s += LineEnding;
  end;

  clipboard.AsText := s;
end;

procedure TListViewCopyMenu.copyAllAsList(sender: TObject);
var
  s: string = '';
  c: integer;
  i: integer;
begin
  c := fList.Items.Count - 1;
  for i := 0 to c do
  begin
    s += getLineAsList(fList.Items[i]);
    if i <> c then
      s += LineEnding + LineEnding;
  end;

  clipboard.AsText := s;
end;

procedure TListViewCopyMenu.copyAllAsMarkdown(sender: TObject);
var
  s: string = '';
  c: integer;
  i: integer;
  f: integer = 0;
begin
  while fList.Column[f].Caption = '' do
    f += 1;
  c := fList.Items.Count - 1;
  for i:= f to fList.ColumnCount-1 do
    s += '| ' + fList.Column[i].Caption;
  s += LineEnding;
  for i:= f to fList.ColumnCount-1 do
    s += '| ---';
  s += LineEnding;
  for i := 0 to c do
  begin
    s += getLine(fList.Items[i]);
    if i <> c then
      s += LineEnding;
  end;
  clipboard.AsText := s;
end;

end.

