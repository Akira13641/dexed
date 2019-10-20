unit u_messages;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  EditBtn, lcltype, u_widget, ActnList, Menus, clipbrd, AnchorDocking, math,
  TreeFilterEdit, Buttons, GraphType, LGHelpers, LGHashMap, strutils, LazFileUtils,
  u_ddemangle, u_writableComponent, u_common, u_synmemo, u_interfaces,
  u_observer, u_sharedres, u_stringrange, u_dsgncontrols;

type

  TEditorMessagePos = class(specialize TGHashMapLP<string,integer>);

  (**
   * the struct linked to a log message. allow to be filtered.
   *)
  PMessageData = ^TMessageData;
  TMessageData = record
    ctxt: TAppMessageCtxt;
    data: Pointer;
  end;

  TMessagesOptions = class(TWritableLfmTextComponent)
  private
    fFastDisplay: boolean;
    fMaxCount: integer;
    fAutoSelect: boolean;
    fSingleClick: boolean;
    fAutoDemangle: boolean;
    fAlwaysFilter: boolean;
    fMaxLineLength: integer;
    fFont: TFont;
    fMsgColors: array[TAppMessageKind] of TColor;
    procedure setFont(value: TFont);
  published
    property alwaysFilter: boolean read fAlwaysFilter write fAlwaysFilter;
    property fastDisplay: boolean read fFastDisplay write fFastDisplay;
    property maxMessageCount: integer read fMaxCount write fMaxCount;
    property maxLineLength: integer read fMaxLineLength write fMaxLineLength default 4096;
    property autoSelect: boolean read fAutoSelect write fAutoSelect;
    property autoDemangle: boolean read fAutoDemangle write fAutoDemangle;
    property singleMessageClick: boolean read fSingleClick write fSingleClick;
    property font: TFont read fFont write setFont;
    property colorBuble: TColor read fMsgColors[amkBub] write fMsgColors[amkBub];
    property colorInfo: TColor read fMsgColors[amkInf] write fMsgColors[amkInf];
    property colorHint: TColor read fMsgColors[amkHint] write fMsgColors[amkHint];
    property colorWarning: TColor read fMsgColors[amkWarn] write fMsgColors[amkWarn];
    property colorError: TColor read fMsgColors[amkErr] write fMsgColors[amkErr];
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure AssignTo(target: TPersistent); override;
  end;

  // gives access to protected fields that should be actually public
  // (scroll position needed for custom draw !)
  TTreeHack = class(TTreeView)
  end;

  { TMessagesWidget }

  TMessagesWidget = class(TDexedWidget, IEditableOptions, IDocumentObserver, IProjectObserver, IMessagesDisplay)
    btnClearCat: TDexedToolButton;
    sepCat: TDexedToolButton;
    btnSelAll: TDexedToolButton;
    btnSelApp: TDexedToolButton;
    btnSelEdit: TDexedToolButton;
    btnSelMisc: TDexedToolButton;
    btnSelProj: TDexedToolButton;
    sep: TDexedToolButton;
    button2: TDexedToolButton;
    button4: TDexedToolButton;
    button6: TDexedToolButton;
    button8: TDexedToolButton;
    List: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure ListCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure toolbarResize(Sender: TObject);
    procedure TreeFilterEdit1AfterFilter(Sender: TObject);
    procedure TreeFilterEdit1ButtonClick(Sender: TObject);
  private
    fImages: TImageList;
    fEditorMessagePos: TEditorMessagePos;
    fMsgColors: array[TAppMessageKind] of TColor;
    fProjCompile: boolean;
    fActAutoSel: TAction;
    fActClearAll: TAction;
    fActClearCurCat: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fActDemangle: TAction;
    fMaxMessCnt: Integer;
    fAlwaysFilter: boolean;
    fProj: ICommonProject;
    fDoc: TDexedMemo;
    fCtxt: TAppMessageCtxt;
    fAutoSelect: boolean;
    fAutoDemangle: boolean;
    fSingleClick: boolean;
    fastDisplay: boolean;
    fOptions: TMessagesOptions;
    fOptionsBackup: TMessagesOptions;
    fBtns: array[TAppMessageCtxt] of TToolButton;
    fFiltering: boolean;
    function itemShouldBeVisible(item: TTreeNode; aCtxt: TAppMessageCtxt): boolean;
    procedure filterMessages(aCtxt: TAppMessageCtxt);
    procedure clearOutOfRangeMessg;
    procedure actDemangleExecute(Sender: TObject);
    procedure actAutoSelExecute(Sender: TObject);
    procedure actClearCurCatExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
    procedure actCopyMsgExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
    procedure setMaxMessageCount(value: Integer);
    procedure setAutoSelectCategory(value: boolean);
    procedure setSingleMessageClick(value: boolean);
    procedure listDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCtxtClick(Sender: TObject);
    function iconIndex(aKind: TAppMessageKind): Integer;
    procedure handleMessageClick(Sender: TObject);
    //
    procedure setColorError(value: TColor);
    procedure setColorInfo(value: TColor);
    procedure setColorHint(value: TColor);
    procedure setColorBuble(value: TColor);
    procedure setColorWarning(value: TColor);
    //
    procedure projNew(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    //
    procedure docNew(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    function openFileFromDmdMessage(const aMessage: string): boolean;
    function getLineFromMessage(const aMessage: string): TPoint;
    function guessMessageKind(const aMessg: string): TAppMessageKind;
    //
    function singleServiceName: string;
    procedure message(const value: string; aData: Pointer; aCtxt: TAppMessageCtxt; aKind: TAppMessageKind);
    procedure clearbyContext(aCtxt: TAppMessageCtxt);
    procedure clearbyData(data: Pointer);
    procedure scrollToBack;
  protected
    procedure setToolBarFlat(value: boolean); override;
    procedure updateLoop; override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    property maxMessageCount: Integer     read fMaxMessCnt  write setMaxMessageCount;
    property autoSelectCategory: boolean  read fAutoSelect  write setAutoSelectCategory;
    property autoDEmangle: boolean        read fAutoDemangle write fAutoDemangle;
    property singleMessageClick: boolean  read fSingleClick write setSingleMessageClick;
    //
    property colorBuble: TColor   read fMsgColors[amkBub]   write setColorBuble;
    property colorInfo: TColor    read fMsgColors[amkInf]   write setColorInfo;
    property colorHint: TColor    read fMsgColors[amkHint]  write setColorHint;
    property colorWarning: TColor read fMsgColors[amkWarn]  write setColorWarning;
    property colorError: TColor   read fMsgColors[amkErr]   write setColorError;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  // Maps an identifier to a message kind
  type messageSemantic = record
  private

    {
          rendered on 2017-Jan-19 21:11:15.6560057 by IsItThere.
           - PRNG seed: 0
           - map length: 64
           - case sensitive: true
    }

    const fWords: array [0..63] of string =
      ('caution', '', '', '', 'Fatal', 'Critical', 'Error', 'Advice',
       'Warn', '', '', 'advice', '', 'warning', '', 'information',
       '', '', '', 'Exception', '', '', 'illegal', '',
       '', 'Hint', 'errorlevel', 'Warning', 'critical', 'Deprecated', 'Invalid', 'fatal',
       '', 'Deprecation', 'hint', '', '', 'error', '', '',
       '', 'Caution', 'Tip', '', 'deprecated', '', '', 'Suggestion',
       'deprecation', '', 'exception', 'ERROR', 'Information', '', '', '',
       'suggestion', 'invalid', 'warn', 'Illegal', '', '', '', 'tip');

    const fFilled: array [0..63] of boolean =
      (true, false, false, false, true, true, true, true, true, false, false,
       true, false, true, false, true, false, false, false, true, false, false,
       true, false, false, true, true, true, true, true, true, true, false, true,
       true, false, false, true, false, false, false, true, true, false, true,
       false, false, true, true, false, true, true, true, false, false, false,
       true, true, true, true, false, false, false, true);

    const fMap: array [0..63] of TAppMessageKind =
      ( amkWarn, amkBub,  amkBub, amkBub, amkErr, amkErr, amkErr, amkHint,
        amkWarn, amkBub,  amkBub, amkHint, amkBub, amkWarn, amkBub, amkInf,
        amkBub, amkBub,  amkBub, amkErr, amkBub, amkBub, amkErr, amkBub,
        amkBub, amkHint,  amkErr, amkWarn, amkErr, amkWarn, amkErr, amkErr,
        amkBub, amkWarn,  amkHint, amkBub, amkBub, amkErr, amkBub, amkBub,
        amkBub, amkWarn,  amkHint, amkBub, amkWarn, amkBub, amkBub, amkHint,
        amkWarn, amkBub,  amkErr, amkErr, amkInf, amkBub, amkBub, amkBub,
        amkHint, amkErr,  amkWarn, amkErr, amkBub, amkBub, amkBub, amkHint);

    const fCoefficients: array [0..255] of Byte =
      (151, 145, 214, 156, 15, 232, 156, 185, 180, 64, 178, 95, 6, 249, 69, 68,
       240, 111, 93, 41, 229, 240, 146, 62, 148, 157, 106, 190, 120, 112, 104,
       207, 85, 123, 228, 254, 43, 2, 236, 108, 39, 221, 41, 251, 144, 192, 247,
       101, 210, 134, 105, 39, 208, 115, 116, 65, 209, 36, 237, 87, 195, 162,
       142, 33, 203, 95, 12, 200, 124, 111, 9, 145, 187, 238, 173, 155, 214,
       127, 229, 197, 232, 87, 213, 92, 39, 25, 218, 24, 193, 223, 45, 35, 157,
       4, 34, 244, 154, 99, 21, 95, 203, 14, 100, 113, 68, 201, 199, 174, 249,
       30, 153, 251, 122, 129, 244, 229, 188, 101, 103, 138, 164, 136, 188, 209,
       164, 192, 76, 159, 40, 182, 137, 202, 107, 115, 9, 23, 14, 166, 47, 71,
       243, 156, 148, 176, 187, 247, 143, 124, 180, 14, 250, 157, 212, 18, 151,
       246, 174, 222, 41, 114, 148, 24, 34, 97, 116, 37, 173, 30, 177, 20, 55,
       18, 15, 149, 94, 129, 87, 72, 25, 3, 82, 200, 198, 214, 49, 228, 10, 39,
       191, 83, 128, 30, 117, 209, 216, 152, 89, 237, 253, 24, 173, 116, 65, 64,
       55, 222, 210, 243, 140, 82, 219, 8, 35, 13, 123, 43, 15, 72, 174, 28, 10,
       242, 74, 136, 18, 198, 247, 240, 196, 146, 49, 39, 175, 186, 38, 8, 110,
       101, 179, 242, 152, 251, 227, 60, 25, 31, 123, 80, 149, 187, 67, 157, 120,
       84, 83, 192);

    class function hash(const w: string): Word; static;
  public
    class function getType(const w: string): TAppMessageKind; static;
  end;

implementation
{$R *.lfm}

const
  optname = 'messages.txt';
  minColor = $232323;

{$REGION TMessagesOptions ----------------------------------------------------}
constructor TMessagesOptions.Create(AOwner: TComponent);
begin
  inherited;
  fFont := TFont.Create;
  fFont.Style := [fsBold];
  {$IFDEF WINDOWS}
  fFont.name := 'Consolas';
  {$ENDIF}
  fFont.Size := ScaleY(11,96);
  fAutoSelect :=true;
  fMaxCount := 1000;
  fMaxLineLength:= 4096;
  fMsgColors[amkBub] := $FCE7D2;
  fMsgColors[amkWarn]:= $B3FFFF;
  fMsgColors[amkErr] := $BDBDFF;
  fMsgColors[amkInf] := $FFD0A8;
  fMsgColors[amkHint]:= $C2FFC2;
end;

destructor TMessagesOptions.destroy;
begin
  fFont.Free;
  inherited;
end;

procedure TMessagesOptions.setFont(value: TFont);
begin
  fFont.Assign(value);
end;

procedure TMessagesOptions.assign(source: TPersistent);
var
  widg : TMessagesWidget;
  opts : TMessagesOptions;
begin
  if source is TMessagesOptions then
  begin
    opts := TMessagesOptions(source);
    fFont.BeginUpdate;
    fFont.Assign(opts.font);
    fMaxCount := opts.fMaxCount;
    fAutoSelect := opts.fAutoSelect;
    fAutoDemangle:= opts.fAutoDemangle;
    fSingleClick := opts.fSingleClick;
    fFastDisplay := opts.fFastDisplay;
    fMsgColors := opts.fMsgColors;
    fAlwaysFilter := opts.fAlwaysFilter;
    fFont.EndUpdate;
  end
  else if source is TMessagesWidget then
  begin
    widg := TMessagesWidget(source);
    fFont.Assign(widg.List.Font);
    fMaxCount := widg.fMaxMessCnt;
    fAutoSelect := widg.fAutoSelect;
    fSingleClick := widg.fSingleClick;
    fFastDisplay := widg.fastDisplay;
    fMsgColors := widg.fMsgColors;
    fAutoDemangle:= widg.fAutoDemangle;
    fAlwaysFilter:=widg.fAlwaysFilter;
  end
  else inherited;
end;

procedure TMessagesOptions.AssignTo(target: TPersistent);
var
  widg : TMessagesWidget;
begin
  if target is TMessagesWidget then
  begin
    widg := TMessagesWidget(target);
    widg.List.Font.Assign(fFont);
    widg.maxMessageCount := fMaxCount;
    widg.autoSelectCategory := fAutoSelect;
    widg.singleMessageClick := fSingleClick;
    widg.fastDisplay:= fFastDisplay;
    widg.fMsgColors := fMsgColors;
    widg.fAutoDemangle:=fAutoDemangle;
    widg.fAlwaysFilter:=fAlwaysFilter;
    if fFastDisplay then
      widg.updaterByLoopInterval:= 70
    else
      widg.updaterByLoopInterval:= 0;
  end
  else inherited;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TMessagesWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  fMaxMessCnt := 500;
  fAutoSelect := true;
  fCtxt := amcAll;

  fActAutoSel := TAction.Create(self);
  fActAutoSel.Caption := 'Auto select message category';
  fActAutoSel.AutoCheck := true;
  fActAutoSel.OnExecute := @actAutoSelExecute;
  fActClearAll := TAction.Create(self);
  fActClearAll.OnExecute := @actClearAllExecute;
  fActClearAll.caption := 'Clear all messages';
  fActClearCurCat := TAction.Create(self);
  fActClearCurCat.OnExecute := @actClearCurCatExecute;
  fActClearCurCat.caption := 'Clear filtered messages';
  fActCopyMsg := TAction.Create(self);
  fActCopyMsg.OnExecute := @actCopyMsgExecute;
  fActCopyMsg.Caption := 'Copy message(s)';
  fActSelAll := TAction.Create(self);
  fActSelAll.OnExecute := @actSelAllExecute;
  fActSelAll.Caption := 'Select all';
  fActSaveMsg := TAction.Create(self);
  fActSaveMsg.OnExecute := @actSaveMsgExecute;
  fActSaveMsg.caption := 'Save selected message(s) to...';
  fActDemangle := TAction.Create(self);
  fActDemangle.OnExecute := @actDemangleExecute;
  fActDemangle.caption := 'Demangle selection';

  inherited;

  fImages:= TImageList.Create(self);
  Case GetIconScaledSize of
    iss16:
    begin
      fImages.Width:=16;
      fImages.Height:=16;
      AssignPng(TreeFilterEdit1.Glyph, 'FILTER_CLEAR');
      fImages.AddResourceName(HINSTANCE, 'BALLOON');
      fImages.AddResourceName(HINSTANCE, 'INFORMATION');
      fImages.AddResourceName(HINSTANCE, 'LIGHTBULB_OFF');
      fImages.AddResourceName(HINSTANCE, 'WARNING');
      fImages.AddResourceName(HINSTANCE, 'EXCLAMATION');
    end;
    iss24:
    begin
      fImages.Width:=24;
      fImages.Height:=24;
      AssignPng(TreeFilterEdit1.Glyph, 'FILTER_CLEAR24');
      fImages.AddResourceName(HINSTANCE, 'BALLOON24');
      fImages.AddResourceName(HINSTANCE, 'INFORMATION24');
      fImages.AddResourceName(HINSTANCE, 'LIGHTBULB_OFF24');
      fImages.AddResourceName(HINSTANCE, 'WARNING24');
      fImages.AddResourceName(HINSTANCE, 'EXCLAMATION24');
    end;
    iss32:
    begin
      fImages.Width:=32;
      fImages.Height:=32;
      AssignPng(TreeFilterEdit1.Glyph, 'FILTER_CLEAR32');
      fImages.AddResourceName(HINSTANCE, 'BALLOON32');
      fImages.AddResourceName(HINSTANCE, 'INFORMATION32');
      fImages.AddResourceName(HINSTANCE, 'LIGHTBULB_OFF32');
      fImages.AddResourceName(HINSTANCE, 'WARNING32');
      fImages.AddResourceName(HINSTANCE, 'EXCLAMATION32');
    end;
  end;
  List.Images := fImages;
  List.DefaultItemHeight:= ScaleY(22,96);

  fMsgColors[amkBub]  := $FCE7D2;
  fMsgColors[amkWarn] := $B3FFFF;
  fMsgColors[amkErr]  := $BDBDFF;
  fMsgColors[amkInf]  := $FFD0A8;
  fMsgColors[amkHint] := $C2FFC2;

  updaterByLoopInterval := 12;
  fOptions := TMessagesOptions.Create(Self);
  fOptions.assign(self);
  fOptions.Name:= 'messageOptions';
  fOptionsBackup := TMessagesOptions.Create(Self);

  List.PopupMenu := contextMenu;
  List.OnDeletion := @ListDeletion;
  List.OnDblClick := @handleMessageClick;

  btnSelProj.OnClick  := @selCtxtClick;
  btnSelMisc.OnClick  := @selCtxtClick;
  btnSelEdit.OnClick  := @selCtxtClick;
  btnSelApp.OnClick   := @selCtxtClick;
  btnSelAll.OnClick   := @selCtxtClick;
  fBtns[amcAll] := btnSelAll;
  fBtns[amcApp] := btnSelApp;
  fBtns[amcEdit]:= btnSelEdit;
  fBtns[amcMisc]:= btnSelMisc;
  fBtns[amcProj]:= btnSelProj;

  btnClearCat.OnClick := @actClearCurCatExecute;

  fEditorMessagePos := TEditorMessagePos.Create;

  fname := getDocPath + optname;
  if fname.fileExists then
  begin
    fOptions.loadFromFile(fname);
    fOptions.AssignTo(self);
  end;

  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
end;

destructor TMessagesWidget.destroy;
begin
  fEditorMessagePos.Free;
  fOptions.saveToFile(getDocPath + optname);
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TMessagesWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  TreeFilterEdit1.Flat:=value;
end;

procedure TMessagesWidget.listDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.data.isNotNil then
    Dispose(PMessageData(Node.Data));
end;

procedure TMessagesWidget.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  case Key of
    VK_BACK, VK_DELETE:
    begin
      if List.SelectionCount > 0 then
      begin
      for i := List.Items.Count-1 downto 0 do
        if List.Items[i].MultiSelected then
          List.Items.Delete(List.Items[i]);
      end
      else clearbyContext(amcAll);
    end;
    VK_UP, VK_DOWN:
      if fOptions.singleMessageClick then handleMessageClick(nil);
    VK_RETURN:
      handleMessageClick(nil);
  end;
end;

procedure TMessagesWidget.toolbarResize(Sender: TObject);
begin
  TreeFilterEdit1.Width := toolbar.Width - TreeFilterEdit1.Left - TreeFilterEdit1.BorderSpacing.Around;
end;

procedure TMessagesWidget.TreeFilterEdit1AfterFilter(Sender: TObject);
begin
  fFiltering := TreeFilterEdit1.Filter.isNotEmpty;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.TreeFilterEdit1ButtonClick(Sender: TObject);
begin
  fFiltering := false;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.selCtxtClick(Sender: TObject);
var
  btn: TToolButton;
  i: Integer;
begin
  if sender.isNil then
    exit;
  //
  fCtxt := amcAll;
  btn := TToolButton(Sender);
  for i := 0 to toolbar.ButtonCount-1 do
    toolbar.Buttons[i].Down := toolbar.Buttons[i] = btn;
  if btn = btnSelAll  then
    fCtxt := amcAll
  else if btn = btnSelEdit then
    fCtxt := amcEdit
  else if btn = btnSelProj then
    fCtxt := amcProj
  else if btn = btnSelApp then
    fCtxt := amcApp
  else if btn = btnSelMisc then
    fCtxt := amcMisc;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.setMaxMessageCount(value: Integer);
begin
  if value < 5 then
    value := 5;
  if fMaxMessCnt = value then
    exit;
  fMaxMessCnt := value;
  clearOutOfRangeMessg;
end;

procedure TMessagesWidget.setAutoSelectCategory(value: boolean);
begin
  fAutoSelect := value;
  fActAutoSel.Checked:= fAutoSelect;
end;

procedure TMessagesWidget.setSingleMessageClick(value: boolean);
begin
  fSingleClick := value;
  if fSingleClick then
  begin
    List.OnClick := @handleMessageClick;
    List.OnDblClick:= nil;
  end else begin
    List.OnClick := nil;
    List.OnDblClick:= @handleMessageClick;
  end;
end;

procedure TMessagesWidget.setColorError(value: TColor);
begin
  fMsgColors[amkErr] := max(value, minColor);
  List.Invalidate;
end;

procedure TMessagesWidget.setColorInfo(value: TColor);
begin
  fMsgColors[amkInf] := max(value, minColor);
  List.Invalidate;
end;

procedure TMessagesWidget.setColorHint(value: TColor);
begin
  fMsgColors[amkHint] := max(value, minColor);
  List.Invalidate;
end;

procedure TMessagesWidget.setColorBuble(value: TColor);
begin
  fMsgColors[amkBub] := max(value, minColor);
  List.Invalidate;
end;

procedure TMessagesWidget.setColorWarning(value: TColor);
begin
  fMsgColors[amkWarn] := value;
  List.Invalidate;
end;

procedure TMessagesWidget.ListCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  x: integer;
  rc: TRect;
begin
  rc := node.DisplayRect(false);
  x := rc.Left + 2 - TTreeHack(list).ScrolledLeft;
  // warning: the cast may become wrong if the enum is modified.
  Sender.Canvas.Brush.Color := fMsgColors[TAppMessageKind(node.ImageIndex + 1)];
  if node.Selected then
  begin
    Sender.Canvas.DrawFocusRect(rc);
    Sender.Canvas.Brush.Color := Sender.Canvas.Brush.Color - minColor;
  end;
  Sender.Canvas.FillRect(rc);
  list.Images.Draw(sender.Canvas, x, (rc.Top + rc.Bottom - list.Images.Height) div 2,
    node.ImageIndex, Node.NodeEffect);
  x += list.Images.Width + 5;
  Sender.Canvas.TextOut(x, rc.Top, node.Text);
  DefaultDraw := false;
end;
{$ENDREGION}

{$REGION IEditableOptions ----------------------------------------------------}
function TMessagesWidget.optionedWantCategory(): string;
begin
  exit('Messages');
end;

function TMessagesWidget.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TMessagesWidget.optionedWantContainer: TPersistent;
begin
  fOptions.assign(self);
  fOptionsBackup.assign(self);
  exit(fOptions);
end;

procedure TMessagesWidget.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept, oeeSelectCat:
      fOptionsBackup.assign(fOptions);
    oeeCancel:
      fOptions.assign(fOptionsBackup);
  end;
  fOptions.AssignTo(self);
  List.Invalidate;
end;

function TMessagesWidget.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION IContextualActions---------------------------------------------------}
function TMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TMessagesWidget.contextActionCount: integer;
begin
  result := 7;
end;

function TMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActAutoSel;
    1: result := fActClearAll;
    2: result := fActClearCurCat;
    3: result := fActCopyMsg;
    4: result := fActSelAll;
    5: result := fActSaveMsg;
    6: result := fActDemangle;
    else result := nil;
  end;
end;

procedure TMessagesWidget.actDemangleExecute(Sender: TObject);
var
  i: integer;
begin
  for i:= 0 to List.SelectionCount-1 do
    list.Selections[i].Text := demangle(list.Selections[i].Text);
end;

procedure TMessagesWidget.actAutoSelExecute(Sender: TObject);
begin
  fAutoSelect := fActAutoSel.Checked;
  fOptions.autoSelect:=fAutoSelect;
end;

procedure TMessagesWidget.actClearAllExecute(Sender: TObject);
begin
  clearbyContext(amcAll);
end;

procedure TMessagesWidget.actClearCurCatExecute(Sender: TObject);
begin
  case fCtxt of
    amcAll, amcApp, amcMisc :
      clearbyContext(fCtxt);
    amcEdit: if fDoc.isNotNil then
      clearbyData(fDoc);
    amcProj: if fProj <> nil then
      clearbyData(fProj);
  end;
end;

procedure TMessagesWidget.actCopyMsgExecute(Sender: TObject);
var
  i: Integer;
  str: string = '';
begin
  for i := 0 to List.Items.Count-1 do
    if List.Items[i].MultiSelected then
      str += List.Items[i].Text + LineEnding;
  Clipboard.AsText := str;
end;

procedure TMessagesWidget.actSelAllExecute(Sender: TObject);
var
  i: Integer;
begin
  List.BeginUpdate;
  for i := 0 to List.Items.Count-1 do
    if List.Items[i].Visible then
      List.Items[i].MultiSelected := true;
  List.EndUpdate;
end;

procedure TMessagesWidget.actSaveMsgExecute(Sender: TObject);
var
  lst: TStringList;
  itm: TtreeNode;
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      lst := TStringList.Create;
      try
        for itm in List.Items do
          lst.Add(itm.Text);
        lst.SaveToFile(filename.normalizePath);
      finally
        lst.Free;
      end;
    end;
  finally
    free;
  end;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TMessagesWidget.projNew(project: ICommonProject);
begin
  fProj := project;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.projClosing(project: ICommonProject);
begin
  if fProj <> project then
    exit;
  //
  clearbyData(fProj);
  fProj := nil;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.projFocused(project: ICommonProject);
begin
  if fProj = project then exit;
  fProj := project;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.projChanged(project: ICommonProject);
begin
end;

procedure TMessagesWidget.projCompiling(project: ICommonProject);
begin
  fProjCompile := true;
end;

procedure TMessagesWidget.projCompiled(project: ICommonProject; success: boolean);
begin
  fProjCompile := false;
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TMessagesWidget.docNew(document: TDexedMemo);
begin

  if fDoc.isNotNil and fOptions.fAutoSelect and (fCtxt = amcEdit) then
  begin
    if list.Selected.isNotNil then
      fEditorMessagePos[fDoc.fileName] := list.Selected.Index
    else
      fEditorMessagePos[fDoc.fileName] := -1;
  end;

  fDoc := document;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.docClosing(document: TDexedMemo);
begin
  if document <> fDoc then exit;
  clearbyData(fDoc);
  fEditorMessagePos.Remove(fDoc.fileName);
  fDoc := nil;
  filterMessages(fCtxt);
end;

procedure TMessagesWidget.docFocused(document: TDexedMemo);
var
  i: integer;
begin
  if fDoc = document then exit;

  if fDoc.isNotNil and fOptions.fAutoSelect and (fCtxt = amcEdit) then
  begin
    if list.Selected.isNotNil then
      fEditorMessagePos[fDoc.fileName] := list.Selected.Index
    else
      fEditorMessagePos[fDoc.fileName] := -1;
  end;

  fDoc := document;
  filterMessages(fCtxt);

  if fOptions.fAutoSelect and (fCtxt = amcEdit) then
  begin
    if fEditorMessagePos.TryGetValue(fDoc.fileName, i) then
    begin
      if (i <> -1) and (i < list.Items.Count) then
      begin
        list.Selected := list.Items[i];
        list.Selected.MakeVisible;
      end;
    end;
  end;
end;

procedure TMessagesWidget.docChanged(document: TDexedMemo);
begin
  fDoc := document;
end;
{$ENDREGION}

{$REGION IMessagesDisplay ----------------------------------------------------}
function TMessagesWidget.singleServiceName: string;
begin
  exit('IMessagesDisplay');
end;

procedure TMessagesWidget.message(const value: string; aData: Pointer;
  aCtxt: TAppMessageCtxt; aKind: TAppMessageKind);
var
  dt: PMessageData;
  item: TTreeNode;
  msg: string;
begin
  showWidget;
  if not fAlwaysFilter then
    TreeFilterEdit1.Filter:='';
  if (value.length > fOptions.maxLineLength) and (fOptions.maxLineLength > 0) then
    msg := value[1..fOptions.maxLineLength]
  else
    msg := value;
  if fAutoDemangle then
    msg := demangle(msg);
  if aKind = amkAuto then
    aKind := guessMessageKind(msg);
  if aCtxt = amcAutoCompile then
  begin
    case fProjCompile of
      false: aCtxt := amcAutoEdit;
      true: aCtxt := amcAutoProj;
    end;
  end;
  if aCtxt = amcAutoEdit then
  begin
    aData := fDoc;
    aCtxt := amcEdit;
  end
  else if aCtxt = amcAutoProj then
  begin
    aData := fProj;
    aCtxt := amcProj;
  end;
  dt := new(PMessageData);
  dt^.data := aData;
  dt^.ctxt := aCtxt;
  if fAutoSelect then if fCtxt <> aCtxt then
    fBtns[aCtxt].Click;
  if fastDisplay then
    IncLoopUpdate;
  item := List.Items.AddObject(nil, msg, dt);
  item.ImageIndex := iconIndex(aKind);
  item.SelectedIndex := item.ImageIndex;
  if not fastDisplay then
  begin
    clearOutOfRangeMessg;
    scrollToBack;
    TTreeHack(list).scrolledLeft := 0;
    List.Update;
    filterMessages(fCtxt);
  end
  else if fAlwaysFilter and fFiltering then
  begin
    filterMessages(fCtxt);
  end;
end;

procedure TMessagesWidget.clearByContext(aCtxt: TAppMessageCtxt);
var
  i: Integer;
  msgdt: PMessageData;
begin
  list.BeginUpdate;
  TreeFilterEdit1.Filter := '';
  if aCtxt = amcAll then
    List.Items.Clear
  else for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if msgdt^.ctxt = aCtxt then
      List.Items.Delete(List.Items[i]);
  end;
  list.EndUpdate;
end;

procedure TMessagesWidget.clearByData(data: Pointer);
var
  i: Integer;
  msgdt: PMessageData;
begin
  if data.isNil then
    exit;
  if (TObject(data) = fDoc) and (fDoc.isNotNil) then
    fEditorMessagePos[fDoc.fileName] := -1;
  list.BeginUpdate;
  TreeFilterEdit1.Filter := '';
  for i := List.Items.Count-1 downto 0 do
  begin
    msgdt := PMessageData(List.Items[i].Data);
    if (msgdt^.data = data) then
      List.Items.Delete(List.Items[i]);
  end;
  list.EndUpdate;
end;
{$ENDREGION}

{$REGION Messages --------------------------------------------------------------}
procedure TMessagesWidget.updateLoop;
begin
  if fastDisplay then
  begin
    clearOutOfRangeMessg;
    scrollToBack;
    List.Update;
    filterMessages(fCtxt);
  end;
end;

function TMessagesWidget.iconIndex(aKind: TAppMessageKind): Integer;
begin
  case aKind of
    amkBub:  exit(0);
    amkInf:  exit(1);
    amkHint: exit(2);
    amkWarn: exit(3);
    amkErr:  exit(4);
    else exit(0);
  end;
end;

procedure TMessagesWidget.clearOutOfRangeMessg;
begin
  list.BeginUpdate;
  while List.Items.Count > fMaxMessCnt do
    List.Items.Delete(List.Items.GetFirstNode);
  list.EndUpdate;
end;

procedure TMessagesWidget.scrollToBack;
begin
  if not Visible then
    exit;
  if List.BottomItem.isNotNil then
    List.BottomItem.MakeVisible;
end;

procedure TMessagesWidget.handleMessageClick(Sender: TObject);
var
  pos: TPoint;
  msg: string;
  dat: PMessageData;
  old: TDexedMemo;
begin
  if List.Selected.isNil then
    exit;
  if ssCtrl in GetKeyShiftState then
    exit;
  old := fDoc;
  msg := List.Selected.Text;
  dat := PMessageData(List.Selected.Data);
  if not openFileFromDmdMessage(msg) then
    exit;

  // fixes strange bug : https://github.com/Basile-z/dexed/issues/320
  if (fDoc <> old) and fOptions.singleMessageClick and
    assigned(dat) and (dat^.ctxt = amcEdit) then
      List.ClearSelection(false);

  // from here, since a doc has the focus, List.Selected is nil
  pos := getLineFromMessage(msg);
  if fDoc.isNil then
    exit;
  fDoc.setFocus;
  fDoc.CaretXY := pos;
  fDoc.SelectLine;
end;

function TMessagesWidget.itemShouldBeVisible(item: TTreeNode;
  aCtxt: TAppMessageCtxt): boolean;
var
  msgDt: PMessageData;
begin
  result := false;
  msgDt := PMessageData(item.Data);
  if (not assigned(msgDt)) then
    exit;
  if aCtxt = amcAll then
    result := true
  else case msgDt^.ctxt of
    amcEdit: result := (fDoc  = TDexedMemo(msgDt^.data)) and (aCtxt = amcEdit);
    amcProj: result := (fProj = ICommonProject(msgDt^.data)) and (aCtxt = amcProj);
    amcApp:  result := aCtxt = amcApp;
    amcMisc: result := aCtxt = amcMisc;
  end;
end;

procedure TMessagesWidget.filterMessages(aCtxt: TAppMessageCtxt);
var
  itm: TTreeNode;
  i: integer;
begin
  if updating then
    exit;
  List.BeginUpdate;
  for i := 0 to List.Items.Count-1 do
  begin
    itm := List.Items.Item[i];
    if fFiltering then
      itm.Visible := itemShouldBeVisible(itm, aCtxt) and
        AnsiContainsText(itm.Text, TreeFilterEdit1.Filter)
    else
      itm.Visible:= itemShouldBeVisible(itm, aCtxt);
    itm.Selected := false;
  end;
  list.EndUpdate;
end;

class function messageSemantic.hash(const w: string): Word;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(w) do
    Result += fCoefficients[Byte(w[i])];
  Result := Result and $3F;
end;

class function messageSemantic.getType(const w: string): TAppMessageKind;
var
  h: Word;
begin
  result := amkBub;
  h := hash(w);
  if fFilled[h] and (fWords[h] = w) then
    result := fMap[h];
end;

function TMessagesWidget.guessMessageKind(const aMessg: string): TAppMessageKind;
var
  idt: string;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
const
  alp = ['a'..'z', 'A'..'Z', '_'];
begin
  result := amkBub;
  rng.init(aMessg);
  while true do
  begin
    if rng.empty then
      break;
    idt := rng.popUntil(alp)^.takeWhile(alp).yield;
    if idt = '' then
      exit;
    result := messageSemantic.getType(idt);
    if result <> amkBub then
      exit;
  end;
end;

function TMessagesWidget.getLineFromMessage(const aMessage: string): TPoint;
var
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  lne: string;
  col: string = '';
begin
  Result := Point(-1,-1);
  if aMessage.isEmpty then
    exit;
  rng.init(aMessage);
  {$IFDEF WINDOWS}
  if (aMessage.length > 3) and (aMessage[2..3] = ':\') then
    rng.popFrontN(3);
  {$ENDIF}
  rng.popUntil(['(', ':'])^.popWhile(['(', ':']);
  lne := rng.takeUntil([',', ':', ')', ' ']).yield;
  if rng.front in [',', ':'] then
    col := rng.popWhile([',', ':'])^.takeUntil(')').yield;
  result.y := strToIntDef(lne, -1);
  result.x := strToIntDef(col, -1);
end;

function TMessagesWidget.openFileFromDmdMessage(const aMessage: string): boolean;
var
  i: integer = 0;
  ident: string = '';
  absName: string;
begin
  result := false;
  while (true) do
  begin
    inc(i);
    if i > aMessage.length then
      exit;
    // '(': line will be indicated after fname
    // -mixin: dmd, error in mixin(token string) '<fname>-mixinXX<index>('
    if isEditable(ident.extractFileExt) and
    ((aMessage[i] = '(') or (aMessage[i] = ':') or
    ((aMessage[i] = '-') and (i < aMessage.length-5) and (aMessage[i..i+5] = '-mixin'))) then
    begin
      // relative fname if project file is the base path to a rel. fname
      absName := ExpandFileName(ident);
      if absName.fileExists then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
      // absolute fname
      if FilenameIsAbsolute(ident) then
      begin
        getMultiDocHandler.openDocument(ident);
        exit(true);
      end;
      // if fname relative to project path
      if fProj <> nil then
      begin
        absName := expandFilenameEx(fProj.filename.extractFileDir + DirectorySeparator, ident);
        if absName.fileExists then
        begin
          getMultiDocHandler.openDocument(absName);
          exit(true);
        end;
      end;
      // finally try using pwd ...
      absName := expandFilenameEx(GetCurrentDir, ident);
      if absName.fileExists then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
      // ... and $HOME
      absName := expandFilenameEx(GetUserDir, ident);
      if absName.fileExists then
      begin
        getMultiDocHandler.openDocument(absName);
        exit(true);
      end;
    end
    // <assertion failure messg>@<filename>
    else if aMessage[i] = '@' then
      ident := ''
    else
      ident += aMessage[i];
  end;
end;
{$ENDREGION}

end.
