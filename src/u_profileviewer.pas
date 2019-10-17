unit u_profileviewer;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TASources, TAGraph, TATransformations, TASeries,
  TATools, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, ComCtrls,
  StdCtrls, TALegend, math,
  u_widget, u_common, u_stringrange, u_dsgncontrols, u_ddemangle,
  u_interfaces, u_observer, u_writableComponent, u_controls;

type

  TProfileViewerOptionsBase = class(TWritableLfmTextComponent)
  private
    fHideAttributes: boolean;
    fHideStandardLibraryCalls: boolean;
    fHideRunTimeCalls: boolean;
    fOtherExclusions: TStringList;
    procedure setOtherExclusions(value: TStringList);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(value: TPersistent); override;
  published
    property hideAttributes: boolean read fHideAttributes write fHideAttributes;
    property hideStandardLibraryCalls: boolean read fHideStandardLibraryCalls write fHideStandardLibraryCalls;
    property hideRuntimeCalls: boolean read fHideRunTimeCalls write fHideRunTimeCalls;
    property otherExclusions: TStringList read fOtherExclusions write setOtherExclusions;
  end;

  TProfileViewerOptions = class(TProfileViewerOptionsBase, IEditableOptions)
  private
    fBackup: TProfileViewerOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  TProfileViewerWidget = class(TDexedWidget, IProjectObserver)
    btnOpen: TDexedToolButton;
    btnOpts: TDexedToolButton;
    btnProj: TDexedToolButton;
    btnRefresh: TDexedToolButton;
    button0: TDexedToolButton;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointHintTool1: TDataPointHintTool;
    ImageList1: TImageList;
    selPieSource: TComboBox;
    datNumCalls: TListChartSource;
    datTreeTime: TListChartSource;
    datFuncTime: TListChartSource;
    datPerCall: TListChartSource;
    Panel1: TPanel;
    pie: TChart;
    list: TListView;
    pieSeries: TPieSeries;
    Splitter1: TSplitter;
    procedure btnOpenClick(Sender: TObject);
    procedure btnProjClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnOptsClick(Sender: TObject);
    procedure selPieSourceSelect(Sender: TObject);
    procedure selPieSourceSelectionChange(Sender: TObject; User: boolean);
    procedure Splitter1CanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure Splitter1Moved(Sender: TObject);
  private
    fOptions: TProfileViewerOptions;
    logFname: string;
    fProj: ICommonProject;
    procedure updateIcons;
    procedure clearViewer;
    procedure updateFromFile(const fname: string);
    procedure updatePie;
    procedure listCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure reloadCurrent;
  end;

  const optFname = 'profileviewer.txt';

implementation
{$R *.lfm}

constructor TProfileViewerOptionsBase.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  fOtherExclusions := TStringList.Create;
end;

destructor TProfileViewerOptionsBase.destroy;
begin
  fOtherExclusions.free;
  inherited;
end;

procedure TProfileViewerOptionsBase.assign(value: TPersistent);
var
  s: TProfileViewerOptionsBase;
begin
  if value is TProfileViewerOptionsBase then
  begin
    s := TProfileViewerOptionsBase(value);
    fOtherExclusions.Assign(s.fOtherExclusions);
    fHideRunTimeCalls:=s.fHideRunTimeCalls;
    fHideStandardLibraryCalls:=fHideStandardLibraryCalls;
  end
  else inherited;
end;

procedure TProfileViewerOptionsBase.setOtherExclusions(value: TStringList);
begin
  fOtherExclusions.assign(value);
end;

constructor TProfileViewerOptions.create(aOwner: TComponent);
var
  s: string;
begin
  inherited create(aOwner);
  fBackup := TProfileViewerOptionsBase.create(nil);
  EntitiesConnector.addObserver(self);
  s := getDocPath + optFname;
  if s.fileExists then
    loadFromFile(s);
end;

destructor TProfileViewerOptions.destroy;
begin
  saveTofile(getDocPath + optFname);
  EntitiesConnector.removeObserver(self);
  fBackup.free;
  inherited;
end;

function TProfileViewerOptions.optionedWantCategory(): string;
begin
  result := 'Profile viewer';
end;

function TProfileViewerOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  result := oekGeneric;
end;

function TProfileViewerOptions.optionedWantContainer: TPersistent;
begin
  result := self;
end;

procedure TProfileViewerOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept:
      begin
        fBackup.assign(self);
        TProfileViewerWidget(owner).reloadCurrent;
      end;
    oeeCancel:
      begin
        self.assign(fBackup);
        TProfileViewerWidget(owner).reloadCurrent;
      end;
    oeeSelectCat:
      fBackup.assign(self);
    oeeChange:
      TProfileViewerWidget(owner).reloadCurrent;
  end;
end;

function TProfileViewerOptions.optionedOptionsModified: boolean;
begin
  result := false;
end;

constructor TProfileViewerWidget.create(aOwner: TComponent);
begin
  inherited;
  EntitiesConnector.addObserver(self);
  fOptions:= TProfileViewerOptions.create(self);
  clearViewer;
  updatePie;
  list.OnCompare:=@listCompare;
  selPieSourceSelect(nil);
  list.PopupMenu := TListViewCopyMenu.create(list);
end;

destructor TProfileViewerWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TProfileViewerWidget.projNew(project: ICommonProject);
begin
end;

procedure TProfileViewerWidget.projChanged(project: ICommonProject);
begin
end;

procedure TProfileViewerWidget.projClosing(project: ICommonProject);
begin
  if project = fProj then
    fProj := nil;
end;

procedure TProfileViewerWidget.projFocused(project: ICommonProject);
begin
  fProj := project;
end;

procedure TProfileViewerWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TProfileViewerWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;

procedure TProfileViewerWidget.reloadCurrent;
var
  fname: string;
begin
  if logFname.isNotEmpty and logFname.fileExists then
    updateFromFile(logFname)
  else
  begin
    fname := GetCurrentDir + DirectorySeparator + 'trace.log';
    if fileExists(fname) then
    begin
      updateFromFile(fname);
      logFname:=fname;
    end;
  end;
end;

procedure TProfileViewerWidget.btnRefreshClick(Sender: TObject);
begin
  reloadCurrent;
end;

procedure TProfileViewerWidget.btnOptsClick(Sender: TObject);
begin
  getOptionsEditor.showOptionEditor(fOptions as IEditableOptions);
end;

procedure TProfileViewerWidget.selPieSourceSelect(Sender: TObject);
begin
  case selPieSource.ItemIndex of
    1: pieSeries.Source := datTreeTime;
    2: pieSeries.Source := datFuncTime;
    3: pieSeries.Source := datPerCall;
    else pieSeries.Source := datNumCalls;
  end;
  updateIcons;
end;

procedure TProfileViewerWidget.btnOpenClick(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if logFname.fileExists and logFname.isNotEmpty then
      FileName := logFname;
    if execute then
    begin
      updateFromFile(filename);
      logFname := FileName;
    end;
  finally
    free;
  end;
end;

procedure TProfileViewerWidget.btnProjClick(Sender: TObject);
var
  fname: string;
begin
  if assigned(fProj) then
  begin
    fname := fProj.outputFilename.extractFileDir + DirectorySeparator + 'trace.log';
    if fileExists(fname) then
    begin
      updateFromFile(fname);
      logFname:=fname;
    end;
  end;
end;

procedure TProfileViewerWidget.updatePie;
begin
  pieSeries.FixedRadius:= max(1, pie.Height div 2 - 10);
end;

procedure TProfileViewerWidget.selPieSourceSelectionChange(Sender: TObject;User: boolean);
begin
  updatePie;
end;

procedure TProfileViewerWidget.Splitter1CanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  if accept then
    updatePie;
end;

procedure TProfileViewerWidget.Splitter1Moved(Sender: TObject);
begin
  updatePie;
end;

procedure TProfileViewerWidget.clearViewer;
begin
  list.Clear;
  pieSeries.Clear;
  datFuncTime.Clear;
  datNumCalls.Clear;
  datTreeTime.Clear;
  datPerCall.Clear;
end;

procedure TProfileViewerWidget.updateIcons;
var
  b: TBitmap;
  i: integer;
begin
  list.SmallImages := nil;
  ImageList1.BeginUpdate;
  ImageList1.Clear;
  b := TBitmap.Create;
  try
    for i:= 0 to pieSeries.Count-1 do
    begin
      b.SetSize(12,12);
      b.Canvas.Brush.Color := pieSeries.GetColor(i);
      b.Canvas.Pen.Color:= clblack;
      b.Canvas.Brush.Style := bsSolid;
      b.Canvas.Pen.Style := psSolid;
      b.Canvas.Pen.Width := 1;
      b.Transparent := false;
      b.Canvas.Rectangle(Rect(0,0,12,12));
      ImageList1.Add(b, nil);
      list.Items.Item[i].ImageIndex:=i;
      b.Clear;
    end;
  finally
    b.Free;
    ImageList1.EndUpdate;
    list.SmallImages := ImageList1;
  end;
end;

procedure TProfileViewerWidget.updateFromFile(const fname: string);
var
  log: string;
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  tps: qword;
  idt: string;
  fnc: qword;
  fft: qword;
  ftt: qword;
  fpc: qword;
  exc: string;

  procedure fillRow();
  var
    itm: TListItem;
    c: TColor;
  begin
    list.AddItem('', nil);
    itm := list.Items[list.Items.Count-1];
    itm.SubItems.Add(fnc.ToString);
    itm.SubItems.Add(fft.ToString);
    itm.SubItems.Add(ftt.ToString);
    itm.SubItems.Add(fpc.ToString);
    itm.SubItems.Add(idt);
    c := Random($70F0F0F0) + $F0F0F0F;
    datNumCalls.Add(0, fnc, idt, c);
    datFuncTime.Add(0, fft, idt, c);
    datTreeTime.Add(0, ftt, idt, c);
    datPerCall.Add(0, fpc, idt, c);
  end;

  function canShow: boolean;
  begin
    result := true;
    if fOptions.hideRuntimeCalls and (Pos(' core.', idt) > 0) then
      exit(false);
    if fOptions.hideStandardLibraryCalls and (Pos(' std.', idt) > 0) then
      exit(false);
    if fOptions.otherExclusions.Count > 0 then
      for exc in fOptions.otherExclusions do
        if Pos(exc, idt) > 0 then
          exit(false);
  end;

  procedure filterAttributes;
  const
    a: array[0..12] of string = ('const','pure','nothrow','@safe','@nogc',
      '@trusted', '@system', 'immutable', 'inout', 'return', '@property',
      'shared', 'scope');
  var
    i: integer = 0;
    j: integer;
    p: integer;
    s: string;
  begin
    if not fOptions.hideAttributes then
      exit;
    p := pos('(', idt);
    if p = 0 then
      p := integer.MaxValue;
    for s in a do
    begin
      j := pos(s, idt);
      if (j > 0) then
        j += s.length + 1;
      if (j < p) and (j >= i) then
        i := j;
    end;
    if i > 0 then
      idt := idt[i..idt.length];
  end;

begin
  clearViewer;

  if not fname.fileExists or (fname.extractFileName <> 'trace.log') then
    exit;

  with TStringList.Create do
  try
    loadFromFile(fname);
    log := strictText;
  finally
    free;
  end;

  if log.length = 0 then
    exit;

  // ======== Timer Is 35.... ============
  rng.init(log);
  rng.popUntil('=')^.popUntil(['0','1','2','3','4','5','6','7','8','9']);
  if rng.empty then
    exit;
  idt := rng.nextWord;
  tps := StrToQWordDef(idt, 0);

  // columns headers
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;
  if rng.popLine^.empty then
    exit;

  list.BeginUpdate;

  // each function
  while true do
  begin

    idt:= 'unknown function';
    fnc:= 0;
    fft:= 0;
    ftt:= 0;
    fpc:= 0;
    // num calls
    fnc := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // function time
    fft := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // tree time
    ftt := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // per call
    fpc := StrToQWordDef(rng.nextWord, 0);
    if not rng.empty then
      rng.popFront
    else
      break;
    // function name
    rng.popWhile(' ')^.empty;
    idt := demangle(rng.takeUntil(#10).yield);

    // apply options and add item
    if canShow then
    begin
      filterAttributes;
      fillRow;
    end;

    if not rng.empty then
      rng.popFront
    else
      break;
  end;

  list.EndUpdate;
  selPieSourceSelect(nil);

end;

procedure TProfileViewerWidget.listCompare(Sender: TObject; item1, item2: TListItem; Data: Integer; var Compare: Integer);
var
  i1, i2: qword;
  col: Integer;
begin
  col := list.SortColumn;
  if col = 5 then
  begin
    Compare := AnsiCompareStr(item1.SubItems[3], item2.SubItems[3]);
  end
  else if col <> 0 then
  begin
    i1 := item1.SubItems[col-1].ToInt64;
    i2 := item2.SubItems[col-1].ToInt64;
    if (i1 = i2) then
      Compare := 0
    else if (i1 < i2) then
      Compare := -1
    else
      Compare := 1;
  end;
  if list.SortDirection = sdDescending then
    Compare := -Compare;
end;

end.

