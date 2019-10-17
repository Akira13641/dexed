unit u_halstead;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, fpjson, math,
  u_common, u_observer, u_interfaces, u_dastworx, u_writableComponent,
  u_synmemo;

type

  TBugCountMethod = (pow23div3000, div3000);

  THalsteadMetricsBase = class(TWritableLfmTextComponent)
  private
    fMaxBugsPerFunction: single;
    fMaxBugsPerModule: single;
    fMaxVolumePerFunction: integer;
    fShowAllResults: boolean;
    fBugCountMethod: TBugCountMethod;
    procedure setMaxBugsPerFunction(value: single);
    procedure setMaxBugsPerModule(value: single);
    procedure setMaxVolumePerFunction(value: integer);
  published
    property maxBugsPerFunction: single read fMaxBugsPerFunction write setMaxBugsPerFunction;
    property maxBugsPerModule: single read fMaxBugsPerModule write setMaxBugsPerModule;
    property maxVolumePerFunction: integer read fMaxVolumePerFunction write setMaxVolumePerFunction;
    property showAllResults: boolean read fShowAllResults write fShowAllResults default false;
    property bugCountMethod: TBugCountMethod read fBugCountMethod write fBugCountMethod default pow23div3000;
  public
    constructor create(aOwner: TComponent); override;
    procedure assign(value: TPersistent); override;
  end;

  THalsteadMetrics = class(THalsteadMetricsBase, IEditableOptions)
  private
    fBackup: THalsteadMetricsBase;
    fMsgs: IMessagesDisplay;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure measure(document: TDexedMemo);
  end;

  function metrics: THalsteadMetrics;

implementation

var
  fMetrics: THalsteadMetrics = nil;
const
  optFname = 'metrics.txt';

function metrics: THalsteadMetrics;
begin
  if fMetrics.isNil then
    fMetrics := THalsteadMetrics.create(nil);
  result := fMetrics;
end;

constructor THalsteadMetricsBase.create(aOwner: TComponent);
begin
  inherited;
  fMaxBugsPerFunction:= 0.5;
  fMaxBugsPerModule:= 2;
  fMaxVolumePerFunction:= 1000;
end;

procedure THalsteadMetricsBase.assign(value: TPersistent);
var
  s: THalsteadMetricsBase;
begin
  if value is THalsteadMetricsBase then
  begin
    s := THalsteadMetricsBase(value);
    fMaxBugsPerFunction:=s.fMaxBugsPerFunction;
    fMaxBugsPerModule:=s.fMaxBugsPerModule;
    fMaxVolumePerFunction:=s.fMaxVolumePerFunction;
    fShowAllResults:=s.fShowAllResults;
    fBugCountMethod:=s.fBugCountMethod;
  end
  else inherited;
end;

procedure THalsteadMetricsBase.setMaxBugsPerFunction(value: single);
begin
  if value < 0 then
    value := 0;
  fMaxBugsPerFunction:=value;
end;

procedure THalsteadMetricsBase.setMaxBugsPerModule(value: single);
begin
  if value < 0 then
    value := 0;
  fMaxBugsPerModule:=value;
end;

procedure THalsteadMetricsBase.setMaxVolumePerFunction(value: integer);
begin
  if value < 0 then
    value := 0;
  fMaxVolumePerFunction:=value;
end;

constructor THalsteadMetrics.create(aOwner: TComponent);
var
  f: string;
begin
  inherited;
  fBackup:= THalsteadMetricsBase.create(self);
  f := getDocPath + optFname;
  if f.fileExists then
    loadFromFile(f);
  fBackup.assign(self);
  EntitiesConnector.addObserver(self);
end;

destructor THalsteadMetrics.destroy;
begin
  EntitiesConnector.removeObserver(self);
  saveTofile(getDocPath + optFname);
  inherited;
end;

function THalsteadMetrics.optionedWantCategory(): string;
begin
  exit('Code metrics');
end;

function THalsteadMetrics.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function THalsteadMetrics.optionedWantContainer: TPersistent;
begin
  fBackup.assign(self);
  exit(self);
end;

procedure THalsteadMetrics.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept: fBackup.assign(self);
    oeeCancel: assign(fBackup);
  end;
end;

function THalsteadMetrics.optionedOptionsModified: boolean;
begin
  exit(false);
end;

procedure THalsteadMetrics.Measure(document: TDexedMemo);

  function checkFunction(const obj: TJSONObject; var bugsSum: single): boolean;
  var
    n1, sn1, n2, sn2: integer;
    val: TJSONData;
    voc, len, line: integer;
    vol, dif, eff: single;
    cpl, bgs: single;
    vwn: boolean;
    bwn: boolean;
  const
    bgt: array[boolean] of TAppMessageKind = (amkInf, amkWarn);
  begin
    result := true;
    val := obj.Find('n1Count');
    if val.isNil then
      exit;
    n1  := val.AsInteger;
    if n1 = 0 then
      exit;

    val := obj.Find('n1Sum');
    if val.isNil then
      exit;
    sn1 := val.AsInteger;

    val := obj.Find('n2Count');
    if val.isNil then
      exit;
    n2  := val.AsInteger;
    if n2 = 0 then
      exit;

    val := obj.Find('n2Sum');
    if val.isNil then
      exit;
    sn2 := val.AsInteger;

    val := obj.Find('line');
    if val.isNil then
      exit;
    line := val.AsInteger;
    val := obj.Find('name');
    if val.isNil then
      exit;

    voc := max(1, n1 + n2);
    len := max(1, sn1 + sn2);
    cpl := n1*log2(n1) + n2*log2(n2);
    vol := len * log2(voc);
    dif := n1 * 0.5 * (sn2 / n2);
    eff := dif * vol;
    case fBugCountMethod of
      pow23div3000: bgs := power(eff, 0.666667) / 3000;
      div3000: bgs := eff / 3000;
      else bgs := 0;
    end;

    bugsSum += bgs;

    vwn := (fMaxVolumePerFunction <> 0) and not IsNan(vol) and (vol >= fMaxVolumePerFunction);
    bwn := (fMaxBugsPerFunction <> 0) and not IsNan(bgs) and (bgs >= fMaxBugsPerFunction);
    result := not vwn and not bwn;

    if fShowAllResults or not result then
    begin
      fMsgs.message(format('%s(%d): metrics for "%s"',
        [document.fileName, line, val.AsString]), document, amcEdit, amkInf);

      fMsgs.message(format('    Vocabulary: %d', [voc]), document, amcEdit, amkInf);
      fMsgs.message(format('    Length: %d', [len]), document, amcEdit, amkInf);
      fMsgs.message(format('    Calculated program length: %f', [cpl]), document, amcEdit, amkInf);
      fMsgs.message(format('    Volume: %.2f', [vol]), document, amcEdit, bgt[vwn]);
      fMsgs.message(format('    Error proneness: %.2f', [dif]), document, amcEdit, amkInf);
      fMsgs.message(format('    Implementation effort: %.2f', [eff]), document, amcEdit, amkInf);
      fMsgs.message(format('    Implementation Time: %.2f secs.', [eff / 18]), document, amcEdit, amkInf);
      fMsgs.message(format('    Estimated bugs: %.2f', [bgs]), document, amcEdit, bgt[bwn]);
    end;
  end;

var
  jsn: TJSONObject = nil;
  fnc: TJSONObject = nil;
  val: TJSONData;
  arr: TJSONArray;
  bgS: single = 0;
  noW: boolean = true;
  fnW: boolean;
  i: integer;
begin
  if not fShowAllResults
  and ((maxBugsPerFunction = 0) and (maxBugsPerModule = 0) and (maxVolumePerFunction = 0)) then
    exit;

  if not assigned(fMsgs) then
    fMSgs := getMessageDisplay;

  getHalsteadMetrics(document.Lines, jsn);
  if jsn.isNil then
    exit;

  val := jsn.Find('functions');
  if val.isNil or (val.JSONType <> jtArray) then
    exit;
  arr := TJSONArray(val);
  for i := 0 to arr.Count-1 do
  begin
    fnc := TJSONObject(arr.Objects[i]);
    if fnc.isNotNil then
    begin
      fnW := checkFunction(fnc, bgS);
      noW := noW and fnW;
    end;
  end;

  if (fMaxBugsPerModule <> 0) and (bgS >= fMaxBugsPerModule) then
    fMsgs.message(format('The estimated number of bugs (%.2f) in this module exceeds the limit', [bgS]),
      document, amcEdit, amkWarn)
  else if noW then
    fMsgs.message('No abnormal values in the code metrics',
      document, amcEdit, amkInf);

  jsn.Free;
end;

initialization
  fMetrics := THalsteadMetrics.create(nil);
finalization
  fMetrics.Free;
end.

