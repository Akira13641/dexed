unit u_dfmt;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, ExtCtrls,
  Menus, Buttons, process, SynEditKeyCmds, u_widget, u_interfaces, u_observer,
  u_synmemo, u_writableComponent, u_common, u_sharedres, PropEdits,
  ObjectInspector;

type

  DfmtEol = (cr, lf, crlf);
  DfmtIndentstyle = (tab, space);
  DfmtBraceStyle = (allman, otbs, stroustrup);
  DfmtConstraint = (condNewLineIndent, condNewLine, alwaysNewLine, alwaysNewLineIndent);


type
  // wraps dfmt options to build the command line with ease
  // and allows to save the options between session.
  TDfmtWrapper = class(TWritableLfmTextComponent)
  private
    fEol: DfmtEol;
    fTabStyle: DfmtIndentstyle;
    fIndentSize: integer;
    fTabWidth: integer;
    fHardLLen: integer;
    fSoftLLen: integer;
    fBraceStyle: DfmtBraceStyle;
    fSpaceCast: boolean;
    fSplitOp: boolean;
    fCompactLbl: boolean;
    fSpaceSelImp: boolean;
    fConstraints: DfmtConstraint;
    procedure setSoftLLen(value: integer);
    procedure setHardLLen(value: integer);
    procedure setTabWidth(value: integer);
    procedure setIndentSize(value: integer);
    procedure setEol(value: DfmtEol);
    procedure setBraceStyle(value: DfmtBraceStyle);
    procedure setIndentStyle(value: DfmtIndentstyle);
    procedure setConstraintsStyle(value: DfmtConstraint);
  published
    property endOfline: DfmtEol read fEol write setEol default lf;
    property indentationStyle: DfmtIndentstyle read fTabStyle write setIndentStyle default space;
    property indentSize: integer read fIndentSize write fIndentSize default 4;
    property tabWidth: integer read fTabWidth write fTabWidth default 4;
    property hardLineLen: integer read fHardLLen write fHardLLen default 120;
    property softLineLen: integer read fSoftLLen write fSoftLLen default 80;
    property braceStyle: DfmtBraceStyle read fBraceStyle write setBraceStyle default allman;
    property spaceAfterCast: boolean read fSpaceCast write fSpaceCast default true;
    property spaceAfterImport: boolean read fSpaceSelImp write fSpaceSelImp default true;
    property splitOpAtPrevLine: boolean read fSplitOp write fSplitOp default true;
    property compactLabeledStatements: boolean read fCompactLbl write fCompactLbl default true;
    property constraintsStyle: DfmtConstraint read fConstraints write setConstraintsStyle default condNewLineIndent;
  public
    constructor create(AOwner: TComponent); override;
    procedure getParameters(str: TStrings; majv, minv: Byte);
  end;

  { TDfmtWidget }

  TDfmtWidget = class(TDexedWidget, IDocumentObserver, ICodeFormatting)
    btnApply: TSpeedButton;
    btnCancel: TSpeedButton;
    pnlFooter: TPanel;
    dfmtOptionEditor: TTIPropertyGrid;
    procedure dfmtOptionEditorEditorFilter(Sender: TObject;
      aEditor: TPropertyEditor; var aShow: boolean);
  private
    fDoc: TDexedMemo;
    fBackup: TStringList;
    fDmtWrapper: TDfmtWrapper;
    //
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    //
    procedure doApply(sender: TObject);
    procedure doCancel(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function singleServiceName: string;
    procedure formatCurrent();
  end;

implementation
{$R *.lfm}

const
  optFname = 'dfmt.txt';

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TDfmtWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  toolbarVisible:=false;
  fDmtWrapper := TDfmtWrapper.Create(self);
  fBackup := TStringList.Create;

  fname := getDocPath + optFname;
  if fname.fileExists then
    fDmtWrapper.loadFromFile(fname);

  btnCancel.OnClick := @doCancel;
  btnApply.OnClick  := @doApply;
  case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnCancel, 'CANCEL');
      AssignPng(btnApply, 'ACCEPT');
    end;
    iss24:
    begin
      AssignPng(btnCancel, 'CANCEL24');
      AssignPng(btnApply, 'ACCEPT24');
    end;
    iss32:
    begin
      AssignPng(btnCancel, 'CANCEL32');
      AssignPng(btnApply, 'ACCEPT32');
    end;
  end;

  dfmtOptionEditor.TIObject := fDmtWrapper;
  dfmtOptionEditor.DefaultItemHeight:=scaleY(24,96);

  EntitiesConnector.addSingleService(self);
end;

destructor TDfmtWidget.destroy;
begin
  dfmtOptionEditor.TIObject := nil;
  fDmtWrapper.saveToFile(getDocPath + optFname);
  fBackup.Free;
  inherited;
end;

constructor TDfmtWrapper.create(AOwner: TComponent);
begin
  inherited;
  fEol          := lf;
  fTabStyle     := DfmtIndentstyle.space;
  fIndentSize   := 4;
  fTabWidth     := 4;
  fHardLLen     := 120;
  fSoftLLen     := 80;
  fBraceStyle   := DfmtBraceStyle.allman;
  fSpaceCast    := true;
  fSpaceSelImp  := true;
  fSplitOp      := true;
  fCompactLbl   := true;
  fConstraints  := DfmtConstraint.condNewLineIndent;
end;

procedure TDfmtWidget.dfmtOptionEditorEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  case aEditor.GetName of
    'Tag', 'Name': aShow := false;
    else aShow := true;
  end;
end;

procedure TDfmtWrapper.setSoftLLen(value: integer);
begin
  if value < 60 then
    value := 60
  else if value > 512 then
    value := 512;
  fSoftLLen := value;
end;

procedure TDfmtWrapper.setHardLLen(value: integer);
begin
  if value < 60 then
    value := 60
  else if value > 512 then
    value := 512;
  fHardLLen := value;
end;

procedure TDfmtWrapper.setTabWidth(value: integer);
begin
  if value < 1 then
    value := 1
  else if value > 8 then
    value := 8;
  fTabWidth := value;
end;

procedure TDfmtWrapper.setIndentSize(value: integer);
begin
  if value < 1 then
    value := 1
  else if value > 8 then
    value := 8;
  fIndentSize := value;
end;

procedure TDfmtWrapper.setEol(value: DfmtEol);
begin
  if not (value in [DfmtEol.cr, DfmtEol.lf, DfmtEol.crlf]) then
    value := DfmtEol.lf;
  fEol:=value;
end;

procedure TDfmtWrapper.setBraceStyle(value: DfmtBraceStyle);
begin
  if not (value in [DfmtBraceStyle.allman, DfmtBraceStyle.otbs,
    DfmtBraceStyle.stroustrup]) then
      value := DfmtBraceStyle.allman;
  fBraceStyle:=value;
end;

procedure TDfmtWrapper.setIndentStyle(value: DfmtIndentstyle);
begin
  if not (value in [DfmtIndentstyle.space, DfmtIndentstyle.tab]) then
    value := DfmtIndentstyle.space;
  fTabStyle:=value;
end;

procedure TDfmtWrapper.setConstraintsStyle(value: DfmtConstraint);
begin
  if not (value in [DfmtConstraint.alwaysNewLine, DfmtConstraint.alwaysNewLineIndent,
    DfmtConstraint.condNewLine, DfmtConstraint.condNewLineIndent]) then
      value := DfmtConstraint.condNewLineIndent;
  fConstraints:=value;
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TDfmtWidget.docNew(document: TDexedMemo);
begin
  fDoc := document;
end;

procedure TDfmtWidget.docFocused(document: TDexedMemo);
begin
  if document = fDoc
    then exit;
  fDoc := document;
end;

procedure TDfmtWidget.docChanged(document: TDexedMemo);
begin
end;

procedure TDfmtWidget.docClosing(document: TDexedMemo);
begin
  if fDoc <> document then
    exit;
  fDoc := nil;
end;
{$ENDREGION}

{$REGION Dfmt things -----------------------------------------------------------}
procedure TDfmtWrapper.getParameters(str: TStrings; majv, minv: Byte);
const
  eol: array[DfmtEol] of string = ('cr', 'lf', 'crlf');
  falsetrue: array[boolean] of string = ('false', 'true');
  idtstyle: array[DfmtIndentstyle] of string = ('tab', 'space');
  brc: array[DfmtBraceStyle] of string = ('allman', 'otbs', 'stroustrup');
  cts: array[DfmtConstraint] of string = ('conditional_newline_indent',
    'conditional_newline', 'always_newline', 'always_newline_indent');
begin
  str.Add('--end_of_line=' + eol[endOfline]);
  str.Add('--max_line_length=' + intToStr(hardLineLen));
  str.Add('--soft_max_line_length=' + intToStr(softLineLen));
  str.Add('--indent_size='  + intToStr(indentSize));
  str.Add('--indent_style=' + idtstyle[indentationStyle]);
  str.Add('--tab_width=' + intToStr(tabWidth));
  str.Add('--brace_style=' + brc[braceStyle]);
  str.Add('--split_operator_at_line_end=' + falsetrue[splitOpAtPrevLine]);
  str.Add('--space_after_cast=' + falsetrue[spaceAfterCast]);
  str.Add('--selective_import_space=' + falsetrue[spaceAfterImport]);
  str.Add('--compact_labeled_statements=' + falsetrue[compactLabeledStatements]);
  if (majv = 0) and (minv > 4) then
    str.Add('--template_constraint_style=' + cts[fConstraints]);
end;

function TDfmtWidget.singleServiceName: string;
begin
  exit('ICodeFormatting');
end;

procedure TDfmtWidget.formatCurrent();
begin
  doApply(self);
end;

procedure TDfmtWidget.doApply(sender: TObject);
var
  inp: string;
  i: integer;
  prc: TProcess;
  str: TStringList;
  majv: byte = 0;
  minv: byte = 4;
begin
  if fDoc.isNil then
    exit;
  if not exeInSysPath('dfmt') then
    exit;

  fBackup.Assign(fDoc.Lines);
  prc := TProcess.create(nil);
  try
    prc.Executable:= exeFullName('dfmt' + exeExt);
    prc.Options:= prc.Options + [poUsePipes, poStderrToOutPut];

    setLength(inp, 20);
    prc.Parameters.Add('--version');
    prc.Execute;
    i := prc.Output.Read(inp[1], 20);
    if (i > 4) and (inp[2] = '.') then
    begin
      majv := Byte(inp[1]) - Byte('0');
      minv := Byte(inp[3]) - Byte('0');
    end;
    while prc.Running do
      sleep(1);

    prc.Parameters.Clear;
    fDmtWrapper.getParameters(prc.Parameters, majv, minv);
    prc.Execute;
    inp := fDoc.Lines.Text;
    prc.Input.Write(inp[1], inp.length);
    prc.CloseInput;
    try
      str := TStringList.Create;
      processOutputToStrings(prc,str);
      fDoc.replaceUndoableContent(str.strictText);
    except
      fDoc.Lines.Assign(fBackup);
    end;
    while prc.Running do
      sleep(1);
  finally
    prc.free;
    str.free;
  end;
end;

procedure TDfmtWidget.doCancel(sender: TObject);
begin
  if fDoc.isNil then
    exit;
  fDoc.Lines.Assign(fBackup);
end;
{$ENDREGION}

end.

