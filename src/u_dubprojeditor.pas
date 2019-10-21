unit u_dubprojeditor;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, StdCtrls, Buttons, ComCtrls, jsonparser, fpjson,
  u_widget, u_common, u_interfaces, u_observer, u_dubproject, u_sharedres,
  u_dsgncontrols, u_dialogs;

type

  TProposalType = (ptArray, ptObject, ptValue);

  TEditorProposal = record
    name: string;
    jtype: TProposalType;
  end;


  TDubPropAddEvent = procedure(const propName: string; tpe: TJSONtype) of object;

  TDubProjectPropAddPanel = class(TForm)
  private
    fSelType: TRadioGroup;
    fEdName: TComboBox;
    fEvent: TDubPropAddEvent;
    fBtnValidate: TBitBtn;
    fJson: TJSONData;
    procedure doValidate(sender: TObject);
    procedure selTypeChanged(sender: TObject);
    procedure setSelFromProposal(sender: TObject);
  public
    constructor construct(event: TDubPropAddEvent; json: TJSONData);
  end;

  { TDubProjectEditorWidget }
  TDubProjectEditorWidget = class(TDexedWidget, IProjectObserver)
    btnAcceptProp: TSpeedButton;
    btnAddProp: TDexedToolButton;
    btnCloneObject: TDexedToolButton;
    btnDelProp: TDexedToolButton;
    btnReload: TDexedToolButton;
    edProp: TEdit;
    fltEdit: TTreeFilterEdit;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    propTree: TTreeView;
    procedure btnAcceptPropClick(Sender: TObject);
    procedure btnAddPropClick(Sender: TObject);
    procedure btnDelPropClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnCloneObjectClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure propTreeSelectionChanged(Sender: TObject);
    procedure toolbarResize(Sender: TObject);
  private
    fSelectedNode: TTreeNode;
    fProj: TDubProject;
    fImages: TImageList;
    procedure updateEditor;
    procedure updateValueEditor;
    procedure setJsonValueFromEditor;
    procedure addProp(const propName: string; tpe: TJSONtype);
    //
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    //
  protected
    procedure SetVisible(value: boolean); override;
    procedure setToolBarFlat(value: boolean); override;
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

const
  proposals: array[0..43] of TEditorProposal = (
    (name: 'authors';             jtype: ptArray),
    (name: 'buildOptions';        jtype: ptArray),
    (name: 'buildRequirements';   jtype: ptArray),
    (name: 'buildTypes';          jtype: ptObject),
    (name: 'configurations';      jtype: ptArray),
    (name: 'copyFiles';           jtype: ptArray),
    (name: 'copyright';           jtype: ptValue),
    (name: 'cov';                 jtype: ptArray),
    (name: 'ddoc';                jtype: ptArray),
    (name: 'ddoxFilterArgs';      jtype: ptArray),
    (name: 'debug';               jtype: ptArray),
    (name: 'debugVersions';       jtype: ptArray),
    (name: 'dependencies';        jtype: ptObject),
    (name: 'description';         jtype: ptValue),
    (name: 'dflags';              jtype: ptArray),
    (name: 'docs';                jtype: ptArray),
    (name: 'excludedSourceFiles'; jtype: ptArray),
    (name: 'homepage';            jtype: ptValue),
    (name: 'lflags';              jtype: ptArray),
    (name: 'libs';                jtype: ptArray),
    (name: 'license';             jtype: ptValue),
    (name: 'mainSourceFile';      jtype: ptValue),
    (name: 'name';                jtype: ptValue),
    (name: 'plain';               jtype: ptArray),
    (name: 'platforms';           jtype: ptArray),
    (name: 'postBuildCommands';   jtype: ptArray),
    (name: 'postGenerateCommands';jtype: ptArray),
    (name: 'preBuildCommands';    jtype: ptArray),
    (name: 'preGenerateCommands'; jtype: ptArray),
    (name: 'profile';             jtype: ptArray),
    (name: 'release';             jtype: ptArray),
    (name: 'sourceFiles';         jtype: ptArray),
    (name: 'sourcePaths';         jtype: ptArray),
    (name: 'stringImportPaths';   jtype: ptArray),
    (name: 'subConfigurations';   jtype: ptObject),
    (name: 'subPackages';         jtype: ptArray),
    (name: 'systemDependencies';  jtype: ptValue),
    (name: 'targetName';          jtype: ptValue),
    (name: 'targetPath';          jtype: ptValue),
    (name: 'targetType';          jtype: ptValue),
    (name: 'unittest';            jtype: ptArray),
    (name: 'unittest-cov';        jtype: ptArray),
    (name: 'versions';            jtype: ptArray),
    (name: 'workingDirectory';    jtype: ptValue)
  );

{$REGION TDubProjectPropAddPanel ---------------------------------------------}
constructor TDubProjectPropAddPanel.construct(event: TDubPropAddEvent; json: TJSONData);
var
  layout: TPanel;
  i: integer;
begin
  inherited create(nil);
  fJson := json;
  width := ScaleX(280,96);
  height := ScaleY(130,96);
  fEvent := event;
  caption := 'add a DUB property';
  Position := poMainFormCenter;
  ShowHint:=true;

  fSelType := TRadioGroup.Create(self);
  fSelType.Parent := self;
  fSelType.Items.AddStrings(['array', 'object', 'value']);
  fSelType.Align:= alClient;
  fSelType.BorderSpacing.Around:=2;
  fSelType.Caption:= 'type';
  fSelType.ItemIndex:=2;
  fSelType.Hint:= 'type of the property to add';
  fSelType.OnSelectionChanged:= @selTypeChanged;
  fSelType.AutoSize:= true;

  layout := TPanel.Create(self);
  layout.Parent := self;
  layout.Align := alBottom;
  layout.Height := ScaleY(32,96);
  layout.BevelOuter:= bvNone;

  fEdName := TComboBox.Create(self);
  fEdName.Parent := layout;
  fEdName.Align:=alClient;
  fEdName.BorderSpacing.Around:=4;
  fEdName.Width:=80;
  fEdName.Hint:='name of the property to add';
  for i:= low(proposals) to high(proposals) do
    fEdName.Items.Add(proposals[i].name);
  fEdName.AutoComplete := true;
  fEdName.OnChange := @setSelFromProposal;
  fEdName.OnSelect:= @setSelFromProposal;
  fEdName.AutoSize:= true;

  fBtnValidate := TBitBtn.Create(self);
  fBtnValidate.Parent := layout;
  fBtnValidate.Align:=alRight;
  fBtnValidate.BorderSpacing.Around:=4;
  fBtnValidate.Width:= ScaleX(26,96);
  fBtnValidate.OnClick:=@doValidate;
  fBtnValidate.Hint:='accept and add a property';
  fBtnValidate.AutoSize:=true;

  case GetIconScaledSize of
    iss16: AssignPng(fBtnValidate, 'ACCEPT');
    iss24: AssignPng(fBtnValidate, 'ACCEPT24');
    iss32:AssignPng(fBtnValidate, 'ACCEPT32');
  end;

  selTypeChanged(nil);
end;

procedure TDubProjectPropAddPanel.selTypeChanged(sender: TObject);
begin
  if fJson.isNotNil then
    fEdName.Enabled := fJson.JSONType <> TJSONtype.jtArray;
end;

procedure TDubProjectPropAddPanel.setSelFromProposal(sender: TObject);
var
  i: integer;
begin
  fSelType.Enabled:=true;
  for i:= low(proposals) to high(proposals) do
  begin
    if fEdName.Text = proposals[i].name then
    begin
      case proposals[i].jtype of
        ptArray:fSelType.ItemIndex:=0;
        ptObject:fSelType.ItemIndex:=1;
        ptValue:fSelType.ItemIndex:=2;
      end;
      fSelType.Enabled := false;
      break;
    end;
  end;
end;

procedure TDubProjectPropAddPanel.doValidate(sender: TObject);
var
  tpe: TJSONtype;
begin
  if assigned(fEvent) then
  begin
    case fSelType.ItemIndex of
      0: tpe := TJSONtype.jtArray;
      1: tpe := TJSONtype.jtObject;
      else tpe := TJSONtype.jtString;
    end;
    if fEdName.Enabled and (fEdName.Text = '') then
      dlgOkError('New properties require a name')
    else
    begin
      fEvent(fEdName.Text, tpe);
      Close;
    end;
  end;
end;
{$ENDREGION}

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TDubProjectEditorWidget.create(aOwner: TComponent);
begin
  inherited;
  setToolBarVisible(true);

  fImages := TImageList.Create(self);
  case GetIconScaledSize of
    iss16:
    begin
      fImages.height := 16;
      fImages.width := 16;
      fImages.AddResourceName(HINSTANCE, 'JSON_OBJECT');
      fImages.AddResourceName(HINSTANCE, 'JSON_ARRAY');
      fImages.AddResourceName(HINSTANCE, 'JSON_VALUE');
      AssignPng(btnAcceptProp, 'ACCEPT');
      AssignPng(fltEdit.Glyph, 'FILTER_CLEAR');
    end;
    iss24:
    begin
      fImages.height := 24;
      fImages.width := 24;
      fImages.AddResourceName(HINSTANCE, 'JSON_OBJECT24');
      fImages.AddResourceName(HINSTANCE, 'JSON_ARRAY24');
      fImages.AddResourceName(HINSTANCE, 'JSON_VALUE24');
      AssignPng(btnAcceptProp, 'ACCEPT24');
      AssignPng(fltEdit.Glyph, 'FILTER_CLEAR24');
    end;
    iss32:
    begin
      fImages.height := 32;
      fImages.width := 32;
      fImages.AddResourceName(HINSTANCE, 'JSON_OBJECT32');
      fImages.AddResourceName(HINSTANCE, 'JSON_ARRAY32');
      fImages.AddResourceName(HINSTANCE, 'JSON_VALUE32');
      AssignPng(btnAcceptProp, 'ACCEPT32');
      AssignPng(fltEdit.Glyph, 'FILTER_CLEAR32');
    end;
  end;

  propTree.Images := fImages;
end;

procedure TDubProjectEditorWidget.SetVisible(value: boolean);
begin
  inherited;
  if not value then
    exit;
  updateEditor;
end;

procedure TDubProjectEditorWidget.setToolBarFlat(value: boolean);
begin
  inherited;
  btnAcceptProp.Flat:=value;
  fltEdit.Flat:=value;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TDubProjectEditorWidget.projNew(project: ICommonProject);
begin
  projFocused(project);
end;

procedure TDubProjectEditorWidget.projChanged(project: ICommonProject);
begin
  if fProj.isNil then
    exit;
  if project.getProject <> fProj then
    exit;
  if not Visible then
    exit;

  updateEditor;
end;

procedure TDubProjectEditorWidget.projClosing(project: ICommonProject);
begin
  if fProj.isNil then
    exit;
  if project.getProject <> fProj then
    exit;
  fProj := nil;

  updateEditor;
  enabled := false;
end;

procedure TDubProjectEditorWidget.projFocused(project: ICommonProject);
begin
  fProj := nil;
  enabled := false;
  if project.getFormat <> pfDUB then
  begin
    updateEditor;
    exit;
  end;
  fProj := TDubProject(project.getProject);
  enabled := true;
  if fProj.isSDL then
  begin
    edProp.Enabled:= false;
    btnAcceptProp.Enabled:=false;
  end else
  begin
    edProp.Enabled:= true;
    btnAcceptProp.Enabled:=true;
  end;
  if not Visible then
    exit;

  updateEditor;
end;

procedure TDubProjectEditorWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TDubProjectEditorWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION Editor ----------------------------------------------------------------}
procedure TDubProjectEditorWidget.propTreeSelectionChanged(Sender: TObject);
var
  tpe: TJSONtype;
begin
  fSelectedNode := nil;
  btnDelProp.Enabled := false;
  btnAddProp.Enabled := false;
  btnCloneObject.Enabled := false;
  if propTree.Selected.isNil then
    exit;

  fSelectedNode := propTree.Selected;
  tpe := TJSONData(fSelectedNode.Data).JSONType;
  btnDelProp.Enabled := (fSelectedNode.Level > 0) and (fSelectedNode.Text <> 'name')
    and fSelectedNode.data.isNotNil;
  btnAddProp.Enabled := tpe in [jtObject, jtArray];
  btnCloneObject.Enabled := (tpe = jtObject) and (fSelectedNode.Level > 0);
  updateValueEditor;
end;

procedure TDubProjectEditorWidget.toolbarResize(Sender: TObject);
begin
  fltEdit.Width := toolbar.Width - fltEdit.Left - fltEdit.BorderSpacing.Around;
end;

procedure TDubProjectEditorWidget.btnAcceptPropClick(Sender: TObject);
begin
  if fSelectedNode.isNil then
    exit;
  setJsonValueFromEditor;
  propTree.FullExpand;
end;

procedure TDubProjectEditorWidget.btnAddPropClick(Sender: TObject);
var
  pnl: TDubProjectPropAddPanel;
begin
  if fSelectedNode.isNil then
    exit;
  pnl := TDubProjectPropAddPanel.construct(@addProp, TJSONData(fSelectedNode.Data));
  pnl.ShowModal;
  pnl.Free;
end;

procedure TDubProjectEditorWidget.addProp(const propName: string;
  tpe: TJSONtype);
var
  arr: TJSONArray;
  obj: TJSONObject;
  nod: TTreeNode;
begin
  if fSelectedNode.isNil then
    exit;
  fProj.beginModification;
  if TJSONData(fSelectedNode.Data).JSONType = jtArray then
  begin
    arr := TJSONArray(fSelectedNode.Data);
    case tpe of
      jtArray: arr.Add(TJSONArray.Create());
      jtObject: arr.Add(TJSONObject.Create());
      jtString:arr.Add('<value>');
      else ;
    end;
  end
  else if TJSONData(fSelectedNode.Data).JSONType = jtObject then
  begin
    obj := TJSONObject(fSelectedNode.Data);
    case tpe of
      jtArray: obj.Add(propName, TJSONArray.Create());
      jtObject: obj.Add(propName, TJSONObject.Create());
      jtString: obj.Add(propName, '<value>');
      else ;
    end;
  end;
  fProj.endModification;
  propTree.FullExpand;
  nod := propTree.Items.FindNodeWithText('<value>');
  if nod.isNil then
    nod := propTree.Items.FindNodeWithText(propName);
  if nod.isNotNil then
  begin
    propTree.Selected := nod;
    propTree.MakeSelectionVisible;
  end;
end;

procedure TDubProjectEditorWidget.btnDelPropClick(Sender: TObject);
var
  prt: TJSONData;
  sel: TTreeNode;
begin
  if fSelectedNode.isNil then exit;
  if fSelectedNode.Level = 0 then exit;
  if fSelectedNode.Text = 'name' then exit;
  if fSelectedNode.Data.isNil then exit;
  if fSelectedNode.Parent.Data.isNil then exit;

  fProj.beginModification;
  prt := TJSONData(fSelectedNode.Parent.Data);
  if prt.JSONType = jtObject then
    TJSONObject(prt).Delete(fSelectedNode.Index)
  else if prt.JSONType = jtArray then
    TJSONArray(prt).Delete(fSelectedNode.Index);
  sel := fSelectedNode.GetPrevSibling;
  if sel.isNil then
    sel := fSelectedNode.GetNextSibling;
  if sel.isNil then
      sel := fSelectedNode.Parent;
  if sel.isNotNil then
    sel.Selected:=true;
  fProj.endModification;

  updateValueEditor;
end;

procedure TDubProjectEditorWidget.btnRefreshClick(Sender: TObject);
var
  f: string;
begin
  if assigned(fProj) then
  begin
    f := fProj.filename;
    if not f.fileExists then
      exit;
    if fProj.modified and
      (dlgYesNo('The project seems to be modified, save before reloading') = mrYes) then
        fProj.saveToFile(f);
    fProj.loadFromFile(f);
  end;
end;

procedure TDubProjectEditorWidget.btnCloneObjectClick(Sender: TObject);
var
  dat: TJSONData;
  prt: TJSONData;
  arr: TJSONArray;
  obj: TJSONObject;
  nme: string = '';
  inm: string;
  idx: integer = 0;
begin
  if fSelectedNode.isNil or fSelectedNode.Data.isNil or fProj.isNil or
    fSelectedNode.Parent.Data.isNil then
      exit;

  dat := TJSONData(fSelectedNode.Data);
  prt := TJSONData(fSelectedNode.Parent.Data);

  if ((prt.JSONType <> jtArray) and (prt.JSONType <> jtObject)) or
    (dat.JSONType <> jtObject) then
      exit;

  dat := dat.Clone;
  if prt.JSONType = jtArray then
  begin
    fProj.beginModification;
    arr := TJSONArray(prt);
    arr.Insert(arr.Count, dat);
    fProj.endModification;
  end
  else
  begin
    if not InputQuery('Clone object', 'name of the clone', nme) then
      exit;
    fProj.beginModification;
    obj := TJSONObject(prt);
    inm := nme;
    while obj.IndexOfName(inm) <> -1 do
    begin
      inm := format('%s_%d', [nme, idx]);
      idx += 1;
    end;
    obj.Add(inm, dat);
    fProj.endModification;
  end;
end;

procedure TDubProjectEditorWidget.MenuItem1Click(Sender: TObject);
begin
  if fProj.isNil or not fProj.filename.fileExists then
    exit;
  fProj.loadFromFile(fProj.filename);
end;

procedure TDubProjectEditorWidget.setJsonValueFromEditor;
var
  dat: TJSONData;
  vFloat: TJSONFloat;
  vInt: integer;
  vInt64: int64;
  vBool: boolean;
begin
  if fSelectedNode.isNil or fSelectedNode.Data.isNil or fProj.isNil then
    exit;

  fProj.beginModification;
  dat := TJSONData(fSelectedNode.Data);
  case dat.JSONType of
    jtNumber:
      case TJSONNumber(dat).NumberType of
        ntFloat:
          if TryStrToFloat(edProp.Text, vFloat) then
            dat.AsFloat := vFloat;
        ntInt64:
          if TryStrToInt64(edProp.Text, vInt64) then
            dat.AsInt64 := vInt64;
        ntInteger:
          if TryStrToInt(edProp.Text, vInt) then
            dat.AsInteger := vInt;
        else ;
      end;
     jtBoolean:
      if TryStrToBool(edProp.Text, vBool) then
        dat.AsBoolean := vBool;
      jtString:
        dat.AsString := edProp.Text;
      else ;
  end;
  fProj.endModification;
end;

procedure TDubProjectEditorWidget.updateValueEditor;
var
  dat: TJSONData;
begin
  edProp.Clear;
  if fSelectedNode.isNil then exit;
  if fSelectedNode.Data.isNil then exit;

  dat := TJSONData(fSelectedNode.Data);
  case dat.JSONType of
    jtNumber:
      case TJSONNumber(dat).NumberType of
        ntFloat:
          edProp.Text := FloatToStr(dat.AsFloat);
        ntInt64:
          edProp.Text := IntToStr(dat.AsInt64);
        ntInteger:
          edProp.Text := IntToStr(dat.AsInteger);
        else ;
      end;
    jtBoolean:
      edProp.Text := BoolToStr(dat.AsBoolean);
    jtString:
      edProp.Text := dat.AsString;
    else ;
  end;
end;

procedure TDubProjectEditorWidget.updateEditor;

  procedure addPropsFrom(node: TTreeNode; data: TJSONData);
  var
    i: integer;
    c: TTreeNode;
  begin
    node.Data:= data;
    if data.JSONType = jtObject then for i := 0 to data.Count-1 do
    begin
      node.ImageIndex:=0;
      node.SelectedIndex:=0;
      node.StateIndex:=0;
      c := node.TreeNodes.AddChildObject(node, TJSONObject(data).Names[i],
        TJSONObject(data).Items[i]);
      case TJSONObject(data).Items[i].JSONType of
        jtObject, jtArray:
          addPropsFrom(c, TJSONObject(data).Items[i]);
        else begin
          c.ImageIndex:=2;
          c.SelectedIndex:=2;
          c.StateIndex:=2;
        end;
      end;
    end else if data.JSONType = jtArray then for i := 0 to data.Count-1 do
    begin
      node.ImageIndex:=1;
      node.SelectedIndex:=1;
      node.StateIndex:=1;
      c := node.TreeNodes.AddChildObject(node, format('item %d',[i]),
        TJSONArray(data).Items[i]);
      case TJSONArray(data).Items[i].JSONType of
        jtObject, jtArray:
          addPropsFrom(c, TJSONArray(data).Items[i]);
        else begin
          c.ImageIndex:=2;
          c.SelectedIndex:=2;
          c.StateIndex:=2;
        end;
      end;
    end;
  end;
var
  str: string = '';
  rcl: TTreeNode;
begin

  if propTree.Selected.isNotNil then
    str := propTree.Selected.GetTextPath;

  propTree.Items.Clear;
  edProp.Clear;
  if fProj.isNil or fProj.json.isNil then
    exit;

  propTree.BeginUpdate;
  addPropsFrom(propTree.Items.Add(nil, 'project'), fProj.json);
  if str.isNotEmpty then
  begin
    rcl := propTree.Items.FindNodeWithTextPath(str);
    if rcl.isNotNil then
    begin
      rcl.Selected := true;
      rcl.MakeVisible;
    end;
  end;
  propTree.EndUpdate;
end;
{$ENDREGION}

end.

