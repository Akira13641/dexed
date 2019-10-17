unit u_optionseditor;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, ExtCtrls,
  Menus, ComCtrls, Buttons, PropEdits, ObjectInspector, u_sharedres,
  u_common, u_widget, u_interfaces, u_observer, u_dialogs;

type

  // store the information about the obsever
  // exposing some editable options.
  PCategoryData = ^TCategoryData;
  TCategoryData = record
    kind: TOptionEditorKind;
    container: TPersistent;
    observer: IEditableOptions;
  end;

  { TOptionEditorWidget }

  TOptionEditorWidget = class(TDexedWidget, IOptionsEditor)
    btnCancel: TSpeedButton;
    btnAccept: TSpeedButton;
    pnlEd: TPanel;
    pnlBody: TPanel;
    pnlFooter: TPanel;
    Splitter1: TSplitter;
    inspector: TTIPropertyGrid;
    selCat: TTreeView;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure inspectorEditorFilter(Sender: TObject; aEditor: TPropertyEditor;
      var aShow: boolean);
    procedure inspectorModified(Sender: TObject);
    procedure selCatChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure selCatDeletion(Sender: TObject; Node: TTreeNode);
    procedure selCatSelectionChanged(Sender: TObject);
  protected
    procedure UpdateShowing; override;
  private
    fUpdatingCat: boolean;
    fCatChanged: boolean;
    fEdOptsSubj: TEditableOptionsSubject;
    procedure updateCategories;
    function allowCategoryChange: boolean;
    function sortCategories(Cat1, Cat2: TTreeNode): integer;
    procedure showOptionEditor(observer: IEditableOptions = nil);
    function singleServiceName: string;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  msg_mod = 'The current category modifications are not validated, discard them and continue ?';

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TOptionEditorWidget.create(aOwner: TComponent);
begin
  inherited;
  toolbarVisible:=false;
  fIsDockable := false;
  fIsModal:= true;
  fEdOptsSubj := TEditableOptionsSubject.create;
  inspector.CheckboxForBoolean := true;
  inspector.PropertyEditorHook.AddHandlerModified(@inspectorModified);
  inspector.DefaultItemHeight := scaleY(22, 96);
  selCat.Width := ScaleX(180, 96);
  width := ScaleX(600, 96);

  case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnCancel, 'CANCEL');
      AssignPng(btnAccept, 'ACCEPT');
    end;
    iss24:
    begin
      AssignPng(btnCancel, 'CANCEL24');
      AssignPng(btnAccept, 'ACCEPT24');
    end;
    iss32:
    begin
      AssignPng(btnCancel, 'CANCEL32');
      AssignPng(btnAccept, 'ACCEPT32');
    end;
  end;

  EntitiesConnector.addSingleService(self);
end;

destructor TOptionEditorWidget.destroy;
begin
  fCatChanged := false;
  fEdOptsSubj.Free;
  inherited;
end;

procedure TOptionEditorWidget.UpdateShowing;
begin
  inherited;
  if Visible then
  begin
    updateCategories;
    inspector.SplitterX := inspector.width div 2;
    inspector.PreferredSplitterX := inspector.SplitterX;
  end;
end;
{$ENDREGION}

{$REGION Option editor things --------------------------------------------------}
procedure TOptionEditorWidget.showOptionEditor(observer: IEditableOptions = nil);
var
  n: TTreeNode;
begin
  if assigned(observer) then
  begin
    if selCat.Items.Count = 0 then
      updateCategories;
    n := selCat.Items.FindNodeWithText(observer.optionedWantCategory());
    if n.isNotNil then
      selCat.Selected := n;
  end;
  showWidget;
end;

function TOptionEditorWidget.singleServiceName: string;
begin
  exit('IOptionsEditor');
end;

procedure TOptionEditorWidget.updateCategories;
var
  i: Integer;
  dt: PCategoryData;
  ed: IEditableOptions;
  sel: string = '';
begin
  if selCat.Selected.isNotNil then
    sel := selCat.Selected.Text;
  fUpdatingCat := true;
  inspector.TIObject := nil;
  selCat.BeginUpdate;
  selCat.Items.Clear;
  for i:= 0 to fEdOptsSubj.observersCount-1 do
  begin
    dt := new(PCategoryData);
    ed := fEdOptsSubj.observers[i] as IEditableOptions;
    selCat.Items.AddObject(nil, ed.optionedWantCategory, dt);
    dt^.container := ed.optionedWantContainer;
    dt^.kind := ed.optionedWantEditorKind;
    dt^.observer := ed;
  end;
  selCat.Items.SortTopLevelNodes(@sortCategories);
  for i := 0 to selCat.Items.Count-1 do
    if SelCat.Items.Item[i].Text = sel then
      SelCat.Selected := SelCat.Items.Item[i];
  selCat.EndUpdate;
  fUpdatingCat := false;
end;

function TOptionEditorWidget.sortCategories(Cat1, Cat2: TTreeNode): integer;
begin
  result := CompareText(Cat1.Text, Cat2.Text);
end;

procedure TOptionEditorWidget.selCatDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data.isNotNil then
    Dispose(PCategoryData(node.Data));
end;

function TOptionEditorWidget.allowCategoryChange: boolean;
var
  dt: PCategoryData;
begin
  result := true;
  if fUpdatingCat then exit;
  if csDestroying in ComponentState then exit;
  if selCat.Selected.isNil then exit;
  if selCat.Selected.Data.isNil then exit;
  // accept/cancel is relative to a single category
  dt := PCategoryData(selCat.Selected.Data);
  // generic editor, changes are tracked directly here
  if dt^.kind = oekGeneric then
  begin
    if fCatChanged then
    begin
      result := dlgYesNo(msg_mod) = mrYes;
      fCatChanged := not result;
      if result then btnCancelClick(nil);
    end;
  // custom editor, changes are notified by optionedOptionsModified()
  end else
  begin
    dt := PCategoryData(selCat.Selected.Data);
    if dt^.container.isNil then exit;
    if dt^.observer = nil then exit;
    if dt^.observer.optionedOptionsModified() then
    begin
      result := dlgYesNo(msg_mod) = mrYes;
      if result then btnCancelClick(nil);
    end;
  end;
end;

procedure TOptionEditorWidget.selCatChanging(Sender: TObject;Node: TTreeNode;
  var AllowChange: Boolean);
begin
  AllowChange := allowCategoryChange;
end;

procedure TOptionEditorWidget.selCatSelectionChanged(Sender: TObject);
var
  dt: PCategoryData;
begin
  // remove either the control, the form or the inspector used as editor.
  inspector.TIObject := nil;
  if pnlEd.ControlCount > 0 then
  begin
    pnlEd.Controls[0].Visible:=false;
    pnlEd.Controls[0].Parent := nil;
  end;
  //
  if selCat.Selected.isNil then exit;
  if selCat.Selected.Data.isNil then exit;
  //
  dt := PCategoryData(selCat.Selected.Data);
  if dt^.container.isNil then exit;
  case dt^.kind of
    oekControl:
      begin
        TWinControl(dt^.container).Parent := pnlEd;
        TWinControl(dt^.container).Align := alClient;
        TWinControl(dt^.container).Visible:=true;
      end;
    oekForm:
      begin
        TCustomForm(dt^.container).ShowInTaskBar:= TShowInTaskbar.stNever;
        TCustomForm(dt^.container).Parent := pnlEd;
        TCustomForm(dt^.container).Align := alClient;
        TCustomForm(dt^.container).BorderIcons:= [];
        TCustomForm(dt^.container).BorderStyle:= bsNone;
        TCustomForm(dt^.container).Show;
        TCustomForm(dt^.container).Visible:=true;
      end;
    oekGeneric:
      begin
        inspector.Parent := pnlEd;
        inspector.Align := alClient;
        inspector.TIObject := dt^.container;
        inspector.Visible:=true;
      end;
  end;
  //
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeSelectCat);
end;

procedure TOptionEditorWidget.inspectorModified(Sender: TObject);
begin
  if selCat.Selected.isNil then exit;
  if selcat.Selected.Data.isNil then exit;
  //
  fCatChanged := true;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeChange);
end;

procedure TOptionEditorWidget.btnCancelClick(Sender: TObject);
begin
  if selCat.Selected.isNil then exit;
  if selcat.Selected.Data.isNil then exit;
  //
  fCatChanged := false;
  if inspector.Parent.isNotNil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeCancel);
end;

procedure TOptionEditorWidget.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  canClose := allowCategoryChange;
end;

procedure TOptionEditorWidget.inspectorEditorFilter(Sender: TObject;aEditor:
  TPropertyEditor; var aShow: boolean);
var
  nme: string;
  len: integer;
begin
  if aEditor.GetComponent(0) is TComponent then
  begin
    nme := aEditor.GetPropInfo^.Name;
    len := nme.length;
    if (len > 2) and (nme[len - 2 .. len] = 'Tag') then
      aShow := false
    else if nme = 'Name' then
      aShow := false
    else if aEditor.GetPropInfo^.PropType = TypeInfo(TCollection) then
      aShow := false;
  end;
end;

procedure TOptionEditorWidget.btnAcceptClick(Sender: TObject);
begin
  if selCat.Selected.isNil then exit;
  if selcat.Selected.Data.isNil then exit;
  //
  fCatChanged := false;
  if inspector.Parent.isNotNil then
    inspector.ItemIndex := -1;
  PCategoryData(selCat.Selected.Data)^
    .observer
    .optionedEvent(oeeAccept);
end;
{$ENDREGION}

end.

