unit u_projinspect;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, TreeFilterEdit, Forms, Controls, Graphics, actnlist,
  Dialogs, ExtCtrls, ComCtrls, Menus, Buttons, lcltype, u_ceproject, u_interfaces,
  u_common, u_widget, u_observer, u_dialogs, u_sharedres, u_dsgncontrols,
  u_dubproject, u_synmemo, u_stringrange, u_writableComponent;

type

  TProjectInspectorOptions = class(TWritableLfmTextComponent)
  private
    fFileListAsTree: boolean;
  published
    property fileListAsTree: boolean read fFileListAsTree write fFileListAsTree;
  end;

  { TProjectInspectWidget }

  TProjectInspectWidget = class(TDexedWidget, IProjectObserver, IDocumentObserver)
    btnAddFile: TDexedToolButton;
    btnAddFold: TDexedToolButton;
    btnReload: TDexedToolButton;
    btnRemFile: TDexedToolButton;
    btnRemFold: TDexedToolButton;
    btnTree: TDexedToolButton;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnAddFoldClick(Sender: TObject);
    procedure btnRemFileClick(Sender: TObject);
    procedure btnRemFoldClick(Sender: TObject);
    procedure btnTreeClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const fnames: array of String);
    procedure toolbarResize(Sender: TObject);
    procedure TreeClick(Sender: TObject);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeSelectionChanged(Sender: TObject);
  protected
    procedure updateImperative; override;
    procedure updateDelayed; override;
    procedure SetVisible(value: boolean); override;
    procedure setToolBarFlat(value: boolean); override;
  private
    fActOpenFile: TAction;
    fActSelConf: TAction;
    fActBuildConf: TAction;
    fProject: ICommonProject;
    fFileNode, fConfNode: TTreeNode;
    fLastFileOrFolder: string;
    fSymStringExpander: ISymStringExpander;
    fImages: TImageList;
    fFileListAsTree: boolean;
    procedure actUpdate(sender: TObject);
    procedure DetectNewDubSources(const document: TDexedMemo);
    procedure TreeDblClick(sender: TObject);
    procedure actOpenFileExecute(sender: TObject);
    procedure actBuildExecute(sender: TObject);
    //
    procedure projNew(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    procedure updateButtons;
    procedure setFileListAsTree(value: boolean);
    //
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
  protected
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    property fileListAsTree: boolean read fFileListAsTree write setFileListAsTree;
  end;

implementation
{$R *.lfm}

const optFname = 'projinspect.txt';
const filterAlign: array[boolean] of integer = (58, 162);

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TProjectInspectWidget.create(aOwner: TComponent);
var
  fname: string;
begin
  fSymStringExpander:= getSymStringExpander;

  fActOpenFile := TAction.Create(self);
  fActOpenFile.Caption := 'Open file in editor';
  fActOpenFile.OnExecute := @actOpenFileExecute;
  fActSelConf := TAction.Create(self);
  fActSelConf.Caption := 'Select configuration';
  fActSelConf.OnExecute := @actOpenFileExecute;
  fActSelConf.OnUpdate := @actUpdate;
  fActBuildConf:= TAction.Create(self);
  fActBuildConf.Caption := 'Build configuration';
  fActBuildConf.OnExecute := @actBuildExecute;
  fActBuildConf.OnUpdate := @actUpdate;

  inherited;

  fImages := TImageList.Create(self);
  case GetIconScaledSize of
    iss16:
    begin
      fImages.Width := 16;
      fImages.Height := 16;
      Tree.Indent := 16;
      fImages.AddResourceName(HINSTANCE, 'DOCUMENT_ALL');
      fImages.AddResourceName(HINSTANCE, 'WRENCH');
      fImages.AddResourceName(HINSTANCE, 'PAGE_TEXT');
      fImages.AddResourceName(HINSTANCE, 'COG');
      fImages.AddResourceName(HINSTANCE, 'COG_GO');
      fImages.AddResourceName(HINSTANCE, 'FOLDER');
      AssignPng(TreeFilterEdit1.Glyph, 'FILTER_CLEAR');
    end;
    iss24:
    begin
      fImages.Width := 24;
      fImages.Height := 24;
      Tree.Indent := 24;
      fImages.AddResourceName(HINSTANCE, 'DOCUMENT_ALL24');
      fImages.AddResourceName(HINSTANCE, 'WRENCH24');
      fImages.AddResourceName(HINSTANCE, 'PAGE_TEXT24');
      fImages.AddResourceName(HINSTANCE, 'COG24');
      fImages.AddResourceName(HINSTANCE, 'COG_GO24');
      fImages.AddResourceName(HINSTANCE, 'FOLDER24');
      AssignPng(TreeFilterEdit1.Glyph, 'FILTER_CLEAR24');
    end;
    iss32:
    begin
      fImages.Width := 32;
      fImages.Height := 32;
      Tree.Indent := 32;
      fImages.AddResourceName(HINSTANCE, 'DOCUMENT_ALL32');
      fImages.AddResourceName(HINSTANCE, 'WRENCH32');
      fImages.AddResourceName(HINSTANCE, 'PAGE_TEXT32');
      fImages.AddResourceName(HINSTANCE, 'COG32');
      fImages.AddResourceName(HINSTANCE, 'COG_GO32');
      fImages.AddResourceName(HINSTANCE, 'FOLDER32');
      AssignPng(TreeFilterEdit1.Glyph, 'FILTER_CLEAR32');
    end;
  end;

  Tree.OnDblClick := @TreeDblClick;
  fFileNode := Tree.Items[0];
  fConfNode := Tree.Items[1];

  Tree.Images := fImages;
  Tree.PopupMenu := contextMenu;
  TreeFilterEdit1.BorderSpacing.Left := ScaleX(filterAlign[false], 96);
  toolbarResize(nil);

  fname := getDocPath + optFname;
  if fname.fileExists then
  begin
    with TProjectInspectorOptions.Create(nil) do
    try
      loadFromFile(fname);
      self.setFileListAsTree(fileListAsTree);
      btnTree.Down:=fileListAsTree;
    finally
      free;
    end;
  end;

  EntitiesConnector.addObserver(self);
end;

destructor TProjectInspectWidget.destroy;
begin
  with TProjectInspectorOptions.Create(nil) do
  try
    fileListAsTree:= self.fileListAsTree;
    saveToFile(getDocPath + optFname);
  finally
    free;
  end;
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TProjectInspectWidget.SetVisible(value: boolean);
begin
  inherited;
  if value then
    updateImperative;
end;

procedure TProjectInspectWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  TreeFilterEdit1.Flat:=value;
end;
{$ENDREGION}

{$REGION IContextualActions---------------------------------------------------}
function TProjectInspectWidget.contextName: string;
begin
  exit('Inspector');
end;

function TProjectInspectWidget.contextActionCount: integer;
begin
  exit(3);
end;

function TProjectInspectWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActOpenFile);
    1: exit(fActSelConf);
    2: exit(fActBuildConf);
    else exit(nil);
  end;
end;

procedure TProjectInspectWidget.actOpenFileExecute(sender: TObject);
begin
  TreeDblClick(sender);
end;

procedure TProjectInspectWidget.actBuildExecute(sender: TObject);
begin
  if fProject <> nil then
  begin
    actOpenFileExecute(sender);
    fProject.compile;
  end;
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TProjectInspectWidget.docNew(document: TDexedMemo);
begin
  DetectNewDubSources(document);
end;

procedure TProjectInspectWidget.docFocused(document: TDexedMemo);
begin
  DetectNewDubSources(document);
end;

procedure TProjectInspectWidget.docChanged(document: TDexedMemo);
begin
end;

procedure TProjectInspectWidget.docClosing(document: TDexedMemo);
begin
  DetectNewDubSources(document);
end;
{$ENDREGION}

{$REGION IProjectObserver -----------------------------------------------------}
procedure TProjectInspectWidget.projNew(project: ICommonProject);
begin
  fLastFileOrFolder := '';
  fProject := project;
  if Visible then
    updateImperative;
  updateButtons;
end;

procedure TProjectInspectWidget.projClosing(project: ICommonProject);
begin
  if not assigned(fProject)  then
    exit;
  if project <> fProject then
    exit;
  fProject := nil;
  fLastFileOrFolder := '';
  updateImperative;
end;

procedure TProjectInspectWidget.projFocused(project: ICommonProject);
begin
  fLastFileOrFolder := '';
  fProject := project;
  DetectNewDubSources(nil);
  updateButtons;
  if Visible then
    beginDelayedUpdate;
end;

procedure TProjectInspectWidget.projChanged(project: ICommonProject);
begin
  if not assigned(fProject)  then
    exit;
  if fProject <> project then
    exit;
  if Visible then
    beginDelayedUpdate;
end;

procedure TProjectInspectWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TProjectInspectWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;

procedure TProjectInspectWidget.updateButtons;
var
  ce: boolean;
  sp: integer;
begin
  ce := fProject.getFormat = pfDEXED;

  btnRemFold.Visible:= ce;
  btnAddFold.Visible:= ce;
  btnRemFile.Visible:= ce;
  btnAddFile.Visible:= ce;

  TreeFilterEdit1.Left := toolbar.Width - 5;
  sp := scaleX(2, 96);
  if ce then
    TreeFilterEdit1.Left  := btnRemFold.Left + btnRemFold.Width + 2
  else
    TreeFilterEdit1.Left  := btnTree.Left + btnRemFold.Width + 2;
  TreeFilterEdit1.width := toolbar.Width - TreeFilterEdit1.Left - sp;
  TreeFilterEdit1.top   := sp;
  TreeFilterEdit1.Height:= toolbar.Height - sp * 2;
end;

procedure TProjectInspectWidget.setFileListAsTree(value: boolean);
begin
  if fFileListAsTree = value then
    exit;
  fFileListAsTree:=value;
  updateImperative;
end;

{$ENDREGION}

{$REGION Inspector things -------------------------------------------------------}
procedure TProjectInspectWidget.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    TreeDblClick(nil);
end;

procedure TProjectInspectWidget.TreeClick(Sender: TObject);
begin
  if Tree.Selected.isNotNil then
  begin
    Tree.MultiSelect := Tree.Selected.Parent = fFileNode;
    if not (Tree.Selected.Parent = fFileNode) then
    begin
      Tree.MultiSelect := false;
      Tree.ClearSelection(true);
      Tree.Selected.MultiSelected:=false;
    end;
  end
  else
  begin
    Tree.MultiSelect := false;
    Tree.ClearSelection(true);
  end;
end;

procedure TProjectInspectWidget.TreeDeletion(Sender: TObject; Node: TTreeNode
  );
begin
  if Node.isNotNil and Node.Data.isNotNil then
    dispose(PString(Node.Data));
end;

procedure TProjectInspectWidget.TreeSelectionChanged(Sender: TObject);
begin
  actUpdate(sender);
  if not assigned(fProject) or Tree.Selected.isNil then
    exit;
  if (Tree.Selected.Parent = fFileNode) then
    fLastFileOrFolder := expandFilenameEx(fProject.basePath,tree.Selected.Text)
  else
    fLastFileOrFolder := tree.Selected.Text;
end;

procedure TProjectInspectWidget.TreeDblClick(sender: TObject);
var
  fname: string;
  i: integer;
begin
  if not assigned(fProject) or Tree.Selected.isNil then
    exit;

  if Tree.Selected.Parent <> fConfNode then
  begin
    if Tree.Selected.Data.isNotNil then
    begin
      fname := PString(Tree.Selected.Data)^;
      if isEditable(fname.extractFileExt) and fname.fileExists then
        getMultiDocHandler.openDocument(fname);
    end;
  end
  else
  begin
    i := Tree.Selected.Index;
    fProject.setActiveConfigurationIndex(i);
    beginDelayedUpdate;
  end;
end;

procedure TProjectInspectWidget.actUpdate(sender: TObject);
begin
  fActSelConf.Enabled := false;
  fActOpenFile.Enabled := false;
  fActBuildConf.Enabled:= false;
  if Tree.Selected.isNil then
    exit;
  fActSelConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActBuildConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActOpenFile.Enabled := Tree.Selected.ImageIndex = 2;
end;

procedure TProjectInspectWidget.DetectNewDubSources(const document: TDexedMemo
  );
begin
  if not assigned(fProject) or (fProject.getFormat <> pfDUB) then
    exit;
  if document.isNotNil then
  begin
    if document.fileName.contains(fProject.basePath) then
      TDubProject(fProject.getProject).updateSourcesList;
  end
  else TDubProject(fProject.getProject).updateSourcesList;
  //updateImperative;
end;

procedure TProjectInspectWidget.btnAddFileClick(Sender: TObject);
var
  fname: string;
  proj: TNativeProject;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDUB) then
    exit;

  proj := TNativeProject(fProject.getProject);
  with TOpenDialog.Create(nil) do
  try
    options := options + [ofAllowMultiSelect];
    if fLastFileOrFolder.fileExists then
      InitialDir := fLastFileOrFolder.extractFilePath
    else if fLastFileOrFolder.dirExists then
      InitialDir := fLastFileOrFolder;
    filter := DdiagFilter;
    if execute then
    begin
      proj.beginUpdate;
      for fname in Files do
        proj.addSource(fname.normalizePath);
      proj.endUpdate;
    end;
  finally
    free;
  end;
end;

procedure TProjectInspectWidget.btnAddFoldClick(Sender: TObject);
var
  dir, fname: string;
  lst: TStringList;
  proj: TNativeProject;
  i: integer;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDUB) then
    exit;

  dir := '';
  proj := TNativeProject(fProject.getProject);
  if fLastFileOrFolder.fileExists then
    dir := fLastFileOrFolder.extractFilePath
  else if fLastFileOrFolder.dirExists then
    dir := fLastFileOrFolder
  else if fProject.fileName.fileExists then
    dir := fProject.fileName.extractFilePath;
  if selectDirectory('sources', dir, dir, true, 0) then
  begin
    proj.beginUpdate;
    lst := TStringList.Create;
    try
      listFiles(lst, dir, true);
      for i := 0 to lst.Count-1 do
      begin
        fname := lst[i];
        if isDlangCompilable(fname.extractFileExt) then
          proj.addSource(fname);
      end;
    finally
      lst.Free;
      proj.endUpdate;
    end;
  end;
end;

procedure TProjectInspectWidget.btnRemFoldClick(Sender: TObject);
var
  dir, fname: string;
  proj: TNativeProject;
  i: Integer;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDUB)
  or Tree.Selected.isNil or (Tree.Selected.Parent <> fFileNode) then
    exit;

  proj := TNativeProject(fProject.getProject);
  fname := Tree.Selected.Text;
  i := proj.Sources.IndexOf(fname);
  if i = -1 then
    exit;
  fname := fProject.sourceAbsolute(i);
  dir := fname.extractFilePath;
  if not dir.dirExists then
    exit;

  proj.beginUpdate;
  for i:= proj.Sources.Count-1 downto 0 do
    if proj.sourceAbsolute(i).extractFilePath = dir then
      proj.Sources.Delete(i);
  proj.endUpdate;
end;

procedure TProjectInspectWidget.btnTreeClick(Sender: TObject);
begin
  setFileListAsTree(btnTree.Down);
end;

procedure TProjectInspectWidget.btnReloadClick(Sender: TObject);
var
  f: string;
begin
  if assigned(fProject) then
  begin
    f := fProject.filename;
    if not f.fileExists then
      exit;
    if fProject.modified and
      (dlgYesNo('The project seems to be modified, save before reloading') = mrYes) then
        fProject.saveToFile(f);
    fProject.loadFromFile(f);
  end;
end;

procedure TProjectInspectWidget.btnRemFileClick(Sender: TObject);
var
  fname: string;
  proj: TNativeProject;
  i, j: integer;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDUB)
  or Tree.Selected.isNil or (Tree.Selected.Parent <> fFileNode) then
    exit;

  proj := TNativeProject(fProject.getProject);
  proj.beginUpdate;
  for j:= 0 to Tree.SelectionCount-1 do
  begin
    fname := Tree.Selections[j].Text;
    i := proj.Sources.IndexOf(fname);
    if i <> -1 then
      proj.Sources.Delete(i);
  end;

  fname := '';
  for i := 0 to proj.sourcesCount-1 do
    if not proj.sourceAbsolute(i).fileExists then
      fname += LineEnding + '    "' + proj.sourceAbsolute(i) + '" ';

  if fname.isNotEmpty and (dlgOkCancel('Other source(s) not found: ' + LineEnding
    + fname + LineEnding + LineEnding + 'Remove all invalid files ?') = mrOK) then
  begin
    for j := proj.sourcesCount-1 downto 0 do
      if not proj.sourceAbsolute(j).fileExists then
        proj.Sources.Delete(j);
  end;

  proj.endUpdate;
end;

procedure TProjectInspectWidget.FormDropFiles(Sender: TObject; const fnames: array of String);
var
  fname, direntry: string;
  lst: TStringList;
  proj: TNativeProject;
procedure addFile(const value: string);
var
  ext: string;
begin
  ext := value.extractFileExt;
  if not isDlangCompilable(ext) then
    exit;
  proj.addSource(value);
  if isEditable(ext) then
    getMultiDocHandler.openDocument(value);
end;
begin
  if not assigned(fProject) or (fProject.getFormat = pfDUB) then
    exit;

  proj := TNativeProject(fProject.getProject);
  lst := TStringList.Create;
  proj.beginUpdate;
  try for fname in fnames do
    if fname.fileExists then
      addFile(fname)
    else if fname.dirExists then
    begin
      lst.Clear;
      listFiles(lst, fname, true);
      for direntry in lst do
        addFile(dirEntry);
    end;
  finally
    proj.endUpdate;
    lst.Free;
  end;
end;

procedure TProjectInspectWidget.toolbarResize(Sender: TObject);
begin
  TreeFilterEdit1.Width := toolbar.Width - TreeFilterEdit1.Left - TreeFilterEdit1.BorderSpacing.Around;
end;

procedure TProjectInspectWidget.updateDelayed;
begin
  updateImperative;
end;

procedure TProjectInspectWidget.updateImperative;
var
  conf: string;
  itm: TTreeNode;
  chd: TTreeNode;
  i,j: integer;
  sel: string = '';
  fld: string;
  rng: TStringRange = (ptr:nil; pos:0; len:0);
begin
  if Tree.Selected.isNotNil then
    sel := Tree.Selected.GetTextPath;
  fConfNode.DeleteChildren;
  fFileNode.DeleteChildren;

  if not assigned(fProject) then
    exit;

  Tree.BeginUpdate;

  if not fFileListAsTree then
    for i := 0 to fProject.sourcesCount-1 do
  begin
    itm := Tree.Items.AddChild(fFileNode, fProject.sourceRelative(i));
    itm.Data:= NewStr(fProject.sourceAbsolute(i));
    itm.ImageIndex := 2;
    itm.SelectedIndex := 2;
  end
  else
  // first pass only creates the folders so that they're shown on top
  for j := 0 to 1 do
    for i := 0 to fProject.sourcesCount-1 do
  begin
    fld := '';
    rng.init(fProject.sourceRelative(i));
    itm := fFileNode;
    while not rng.empty do
    begin
      chd := nil;
      fld := rng.takeUntil(['/','\']).yield;
      chd := itm.FindNode(fld);
      if chd.isNil and ((rng.empty and (j = 1)) or (not rng.empty and (j = 0))) then
        chd := Tree.Items.AddChild(itm, fld);
      if chd.isNotNil then
        itm := chd;
      // reached fname
      if rng.empty and (j = 1) then
      begin
        itm.Data:= NewStr(fProject.sourceAbsolute(i));
        itm.ImageIndex := 2;
        itm.SelectedIndex := 2;
      end
      // next folder or fname
      else
      begin
        rng.popWhile(['/','\']);
        itm.ImageIndex := 5;
        itm.SelectedIndex := 5;
      end;
    end;
  end;


  j := fProject.getActiveConfigurationIndex;
  for i := 0 to fProject.configurationCount-1 do
  begin
    conf := fProject.configurationName(i);
    if i = j then
      conf += ' (active)';
    itm := Tree.Items.AddChild(fConfNode, conf);
    if i = j then
    begin
      itm.ImageIndex := 4;
      itm.SelectedIndex:= 4;
    end
    else
    begin
      itm.ImageIndex := 3;
      itm.SelectedIndex:= 3;
    end;
  end;
  if sel.isNotEmpty then
  begin
    itm := Tree.Items.FindNodeWithTextPath(sel);
    if itm.isNotNil then
    begin
      itm.Selected := true;
      itm.MakeVisible;
    end;
  end;
  Tree.EndUpdate;
end;
{$ENDREGION --------------------------------------------------------------------}

end.
