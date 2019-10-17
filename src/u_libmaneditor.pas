unit u_libmaneditor;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, Buttons, LazFileUtils, fphttpclient, StdCtrls,
  fpjson, jsonparser,
  u_widget, u_interfaces, u_ceproject, u_dmdwrap, u_common, u_dialogs,
  u_sharedres, process, u_dubproject, u_observer, u_libman,
  u_projutils, u_dsgncontrols, u_controls;

type

  TDubPackageQueryForm = class(TForm)
  private
    class var fList: TJSONData;
    class var fGetLatestTag: boolean;
    cbb: TComboBox;
    function getPackageName: string;
    function getPackageVersion: string;
    procedure getList(sender: TObject);
    procedure fillList;
    procedure btnTagCLick(sender: TObject);
    procedure updateHint(sender: TObject);
  public
    class function showAndWait(out pName, pVersion: string): TModalResult; static;
    class destructor classDtor;
    constructor Create(TheOwner: TComponent); override;
    property packageName: string read getPackageName;
    property packageVersion: string read getPackageVersion;
  end;

  TLibManEditorWidget = class(TDexedWidget, IProjectObserver)
    btnAddLib: TDexedToolButton;
    btnDubFetch: TDexedToolButton;
    btnEditAlias: TDexedToolButton;
    btnEnabled: TDexedToolButton;
    btnMoveDown: TDexedToolButton;
    btnMoveUp: TDexedToolButton;
    btnOpenProj: TDexedToolButton;
    btnReg: TDexedToolButton;
    btnRemLib: TDexedToolButton;
    btnSelFile: TDexedToolButton;
    btnSelfoldOfFiles: TDexedToolButton;
    btnSelProj: TDexedToolButton;
    btnSelRoot: TDexedToolButton;
    List: TListView;
    procedure btnAddLibClick(Sender: TObject);
    procedure btnEnabledClick(Sender: TObject);
    procedure btnDubFetchClick(Sender: TObject);
    procedure btnEditAliasClick(Sender: TObject);
    procedure btnOpenProjClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnRemLibClick(Sender: TObject);
    procedure btnSelFileClick(Sender: TObject);
    procedure btnSelfoldOfFilesClick(Sender: TObject);
    procedure btnSelProjClick(Sender: TObject);
    procedure btnSelRootClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure ListEdited(Sender: TObject; Item: TListItem; var value: string);
    procedure ListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    fProj: ICommonProject;
    fFreeProj: ICommonProject;
    procedure updateButtonsState;
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    function  itemForRow(row: TListItem): TLibraryItem;
    procedure RowToLibrary(row: TListItem; added: boolean = false);
    procedure dataToGrid;
    function isAliasRegistered(const anAlias: string): boolean;
  protected
    procedure DoShow; override;
  public
    constructor Create(aOwner: TComponent); override;
  end;


implementation
{$R *.lfm}

const
  notav: string = '< n/a >';
  enableStr: array [boolean] of string = ('false','true');


function YesOrNoAddProjSourceFolder: TModalResult;
begin
  result :=
  dlgYesNo('The registered project is not a library '+
    'however it is possible to make its sources accessible for unittesting and executing runnable modules. ' +
    'If you click `YES` this will be done, otherwise the new entry will only be used for the completions.');
end;

constructor TLibManEditorWidget.Create(aOwner: TComponent);
begin
  inherited;
  TListViewCopyMenu.create(List);
end;

procedure TLibManEditorWidget.updateButtonsState;
var
  i: TIconScaledSize;
begin
  btnReg.Enabled := (fProj <> nil) and fProj.Filename.fileExists;
  btnOpenProj.Enabled := List.Selected.isNotNil and
    List.Selected.SubItems[2].fileExists;
  i := GetIconScaledSize;
  if List.Selected.isNotNil and itemForRow(List.Selected).isNotNil and
    itemForRow(List.Selected).enabled then
  begin
    case i of
      iss16: btnEnabled.resourceName := 'BOOK';
      iss24: btnEnabled.resourceName := 'BOOK24';
      iss32: btnEnabled.resourceName := 'BOOK32';
    end;
  end
  else
  begin
    case i of
      iss16: btnEnabled.resourceName := 'BOOK_GREY';
      iss24: btnEnabled.resourceName := 'BOOK_GREY24';
      iss32: btnEnabled.resourceName := 'BOOK_GREY32';
    end;
  end;
end;

function TLibManEditorWidget.isAliasRegistered(const anAlias: string): boolean;
var
  i: TListItem = nil;
begin
  result := list.Items.findCaption(anAlias, i);
end;

procedure TLibManEditorWidget.projNew(project: ICommonProject);
begin
  fProj := project;
  if not project.inGroup then
    fFreeProj := project;
end;

procedure TLibManEditorWidget.projChanged(project: ICommonProject);
begin
  if fProj = nil then exit;
  if fProj <> project then
    exit;

  updateButtonsState;
end;

procedure TLibManEditorWidget.projClosing(project: ICommonProject);
begin
  if fProj = project then
    fProj := nil;
  if project = fFreeProj then
    fFreeProj := nil;
  updateButtonsState;
end;

procedure TLibManEditorWidget.projFocused(project: ICommonProject);
begin
  fProj := project;
  if not project.inGroup then
    fFreeProj := project
  else if project = fFreeProj then
    fFreeProj := nil;
  updateButtonsState;
end;

procedure TLibManEditorWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TLibManEditorWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;

function TLibManEditorWidget.itemForRow(row: TListItem): TLibraryItem;
begin
  result := TLibraryItem(row.Data);
end;

procedure TLibManEditorWidget.ListEdited(Sender: TObject; Item: TListItem; var value: string);
begin
  if Item.isNotNil then
    RowToLibrary(item);
end;

procedure TLibManEditorWidget.ListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  updateButtonsState;
end;

procedure TLibManEditorWidget.btnAddLibClick(Sender: TObject);
var
  itm: TListItem;
begin
  itm := List.Items.Add;
  itm.Data := LibMan.libraries.Add;
  itm.Caption := notav;
  itm.SubItems.Add(notav);
  itm.SubItems.Add(notav);
  itm.SubItems.Add(notav);
  itm.SubItems.Add(enableStr[true]);
  SetFocus;
  itm.Selected := True;
end;

class destructor TDubPackageQueryForm.classDtor;
begin
  fList.Free;
end;

constructor TDubPackageQueryForm.Create(TheOwner: TComponent);
var
  bok: TBitBtn;
  bno: TBitBtn;
  bww: TBitBtn;
  bsv: TSpeedButton;
  ics: TIconScaledSize;
begin
  inherited;

  ics := GetIconScaledSize;

  width  := ScaleX(400,96);
  height := ScaleY(40,96);
  BorderStyle:= bsToolWindow;
  caption := 'Select or type the DUB package name';
  Position:= poMainFormCenter;

  cbb := TComboBox.Create(self);
  cbb.Parent := self;
  cbb.AutoComplete := true;
  cbb.Align := alClient;
  cbb.BorderSpacing.Around := 6;
  cbb.Sorted:= true;
  cbb.ShowHint:=true;
  cbb.OnSelect:= @updateHint;
  cbb.OnCloseUp:=@updateHint;
  cbb.AutoSize:=true;

  bsv := TSpeedButton.Create(self);
  bsv.Parent := self;
  bsv.Align := alRight;
  bsv.AutoSize:= true;
  bsv.BorderSpacing.Around := 4;
  bsv.ShowHint := true;
  bsv.Hint := 'get latest tag, by default get master';
  bsv.OnClick:= @btnTagCLick;
  bsv.AllowAllUp := true;
  bsv.GroupIndex := 1;
  bsv.Layout:= blGlyphTop;
  bsv.Spacing:= 2;
  bsv.Down:=fGetLatestTag;
  case ics of
    iss16: AssignPng(bsv, 'TAG_PURPLE');
    iss24: AssignPng(bsv, 'TAG_PURPLE24');
    iss32: AssignPng(bsv, 'TAG_PURPLE32');
  end;


  bww := TBitBtn.Create(self);
  bww.Parent := self;
  bww.Align := alRight;
  bww.AutoSize:=true;
  bww.BorderSpacing.Around := 4;
  bww.ShowHint := true;
  bww.Hint := 'get the package list';
  bww.OnClick:= @getList;
  bww.Layout:= blGlyphTop;
  bww.Spacing:= 2;
  case ics of
    iss16: AssignPng(bww, 'ARROW_UPDATE');
    iss24: AssignPng(bww, 'ARROW_UPDATE24');
    iss32: AssignPng(bww, 'ARROW_UPDATE32');
  end;

  bok := TBitBtn.Create(self);
  bok.Parent := self;
  bok.ModalResult:= mrOk;
  bok.Align := alRight;
  bok.AutoSize:=true;
  bok.BorderSpacing.Around := 4;
  bok.Hint := 'try to fetch, compile and auto-register';
  bok.ShowHint := true;
  bok.Layout:= blGlyphTop;
  bok.Spacing:= 2;
  case ics of
    iss16: AssignPng(bok, 'ACCEPT');
    iss24: AssignPng(bok, 'ACCEPT24');
    iss32: AssignPng(bok, 'ACCEPT32');
  end;

  bno := TBitBtn.Create(self);
  bno.Parent := self;
  bno.ModalResult:= mrCancel;
  bno.Align := alRight;
  bno.AutoSize:=true;
  bno.BorderSpacing.Around := 4;
  bno.Hint := 'cancel and do nothing';
  bno.ShowHint := true;
  bno.Layout:= blGlyphTop;
  bno.Spacing:= 2;
  case ics of
    iss16: AssignPng(bno, 'CANCEL');
    iss24: AssignPng(bno, 'CANCEL24');
    iss32: AssignPng(bno, 'CANCEL32');
  end;


  fillList;
end;

procedure TDubPackageQueryForm.btnTagCLick(sender: TObject);
begin
  fGetLatestTag:= TSpeedButton(sender).down;
end;

procedure TDubPackageQueryForm.getList(sender: TObject);
var
  pge: string;
  cli: TFPHTTPClient;
  prs: TJSONParser;
begin
  if assigned(fList) then
    fList.free;
  cli := TFPHTTPClient.Create(nil);
  try
    try
      //TODO: use HTTPS when FCL-WEB will allow it again.
      pge := cli.Get('http://code.dlang.org/api/packages/search');
    except
      pge := '[]';
    end;
  finally
    cli.Free;
  end;
  prs := TJSONParser.Create(pge, []);
  try
    fList := prs.Parse;
  finally
    prs.Free;
  end;
  fillList;
end;

procedure TDubPackageQueryForm.fillList;
var
  itm: TJSONData;
  i: integer;
begin
  cbb.Clear;
  if fList.isNotNil and (fList.JSONType = jtArray) then
    for i := 0 to fList.Count -1 do
  begin
    itm := fList.Items[i].FindPath('version');
    if itm.isNil then
      continue;
    itm := fList.Items[i].FindPath('name');
    if itm.isNil then
      continue;
    cbb.Items.AddObject(itm.AsString, fList.Items[i]);
  end;
end;

function TDubPackageQueryForm.getPackageName: string;
begin
  result := cbb.Text;
end;

function TDubPackageQueryForm.getPackageVersion: string;
var
  jsn: TJSONData;
begin
  result := 'master';
  if fGetLatestTag then
  begin
    // list is updated
    if fList.isNotNil and (cbb.ItemIndex <> -1) and
      cbb.Items.Objects[cbb.ItemIndex].isNotNil then
    begin
      jsn := TJSONData(cbb.Items.Objects[cbb.ItemIndex]);
      jsn := jsn.FindPath('version');
      result := jsn.AsString;
    end
    else
    // use API
    begin
      with TFPHTTPClient.Create(nil) do
      try
        try
          //TODO: use HTTPS when FCL-WEB will allow it again.
          result := Get('http://code.dlang.org/api/packages/' + packageName + '/latest');
        except
          result := 'master';
        end;
      finally
        Free;
      end;
      if (result.length >= 7) and (result[2] in ['0'..'9']) then
        result := result[2..result.length-1]
    end;
  end;
end;

procedure TDubPackageQueryForm.updateHint(sender: TObject);
var
  jsn: TJSONData;
begin
  if (cbb.ItemIndex <> -1) and cbb.Items.Objects[cbb.ItemIndex].isNotNil then
  try
    jsn := TJSONData(cbb.Items.Objects[cbb.ItemIndex]);
    jsn := jsn.FindPath('description');
    if jsn.isNotNil then
      cbb.Hint:= jsn.AsString;
  except
  end;
end;

class function TDubPackageQueryForm.showAndWait(out pName, pVersion: string): TModalResult;
var
  frm: TDubPackageQueryForm;
begin
  frm := TDubPackageQueryForm.Create(nil);
  result := frm.ShowModal;
  if result = mrOk then
  begin
    pName := frm.packageName;
    pVersion := frm.packageVersion;
  end
  else
  begin
    pName := '';
    pVersion := '';
  end;
  frm.Free;
end;

procedure TLibManEditorWidget.btnDubFetchClick(Sender: TObject);
var
  dub: TProcess;
  nme: string = '';
  ver: string;
  msg: string;
  pth: string;
  dfn: string;
  str: TStringList;
  itf: IMessagesDisplay;
  err: integer;
  prj: TDubProject;
  ovw: boolean = false;
  row: TListItem = nil;
begin
  if TDubPackageQueryForm.showAndWait(nme, ver) <> mrOk then
    exit;
  if isAliasRegistered(nme) then
  begin
    if dlgYesNo(format('a library item with the alias "%s" already exists, do you wish to update it ?',
      [nme])) <> mrYes then
        exit
    else
      ovw := true;
  end;
  {$IFDEF WINDOWS}
  pth := GetEnvironmentVariable('APPDATA') + '\dub\packages\' + nme + '-' + ver;
  {$ELSE}
  pth := GetEnvironmentVariable('HOME') + '/.dub/packages/' + nme + '-' + ver;
  {$ENDIF}
  itf := getMessageDisplay;
  if pth.dirExists and not DeleteDirectory(pth, false) then
  begin
    itf.message('the existing package cant be deleted. To be updated the package must be deleted manually',
      nil, amcMisc, amkWarn);
    exit;
  end;

  // fetch
  dub := TProcess.Create(nil);
  try
    dub.Executable:= 'dub';
    dub.Options:= [poUsePipes, poStderrToOutPut];
    dub.ShowWindow:= swoHIDE;
    dub.Parameters.Add('fetch');
    dub.Parameters.Add(nme);
    if ver = 'master' then
      dub.Parameters.Add('--version=~master')
    else
      dub.Parameters.Add('--version=' + ver);
    dub.Execute;
    str := TStringList.Create;
    try
      processOutputToStrings(dub, str);
      while dub.Running do;
      err := dub.ExitStatus;
      for msg in str do
        itf.message(msg, nil, amcMisc, amkAuto);
    finally
      str.Free;
    end;
  finally
    dub.Free;
  end;
  if err <> 0 then
  begin
    itf.message('error, failed to fetch the package', nil, amcMisc, amkErr);
    exit;
  end;

  // get the description
  if FileExists(pth + DirectorySeparator + 'dub.json') then
    dfn := pth + DirectorySeparator + 'dub.json'
  else if FileExists(pth + DirectorySeparator + 'package.json') then
    dfn := pth + DirectorySeparator + 'package.json'
  else if FileExists(pth + DirectorySeparator + nme + DirectorySeparator + 'dub.json') then
    dfn := pth + DirectorySeparator + nme + DirectorySeparator + 'dub.json'
  else if FileExists(pth + DirectorySeparator + nme + DirectorySeparator + 'package.json') then
    dfn := pth + DirectorySeparator + nme + DirectorySeparator + 'package.json'
  else
    dfn := '';

  if not dfn.fileExists or dfn.isEmpty then
  begin
    itf.message('error, the DUB description cannot be located or it has not the JSON format',
      nil, amcMisc, amkErr);
    exit;
  end;
  pth := dfn.extractFileDir;

  // build
  dub := TProcess.Create(nil);
  try
    dub.Executable:= 'dub';
    dub.ShowWindow:= swoHIDE;
    dub.Options:= [poUsePipes, poStderrToOutPut];
    dub.Parameters.Add('build');
    dub.Parameters.Add('--build=release');
    dub.Parameters.Add('--force');
    dub.Parameters.Add('--compiler=' + DubCompilerFilename);
    dub.CurrentDirectory:= pth;
    dub.Execute;
    str := TStringList.Create;
    try
      processOutputToStrings(dub, str);
      while dub.Running do ;
      err := dub.ExitStatus;
      for msg in str do
        itf.message(msg, nil, amcMisc, amkAuto);
    finally
      str.Free;
    end;
  finally
    dub.Free;
  end;
  if err <> 0 then
  begin
    // allow "sourceLibrary"
    EntitiesConnector.beginUpdate;
    prj := TDubProject.create(nil);
    try
      prj.loadFromFile(dfn);
      if prj.json.isNotNil and TJSONObject(prj.json).Find('targetType').isNotNil
        and (TJSONObject(prj.json).Find('targetType').AsString = 'sourceLibrary')
      then
      begin
        if (ovw and not List.items.findCaption(nme, row)) or not ovw then
          row := List.Items.Add;
        if row.Data.isNil then
          row.Data := LibMan.libraries.Add;
        row.Caption:= nme;
        row.SubItems.Clear;
        nme := projectSourcePath(prj as ICommonProject);
        row.SubItems.Add(nme);
        row.SubItems.Add(nme);
        row.SubItems.Add(prj.filename);
        row.SubItems.Add(enableStr[true]);
        row.Selected:=true;
        RowToLibrary(row, true);
        row.MakeVisible(false);
        itf.message('The package to register is a source library.' +
          'It is not pre-compiled but its sources are registered', nil, amcMisc, amkInf);
      end else
        itf.message('error, failed to compile the package to register', nil, amcMisc, amkErr);
    finally
      prj.Free;
      EntitiesConnector.endUpdate;
    end;
    showWidget;
    exit;
  end;

  // project used to get the infos
  EntitiesConnector.beginUpdate;
  prj := TDubProject.create(nil);
  try
    prj.loadFromFile(dfn);
    if prj.filename.isNotEmpty then
    begin
      if (ovw and not List.items.findCaption(nme, row)) or not ovw then
        row := List.Items.Add;
      if row.Data.isNil then
        row.Data := LibMan.libraries.Add;
      row.Caption := nme;
      row.SubItems.Clear;
      if prj.binaryKind = staticlib then
        row.SubItems.Add(prj.outputFilename)
      else
      begin
        if YesOrNoAddProjSourceFolder() = mrYes then
          row.SubItems.add(projectSourcePath(prj))
        else
          row.SubItems.Add('');
      end;
      row.SubItems.Add(projectSourcePath(prj as ICommonProject));
      row.SubItems.Add(prj.filename);
      row.SubItems.Add(enableStr[true]);
      row.Selected:=true;
      RowToLibrary(row, true);
      row.MakeVisible(false);
      showWidget;
    end else
      itf.message('warning, the package json description can not be found or the target is not a static library',
        nil, amcMisc, amkWarn);
  finally
    prj.Free;
    EntitiesConnector.endUpdate;
  end;
end;

procedure TLibManEditorWidget.btnEditAliasClick(Sender: TObject);
var
  al: string;
  i: integer;
begin
  if List.Selected.isNil then
    exit;

  al := List.Selected.Caption;
  if inputQuery('library alias', '', al) then
  begin
    for i := 0 to LibMan.librariesCount-1 do
      if (LibMan.libraryByIndex[i].libAlias = al) and
        (LibMan.libraryByIndex[i] <> itemForRow(List.Selected)) then
    begin
      dlgOkError('This alias is already used by another library, the renaming is canceled');
      exit;
    end;
    List.Selected.Caption := al;
    LibMan.updateItemsByAlias;
    RowToLibrary(List.Selected);
  end;

end;

procedure TLibManEditorWidget.btnEnabledClick(Sender: TObject);
begin
  if List.Selected.isNil then
    exit;

  if List.Selected.SubItems[3] = 'true' then
    List.Selected.SubItems[3] := 'false'
  else
    List.Selected.SubItems[3] := 'true';
  RowToLibrary(List.Selected);
  updateButtonsState;
end;

procedure TLibManEditorWidget.btnOpenProjClick(Sender: TObject);
var
  fname: string;
  fmt: TProjectFileFormat;
begin
  if List.Selected.isNil then
    exit;
  fname := List.Selected.SubItems[2];
  if not fname.fileExists then
    exit;

  fmt := projectFormat(fname);
  if fmt in [pffDexed, pffDub] then
  begin
    if assigned(fFreeProj) then
    begin
      if fFreeProj.modified and (dlgFileChangeClose(fFreeProj.filename, UnsavedProj) = mrCancel) then
        exit;
      fFreeProj.getProject.Free;
    end;
    if fmt = pffDexed then
      TNativeProject.create(nil)
    else
      TDubProject.create(nil);
    fProj.loadFromFile(fname);
    fProj.activate;
  end
  else dlgOkInfo('the project file for this library seems to be invalid');
end;

procedure TLibManEditorWidget.btnRegClick(Sender: TObject);
var
  str: TStringList;
  fname: string;
  root: string;
  lalias: string;
  row: TListItem;
  itf: IMessagesDisplay;
begin
  if fProj = nil then
    exit;

  fname := fProj.outputFilename;
  lalias := ExtractFileNameOnly(fname);
  if isAliasRegistered(lalias) then
  begin
    dlgOkInfo(format('a library item with the alias "%s" already exists, delete it before trying again.',
      [lalias]));
    exit;
  end;

  itf := getMessageDisplay;

  str := TStringList.Create;
  try
    root := projectSourcePath(fProj);
    if root.isEmpty then
    begin
      dlgOkInfo('the static library can not be registered because its source files have no common folder');
      exit;
    end;

    row := List.Items.Add;
    row.Data := LibMan.libraries.Add;
    row.Caption := lalias;
    if (fname.extractFileExt <> libExt) then
    begin
      if (fname + libExt).fileExists then
      begin
        row.SubItems.add(fname + libExt);
        if not row.SubItems[0].fileExists then
          itf.message('warning, the library file does not exist, maybe the project not been already compiled ?',
            nil, amcMisc, amkWarn);
      end
      else
      begin
        if YesOrNoAddProjSourceFolder() = mrYes then
          row.SubItems.add(projectSourcePath(fProj))
        else
          row.SubItems.add('');
      end;
    end
    else
      row.SubItems.add(fname);
    row.SubItems.add(root);
    row.SubItems.add(fProj.filename);
    row.SubItems.add(enableStr[true]);
    row.Selected:= true;
    row.MakeVisible(false);
    SetFocus;
    RowToLibrary(row, true);
  finally
    str.free;
  end;
end;

procedure TLibManEditorWidget.btnRemLibClick(Sender: TObject);
begin
  if List.Selected.isNil then
    exit;

  LibMan.libraries.Delete(List.Selected.Index);
  List.Items.Delete(List.Selected.Index);
  updateButtonsState;
end;

procedure TLibManEditorWidget.btnSelProjClick(Sender: TObject);
var
  ini: string;
begin
  if List.Selected.isNil then
    exit;

  ini := List.Selected.SubItems[2];
  with TOpenDialog.Create(nil) do
  try
    Title := 'Select the project that compiles the library';
    FileName := ini;
    if Execute then
      List.Selected.SubItems[2] := FileName.normalizePath;
  finally
    free;
  end;
  RowToLibrary(List.Selected);
end;

procedure TLibManEditorWidget.btnSelFileClick(Sender: TObject);
var
  ini: string = '';
begin
  if List.Selected.isNil then
    exit;

  ini := List.Selected.SubItems[0];
  with TOpenDialog.Create(nil) do
  try
    Title := 'Select the static library file';
    filename := ini;
    if Execute then
    begin
      filename := filename.normalizePath;
      if not filename.fileExists then
        List.Selected.SubItems[0] := filename.extractFilePath
      else
      begin
        List.Selected.SubItems[0] := filename;
        if (List.Selected.Caption.isEmpty) or (List.Selected.Caption = notav) then
          List.Selected.Caption := ChangeFileExt(filename.extractFileName, '');
      end;
    end;
  finally
    Free;
  end;
  RowToLibrary(List.Selected);
end;

procedure TLibManEditorWidget.btnSelfoldOfFilesClick(Sender: TObject);
var
  dir, outdir: string;
begin
  if List.Selected.isNil then
    exit;

  dir := List.Selected.SubItems[0];
  if selectDirectory('folder of static libraries', dir, outdir, True, 0) then
    List.Selected.SubItems[0] := outdir;
  RowToLibrary(List.Selected);
end;

procedure TLibManEditorWidget.btnSelRootClick(Sender: TObject);
var
  dir: string;
begin
  if List.Selected.isNil then
    exit;

  dir := List.Selected.SubItems[1];
  with TSelectDirectoryDialog.Create(nil) do
  try
    InitialDir:= dir;
    Title := 'Select the root of the sources';
    Options := options + [ofNoDereferenceLinks, ofForceShowHidden];
    if execute then
      List.Selected.SubItems[1] := FileName;
  finally
    free;
  end;
  RowToLibrary(List.Selected);
end;

procedure TLibManEditorWidget.btnMoveUpClick(Sender: TObject);
var
  i: integer;
begin
  if list.Selected.isNil or (list.Selected.Index = 0) then
    exit;

  i := list.Selected.Index;
  list.Items.Exchange(i, i - 1);
  LibMan.libraries.Exchange(i, i - 1);
end;

procedure TLibManEditorWidget.btnMoveDownClick(Sender: TObject);
var
  i: integer;
begin
  if list.Selected.isNil or (list.Selected.Index = list.Items.Count - 1) then
    exit;

  i := list.Selected.Index;
  list.Items.Exchange(i, i + 1);
  LibMan.libraries.Exchange(i, i + 1);
end;

procedure TLibManEditorWidget.DoShow;
begin
  inherited;
  dataToGrid;
end;

procedure TLibManEditorWidget.dataToGrid;
var
  itm: TLibraryItem;
  row: TListItem;
  i: Integer;
begin
  if LibMan.isNil then
    exit;

  List.BeginUpdate;
  List.Clear;
  for i := 0 to LibMan.libraries.Count - 1 do
  begin
    itm := TLibraryItem(LibMan.libraries.Items[i]);
    row := List.Items.Add;
    row.Data:= itm;
    row.Caption := itm.libAlias;
    row.SubItems.Add(itm.libFile);
    row.SubItems.Add(itm.libSourcePath);
    row.SubItems.Add(itm.libProject);
    row.SubItems.Add(enableStr[itm.enabled]);
  end;
  List.EndUpdate;
end;

procedure TLibManEditorWidget.RowToLibrary(row: TListItem; added: boolean = false);
var
  itm: TLibraryItem;
begin
  itm := itemForRow(row);
  if itm.isNil then
    exit;

  itm.libAlias      := row.Caption;
  itm.libFile       := row.SubItems[0];
  itm.libSourcePath := row.SubItems[1];
  itm.libProject    := row.SubItems[2];
  itm.enabled       := row.SubItems[3] = enableStr[true];
  itm.updateModulesInfo;

  LibMan.updateDCD;
  if added then
    LibMan.updateCrossDependencies
  else
    Libman.updateAfterAddition(itm);
end;

end.
