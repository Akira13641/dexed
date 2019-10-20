unit u_libman;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, u_writableComponent, LazFileUtils,
  LGHelpers, LGVector, u_common,
  u_dcd, u_projutils, u_interfaces, u_dlang, u_dastworx,
  u_compilers;

type

  // TODO-clibman: improve import analysis, update (currently always full).

  (**
   * Information for a module in a library manager entry
   *)
  TModuleInfo = class(TCollectionItem)
  strict private
    fName: string;
    fImports: TStringList;
    procedure setImports(value: TStringList);
  published
    // the.module.name
    property name: string read fName write fname;
    // all the imports in this module
    property imports: TStringList read fImports write setImports;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;


  (**
   * Represents a D static library.
   *)
  TLibraryItem = class(TCollectionItem)
  type
    TModulesByName = specialize TStringHashMap<TModuleInfo>;
  strict private
    fAlias: string;
    fLibSourcePath: string;
    fLibFile: string;
    fLibProject: string;
    fEnabled: boolean;
    fDependencies: TStringList;
    fClients: TStringList;
    fModules: TCollection;
    fModulesByName: TModulesByName;
    fHasValidSourcePath: boolean;
    fHasValidLibFile: boolean;
    fHasValidLibProject: boolean;
    procedure setLibProject(const value: string);
    procedure setLibFile(const value: string);
    procedure setLibSourcePath(const value: string);
    function getModule(value: integer): TModuleInfo;
    function getModule(const value: string): TModuleInfo;
    function moduleCount: integer;
  published
    property libAlias: string read fAlias write fAlias;
    property libSourcePath: string read fLibSourcePath write setLibSourcePath;
    property libFile: string read fLibFile write setLibFile;
    property libProject: string read fLibProject write setLibProject;
    property enabled: boolean read fEnabled write fEnabled default true;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure updateModulesInfo;
    function addModuleInfo: TModuleInfo;
    function hasModule(const value: string): boolean;
    property hasValidLibFile: boolean read fHasValidLibFile;
    property hasValidLibProject: boolean read fHasValidLibProject;
    property hasValidLibSourcePath: boolean read fHasValidSourcePath;
    property dependencies: TStringList read fDependencies;
    property clients: TStringList read fClients;
    property modules: TCollection read fModules;
    property moduleByIndex[value: integer]: TModuleInfo read getModule;
  end;

  TLibraryList = specialize TObjectHashSet<TLibraryItem>;

  (**
   * Represents all the D libraries handled by Dexed.
   *)
  TLibraryManager = class(TWritableLfmTextComponent, IFPObserver)
  type
    TItemsByAlias = specialize TStringHashMap<TLibraryItem>;
  strict private
    fCollection: TCollection;
    fItemsByAlias: TItemsByAlias;
    fMsgs: IMessagesDisplay;
    function getLibraryByIndex(index: integer): TLibraryItem;
    function getLibraryByAlias(const value: string): TLibraryItem;
    function getLibraryByImport(const value: string): TLibraryItem;
    procedure setCollection(value: TCollection);
    function getLibrariesCount: integer;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data : Pointer);
  published
    property libraries: TCollection read fCollection write setCollection;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    (**
     * The caller gets all the static library files in "list" if "aliases" is nil
     * otherwise the static library files selected by "aliases".
     *)
    procedure getLibFiles(aliases, list: TStrings);
    (**
     * The caller gets all the paths were are located the library sources in "list"
     * if "aliases" is nil otherwise the paths where are located the sources of the
     * libraries selected by "aliases".
     *)
    procedure getLibSourcePath(aliases, list: TStrings);
    (**
     * The caller gets static libraries files in "libs" and source paths
     * in "paths", as required by the specified "source" code.
     *)
    procedure getLibsForSource(source, libs, paths: TStrings);
    (**
     * The caller gets the "-I" (import) command line arguments in opts
     *  for the imports (as identifier chains) in "imports".
     *)
    procedure getLibFilesForImports(imports: TStrings; opts: TStrings);
    //
    procedure updateDCD;
    // find the aliases of the libraries used by the libraries.
    procedure updateCrossDependencies;
    procedure updateAfterAddition(lib: TLibraryItem);
    procedure updateItemsByAlias;
    property librariesCount: integer read getLibrariesCount;
    property libraryByIndex[value: integer]: TLibraryItem read getLibraryByIndex;
    property libraryByAlias[const value: string]: TLibraryItem read getLibraryByAlias;
    property libraryByImport[const value: string]: TLibraryItem read getLibraryByImport;
  end;

const
  libFname = 'libraryManager.txt';

function LibMan: TLibraryManager;

implementation

var
  fLibMan: TLibraryManager = nil;

const
  deactivatedMessage = 'Library item "%s" could be detected but it is marked disabled, ' +
      'the compilation will fail.';

constructor TModuleInfo.Create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fImports := TStringList.Create;
end;

destructor TModuleInfo.Destroy;
begin
  fImports.free;
  inherited;
end;

procedure TModuleInfo.setImports(value: TStringList);
begin
  fImports.Assign(value);
end;

constructor TLibraryItem.Create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fModules := TCollection.Create(TModuleInfo);
  fModulesByName := TModulesByName.create;
  fDependencies := TStringList.Create;
  fClients := TStringList.Create;
  fEnabled:=true;
end;

destructor TLibraryItem.Destroy;
begin
  fModulesByName.Free;
  fDependencies.Free;
  fModules.Free;
  fClients.Free;
  inherited;
end;

function TLibraryItem.moduleCount: integer;
begin
  exit(fModules.Count);
end;

function TLibraryItem.getModule(value: integer): TModuleInfo;
begin
  exit(TModuleInfo(fModules.Items[value]));
end;

function TLibraryItem.getModule(const value: string): TModuleInfo;
begin
  if not fModulesByName.TryGetValue(value, result) then
    result := nil;
end;

function TLibraryItem.addModuleInfo: TModuleInfo;
begin
  exit(TModuleInfo(fModules.Add));
end;

function TLibraryItem.hasModule(const value: string): boolean;
begin
  exit(fModulesByName.Contains(value));
end;

procedure TLibraryItem.setLibProject(const value: string);
begin
  if fLibProject = value then
    exit;
  fLibProject:=value;
  fHasValidLibProject:=value.fileExists;
end;

procedure TLibraryItem.setLibFile(const value: string);
begin
  if fLibFile = value then
    exit;
  fLibFile:=value;
  fHasValidLibFile:=value.fileExists or value.dirExists;
end;

procedure TLibraryItem.setLibSourcePath(const value: string);
begin
  if fLibSourcePath = value then
    exit;
  fLibSourcePath:=value;
  fHasValidSourcePath:=value.dirExists;
end;

procedure TLibraryItem.updateModulesInfo;
var
  prj: ICommonProject;
  str: TStringList;
  mdi: TModuleInfo = nil;
  fls: string = '';
  fle: string;
  lne: string;
  i: integer;
begin
  fModules.Clear;
  fModulesByName.Free;
  fModulesByName := TModulesByName.create;
  if hasValidLibProject then
  begin
    prj := loadProject(fLibProject, true);
    str := TStringList.Create;
    try
      for i:= 0 to prj.sourcesCount-1 do
      begin
        fle := prj.sourceAbsolute(i);
        if not hasDlangSyntax(fle.extractFileExt) then
          continue;
        fls += fle;
        if i <> prj.sourcesCount-1 then
          fls += PathSeparator;
      end;
      getModulesImports(fls, str);
      for i := 0 to str.Count-1 do
      begin
        lne := str[i];
        if lne[1] = '"' then
        begin
          lne := lne[2..lne.length-1];
          mdi := addModuleInfo;
          mdi.name:= lne;
          fModulesByName.Add(lne, mdi);
        end else
        begin
          if not lne.isEmpty and mdi.isNotNil then
            mdi.imports.Add(lne);
        end;
      end;
    finally
      str.Free;
      prj.getProject.Free;
    end;
  end else if hasValidLibSourcePath then
  begin
    str := TStringList.Create;
    try
      listFiles(str, fLibSourcePath, true);
      for i:= 0 to str.Count-1 do
      begin
        fle := str[i];
        if not hasDlangSyntax(fle.extractFileExt) then
          continue;
        fls += fle;
        if i <> str.Count-1 then
          fls += PathSeparator;
      end;
      str.Clear;
      getModulesImports(fls, str);
      for i := 0 to str.Count-1 do
      begin
        lne := str[i];
        if lne[1] = '"' then
        begin
          lne := lne[2..lne.length-1];
          mdi := addModuleInfo;
          mdi.name:= lne;
          fModulesByName.Add(lne, mdi);
        end else
        begin
          if not lne.isEmpty and mdi.isNotNil then
            mdi.imports.Add(lne);
        end;
      end;
    finally
      str.Free;
    end;
  end;
end;

constructor TLibraryManager.create(aOwner: TComponent);
var
  nme: string;
  lb2: TLibraryItem;
  lb1: TLibraryItem;
  i: integer;
begin
  inherited;
  fItemsByAlias := TItemsByAlias.create;
  fCollection := TCollection.Create(TLibraryItem);
  fCollection.FPOAttachObserver(self);
  nme := getDocPath + libFname;
  if nme.fileExists then
    loadFromFile(nme);
  for i := fCollection.Count-1 downto 0 do
  begin
    lb2 := libraryByIndex[i];
    lb1 := libraryByAlias[lb2.libAlias];
    if lb1.isNotNil and (lb1.Index < lb2.Index) then
    begin
      fCollection.Delete(i);
      continue;
    end;
    lb2.updateModulesInfo;
  end;
  updateItemsByAlias;
  updateDCD;
  updateCrossDependencies;
end;

destructor TLibraryManager.destroy;
begin
  ForceDirectoriesUTF8(getDocPath);
  LibMan.saveToFile(getDocPath + libFname);
  fItemsByAlias.Free;
  fCollection.Free;
  inherited;
end;

procedure TLibraryManager.updateItemsByAlias;
var
  i: integer;
begin
  fItemsByAlias.Free;
  fItemsByAlias := TItemsByAlias.create;
  for i:= 0 to fCollection.Count-1 do
    fItemsByAlias.Add(libraryByIndex[i].libAlias, libraryByIndex[i]);
end;

procedure TLibraryManager.setCollection(value: TCollection);
begin
  fCollection.assign(value);
end;

procedure TLibraryManager.FPOObservedChanged(ASender: TObject; Operation:
  TFPObservedOperation; Data: Pointer);
var
  i,j: integer;
  obj: TObject;
  lib: TLibraryItem;
  cli: TLibraryItem;
  dep: TLibraryItem;
begin
  if data.isNil then
    exit;
  obj := TObject(data);
  if not (obj is TLibraryItem) then
    exit;
  lib := TLibraryItem(data);
  case operation of
    ooDeleteItem: if fItemsByAlias.Contains(lib.libAlias) then
    begin
      for i:= 0 to lib.dependencies.Count-1 do
      begin
        dep := libraryByAlias[lib.dependencies[i]];
        if assigned(dep) then
        begin
          j := dep.clients.IndexOf(lib.libAlias);
          if j <> -1 then
            dep.clients.Delete(j);
        end;
      end;
      for i:= 0 to lib.clients.Count-1 do
      begin
        cli := libraryByAlias[lib.clients[i]];
        if assigned(cli) then
        begin
          j := cli.dependencies.IndexOf(lib.libAlias);
          if j <> -1 then
            cli.dependencies.Delete(j);
        end;
      end;
      DCDWrapper.remImportFolder(lib.libSourcePath);
      fItemsByAlias.Remove(lib.libAlias);
    end;
  end;
end;

procedure TLibraryManager.updateAfterAddition(lib: TLibraryItem);
begin
  fItemsByAlias.Add(lib.libAlias, lib);
  updateCrossDependencies;
end;

function TLibraryManager.getLibraryByIndex(index: integer): TLibraryItem;
begin
  exit(TLibraryItem(fCollection.Items[index]));
end;

function TLibraryManager.getLibraryByAlias(const value: string): TLibraryItem;
begin
  if not fItemsByAlias.TryGetValue(value, result) then
    result := nil;
end;

function TLibraryManager.getLibraryByImport(const value: string): TLibraryItem;
var
  i: integer;
  s: TLibraryItem;
begin
  result := nil;
  for i := 0 to librariesCount-1 do
  begin
    s := libraryByIndex[i];
    if s.hasModule(value) then
      exit(s);
  end;
end;

function TLibraryManager.getLibrariesCount: integer;
begin
  exit(fCollection.Count);
end;

procedure TLibraryManager.updateDCD;
var
  itm: TLibraryItem;
  add: TStringList;
  rem: TStringList;
  i: Integer;
begin
  if not DcdWrapper.available then
    exit;

  add := TStringList.Create;
  rem := TStringList.Create;
  try
    for i := 0 to fCollection.Count-1 do
    begin
      itm := TLibraryItem(fCollection.Items[i]);
      if itm.enabled then
      begin
        if itm.hasValidLibSourcePath then
          add.Add(itm.libSourcePath)
        else
          rem.Add(itm.libSourcePath);
      end
      else
        rem.Add(itm.libSourcePath);
    end;
    if add.Count > 0 then
      DcdWrapper.addImportFolders(add);
    if rem.Count > 0 then
      DCDWrapper.remImportFolders(rem);
  finally
    add.Free;
    rem.Free;
  end;
end;

procedure TLibraryManager.getLibFiles(aliases, list: TStrings);
  procedure add(lib: TLibraryItem);
  var
    j: integer;
    e: string;
    f: string;
    x: string;
    dir: string;
    lst: TstringList;
  begin
    // as a trick a folder can be set as source, this allows to pass
    // multiple sources | static libs in an automated way.
    if lib.libFile.dirExists then
    begin
      lst := TStringList.Create;
      try
        dir := lib.libFile;
        if lib.libFile[dir.length] = DirectorySeparator then
          dir := dir[1..dir.length-1];
        listFiles(lst, dir, true);
        for j:= 0 to lst.Count-1 do
        begin
          f := lst[j];
          x := f.extractFileName;
          // The libman allows registration of projects thate not libs and using
          // a folder of sources instead of the *.a / *.lib file.
          // Following DUB conventions here are filtered out named supposed to
          // contain the __Dmain() func, which is not wanted for runnables mods.
          if (x = 'app.d') or (x = 'main.d') then
            continue;
          e := f.extractFileExt;
          if ((e = libExt) or (e = '.d')) and (list.IndexOf(f) = -1) then
            list.Add(f);
        end;
      finally
        lst.Free;
      end;
    end
    else if lib.hasValidLibFile then
      list.Add(lib.libFile);
  end;
var
  lib: TLibraryItem;
  i: Integer;
begin
  // no selector = all libs
  if aliases.isNil then
  begin
    for i:= 0 to librariesCount-1 do
    begin
      lib := libraryByIndex[i];
      if lib.enabled then
        add(lib);
    end;
  end
  // else get selected libs
  else for i := 0 to aliases.Count-1 do
  begin
    lib := libraryByAlias[aliases[i]];
    if lib.isNotNil and lib.enabled then
      add(lib);
  end;
end;

procedure TLibraryManager.getLibSourcePath(aliases, list: TStrings);
var
  lib: TLibraryItem;
  i: Integer;
begin
  if fMsgs = nil then
    fMsgs := getMessageDisplay;
  // no selector = all libs
  if aliases.isNil then
  begin
    for i:= 0 to librariesCount-1 do
    begin
      lib := libraryByIndex[i];
      if lib.hasValidLibSourcePath then
      begin
        if not lib.enabled then
          fMsgs.message(format(deactivatedMessage, [lib.libAlias]),
            nil, amcAutoCompile, amkWarn)
        else
          list.Add('-I' + lib.libSourcePath);
      end;
    end;
  end
  // else get selected libs
  else
  begin
    for i := 0 to aliases.Count-1 do
    begin
      lib := libraryByAlias[aliases[i]];
      if lib.isNotNil and lib.enabled and lib.hasValidLibSourcePath then
        list.Add('-I' + lib.libSourcePath);
    end;
  end;
end;

procedure TLibraryManager.getLibsForSource(source, libs, paths: TStrings);
var
  imp: TStringList;
  i,j: integer;
  itm: TLibraryItem;
  dep: TLibraryItem;
  sel: TLibraryList;
begin
  if fMsgs = nil then
    fMsgs := getMessageDisplay;
  imp := TStringList.Create;
  sel := TLibraryList.create;
  try
    getModuleImports(source, imp);
    for i:= 1 to imp.Count-1 do
    begin
      // get library for import I
      itm := libraryByImport[imp[i]];
      if itm.isNotNil then
      begin
        if sel.contains(itm) then
          continue;
        sel.Add(itm);
        // get libraries for import I dependencies
        for j:= itm.dependencies.Count-1 downto 0 do
        begin
          dep := libraryByAlias[itm.dependencies[j]];
          if dep.isNotNil then
            sel.Add(dep)
          else
            //auto update: item removed, detect on usage that it has disapeared
            itm.dependencies.Delete(j);
        end;
      end;
    end;
    // add the library files and the import paths for the selection
    if not (sel.Count = 0) then with sel.GetEnumerator do
    begin
      while true do
      begin
        itm := GetCurrent;
        if itm.isNil then
          break;
        if itm.hasValidLibFile then
        begin
          if not itm.enabled then
            fMsgs.message(format(deactivatedMessage, [itm.libAlias]),
              nil, amcAutoCompile, amkWarn)
          else
          begin
            libs.Add(itm.libFile);
            paths.Add('-I' + itm.libSourcePath);
          end;
        end;
        if not MoveNext then
          break;
      end;
      free;
    end;
  finally
    sel.Free;
    imp.Free;
  end;
end;

procedure TLibraryManager.getLibFilesForImports(imports: TStrings; opts: TStrings);
var
  i: integer;
  b: TLibraryItem;
  s: string;
begin
  for i := 0 to imports.Count-1 do
  begin
    b := libraryByImport[imports[i]];
    if b.isNotNil and b.enabled then
    begin
      s := b.libFile;
      if (opts.IndexOf(s) = -1) and (not s.isEmpty) then
      begin
        opts.Add(s);
        opts.Add('-I' + b.libSourcePath);
      end;
    end;
  end;
end;

procedure TLibraryManager.updateCrossDependencies;
var
  i, j, m: integer;
  lib: TLibraryItem;
  dep: TLibraryItem;
  imp: string;
begin
  updateItemsByAlias;
  for i := 0 to fCollection.Count-1 do
  begin
    lib := libraryByIndex[i];
    lib.clients.Clear;
  end;
  for i := 0 to fCollection.Count-1 do
  begin
    lib := libraryByIndex[i];
    lib.dependencies.Clear;
    for j := 0 to lib.modules.Count-1 do
      for m := 0 to lib.moduleByIndex[j].imports.Count-1 do
    begin
      imp := lib.moduleByIndex[j].imports[m];
      // module of the same package so...
      if lib.hasModule(imp) then
        continue;
      dep := libraryByImport[imp];
      // std / core / etc ...
      if dep.isNil then
        continue;
      // ... this should not happen
      if dep = lib then
        continue;
      // add deps
      if lib.dependencies.IndexOf(dep.libAlias) > -1 then
        continue;
      lib.dependencies.Add(dep.libAlias);
      dep.clients.Add(lib.libAlias);
    end;
  end;
  for i := 0 to fCollection.Count-1 do
  begin
    lib := libraryByIndex[i];
    deleteDups(lib.clients);
    deleteDups(lib.dependencies);
  end;
end;

function LibMan: TLibraryManager;
begin
  if fLibMan.isNil then
    fLibMan := TLibraryManager.create(nil);
  result := fLibMan;
end;

finalization
  fLibMan.Free;
end.
