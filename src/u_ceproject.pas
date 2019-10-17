unit u_ceproject;

{$I u_defines.inc}

interface

uses
  {$IFDEF DEBUG}
  LclProc,
  {$ENDIF}
  {$IFNDEF CEBUILD}
  u_dialogs,
  {$ENDIF}
  Classes, SysUtils, process, strUtils, RegExpr,
  u_common, u_writableComponent, u_dmdwrap, u_observer, u_interfaces,
  u_processes, LazFileUtils, u_dastworx;

type

(*******************************************************************************
 * Represents a D project.
 *
 * It includes all the options defined in u_dmdwrap, organized in
 * a collection to allow multiples configurations.
 *
 * Basically it' s designed to provide the options for the dmd process.
 *)
  TNativeProject = class(TWritableLfmTextComponent, ICommonProject)
  private
    fInGroup: boolean;
    fCompilProc: TDexedProcess;
    fOnChange: TNotifyEvent;
    fModified: boolean;
    fPreCompilePath: string;
    fRootFolder: string;
    fBasePath: string;
    fRunnerOldCwd: string;
    fVersion: string;
    fLibAliases: TStringList;
    fAutoDeps: boolean;
    fConfigs: TCollection;
    fSrcs: TStringList;
    fConfIx: Integer;
    fUpdateCount: NativeInt;
    fProjectSubject: TProjectSubject;
    fRunner: TDexedProcess;
    fOutputFilename: string;
    fCanBeRun: boolean;
    fBaseConfig: TCompilerConfiguration;
    fCompiled: boolean;
    fSymStringExpander: ISymStringExpander;
    fMsgs: IMessagesDisplay;
    fAsProjectItf: ICommonProject;
    fVersionFile: string;
    procedure updateOutFilename;
    procedure doChanged(modified: boolean = true);
    procedure getBaseConfig;
    procedure setLibAliases(value: TStringList);
    procedure subMemberChanged(sender : TObject);
    procedure setOptsColl(value: TCollection);
    procedure setRoot(const value: string);
    procedure setSrcs(value: TStringList);
    procedure setConfIx(value: Integer);
    function getConfig(value: integer): TCompilerConfiguration;
    function getCurrConf: TCompilerConfiguration;
    function runPrePostProcess(processInfo: TCompileProcOptions): Boolean;
    function getVersion(): string;
    // passes pre/post/executed project/ outputs as bubles.
    procedure runProcOutput(sender: TObject);
    // passes compilation message as "to be guessed"
    procedure compProcOutput(proc: TObject);
    procedure compProcTerminated(proc: TObject);
    function getObjectsDirectory: string; inline;
    procedure getUpToDateObjects(str: TStrings);
  protected
    procedure beforeLoad; override;
    procedure afterSave; override;
    procedure afterLoad; override;
    procedure customSaveToFile(const fname: string); override;
    procedure customLoadFromFile(const fname: string); override;
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean); override;
  published
    property RootFolder: string read fRootFolder write setRoot;
    property OptionsCollection: TCollection read fConfigs write setOptsColl;
    property Sources: TStringList read fSrcs write setSrcs; // 'read' should return a copy to avoid abs/rel errors
    property ConfigurationIndex: Integer read fConfIx write setConfIx;
    property LibraryAliases: TStringList read fLibAliases write setLibAliases;
    property AutoSolveDependencies: boolean read fAutoDeps write fAutoDeps default false;
    property version: string read getVersion write fVersion;
    property versionFile: TPathname read fVersionFile write fVersionFile;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure beginUpdate;
    procedure endUpdate(modified: boolean = true);
    procedure reset;
    procedure addDefaults;
    procedure addSource(const fname: string);
    function addConfiguration: TCompilerConfiguration;
    procedure getOpts(opts: TStrings);
    //
    procedure activate;
    procedure inGroup(value: boolean);
    function inGroup: boolean;
    function getFormat: TProjectFormat;
    function getProject: TObject;
    function filename: string;
    function basePath: string;
    function outputFilename: string;
    function binaryKind: TProjectBinaryKind;
    function getCommandLine: string;
    function modified: boolean;
    procedure reload;
    procedure stopCompilation;
    //
    function configurationCount: integer;
    procedure setActiveConfigurationIndex(index: integer);
    function configurationName(index: integer): string;
    function getActiveConfigurationIndex: integer;
    //
    function sourcesCount: integer;
    function sourceRelative(index: integer): string;
    function sourceAbsolute(index: integer): string;
    function isSource(const fname: string): boolean;
    function importsPathCount: integer;
    function importPath(index: integer): string;
    //
    procedure run(const runArgs: string = '');
    function compiled: Boolean;
    procedure compile;
    procedure test;
    function targetUpToDate: boolean;
    procedure checkMissingFiles;
    //
    property configuration[ix: integer]: TCompilerConfiguration read getConfig;
    property currentConfiguration: TCompilerConfiguration read getCurrConf;
    property onChange: TNotifyEvent read fOnChange write fOnChange;
    property canBeRun: Boolean read fCanBeRun;
  end;

  // native project have no ext constraint, this function tells if filename is project
  function isValidNativeProject(const filename: string): boolean;

  function getCEProjectCompiler: DCompiler;
  procedure setCEProjectCompiler(value: DCompiler);

implementation

uses
  controls, dialogs, u_libman, u_dcd;

var
  CEProjectCompilerFilename: string = 'dmd';
  CEProjectCompiler: DCompiler;

constructor TNativeProject.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  fAsProjectItf := self as ICommonProject;
  fSymStringExpander := getSymStringExpander;
  fMsgs:= getMessageDisplay;
  //
  fRunnerOldCwd := GetCurrentDirUTF8;
  fProjectSubject := TProjectSubject.create;
  //
  fLibAliases := TStringList.Create;
  fSrcs := TStringList.Create;
  fSrcs.OnChange := @subMemberChanged;
  fConfigs := TCollection.create(TCompilerConfiguration);
  //
  reset;
  addDefaults;
  subjProjNew(fProjectSubject, self);
  subjProjChanged(fProjectSubject, self);
  //
  {$IFNDEF WINDOWS}
  fBasePath := '/';
  {$ENDIF}
  //
  fModified := false;
end;

destructor TNativeProject.destroy;
begin
  killProcess(fCompilProc);
  subjProjClosing(fProjectSubject, self);
  fProjectSubject.Free;
  //
  fOnChange := nil;
  fLibAliases.Free;
  fSrcs.free;
  fConfigs.free;
  killProcess(fRunner);
  inherited;
end;

function TNativeProject.inGroup: boolean;
begin
  exit(fInGroup);
end;

procedure TNativeProject.inGroup(value: boolean);
begin
  fInGroup:=value;
end;

procedure TNativeProject.activate;
begin
  subjProjFocused(fProjectSubject, fAsProjectItf);
end;

function TNativeProject.getFormat: TProjectFormat;
begin
  exit(pfDEXED);
end;

function TNativeProject.getProject: TObject;
begin
  exit(Self);
end;

function TNativeProject.addConfiguration: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fConfigs.Add);
  result.onChanged := @subMemberChanged;
end;

procedure TNativeProject.setOptsColl(value: TCollection);
var
  i: nativeInt;
begin
  fConfigs.Assign(value);
  for i:= 0 to fConfigs.Count-1 do
    Configuration[i].onChanged := @subMemberChanged;
end;

procedure TNativeProject.addSource(const fname: string);
var
  relSrc, absSrc: string;
  expand: boolean;
begin
  if not isDlangCompilable(fname.extractFileExt) then
    exit;
  expand := fBasePath.dirExists;
  for relSrc in fSrcs do
  begin
    if not expand then absSrc := relSrc
    else absSrc := expandFilenameEx(fBasePath, relsrc);
    if SameFileName(fname, absSrc) then exit;
  end;
  relSrc := ExtractRelativePath(fBasePath, fname);
  fSrcs.Add(relSrc);
end;

procedure TNativeProject.setRoot(const value: string);
begin
  if fRootFolder = value then exit;
  beginUpdate;
  fRootFolder := value;
  endUpdate;
end;

procedure TNativeProject.reload;
begin
  if fFilename.fileExists then
    loadFromFile(fFilename);
end;

procedure TNativeProject.customLoadFromFile(const fname: string);
var
  f: string;
begin
  f := fname;
  if not FilenameIsAbsolute(f) then
    f := ExpandFileName(f);
  fbasePath := f.extractFilePath;
  inherited customLoadFromFile(f);
  if hasLoaded and (fname.extractFileExt <> '.dprj') then
  begin
    dlgOkInfo('project file extension automatically updated to "dprj"');
    f := ChangeFileExt(fname, '.dprj');
    RenameFile(fname, f);
  end;
end;

procedure TNativeProject.customSaveToFile(const fname: string);
var
  oldAbs, newRel, oldBase: string;
  f: string;
  i: NativeInt;
begin
  beginUpdate;
  f := fname;
  if f <> fFilename then
    inGroup(false);
  oldBase := fBasePath;
  fBasePath := f.extractFilePath;
  //
  for i:= 0 to fSrcs.Count-1 do
  begin
    oldAbs := expandFilenameEx(oldBase,fSrcs[i]);
    newRel := ExtractRelativepath(fBasePath, oldAbs);
    fSrcs[i] := newRel;
  end;
  endUpdate;
  f := ChangeFileExt(f, '.dprj');
  inherited customSaveToFile(f);
end;

procedure TNativeProject.setLibAliases(value: TStringList);
begin
  beginUpdate;
  fLibAliases.Assign(value);
  endUpdate;
end;

procedure TNativeProject.setSrcs(value: TStringList);
begin
  beginUpdate;
  fSrcs.Assign(value);
  patchPlateformPaths(fSrcs);
  endUpdate;
end;

procedure TNativeProject.setConfIx(value: Integer);
begin
  beginUpdate;
  if value < 0 then
    value := 0;
  if value > fConfigs.Count-1 then
    value := fConfigs.Count-1;
  fConfIx := value;
  endUpdate(false);
end;

procedure TNativeProject.getBaseConfig;
var
  i: integer;
begin
  fBaseConfig := nil;
  for i:= 0 to fConfigs.Count-1 do
    if configuration[i].isBaseConfiguration then
      fBaseConfig := configuration[i];
  // silently disables any other config. set as base without calling doChange
  Inc(fUpdateCount);
  for i := 0 to fConfigs.Count-1 do
    if configuration[i].isBaseConfiguration then
      if configuration[i] <> fBaseConfig then
        configuration[i].isBaseConfiguration := false;
  Dec(fUpdateCount);
end;

procedure TNativeProject.subMemberChanged(sender : TObject);
begin
  beginUpdate;
  fModified := true;
  endUpdate;
end;

procedure TNativeProject.beginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TNativeProject.endUpdate(modified: boolean = true);
begin
  Dec(fUpdateCount);
  if fUpdateCount > 0 then
    exit;
  fUpdateCount := 0;
  doChanged(modified);
end;

procedure TNativeProject.doChanged(modified: boolean = true);
begin
  fModified := modified;
  updateOutFilename;
  getBaseConfig;
  subjProjChanged(fProjectSubject, self);
  if assigned(fOnChange) then
    fOnChange(Self);
end;

function TNativeProject.getConfig(value: integer): TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fConfigs.Items[value]);
  result.onChanged := @subMemberChanged;
end;

function TNativeProject.getCurrConf: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fConfigs.Items[fConfIx]);
end;

function TNativeProject.getVersion(): string;
var
  p: string = '';
begin
  if fVersionFile.isNotEmpty then
  begin
    if FilenameIsAbsolute(fVersionFile) then
      p := fVersionFile
    else
      p := expandFilenameEx(fBasePath, fVersionFile);
  end;
  if p.isNotEmpty and p.fileExists then
    with TStringList.Create do
  try
    LoadFromFile(p);
    fVersion := trim(strictText);
  finally
    free;
  end;
  result := fVersion;
end;

procedure TNativeProject.addDefaults;
begin
  with TCompilerConfiguration(fConfigs.Add) do
  begin
    Name := 'debug';
    debugingOptions.debugConditions := true;
    debugingOptions.generateInfos := true;
    outputOptions.boundsCheck := onAlways;
  end;
  with TCompilerConfiguration(fConfigs.Add) do
  begin
    Name := 'unittest';
    outputOptions.unittest := true;
    outputOptions.boundsCheck := onAlways;
  end;
  with TCompilerConfiguration(fConfigs.Add) do
  begin
    Name := 'release';
    outputOptions.release := true;
    outputOptions.inlining := true;
    outputOptions.boundsCheck := offAlways;
    outputOptions.optimizations := true;
  end;
end;

procedure TNativeProject.reset;
var
  defConf: TCompilerConfiguration;
begin
  beginUpdate;
  fConfIx := 0;
  fConfigs.Clear;
  defConf := addConfiguration;
  defConf.name := 'default';
  fSrcs.Clear;
  fFilename := '';
  endUpdate;
  fModified := false;
end;

procedure TNativeProject.getOpts(opts: TStrings);
var
  i: Integer;
  exc: TStringList;
  als: TStringList;
  cfg: TCompilerConfiguration;
  str: string;
  rel: string;
  lst: TStringList;
begin
  if fConfIx = -1 then exit;
  exc := TStringList.Create;
  try
    cfg := currentConfiguration;
    // prepares the exclusions
    for i := 0 to cfg.pathsOptions.exclusions.Count-1 do
    begin
      str := fSymStringExpander.expand(cfg.pathsOptions.exclusions[i]);
      exc.Add(str)
    end;
    // sources
    for rel in fSrcs do if rel <> '' then
      opts.Add(expandFilenameEx(fBasePath, rel)); // note: process.inc ln 249. double quotes are added if there's a space.
    // exclusions
    if exc.Count > 0 then with TRegExpr.Create do
    try
      for str in exc do
      begin
        try
          Expression:= globToReg(str);
          Compile;
          for i := opts.Count-1 downto 0 do
            if Exec(opts[i]) then
              opts.Delete(i);
        except
          continue;
        end;
      end;
    finally
      free;
    end;

    // libraries: an asterisk in opts selects all the entries
    als := fLibAliases;
    if (fLibAliases.Count > 0) and (fLibAliases[0] = '*') then
      als := nil;

    {$IFDEF WINDOWS}
    // only link lib file if executable/shared lib
    // OS switch: read more @ http://forum.dlang.org/post/ooekdkwrefposmchekrp@forum.dlang.org
    if (cfg.outputOptions.binaryKind in [executable, sharedlib]) or
      cfg.outputOptions.alwaysLinkStaticLibs then
    {$ENDIF}
    LibMan.getLibFiles(als, opts);
    // but always adds -I<path>
    LibMan.getLibSourcePath(als, opts);

    if fAutoDeps then
    begin
      lst := TStringList.Create;
      try
        str := '';
        for i := 0 to fSrcs.Count-1 do
          str += sourceAbsolute(i) + PathSeparator;
        cfg.pathsOptions.getExtraSources(lst);
        for i := 0 to lst.Count-1 do
          str += lst[i] + PathSeparator;
        lst.Clear;
        getModulesImports(str, lst);
        Libman.getLibFilesForImports(lst, opts);
      finally
        lst.Free;
      end;
    end;

    // config
    if cfg.isOverriddenConfiguration then
    begin
      cfg.getOpts(opts, fBaseConfig);
      cfg.otherOptions.getCompilerSpecificOpts(opts, fBaseConfig.otherOptions,
        CEProjectCompiler);
    end
    else
    begin
      cfg.getOpts(opts);
      cfg.otherOptions.getCompilerSpecificOpts(opts, nil, CEProjectCompiler);
    end;
  finally
    exc.Free;
  end;
end;

function TNativeProject.isSource(const fname: string): boolean;
var
  i: Integer;
begin
  for i := 0 to fSrcs.Count-1 do
    if sourceAbsolute(i) = fname then
      exit(true);
  exit(false);
end;

procedure TNativeProject.afterSave;
begin
  fModified := false;
  updateOutFilename;
end;

procedure TNativeProject.beforeLoad;
begin
  beginUpdate;
  Inherited;
end;

procedure TNativeProject.checkMissingFiles;
var
  hasPatched: Boolean = false;
  // either all the source files have moved or only the project file
  procedure checkMissingAllSources;
  var
    allMissing: boolean;
    dirHint: string;
    newdir: string;
    ini: string;
    src: string;
    i: Integer;
  begin
    if fSrcs.Count = 0 then exit;
    allMissing := true;
    for i:= 0 to fSrcs.Count-1 do
      if sourceAbsolute(i).fileExists then
        allMissing := false;
    if not allMissing then exit;
    if dlgYesNo( 'The project source(s) are all missing. ' + LineEnding +
      'This can be encountered if the project file has been moved from its original location.' + LineEnding + LineEnding +
      'Do you wish to select the new root folder ?') <> mrYes then exit;
    // TODO-cimprovement: use commonFolder() when it'll be compat. with the rel. paths.
    // hint for the common dir
    dirHint := fSrcs[i];
    while (dirHint[1] = '.') or (dirHint[1] = DirectorySeparator) do
        dirHint := dirHint[2..dirHint.length];
    ini := fFilename.extractFilePath;
    if not selectDirectory( format('select the folder (that contains "%s")',[dirHint]), ini, newdir) then
      exit;
    for i := 0 to fSrcs.Count-1 do
    begin
      src := fSrcs[i];
      while (src[1] = '.') or (src[1] = DirectorySeparator) do
        src := src[2..src.length];
      if fileExists(expandFilenameEx(fBasePath, newdir + DirectorySeparator + src)) then
        fSrcs[i] := ExtractRelativepath(fBasePath, newdir + DirectorySeparator + src);
      hasPatched := true;
    end;
  end;
  // single sources files are missing
  procedure checkMissingSingleSource;
  var
    oldsrc: string;
    opendlg: TOpenDialog;
    i: Integer;
  begin
    for i:= fSrcs.Count-1 downto 0 do
    begin
      oldsrc := sourceAbsolute(i);
      if oldsrc.fileExists then continue;
      if dlgYesNo(format('a particular project source file ("%s") is missing. '
        + LineEnding + 'This happends if a source file has been moved, renamed ' +
        'or deleted.' + LineEnding + LineEnding +
        'Do you wish to select its new location?', [fSrcs[i]])) <> mrYes then exit;
      //
      opendlg := TOpenDialog.Create(nil);
      try
        opendlg.InitialDir := fFilename.extractFilePath;
        opendlg.FileName := fSrcs[i];
        if opendlg.execute then
        begin
          if oldsrc.extractFileName <> opendlg.filename.extractFileName then
            if dlgYesNo('the filenames are different, replace the old file ?') <> mrYes then
              continue;
            fSrcs[i] := ExtractRelativepath(fBasePath, opendlg.Filename);
            hasPatched := true;
        end else
        begin
          if dlgYesNo('You have choosen not to update the file, ' +
          'do you wish to remove it from the project ?') <> mrYes then
              continue;
          fSrcs.Delete(i);
          hasPatched := true;
        end;
      finally
        opendlg.Free;
      end;
    end;
  end;
begin
  beginUpdate;
  checkMissingAllSources;
  checkMissingSingleSource;
  endUpdate;
  if hasPatched then
  begin
    fModified:= true;
    dlgOkInfo('some source file paths has been patched, some others invalid ' +
    'paths or file may still exist (-of, -od, extraSources, etc)' +
    'but cannot be automatically handled. Note that the modifications have not been saved.');
  end
  else fModified:= false;
end;

procedure TNativeProject.afterLoad;
begin
  //if not fHasLoaded then
  //begin
  //  dlgOkError('"' + shortenPath(fFilename) + '"' + 'does not seem to be a valid CE project');
  //  fFilename:= '';
  //end;
  patchPlateformPaths(fSrcs);
  updateOutFilename;
  endUpdate;
  fModified := false;
end;

procedure TNativeProject.readerPropNoFound(Reader: TReader; Instance: TPersistent;
  var PropName: string; IsPath: Boolean; var Handled, Skip: Boolean);
begin
  // errors are avoided by property deprecation, error here means "not a project".
  Skip := true;
  Handled := false;
end;

procedure TNativeProject.updateOutFilename;
var
  fe: boolean = false;
  ext: string;
begin
  fOutputFilename := currentConfiguration.pathsOptions.outputFilename;
  fe := currentConfiguration.pathsOptions.forceExtension;
  if currentConfiguration.isOverriddenConfiguration and fOutputFilename.isEmpty and
    fBaseConfig.isNotNil then
  begin
    fOutputFilename := fBaseConfig.pathsOptions.outputFilename;
    fe := fBaseConfig.pathsOptions.forceExtension;
  end;
  // field is specified
  if fOutputFilename.isNotEmpty then
  begin
    if (fSymStringExpander <> nil) then
      fOutputFilename := fSymStringExpander.expand(fOutputFilename);
    fOutputFilename := expandFilenameEx(fBasePath, fOutputFilename);
    {$IFDEF WINDOWS}
    // field is specified without ext or with a dot in the name.
    // DMD will add the ext. (e.g: "-ofresourced")
    // https://issues.dlang.org/show_bug.cgi?id=13989
    if fileexists(fOutputFilename + exeExt) then
      if currentConfiguration.outputOptions.binaryKind = executable then
        fOutputFilename := fOutputFilename + exeExt;
    {$ENDIF}
  end
  // try to guess
  else if Sources.Count > 0 then
  begin
    // ideally, main() should be searched for, when project binaryKind is executable
    fOutputFilename := Sources[0].extractFileName;
    fOutputFilename := fOutputFilename.stripFileExt;
    if fileName.fileExists then
      fOutputFilename := fileName.extractFilePath + fOutputFilename
    else
      fOutputFilename := GetTempDir(false) + fOutputFilename;
    fe := true;
  end;
  //
  if fe then
  begin
    ext := fOutputFilename.extractFileExt;
    case currentConfiguration.outputOptions.binaryKind of
      {$IFDEF WINDOWS}
      executable: if ext <> exeExt then fOutputFilename += exeExt;
      {$ENDIF}
      staticlib:  if ext <> libExt then fOutputFilename += libExt;
      sharedlib:  if ext <> dynExt then fOutputFilename += dynExt;
      obj:        if ext <> dynExt then fOutputFilename += objExt;
    end;
  end;
  //
  fCanBeRun := false;
  if currentConfiguration.outputOptions.binaryKind = executable then
    fCanBeRun := fOutputFilename.fileExists;
end;

function TNativeProject.runPrePostProcess(processInfo: TCompileProcOptions): Boolean;
var
  lst: TStringList;
  com: boolean;
  prc: TProcess;
  nme: string;
  i, j: integer;
begin
  for i := 0 to processInfo.simpleCommands.Count-1 do
  begin
    nme := fSymStringExpander.expand(processInfo.simpleCommands[i]);
    if nme.isBlank then
      continue;
    prc := TProcess.Create(nil);
    lst := TStringList.Create;
    try
      CommandToList(nme, lst);
      prc.Executable := lst[0];
      prc.Options:= [poUsePipes, poStderrToOutPut];
      lst.Delete(0);
      prc.Parameters.Assign(lst);
      prc.XTermProgram:=consoleProgram;
      prc.Execute;
      lst.Clear;
      processOutputToStrings(prc, lst);
      while prc.Running do
        sleep(1);
      com := prc.ExitStatus = 0;
      for j := 0 to lst.Count -1 do
        fMsgs.message(lst[j], fAsProjectItf, amcProj, amkAuto);
    finally
      prc.Free;
      lst.Free;
    end;
    if not com then
      exit(false);
  end;
  //
  nme := fSymStringExpander.expand(processInfo.executable);
  if (not exeInSysPath(nme)) and nme.isNotEmpty then
    exit(false)
  else if nme.isEmpty then
    exit(true);
  //
  prc := TProcess.Create(nil);
  try
    processInfo.setProcess(prc);
    prc.Executable := exeFullName(nme);
    j := prc.Parameters.Count-1;
    for i:= 0 to j do
      prc.Parameters.AddText(fSymStringExpander.expand(prc.Parameters[i]));
    for i:= 0 to j do
      prc.Parameters.Delete(0);
    if prc.CurrentDirectory.isNotEmpty then
      prc.CurrentDirectory := fSymStringExpander.expand(prc.CurrentDirectory);
    // else cwd is set to project dir in compile()
    ensureNoPipeIfWait(prc);
    prc.Execute;
    while prc.Running do
      if poUsePipes in prc.Options then
        runProcOutput(prc);
  finally
    result := prc.ExitStatus = 0;
    prc.Free;
  end;
end;

function TNativeProject.compiled: boolean;
begin
  exit(fCompiled);
end;

procedure TNativeProject.stopCompilation;
begin
  if fCompilProc.isNotNil and fCompilProc.Running then
    fCompilProc.Terminate(1);
end;

procedure TNativeProject.compile;
var
  config: TCompilerConfiguration;
  prjpath: string;
  prjname: string;
begin
  if fCompilProc.isNotNil and fCompilProc.Active then
  begin
    fMsgs.message('the project is already being compiled',
      fAsProjectItf, amcProj, amkWarn);
    exit;
  end;
  killProcess(fCompilProc);
  fCompiled := false;
  config := currentConfiguration;
  if config.isNil then
  begin
    fMsgs.message('unexpected project error: no active configuration',
      fAsProjectItf, amcProj, amkErr);
    exit;
  end;
  //
  fMsgs.clearByData(fAsProjectItf);
  subjProjCompiling(fProjectSubject, Self);
  //
  prjpath := fFileName.extractFilePath;
  fPreCompilePath := GetCurrentDirUTF8;
  SetCurrentDirUTF8(prjpath);
  //
  if not runPrePostProcess(config.preBuildProcess) then
    fMsgs.message('warning: pre-compilation process or commands not properly executed',
      fAsProjectItf, amcProj, amkWarn);
  //
  SetCurrentDirUTF8(prjpath);
  //
  if (Sources.Count = 0) and (config.pathsOptions.extraSources.Count = 0) then
  begin
    SetCurrentDirUTF8(fPreCompilePath);
    exit;
  end;
  //
  prjname := shortenPath(filename, 25);
  fCompilProc := TDexedProcess.Create(nil);
  subjProjCompiling(fProjectSubject, fAsProjectItf);
  fMsgs.message('compiling ' + prjname, fAsProjectItf, amcProj, amkInf);
  fMsgs.message(usingCompilerInfo(CEProjectCompiler), fAsProjectItf, amcProj, amkInf);
  // this doesn't work under linux, so the previous ChDir.
  if prjpath.dirExists then
    fCompilProc.CurrentDirectory := prjpath;
  fCompilProc.Executable := CEProjectCompilerFilename;
  fCompilProc.Options := fCompilProc.Options + [poStderrToOutPut, poUsePipes];
  fCompilProc.ShowWindow := swoHIDE;
  fCompilProc.OnReadData:= @compProcOutput;
  fCompilProc.OnTerminate:= @compProcTerminated;
  getOpts(fCompilProc.Parameters);
  //getUpToDateObjects(fCompilProc.Parameters);
  if CEProjectCompiler = gdc then
    fCompilProc.Parameters.Add('-gdc=gdc');
  fCompilProc.Execute;
end;

procedure TNativeProject.run(const runArgs: string = '');
var
  prm: string;
  i: Integer;
  cwd: string;
begin
  killProcess(fRunner);
  if fRunnerOldCwd.dirExists then
    ChDir(fRunnerOldCwd);
  //
  fRunner := TDexedProcess.Create(nil);
  fRunner.XTermProgram:=consoleProgram;
  currentConfiguration.runOptions.setProcess(fRunner);
  if runArgs.isNotEmpty then
  begin
    i := 1;
    repeat
      prm := ExtractDelimited(i, runArgs, [' ']);
      prm := fSymStringExpander.expand(prm);
      if prm.isNotEmpty then
        fRunner.Parameters.AddText(prm);
      Inc(i);
    until prm = '';
  end;
  //
  if not outputFilename.fileExists then
  begin
    fMsgs.message('output executable missing: ' + shortenPath(outputFilename, 25),
      fAsProjectItf, amcProj, amkErr);
    exit;
  end;
  //
  fRunner.Executable := outputFilename;
  fRunnerOldCwd := GetCurrentDirUTF8;
  if fRunner.CurrentDirectory.isEmpty then
  begin
    cwd := fRunner.Executable.extractFilePath;
    SetCurrentDirUTF8(cwd);
    fRunner.CurrentDirectory := cwd;
  end;
  if poUsePipes in fRunner.Options then
  begin
    fRunner.OnReadData := @runProcOutput;
    fRunner.OnTerminate := @runProcOutput;
    getprocInputHandler.addProcess(fRunner);
  end;
  fRunner.Execute;
end;

procedure TNativeProject.runProcOutput(sender: TObject);
var
  lst: TStringList;
  str: string;
  proc: TProcess;
begin
  lst := TStringList.Create;
  try
    if (sender is TDexedProcess) then
      (sender as TDexedProcess).getFullLines(lst)
    else
      processOutputToStrings(TProcess(sender), lst);
    for str in lst do
      fMsgs.message(str, fAsProjectItf, amcProj, amkBub);
  finally
    lst.Free;
  end;
  //
  proc := TProcess(sender);
  if not proc.Running then
  begin
    getprocInputHandler.removeProcess(TProcess(sender));
    SetCurrentDirUTF8(fRunnerOldCwd);

    if (proc.ExitStatus <> 0) then
      fMsgs.message(format('error: the process (%s) has returned the status %s',
        [proc.Executable, prettyReturnStatus(proc)]), fAsProjectItf, amcProj, amkErr);
  end;
end;

procedure TNativeProject.compProcOutput(proc: TObject);
var
  lst: TStringList;
  str: string;
begin
  lst := TStringList.Create;
  try
    fCompilProc.getFullLines(lst);
    for str in lst do
      fMsgs.message(str, fAsProjectItf, amcProj, amkAuto);
  finally
    lst.Free;
  end;
end;

procedure TNativeProject.compProcTerminated(proc: TObject);
var
  prjname: string;
begin
  compProcOutput(proc);
  prjname := shortenPath(filename);
  fCompiled := fCompilProc.ExitStatus = 0;
  updateOutFilename;
  if fCompiled then
    fMsgs.message(prjname + ' has been successfully compiled',
      fAsProjectItf, amcProj, amkInf)
  else
    fMsgs.message(prjname + ' has not been compiled',
      fAsProjectItf, amcProj, amkWarn);
  //
  if not runPrePostProcess(getCurrConf.postBuildProcess) then
    fMsgs.message( 'warning: post-compilation process or commands not properly executed',
      fAsProjectItf, amcProj, amkWarn);
  subjProjCompiled(fProjectSubject, fAsProjectItf, fCompiled);
  //
  SetCurrentDirUTF8(fPreCompilePath);
end;

function TNativeProject.targetUpToDate: boolean;
var
  dt: double;
  i: integer;
begin
  result := false;
  if not fOutputFilename.fileExists then exit;
  dt := FileAge(fOutputFilename);
  for i := 0 to fSrcs.Count-1 do
    if fileAge(sourceAbsolute(i)) > dt then exit;
  result := true;
end;

function TNativeProject.getObjectsDirectory: string; inline;
var
  cfg: TCompilerConfiguration;
begin
  result := '';
  cfg := currentConfiguration;
  if (cfg.pathsOptions.objectDirectory <> '') and
    DirectoryExistsUTF8(cfg.pathsOptions.objectDirectory) then
      result := cfg.pathsOptions.objectDirectory;
end;

procedure TNativeProject.getUpToDateObjects(str: TStrings);
var
  odr: string;
  src: string;
  obj: string;
  i: integer;
begin
  odr := getObjectsDirectory;
  if odr.isEmpty then
  begin
    for i := 0 to fSrcs.Count-1 do
    begin
      src := sourceAbsolute(i);
      obj := src.stripFileExt + objExt;
      if obj.fileExists and src.fileExists then
      begin
        if FileAgeUTF8(src) > FileAgeUTF8(obj) then
          DeleteFile(obj)
        else
          str.Add(obj);
      end;
    end;
  end
  else
  begin

  end;
end;

function TNativeProject.outputFilename: string;
begin
  exit(fOutputFilename);
end;

function TNativeProject.configurationCount: integer;
begin
  exit(fConfigs.Count);
end;

procedure TNativeProject.setActiveConfigurationIndex(index: integer);
begin
  setConfIx(index);
end;

function TNativeProject.getActiveConfigurationIndex: integer;
begin
  exit(fConfIx);
end;

function TNativeProject.configurationName(index: integer): string;
begin
  if index > fConfigs.Count -1 then index := fConfigs.Count -1;
  if index < 0 then index := 0;
  result := getConfig(index).name;
end;

function TNativeProject.filename: string;
begin
  exit(fFilename);
end;

function TNativeProject.modified: boolean;
begin
  exit(fModified);
end;

function TNativeProject.basePath: string;
begin
  exit(fBasePath);
end;

function TNativeProject.binaryKind: TProjectBinaryKind;
begin
  exit(currentConfiguration.outputOptions.binaryKind);
end;

function TNativeProject.getCommandLine: string;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    str.Add(CEProjectCompilerFilename);
    getOpts(str);
    result := str.Text;
  finally
    str.Free;
  end;
end;

function TNativeProject.sourcesCount: integer;
begin
  exit(fSrcs.Count);
end;

function TNativeProject.sourceRelative(index: integer): string;
begin
  exit(fSrcs[index]);
end;

function TNativeProject.sourceAbsolute(index: integer): string;
var
  fname: string;
begin
  fname := fSrcs[index];
  if FilenameIsAbsolute(fname) then
    result := fname
  else
    result := expandFilenameEx(fBasePath, fname);
end;

function TNativeProject.importsPathCount: integer;
begin
  result := currentConfiguration.pathsOptions.importModulePaths.Count;
end;

function TNativeProject.importPath(index: integer): string;
begin
  result := currentConfiguration.pathsOptions.importModulePaths[index];
  if fBasePath.dirExists then
    result := expandFilenameEx(fBasePath, result);
end;

procedure TNativeProject.test;
begin
end;

function isValidNativeProject(const filename: string): boolean;
var
  maybe: TNativeProject;
begin
  result := false;
  if isDlangCompilable(filename.extractFileExt) then
    exit;
  // avoid the project to notify the observers, current project is not replaced
  EntitiesConnector.beginUpdate;
  maybe := TNativeProject.create(nil);
  try
    maybe.loadFromFile(filename);
    result := maybe.hasLoaded;
  finally
    maybe.Free;
    EntitiesConnector.endUpdate;
  end;
end;

function getCEProjectCompiler: DCompiler;
begin
  exit(CEProjectCompiler);
end;

procedure setCEProjectCompiler(value: DCompiler);
var
  sel: ICompilerSelector;
begin
  sel := getCompilerSelector;
  if value = gdc then
    value := gdmd
  else if value = ldc then
    value := ldmd;
  CEProjectCompiler := value;
  if not sel.isCompilerValid(CEProjectCompiler) then
    CEProjectCompiler := dmd;
  CEProjectCompilerFilename:=sel.getCompilerPath(CEProjectCompiler);
end;

initialization
  setCEProjectCompiler(dmd);
end.
