unit u_dmdwrap;

{$I u_defines.inc}

interface

uses
  classes, sysutils, process, asyncprocess,
  u_common, u_processes, u_interfaces;

(*

procedure to add a new compiler option:
- the option must be published with a setter proc, in the setter 'doChanged' must be called.
- getOpts must be updated to generate the new option.
- Assign() must be updated to copy the new option. (used when cloning a configuration)

*)

type

  (*****************************************************************************
   * Base class designed to encapsulate some compiler options.
   * A descendant must be able to generate the related options
   * as a string representing the partial switches/arguments.
   *)
  TOptsGroup = class(TPersistent)
  private
    fOnChange: TNotifyEvent;
    procedure doChanged;
  protected
    fSymStringExpander: ISymStringExpander;
    property onChange: TNotifyEvent read fOnChange write fOnChange;
  public
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); virtual; abstract;
    constructor create; virtual;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the DDoc and JSON generation.
   *)
  TDocOpts = class(TOptsGroup)
  private
    fGenDoc: boolean;
    fDocDir: TPathname;
    fGenJson: boolean;
    fJsonFname: TFilename;
    procedure setGenDoc(const value: boolean);
    procedure setGenJSON(const value: boolean);
    procedure setDocDir(const value: TPathname);
    procedure setJSONFile(const value: TFilename);
  published
    property generateDocumentation: boolean read fGenDoc write setGenDoc default false;
    property generateJSON: boolean read fGenJson write setGenJSON default false;
    property DocumentationDirectory: TPathname read fDocDir write setDocDir;
    property JSONFilename: TFilename read fJsonFname write setJSONFile;
  public
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); override;
  end;


  (*****************************************************************************
   * Describes the different deprecation handling.
   *)
  TDepHandling = (silent, warning, error);

  (*****************************************************************************
   * Encapsulates the options/args related to the compiler output messages.
   *)
  TMsgOpts = class(TOptsGroup)
  private
    fDepHandling : TDepHandling;
    fVerbose: boolean;
    fWarnings: boolean;
    fWarnInfo: boolean;
    fVtls: boolean;
    fQuiet: boolean;
    fVgc: boolean;
    fCol: boolean;
    procedure setDepHandling(const value: TDepHandling);
    procedure setVerbose(const value: boolean);
    procedure setWarnings(const value: boolean);
    procedure setWarnInfo(const value: boolean);
    procedure setVtls(const value: boolean);
    procedure setQuiet(const value: boolean);
    procedure setVgc(const value: boolean);
    procedure setCol(const value: boolean);
  published
    property deprecationHandling: TDepHandling read fDepHandling write setDepHandling default warning;
    property verbose: boolean read fVerbose write setVerbose default false;
    property warnings: boolean read fWarnings write setWarnings default true;
    property warningsAsInfo: boolean read fWarnInfo write setWarnInfo default false;
    property tlsInformations: boolean read fVtls write setVtls default false;
    property quiet: boolean read fQuiet write setQuiet default false;
    property showHiddenAlloc: boolean read fVgc write setVgc default false;
    property showColumnsNumber: boolean read fCol write setCol default false;
  public
    constructor create; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); override;
  end;

  (**
   * Describes the target registry size.
   *)
  TTargetSystem = (auto, os32bit, os64bit);

  (**
   * Describes the bounds check kinds.
   *)
  TBoundCheckKind = (onAlways, safeOnly, offAlways);

  (*****************************************************************************
   * Encapsulates the options/args related to the analysis & the code gen.
   *)
  TOutputOpts = class(TOptsGroup)
  private
    fTrgKind: TTargetSystem;
    fBinKind: TProjectBinaryKind;
    fUnittest: boolean;
    fVerIds: TStringList;
    fInline: boolean;
    fBoundsCheck: TBoundCheckKind;
    fOptimz: boolean;
    fGenStack: boolean;
    fAddMain: boolean;
    fRelease: boolean;
    fAllInst: boolean;
    fStackStomp: boolean;
    fAlwayLinkLibs: boolean;
    procedure setAlwaysLinkLibs(const value: boolean);
    procedure setAllInst(const value: boolean);
    procedure setUnittest(const value: boolean);
    procedure setTrgKind(const value: TTargetSystem);
    procedure setBinKind(const value: TProjectBinaryKind);
    procedure setInline(const value: boolean);
    procedure setBoundsCheck(const value: TBoundCheckKind);
    procedure setOptims(const value: boolean);
    procedure setGenStack(const value: boolean);
    procedure setAddMain(const value: boolean);
    procedure setRelease(const value: boolean);
    procedure setVerIds(const value: TStringList);
    procedure setStackStomp(const value: boolean);
  published
    property alwaysLinkStaticLibs: boolean read fAlwayLinkLibs write setAlwaysLinkLibs default false;
    property targetKind: TTargetSystem read fTrgKind write setTrgKind default auto;
    property binaryKind: TProjectBinaryKind read fBinKind write setBinKind default executable;
    property inlining: boolean read fInline write setInline default false;
    property boundsCheck: TBoundCheckKind read fBoundsCheck write setBoundsCheck default safeOnly;
    property optimizations: boolean read fOptimz write setOptims default false;
    property addMain: boolean read fAddMain write setAddMain default false;
    property release: boolean read fRelease write setRelease default false;
    property unittest: boolean read fUnittest write setUnittest default false;
    property versionIdentifiers: TStringList read fVerIds write setVerIds;
    property generateAllTmpCode: boolean read fAllInst write setAllInst default false;
    property addStackStompCode: boolean read fStackStomp write setStackStomp default false;
  public
    constructor create; override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the debugging
   *)
  TDebugOpts = class(TOptsGroup)
  private
    fDebugConditions: boolean;
    fGenInfos: boolean;
    fDbgC: boolean;
    fGenMap: boolean;
    fDbgIdents: TStringList;
    fDbgLevel: Integer;
    fForceDbgBool: boolean;
    fGenFrame: boolean;
    procedure updateForceDbgBool;
    procedure setGenFrame(const value: boolean);
    procedure setDebugConditions(const value: boolean);
    procedure setGenInfos(const value: boolean);
    procedure setDbgC(const value: boolean);
    procedure setGenMap(const value: boolean);
    procedure setDbgLevel(const value: Integer);
    procedure setDbgIdents(value: TStringList);
  published
    property debugConditions: boolean read fDebugConditions write setDebugConditions default false;
    property debugIdentifiers: TStringList read fDbgIdents write setDbgIdents;
    property debugLevel: Integer read fDbgLevel write setDbgLevel default 0;
    property generateInfos: boolean read fGenInfos write setGenInfos default false;
    property generateMapFile: boolean read fGenMap write setGenMap default false;
    property generateStackFrame: boolean read fGenFrame write setGenFrame default false;
  public
    constructor create; override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings;base: TOptsGroup = nil); override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the output and include paths
   *)
  TPathsOpts = class(TOptsGroup)
  private
    fExtraSrcs: TStringList;
    fImpMod: TStringList;
    fImpStr: TStringList;
    fExcl: TStringList;
    fFname: TFilename;
    fObjDir: TPathname;
    fForceExt: boolean;
    procedure setForceExt(value: boolean);
    procedure setFname(const value: TFilename);
    procedure setObjDir(const value: TPathname);
    procedure setSrcs(value: TStringList);
    procedure setIncl(value: TStringList);
    procedure setImpt(value: TStringList);
    procedure setExcl(value: TStringList);
    procedure strLstChange(sender: TObject);
  published
    property outputFilename: TFilename read fFname write setFname;
    property objectDirectory: TPathname read fObjDir write setObjDir;
    property exclusions: TStringList read fExcl write setExcl;
    property extraSources: TStringList read fExtraSrcs write setSrcs;
    property importModulePaths: TStringList read fImpMod write setIncl;
    property importStringPaths: TStringList read fImpStr write setImpt;
    property forceExtension: boolean read fForceExt write setForceExt default false;
  public
    constructor create; override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); override;
    procedure getExtraSources(list: TStrings);
  end;

  (*****************************************************************************
   * Encapsulates the unclassified and custom options/args
   *)
  TOtherOpts = class(TOptsGroup)
  private
    fCov: boolean;
    fGui: boolean;
    fCustom: TStringList;
    fDmdOthers: TstringList;
    fLdcOthers: TStringList;
    fGdcOthers: TStringList;
    procedure setCov(value: boolean);
    procedure setGui(value: boolean);
    procedure setCustom(value: TStringList);
    procedure setDmdOtherOptions(value: TStringList);
    procedure setLdcOtherOptions(value: TStringList);
    procedure setGdcOtherOptions(value: TStringList);
  published
    property guiApplication: boolean read fGui write setGui default false;
    property coverage: boolean read fCov write setCov default false;
    property customOptions: TStringList read fCustom write setCustom;
    property dmdOtherOptions: TStringList read fDmdOthers write setDmdOtherOptions;
    property ldcOtherOptions: TStringList read fLdcOthers write setLdcOtherOptions;
    property gdcOtherOptions: TStringList read fGdcOthers write setGdcOtherOptions;
  public
    constructor create; override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); override;
    procedure getCompilerSpecificOpts(list: TStrings; base: TOptsGroup = nil; compiler: DCompiler = dmd);
  end;

  (*****************************************************************************
   * Encapsulates the most common TProcess options.
   * Used to simplify pre/post-compilation and run process options.
   *)
  TCustomProcOptions = class(TOptsGroup)
  private
    fExecutable: TFilename;
    fWorkDir: TPathname;
    fOptions: TProcessOptions;
    fParameters: TStringList;
    fShowWin: TShowWindowOptions;
    fCommands: TStringList;
    procedure setExecutable(const value: TFilename);
    procedure setWorkDir(const value: TPathname);
    procedure setOptions(const value: TProcessOptions);
    procedure setParameters(value: TStringList);
    procedure setShowWin(value: TShowWindowOptions);
    procedure setCommands(value: TStringList);
  protected
    property executable: TFilename read fExecutable write setExecutable;
    property workingDirectory: TPathname read fWorkDir write setWorkDir;
    property options: TProcessOptions read fOptions write setOptions default [];
    property parameters: TStringList read fParameters write setParameters;
    property showWindow: TShowWindowOptions read fShowWin write setShowWin default swoNone;
    property simpleCommands: TStringList read fCommands write setCommands;
  public
    constructor create; override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TOptsGroup = nil); override;
    { TAsyncProcess "Parameters" inherits from UTF8 process,
      and the property reader is not anymore "fParameters" but "fUTF8Parameters"
      without the overload aProcess does not get the Parameters if aProcess is TAsynProcess...}
    procedure setProcess(var process: TProcess);
    procedure setProcess(var process: TAsyncProcess);
    procedure setProcess(var process: TDexedProcess);
  end;

  (*****************************************************************************
   * Encapsulates the options for the pre/post compilation processes
   *)
  TCompileProcOptions = class(TCustomProcOptions)
  published
    property executable;
    property workingDirectory;
    property options default [];
    property parameters;
    property showWindow default swoNone;
    property simpleCommands;
  end;

  (*****************************************************************************
   * Encapsulates the options for the project run process.
   * 'executable' prop is hidden since it's defined by the project.
   *)
  TProjectRunOptions = class(TCustomProcOptions)
  published
    property workingDirectory;
    property options default [];
    property parameters;
    property showWindow default swoNone;
  end;

  (*****************************************************************************
   * Encapsulates all the contextual options/args
   *)
  TCompilerConfiguration = class(TCollectionItem)
  private
    fSymStringExpander: ISymStringExpander;
    fName: string;
    fOnChanged: TNotifyEvent;
    fDocOpts: TDocOpts;
    fDebugOpts: TDebugOpts;
    fMsgOpts: TMsgOpts;
    fOutputOpts: TOutputOpts;
    fPathsOpts: TPathsOpts;
    fOthers: TOtherOpts;
    fPreProcOpt: TCompileProcOptions;
    fPostProcOpt: TCompileProcOptions;
    fRunProjOpt: TProjectRunOptions;
    fIsBaseConfiguration: boolean;
    fIsOverriddenConfiguration: boolean;
    procedure doChanged;
    procedure subOptsChanged(sender: TObject);
    procedure setName(const value: string);
    procedure setDocOpts(const value: TDocOpts);
    procedure setDebugOpts(const value: TDebugOpts);
    procedure setMsgOpts(const value: TMsgOpts);
    procedure setOutputOpts(const value: TOutputOpts);
    procedure setPathsOpts(const value: TPathsOpts);
    procedure setOthers(const value: TOtherOpts);
    procedure setPreProcOpt(const value: TCompileProcOptions);
    procedure setPostProcOpt(const value: TCompileProcOptions);
    procedure setRunProjOpt(const value: TProjectRunOptions);
    procedure setisBaseConfiguration(const value: boolean);
    procedure setisOverriddenConfiguration(const value: boolean);
  protected
    function nameFromID: string;
  published
    property name: string read fName write setName;
    property documentationOptions: TDocOpts read fDocOpts write setDocOpts;
    property debugingOptions: TDebugOpts read fDebugOpts write setDebugOpts;
    property messagesOptions: TMsgOpts read fMsgOpts write setMsgOpts;
    property outputOptions: TOutputOpts read fOutputOpts write setOutputOpts;
    property pathsOptions: TPathsOpts read fPathsOpts write setPathsOpts;
    property otherOptions: TOtherOpts read fOthers write setOthers;
    property preBuildProcess: TCompileProcOptions read fPreProcOpt write setPreProcOpt;
    property postBuildProcess: TCompileProcOptions read fPostProcOpt write setPostProcOpt;
    property runOptions: TProjectRunOptions read fRunProjOpt write setRunProjOpt;
    property isBaseConfiguration: boolean read fIsBaseConfiguration write setisBaseConfiguration default false;
    property isOverriddenConfiguration: boolean read fIsOverriddenConfiguration write setisOverriddenConfiguration default false;
  public
    constructor create(aCollection: TCollection); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
    procedure getOpts(list: TStrings; base: TCompilerConfiguration = nil);
    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

constructor TOptsGroup.create;
begin
  fSymStringExpander := getSymStringExpander;
end;

procedure TOptsGroup.doChanged;
begin
  if assigned(fOnChange) then fOnChange(self);
end;

{$REGION TDocOpts --------------------------------------------------------------}
procedure TDocOpts.getOpts(list: TStrings; base: TOptsGroup = nil);
var
  baseopt: TDocOpts;
begin
  if base.isNil then
  begin
    if fGenDoc then
      list.Add('-D');
    if fGenJson then
      list.Add('-X');
    if fDocDir <> '' then
      list.Add('-Dd' + fSymStringExpander.expand(fDocDir));
    if fJsonFname <> '' then
      list.Add('-Xf' + fSymStringExpander.expand(fJsonFname));
  end else
  begin
    baseopt := TDocOpts(base);
    if baseopt.fGenDoc or fGenDoc then
      list.Add('-D');
    if baseopt.fGenJson or fGenJson then
      list.Add('-X');
    if (baseopt.fDocDir <> '') and (fDocDir <> '') then
      list.Add('-Dd' + fSymStringExpander.expand(fDocDir))
    else if (fDocDir <> '') then
      list.Add('-Dd' + fSymStringExpander.expand(fDocDir))
    else if (baseopt.fDocDir <> '') then
      list.Add('-Dd' + fSymStringExpander.expand(baseopt.fDocDir));
    if (baseopt.fJsonFname <> '') and (fJsonFname <> '') then
      list.Add('-Xf' + fSymStringExpander.expand(fJsonFname))
    else if fJsonFname <> '' then
      list.Add('-Xf' + fSymStringExpander.expand(fJsonFname))
    else if (baseopt.fJsonFname <> '') then
      list.Add('-Dd' + fSymStringExpander.expand(baseopt.fJsonFname));
  end;
end;

procedure TDocOpts.assign(source: TPersistent);
var
  src: TDocOpts;
begin
  if (source is TDocOpts) then
  begin
    src       := TDocOpts(source);
    //
    fGenDoc   := src.fGenDoc;
    fGenJson  := src.fGenJson;
    fDocDir   := patchPlateformPath(src.fDocDir);
    fJsonFname:= patchPlateformPath(src.fJsonFname);
  end
  else inherited;
end;

procedure TDocOpts.setGenDoc(const value: boolean);
begin
  if fDocDir <> '' then
  begin
    fGenDoc := true;
    exit;
  end;
  //
  if fGenDoc = value then
    exit;
  fGenDoc := value;
  doChanged;
end;

procedure TDocOpts.setGenJSON(const value: boolean);
begin
  if fJsonFname <> '' then
  begin
    fGenJson := true;
    exit;
  end;
  //
  if fGenJson = value then
    exit;
  fGenJson := value;
  doChanged;
end;

procedure TDocOpts.setDocDir(const value: TPathname);
begin
  if fDocDir = value then
    exit;
  fDocDir := patchPlateformPath(value);
  if fDocDir <> '' then
    setGenDoc(true);
  doChanged;
end;

procedure TDocOpts.setJSONFile(const value: TFilename);
begin
  if fJsonFname = value then
    exit;
  fJsonFname := patchPlateformPath(value);
  if fJsonFname <> '' then
    setGenJSON(true);
  doChanged;
end;
{$ENDREGION}

{$REGION TMsgOpts --------------------------------------------------------------}
constructor TMsgOpts.create;
begin
  inherited;
  fDepHandling := TDepHandling.warning;
  fWarnings := true;
end;

procedure TMsgOpts.getOpts(list: TStrings; base: TOptsGroup = nil);
var
  dep, depbase: string;
  baseopt: TMsgOpts;
const
  DepStr : array[TDepHandling] of string = ('-d', '', '-de');
begin
  if base.isNil then
  begin
    dep := DepStr[fDepHandling];
    if dep.isNotEmpty then list.Add(dep);
    if fVerbose then list.Add('-v');
    if fWarnings then list.Add('-w');
    if fWarnInfo then list.Add('-wi');
    if fVtls then list.Add('-vtls');
    if fQuiet then list.Add('-quiet');
    if fVgc then list.Add('-vgc');
    if fCol then list.Add('-vcolumns');
  end else
  begin
    baseopt := TMsgOpts(base);
    dep := DepStr[fDepHandling];
    depbase := DepStr[baseopt.fDepHandling];
    if dep <> depbase then list.Add(dep) else list.Add(depbase);
    if baseopt.fVerbose or fVerbose then list.Add('-v');
    if baseopt.fWarnings or fWarnings then list.Add('-w');
    if baseopt.fWarnInfo or fWarnInfo then list.Add('-wi');
    if baseopt.fVtls or fVtls then list.Add('-vtls');
    if baseopt.fQuiet or fQuiet then list.Add('-quiet');
    if baseopt.fVgc or fVgc then list.Add('-vgc');
    if baseopt.fCol or fCol then list.Add('-vcolumns');
  end;
end;

procedure TMsgOpts.assign(source: TPersistent);
var
  src: TMsgOpts;
begin
  if (source is TMsgOpts) then
  begin
    src := TMsgOpts(source);
    //
    fDepHandling := src.fDepHandling;
    fVerbose  := src.fVerbose;
    fWarnings := src.fWarnings;
    fWarnInfo   := src.fWarnInfo;
    fVtls     := src.fVtls;
    fQuiet    := src.fQuiet;
    fVgc      := src.fVgc;
    fCol      := src.fCol;
  end
  else inherited;
end;

procedure TMsgOpts.setDepHandling(const value: TDepHandling);
begin
  if fDepHandling = value then exit;
  fDepHandling := value;
  doChanged;
end;

procedure TMsgOpts.setVerbose(const value: boolean);
begin
  if fVerbose = value then exit;
  fVerbose := value;
  doChanged;
end;

procedure TMsgOpts.setWarnings(const value: boolean);
begin
  if fWarnings = value then exit;
  fWarnings := value;
  doChanged;
end;

procedure TMsgOpts.setWarnInfo(const value: boolean);
begin
  if fWarnInfo = value then exit;
  fWarnInfo := value;
  doChanged;
end;

procedure TMsgOpts.setVtls(const value: boolean);
begin
  if fVtls = value then exit;
  fVtls := value;
  doChanged;
end;

procedure TMsgOpts.setQuiet(const value: boolean);
begin
  if fQuiet = value then exit;
  fQuiet := value;
  doChanged;
end;

procedure TMsgOpts.setVgc(const value: boolean);
begin
  if fVgc = value then exit;
  fVgc := value;
  doChanged;
end;

procedure TMsgOpts.setCol(const value: boolean);
begin
  if fCol = value then exit;
  fCol := value;
  doChanged;
end;
{$ENDREGION}

{$REGION TOutputOpts -----------------------------------------------------------}
constructor TOutputOpts.create;
begin
  inherited;
  fVerIds := TStringList.Create;
  fBoundsCheck := safeOnly;
end;

destructor TOutputOpts.destroy;
begin
  fVerIds.Free;
  inherited;
end;

procedure TOutputOpts.getOpts(list: TStrings; base: TOptsGroup = nil);
var
  str, strbase: string;
  baseopt: TOutputOpts;
const
  trgKindStr: array[TTargetSystem] of string = ('', '-m32','-m64');
  binKindStr: array[TProjectBinaryKind] of string = ('', '-lib', '-shared', '-c');
  bchKindStr: array[TBoundCheckKind] of string = ('on', 'safeonly', 'off');
begin
  if base.isNil then
  begin
    str := binKindStr[fBinKind];
    if str.isNotEmpty then list.Add(str);
    {$IFNDEF WINDOWS}
    if fBinKind = sharedlib then
      list.Add('-fPIC');
    {$ENDIF}
    str := trgKindStr[fTrgKind];
    if str.isNotEmpty then list.Add(str);
    if fUnittest then list.Add('-unittest');
    if fInline then list.Add('-inline');
    if fOptimz then list.Add('-O');
    if fStackStomp then list.Add('-gx');
    if fAllInst then list.Add('-allinst');
    if fAddMain then list.Add('-main');
    if fRelease then list.Add('-release');
    for str in fVerIds do
      if not isStringDisabled(str) then list.Add('-version=' + str);
    //
    if fRelease then
      begin
        if fBoundsCheck <> safeOnly then
          list.Add('-boundscheck=' + bchKindStr[fBoundsCheck] );
      end
    else
      if fBoundsCheck <> onAlways then
        list.Add('-boundscheck=' + bchKindStr[fBoundsCheck] );
  end else
  begin
    baseopt := TOutputOpts(base);
    str := binKindStr[fBinKind];
    strbase := binKindStr[baseopt.fBinKind];
    if (str <> strbase) then
    begin
      list.Add(str);
      {$IFNDEF WINDOWS}
      if fBinKind = sharedlib then
        list.Add('-fPIC');
      {$ENDIF}
    end
    else
    begin
      list.Add(strbase);
      {$IFNDEF WINDOWS}
      if baseopt.fBinKind = sharedlib then
        list.Add('-fPIC');
      {$ENDIF}
    end;
    str := trgKindStr[fTrgKind];
    strbase := trgKindStr[baseopt.fTrgKind];
    if (str <> strbase) then list.Add(str) else list.Add(strbase);
    if baseopt.fUnittest or fUnittest then list.Add('-unittest');
    if baseopt.fInline or fInline then list.Add('-inline');
    if baseopt.fOptimz or fOptimz then list.Add('-O');
    if baseopt.fStackStomp or fStackStomp then list.Add('-gx');
    if baseopt.fAllInst or fAllInst then list.Add('-allinst');
    if baseopt.fAddMain or fAddMain then list.Add('-main');
    if baseopt.fRelease or fRelease then list.Add('-release');
    if (fVerIds.Count = 0) then for str in baseopt.fVerIds do begin
      if not isStringDisabled(str) then list.Add('-version=' + str);
    end else for str in fVerIds do
      if not isStringDisabled(str) then list.Add('-version=' + str);
    // default values are not handled here, TODO
    if fBoundsCheck <> baseopt.fBoundsCheck then
      list.Add('-boundscheck=' + bchKindStr[fBoundsCheck] )
    else
      list.Add('-boundscheck=' + bchKindStr[baseopt.fBoundsCheck] );
  end;
end;

procedure TOutputOpts.assign(source: TPersistent);
var
  src: TOutputOpts;
begin
  if (source is TOutputOpts) then
  begin
    src := TOutputOpts(source);
    //
    fVerIds.Assign(src.fVerIds);
    fBinKind     := src.fBinKind;
    fTrgKind     := src.fTrgKind;
    fUnittest    := src.fUnittest;
    fInline      := src.fInline;
    fBoundsCheck := src.fBoundsCheck;
    fOptimz      := src.fOptimz;
    fGenStack    := src.fGenStack;
    fAddMain     := src.fAddMain;
    fRelease     := src.fRelease;
    fAllinst     := src.fAllInst;
    fStackStomp  := src.fStackStomp;
    fAlwayLinkLibs := src.fAlwayLinkLibs;
  end
  else inherited;
end;

procedure TOutputOpts.setUnittest(const value: boolean);
begin
  if fUnittest = value then exit;
  fUnittest := value;
  doChanged;
end;

procedure TOutputOpts.setAllInst(const value: boolean);
begin
  if fAllinst = value then exit;
  fAllinst := value;
  doChanged;
end;

procedure TOutputOpts.setAlwaysLinkLibs(const value: boolean);
begin
  if fAlwayLinkLibs = value then exit;
  fAlwayLinkLibs := value;
  doChanged;
end;

procedure TOutputOpts.setVerIds(const value: TStringList);
begin
  fVerIds.Assign(value);
  doChanged;
end;

procedure TOutputOpts.setTrgKind(const value: TTargetSystem);
begin
  if fTrgKind = value then exit;
  fTrgKind := value;
  doChanged;
end;

procedure TOutputOpts.setBinKind(const value: TProjectBinaryKind);
begin
  if fBinKind = value then exit;
  fBinKind := value;
  doChanged;
end;

procedure TOutputOpts.setInline(const value: boolean);
begin
  if fInline = value then exit;
  fInline := value;
  doChanged;
end;

procedure TOutputOpts.setBoundsCheck(const value: TBoundCheckKind);
begin
  if fBoundsCheck = value then exit;
  fBoundsCheck := value;
  doChanged;
end;

procedure TOutputOpts.setOptims(const value: boolean);
begin
  if fOptimz = value then exit;
  fOptimz := value;
  doChanged;
end;

procedure TOutputOpts.setGenStack(const value: boolean);
begin
  if fGenStack = value then exit;
  fGenStack := value;
  doChanged;
end;

procedure TOutputOpts.setAddMain(const value: boolean);
begin
  if fAddMain = value then exit;
  fAddMain := value;
  doChanged;
end;

procedure TOutputOpts.setRelease(const value: boolean);
begin
  if fRelease = value then exit;
  fRelease := value;
  doChanged;
end;

procedure TOutputOpts.setStackStomp(const value: boolean);
begin
  if fStackStomp = value then exit;
  fStackStomp := value;
  doChanged;
end;
{$ENDREGION}

{$REGION TDebugOpts ------------------------------------------------------------}
constructor TDebugOpts.create;
begin
  inherited;
  fDbgIdents := TStringList.Create;
end;

destructor TDebugOpts.destroy;
begin
  fDbgIdents.Free;
  inherited;
end;

procedure TDebugOpts.getOpts(list: TStrings; base: TOptsGroup = nil);
var
  idt: string;
  baseopt: TDebugOpts;
begin
  if base.isNil then
  begin
    if fDebugConditions then list.Add('-debug');
    if fDbgLevel <> 0 then
      list.Add('-debug=' + intToStr(fDbgLevel));
    for idt in fDbgIdents do
      list.Add('-debug=' + idt);
    if fGenInfos then list.Add('-g');
    if fDbgC then list.Add('-gc');
    if fGenMap then list.Add('-map');
    if fGenFrame and (list.IndexOf('-gs') = -1) then list.Add('-gs');
  end else
  begin
    baseopt := TDebugOpts(base);
    if baseopt.fDebugConditions or fDebugConditions then list.Add('-debug');
    if (baseopt.fDbgLevel <> 0) and (fDbgLevel = 0) then
      list.Add('-debug=' + intToStr(baseopt.fDbgLevel))
    else if fDbgLevel <> 0 then
      list.Add('-debug=' + intToStr(fDbgLevel));
    if fDbgIdents.Count = 0 then
      for idt in baseopt.fDbgIdents do list.Add('-debug=' + idt)
    else for idt in fDbgIdents do list.Add('-debug=' + idt);
    if baseopt.fGenInfos or fGenInfos then list.Add('-g');
    if baseopt.fDbgC or fDbgC then list.Add('-gc');
    if baseopt.fGenMap or fGenMap then list.Add('-map');
    if (baseopt.fGenFrame or fGenFrame) and (list.IndexOf('-gs') = -1) then list.Add('-gs');
  end;
end;

procedure TDebugOpts.assign(source: TPersistent);
var
  src: TDebugOpts;
begin
  if (source is TDebugOpts) then
  begin
    src := TDebugOpts(source);
    //
    fDbgIdents.Assign(src.fDbgIdents);
    fDebugConditions    := src.fDebugConditions;
    fDbgLevel := src.fDbgLevel;
    fGenInfos     := src.fGenInfos;
    fDbgC     := src.fDbgC;
    fGenMap   := src.fGenMap;
    fGenFrame := src.fGenFrame;
  end
  else inherited;
end;

procedure TDebugOpts.updateForceDbgBool;
begin
  fForceDbgBool := (fDbgLevel > 0) or (fDbgIdents.Count > 0);
  if fForceDbgBool then setDebugConditions(true);
end;

procedure TDebugOpts.setDebugConditions(const value: boolean);
begin
  if fForceDbgBool then
  begin
    fDebugConditions := true;
    exit;
  end;
  if fDebugConditions = value then exit;
  fDebugConditions := value;
  doChanged;
end;

procedure TDebugOpts.setGenFrame(const value: boolean);
begin
  if fGenFrame = value then exit;
  fGenFrame:=value;
  doChanged;
end;

procedure TDebugOpts.setGenInfos(const value: boolean);
begin
  if fGenInfos = value then exit;
  fGenInfos := value;
  doChanged;
end;

procedure TDebugOpts.setDbgC(const value: boolean);
begin
  if fDbgC = value then exit;
  fDbgC := value;
  doChanged;
end;

procedure TDebugOpts.setGenMap(const value: boolean);
begin
  if fGenMap = value then exit;
  fGenMap := value;
  doChanged;
end;

procedure TDebugOpts.setDbgLevel(const value: Integer);
begin
  if fDbgLevel = value then exit;
  fDbgLevel := value;
  if fDbgLevel < 0 then fDbgLevel := 0;
  updateForceDbgBool;
  doChanged;
end;

procedure TDebugOpts.setDbgIdents(value: TStringList);
begin
  fDbgIdents.Assign(value);
  updateForceDbgBool;
  doChanged;
end;
{$ENDREGION}

{$REGION TPathsOpts ------------------------------------------------------------}
constructor TPathsOpts.create;
begin
  inherited;
  fExtraSrcs := TStringList.Create;
  fImpMod := TStringList.Create;
  fImpStr := TStringList.Create;
  fExcl := TStringList.Create;
  // setSrcs(), setIncl(), etc are not called when reloading from
  // a stream but rather the TSgringList.Assign()
  fExtraSrcs.OnChange := @strLstChange;
  fImpMod.OnChange := @strLstChange;
  fImpStr.OnChange := @strLstChange;
  fExcl.OnChange := @strLstChange;
end;

procedure TPathsOpts.strLstChange(sender: TObject);
begin
  TStringList(sender).BeginUpdate; // onChange not called anymore
  patchPlateformPaths(TStringList(sender));
  // EndUpdate is not called to avoid an infinite loop
end;

procedure TPathsOpts.getExtraSources(list: TStrings);
var
  e: TStringList;
  s: string;
  i: integer;
begin
  e := TStringList.create;
  try
    e.AddStrings(['.d','.di']);
    for i := 0 to fExtraSrcs.Count-1 do
    begin
      s := fExtraSrcs[i];
      if isStringDisabled(s) then
        continue;
      s := fSymStringExpander.expand(s);
      if not listAsteriskPath(s, list, e) then
        list.Add(s);
    end;
  finally
    e.free;
  end;
end;

procedure TPathsOpts.getOpts(list: TStrings; base: TOptsGroup = nil);
var
  str, sym: string;
  exts: TStringList;
  baseopt: TPathsOpts;
  rightList: TStringList;
begin
  if base.isNil then
  begin
    exts := TStringList.Create;
    try
      exts.AddStrings(['.d', '.di', '.dd']);
      for str in fExtraSrcs do
      begin
        if isStringDisabled(str) then
          continue;
        sym := fSymStringExpander.expand(str);
        if not listAsteriskPath(sym, list, exts) then
          list.Add(sym);
      end;
    finally
      exts.Free;
    end;
    for str in fImpMod do if not isStringDisabled(str) then
      list.Add('-I'+ fSymStringExpander.expand(str));
    for str in fImpStr do if not isStringDisabled(str) then
      list.Add('-J'+ fSymStringExpander.expand(str));
    if fFname <> '' then
      list.Add('-of' + fSymStringExpander.expand(fFname));
    if fObjDir <> '' then
      list.Add('-od' + fSymStringExpander.expand(fObjDir));
  end else
  begin
    baseopt := TPathsOpts(base);
    if fExtraSrcs.Count = 0 then rightList := baseopt.fExtraSrcs
    else rightList := fExtraSrcs;
    exts := TStringList.Create;
    try
      exts.AddStrings(['.d', '.di', '.dd']);
      for str in rightList do
      begin
        if isStringDisabled(str) then
          continue;
        sym := fSymStringExpander.expand(str);
        if not listAsteriskPath(sym, list, exts) then
          list.Add(sym);
      end;
    finally
      exts.Free;
    end;
    //
    if fImpMod.Count = 0 then rightList := baseopt.fImpMod
    else rightList := fImpMod;
    for str in rightList do if not isStringDisabled(str) then
      list.Add('-I'+ fSymStringExpander.expand(str));
    //
    if fImpStr.Count = 0 then rightList := baseopt.fImpStr
    else rightList := fImpStr;
    for str in rightList do if not isStringDisabled(str) then
      list.Add('-J'+ fSymStringExpander.expand(str));
    //
    str := '';
    if fFname <> '' then str := fFname else
      if baseopt.fFname <> '' then str := baseopt.fFname;
    if str.isNotEmpty then list.Add('-of' + fSymStringExpander.expand(str));
    //
    str := '';
    if fObjDir <> '' then str := fObjDir else
      if baseopt.fObjDir <> '' then str := baseopt.fObjDir;
    if str.isNotEmpty then list.Add('-od' + fSymStringExpander.expand(str));
  end;
end;

procedure TPathsOpts.assign(source: TPersistent);
var
  src: TPathsOpts;
begin
  if (source is TPathsOpts) then
  begin
    src := TPathsOpts(source);
    //
    fExtraSrcs.Assign(src.fExtraSrcs);
    fImpMod.Assign(src.fImpMod);
    fImpStr.Assign(src.fImpStr);
    fExcl.Assign(src.fExcl);
    fForceExt:= src.fForceExt;
    fFName  := patchPlateformPath(src.fFname);
    fObjDir := patchPlateformPath(src.fObjDir);
  end
  else inherited;
end;

destructor TPathsOpts.destroy;
begin
  fExtraSrcs.free;
  fImpMod.free;
  fImpStr.free;
  fExcl.free;
  inherited;
end;

procedure TPathsOpts.setForceExt(value: boolean);
begin
  if fForceExt = value then exit;
  fForceExt:=value;
  doChanged;
end;

procedure TPathsOpts.setFname(const value: TFilename);
begin
  if fFname = value then exit;
  fFname := patchPlateformPath(value);
  fFname := patchPlateformExt(fFname);
  doChanged;
end;

procedure TPathsOpts.setObjDir(const value: TPathname);
begin
  if fObjDir = value then exit;
  fObjDir := patchPlateformPath(value);
  doChanged;
end;

procedure TPathsOpts.setSrcs(value: TStringList);
begin
  fExtraSrcs.Assign(value);
  patchPlateformPaths(fExtraSrcs);
  doChanged;
end;

procedure TPathsOpts.setIncl(value: TStringList);
begin
  fImpMod.Assign(value);
  patchPlateformPaths(fImpMod);
  doChanged;
end;

procedure TPathsOpts.setImpt(value: TStringList);
begin
  fImpStr.Assign(value);
  patchPlateformPaths(fImpStr);
  doChanged;
end;

procedure TPathsOpts.setExcl(value: TStringList);
begin
  fExcl.Assign(value);
  patchPlateformPaths(fExcl);
  doChanged;
end;
{$ENDREGION}

{$REGION TOtherOpts ------------------------------------------------------------}
constructor TOtherOpts.create;
begin
  inherited;
  fCustom := TStringList.Create;
  fDmdOthers := TStringList.Create;
  fLdcOthers := TStringList.Create;
  fGdcOthers := TStringList.Create;
end;

procedure TOtherOpts.assign(source: TPersistent);
var
  src: TOtherOpts;
begin
  if (source is TOtherOpts) then
  begin
    src := TOtherOpts(source);
    fCov := src.fCov;
    fGUi := src.fGui;
    fCustom.Assign(src.fCustom);
    fDmdOthers.Assign(src.fDmdOthers);
    fLdcOthers.Assign(src.fLdcOthers);
    fGdcOthers.Assign(src.fGdcOthers);
  end
  else inherited;
end;

destructor TOtherOpts.destroy;
begin
  fCustom.Free;
  fDmdOthers.Free;
  fLdcOthers.Free;
  fGdcOthers.Free;
  inherited;
end;

procedure TOtherOpts.setCov(value: boolean);
begin
  if fCov = value then
    exit;
  fCov := value;
  doChanged;
end;

procedure TOtherOpts.setGui(value: boolean);
begin
  if fGui = value then
    exit;
  fGui := value;
  doChanged;
end;

procedure TOtherOpts.getOpts(list: TStrings; base: TOptsGroup = nil);
var
  i: integer;
  str: string;
  baseopt: TOtherOpts;
  rightList: TStringList;
begin
  if base.isNil then
  begin
    for i := 0 to fCustom.Count-1 do
    begin
      str := fCustom[i];
      if str.isEmpty or isStringDisabled(str) then
        continue;
      if str[1] <> '-' then
        str := '-' + str;
      list.AddText(fSymStringExpander.expand(str));
    end;
    if fCov then
      list.Add('-cov');
    if fGui then
      list.Add('-L/SUBSYSTEM:WINDOWS:5.0');
  end else
  begin
    baseopt := TOtherOpts(base);
    if fCustom.Count = 0 then
      rightList := baseopt.fCustom
    else
      rightList := fCustom;
    for i := 0 to rightList.Count-1 do
    begin
      str := rightList[i];
      if str.isEmpty or isStringDisabled(str) then
        continue;
      if str[1] <> '-' then
        str := '-' + str;
      list.AddText(fSymStringExpander.expand(str));
    end;
    if baseopt.fCov or fCov then
      list.Add('-cov');
    if baseopt.fGui or fGui then
      list.Add('-L/SUBSYSTEM:WINDOWS:5.0');
  end;
end;

procedure TOtherOpts.getCompilerSpecificOpts(list: TStrings; base:
    TOptsGroup = nil; compiler: DCompiler = dmd);
var
  i: integer;
  str: string;
  baseopt: TOtherOpts;
  lst: TStringList = nil;
begin
  if base.isNil then
  begin
    case compiler of
      dmd: lst := fDmdOthers;
      ldc, ldmd: lst := fLdcOthers;
      gdc, gdmd: lst := fGdcOthers;
    end;
    if lst.isNotNil then for i := 0 to lst.Count-1 do
    begin
      str := lst[i];
      if str.isEmpty or isStringDisabled(str) then
        continue;
      if str[1] <> '-' then
        str := '-' + str;
      list.AddText(fSymStringExpander.expand(str));
    end;
  end else
  begin
    baseopt := TOtherOpts(base);
    case compiler of
      dmd:
        if fDmdOthers.Count = 0 then
          lst := baseopt.fDmdOthers
        else
          lst := fDmdOthers;
      ldc, ldmd:
        if fLdcOthers.Count = 0 then
          lst := baseopt.fLdcOthers
        else
          lst := fLdcOthers;
      gdc, gdmd:
        if fGdcOthers.Count = 0 then
          lst := baseopt.fGdcOthers
        else
          lst := fGdcOthers;
    end;
    if lst.isNotNil then for i := 0 to lst.Count-1 do
    begin
      str := lst[i];
      if str.isEmpty or isStringDisabled(str) then
        continue;
      if str[1] <> '-' then
        str := '-' + str;
      list.AddText(fSymStringExpander.expand(str));
    end;
  end;
end;

procedure TOtherOpts.setCustom(value: TStringList);
begin
  fCustom.Assign(value);
  doChanged;
end;

procedure TOtherOpts.setDmdOtherOptions(value: TStringList);
begin
  fDmdOthers.Assign(value);
  doChanged;
end;

procedure TOtherOpts.setLdcOtherOptions(value: TStringList);
begin
  fLdcOthers.Assign(value);
  doChanged;
end;

procedure TOtherOpts.setGdcOtherOptions(value: TStringList);
begin
  fGdcOthers.Assign(value);
  doChanged;
end;
{$ENDREGION}

{$REGION TCustomProcOptions ----------------------------------------------------}
constructor TCustomProcOptions.create;
begin
  inherited;
  fParameters := TStringList.Create;
  fCommands := TStringList.Create;
end;

destructor TCustomProcOptions.destroy;
begin
  fParameters.Free;
  fCommands.Free;
  inherited;
end;

procedure TCustomProcOptions.assign(source: TPersistent);
var
  src: TCustomProcOptions;
begin
  if source is TCustomProcOptions then
  begin
    src := TCustomProcOptions(source);
    //
    Parameters.Assign(src.Parameters);
    fOptions    := src.fOptions;
    fExecutable := src.fExecutable;
    fShowWin    := src.fShowWin;
  end
  else inherited;
end;

procedure TCustomProcOptions.getOpts(list: TStrings; base: TOptsGroup = nil);
begin
end;

procedure TCustomProcOptions.setProcess(var process: TProcess);
begin
  process.Parameters.Clear;
  process.Parameters.AddText(fSymStringExpander.expand(Parameters.Text));
  process.Executable := fExecutable;
  process.ShowWindow := fShowWin;
  process.Options    := fOptions;
  process.CurrentDirectory := fSymStringExpander.expand(fWorkDir);
  process.StartupOptions := process.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setProcess(var process: TAsyncProcess);
begin
  process.Parameters.Clear;
  process.Parameters.AddText(fSymStringExpander.expand(Parameters.Text));
  process.Executable := fExecutable;
  process.ShowWindow := fShowWin;
  process.Options    := fOptions;
  process.CurrentDirectory := fSymStringExpander.expand(fWorkDir);
  process.StartupOptions := process.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setProcess(var process: TDexedProcess);
begin
  process.Parameters.Clear;
  process.Parameters.AddText(fSymStringExpander.expand(Parameters.Text));
  process.Executable := fExecutable;
  process.ShowWindow := fShowWin;
  process.Options    := fOptions;
  process.CurrentDirectory := fSymStringExpander.expand(fWorkDir);
  process.StartupOptions := process.StartupOptions + [suoUseShowWindow];
end;

procedure TCustomProcOptions.setExecutable(const value: TFilename);
begin
  if fExecutable = value then exit;
  fExecutable := value;
  doChanged;
end;

procedure TCustomProcOptions.setWorkDir(const value: TPathname);
begin
  if fWorkDir = value then exit;
  fWorkDir := value;
  doChanged;
end;

procedure TCustomProcOptions.setOptions(const value: TProcessOptions);
begin
  if fOptions = value then exit;
  fOptions := value;
  doChanged;
end;

procedure TCustomProcOptions.setParameters(value: TStringList);
begin
  fParameters.Assign(value);
  doChanged;
end;

procedure TCustomProcOptions.setCommands(value: TStringList);
begin
  fCommands.Assign(value);
  doChanged;
end;

procedure TCustomProcOptions.setShowWin(value: TShowWindowOptions);
begin
  if fShowWin = value then exit;
  fShowWin := value;
  doChanged;
end;
{$ENDREGION}

{$REGION TCompilerConfiguration ------------------------------------------------}
constructor TCompilerConfiguration.create(aCollection: TCollection);
begin
  inherited create(aCollection);

  fSymStringExpander:= getSymStringExpander;

  fDocOpts    := TDocOpts.create;
  fDebugOpts  := TDebugOpts.create;
  fMsgOpts    := TMsgOpts.create;
  fOutputOpts := TOutputOpts.create;
  fPathsOpts  := TPathsOpts.create;
  fOthers     := TOtherOpts.create;
  fPreProcOpt := TCompileProcOptions.create;
  fPostProcOpt:= TCompileProcOptions.create;
  fRunProjOpt := TProjectRunOptions.create;

  fDocOpts.onChange     := @subOptsChanged;
  fDebugOpts.onChange   := @subOptsChanged;
  fMsgOpts.onChange     := @subOptsChanged;
  fOutputOpts.onChange  := @subOptsChanged;
  fPathsOpts.onChange   := @subOptsChanged;
  fOthers.onChange      := @subOptsChanged;
  fPreProcOpt.onChange  := @subOptsChanged;
  fPostProcOpt.onChange := @subOptsChanged;
  fRunProjOpt.onChange  := @subOptsChanged;

  fName := nameFromID;
end;

destructor TCompilerConfiguration.destroy;
begin
  fOnChanged := nil;
  fDocOpts.free;
  fDebugOpts.free;
  fMsgOpts.free;
  fOutputOpts.free;
  fPathsOpts.free;
  fOthers.free;
  fPreProcOpt.free;
  fPostProcOpt.free;
  fRunProjOpt.Free;
  inherited;
end;

procedure TCompilerConfiguration.assign(source: TPersistent);
var
  src: TCompilerConfiguration;
begin
  if (source is TCompilerConfiguration) then
  begin
    src := TCompilerConfiguration(source);
    //
    fDocOpts.assign(src.fDocOpts);
    fDebugOpts.assign(src.fDebugOpts);
    fMsgOpts.assign(src.fMsgOpts);
    fOutputOpts.assign(src.fOutputOpts);
    fPathsOpts.assign(src.fPathsOpts);
    fOthers.assign(src.fOthers);
    fPreProcOpt.assign(src.fPreProcOpt);
    fPostProcOpt.assign(src.fPostProcOpt);
    fRunProjOpt.assign(src.fRunProjOpt);
    //
    // isBase / isOverriden not copied on purpose.
  end
  else inherited;
end;

function TCompilerConfiguration.nameFromID: string;
begin
  result := format('<configuration %d>', [ID]);
end;

procedure TCompilerConfiguration.getOpts(list: TStrings; base: TCompilerConfiguration = nil);
var
  ext, nme: string;
  fe: boolean;
  i: integer;
begin
  if (base = nil) or (base = self) then
  begin
    fDocOpts.getOpts(list);
    fDebugOpts.getOpts(list);
    fMsgOpts.getOpts(list);
    fOutputOpts.getOpts(list);
    fPathsOpts.getOpts(list);
    fOthers.getOpts(list);
    fe := fPathsOpts.forceExtension;
    nme := fPathsOpts.outputFilename;
  end else
  begin
    fDocOpts.getOpts(list, base.fDocOpts);
    fDebugOpts.getOpts(list, base.fDebugOpts);
    fMsgOpts.getOpts(list, base.fMsgOpts);
    fOutputOpts.getOpts(list, base.fOutputOpts);
    fPathsOpts.getOpts(list, base.fPathsOpts);
    fOthers.getOpts(list, base.fOthers);
    fe := fPathsOpts.forceExtension or base.fPathsOpts.forceExtension;
    nme := fPathsOpts.outputFilename;
    if base.fPathsOpts.outputFilename <> '' then
      nme := base.fPathsOpts.outputFilename;
  end;
  if fe and nme.isNotEmpty then
  begin
    nme := fSymStringExpander.expand(nme);
    ext := nme.extractFileExt;
    nme := '-of' + nme;
    i := list.IndexOf(nme);
    if i <> -1 then case fOutputOpts.binaryKind of
      {$IFDEF WINDOWS}
      executable: if ext <> exeExt then
        list[i] := list[i] + exeExt;
      {$ENDIF}
      obj: if ext <> objExt then
        list[i] := list[i] + objExt;
      sharedlib: if ext <> dynExt then
        list[i] := list[i] + dynExt;
      staticlib: if ext <> libExt then
        list[i] := list[i] + libExt;
    end;
  end;
end;

procedure TCompilerConfiguration.setName(const value: string);
begin
  if fName = value then
    exit;
  fName := value;
  if fName = '' then
    fName := nameFromID;
  doChanged;
end;

procedure TCompilerConfiguration.subOptsChanged(sender: TObject);
begin
  doChanged;
end;

procedure TCompilerConfiguration.doChanged;
begin
  if assigned(fOnChanged) then fOnChanged(self);
end;

procedure TCompilerConfiguration.setDocOpts(const value: TDocOpts);
begin
  fDocOpts.assign(value);
end;

procedure TCompilerConfiguration.setDebugOpts(const value: TDebugOpts);
begin
  fDebugOpts.assign(value);
end;

procedure TCompilerConfiguration.setMsgOpts(const value: TMsgOpts);
begin
  fMsgOpts.assign(value);
end;

procedure TCompilerConfiguration.setOutputOpts(const value: TOutputOpts);
begin
  fOutputOpts.assign(value);
end;

procedure TCompilerConfiguration.setPathsOpts(const value: TPathsOpts);
begin
  fPathsOpts.assign(value);
end;

procedure TCompilerConfiguration.setOthers(const value: TOtherOpts);
begin
  fOthers.Assign(value);
end;

procedure TCompilerConfiguration.setPreProcOpt(const value: TCompileProcOptions);
begin
  fPreProcOpt.assign(value);
end;

procedure TCompilerConfiguration.setPostProcOpt(const value: TCompileProcOptions);
begin
  fPostProcOpt.assign(value);
end;

procedure TCompilerConfiguration.setRunProjOpt(const value: TProjectRunOptions);
begin
  fRunProjOpt.assign(value);
end;

procedure TCompilerConfiguration.setisBaseConfiguration(const value: boolean);
begin
  fIsBaseConfiguration := value;
  doChanged;
end;

procedure TCompilerConfiguration.setisOverriddenConfiguration(const value: boolean);
begin
  fIsBaseConfiguration := false;
  fIsOverriddenConfiguration := value;
  doChanged;
end;
{$ENDREGION}

end.
