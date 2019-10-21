unit u_dubproject;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, jsonscanner, process, strutils,
  LazFileUtils, RegExpr, LGHelpers, LGVector,
  u_common, u_interfaces, u_observer, u_dialogs, u_processes,
  u_writableComponent, u_compilers, u_semver, u_stringrange;

type

  TDubLinkMode = (dlmSeparate, dlmAllAtOnce, dlmSingleFile);

  TDubDependencyCheck = (dcStandard, dcOffline, dcNo);

  TDubVerbosity = (default, quiet, verbose, veryVerbose, onlyWarnAndError, onlyError);

  TDubArchOverride = (auto, x86, x86_64);

  PDubLocalPackage = ^TDubLocalPackage;

  TSemVerList = specialize TGVector<PSemVer>;

  TDubLocalPackage = class
  strict private
    fName : string;
    fVersions: TSemVerList;
  public
    constructor create;
    destructor destroy; override;
    procedure addVersion(const value: string);
    function findVersion(constref value: TSemVer): PSemVer;
    function highestInInterval(constref lo, hi: TSemVer): PSemVer;
    function highest: PSemVer;
    property name: string read fName write fName;
  end;

  TDubLocalPackages = record
  strict private
    class var fLocalPackages: array of TDubLocalPackage;
    class var fDoneFirstUpdate: boolean;
  public
    class procedure deinit; static;
    class procedure update; static;
    class function find(const name: string; out package: PDubLocalPackage): boolean; static; overload;
    class function find(const name, op: string; constref opVer: TSemVer; out package: PDubLocalPackage): PSemver; static; overload;
  end;

  (**
   * Stores the build options, always applied when a project is build
   *)
  TDubBuildOptionsBase = class(TWritableLfmTextComponent)
  strict private
    fParallel: boolean;
    fForceRebuild: boolean;
    fLinkMode: TDubLinkMode;
    fCombined: boolean;
    fDepCheck: TDubDependencyCheck;
    fVerbosity: TDubVerbosity;
    fArchOverride: TDubArchOverride;
    fOther: string;
    fCompiler: DCompiler;
    fShowConsole: boolean;
    fAutoFetch: boolean;
    fAutoSelectTestConfig: boolean;
    procedure setLinkMode(value: TDubLinkMode);
    procedure setCompiler(value: DCompiler);
    function getCompiler: DCompiler;
  published
    property showConsole: boolean read fShowConsole write fShowConsole default false;
    property compiler: DCompiler read getCompiler write setCompiler;
    property parallel: boolean read fParallel write fParallel;
    property forceRebuild: boolean read fForceRebuild write fForceRebuild;
    property linkMode: TDubLinkMode read fLinkMode write setLinkMode;
    property combined: boolean read fCombined write fCombined;
    property other: string read fOther write fOther;
    property dependenciesCheck: TDubDependencyCheck read fDepCheck write fDepCheck;
    property verbosity: TDubVerbosity read fVerbosity write fVerbosity default default;
    property archOverride: TDubArchOverride read fArchOverride write fArchOverride default auto;
    property autoFetch: boolean read fAutoFetch write fAutoFetch default false;
    property autoSelectTestConfig: boolean read fAutoSelectTestConfig write fAutoSelectTestConfig default true;
  public
    procedure assign(source: TPersistent); override;
    procedure getOpts(options: TStrings);
  end;

  (**
   * Make the build options editable
   *)
  TDubBuildOptions = class(TDubBuildOptionsBase, IEditableOptions)
  strict private
    fBackup: TDubBuildOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  TDubCommand = (dcBuild, dcRun, dcTest);

  TDubProject = class(TComponent, ICommonProject)
  private
    fIsSdl: boolean;
    fInGroup: boolean;
    fHasLoaded: boolean;
    fDubProc: TDexedProcess;
    fPreCompilePath: string;
    fPackageName: string;
    fFilename: string;
    fModified: boolean;
    fJSON: TJSONObject;
    fSrcs: TStringList;
    fProjectSubject: TProjectSubject;
    fConfigsCount: integer;
    fImportPaths: TStringList;
    fBuildTypes: TStringList;
    fConfigs: TStringList;
    fBuiltTypeIx: integer;
    fConfigIx: integer;
    fBinKind: TProjectBinaryKind;
    fBasePath: string;
    fModificationCount: integer;
    fOutputFileName: string;
    fSaveAsUtf8: boolean;
    fCompiled: boolean;
    fMsgs: IMessagesDisplay;
    fNextTerminatedCommand: TDubCommand;
    fAsProjectItf: ICommonProject;
    procedure doModified;
    procedure updateFields;
    procedure updatePackageNameFromJson;
    procedure udpateConfigsFromJson;
    procedure updateSourcesFromJson;
    procedure updateTargetKindFromJson;
    procedure updateImportPathsFromJson;
    procedure updateOutputNameFromJson;
    function findTargetKindInd(value: TJSONObject): boolean;
    procedure dubProcOutput(proc: TObject);
    procedure dubProcTerminated(proc: TObject);
    function getCurrentCustomConfig: TJSONObject;
    procedure executeDub(command: TDubCommand; const runArgs: string = '');
    procedure restorePersistentConfigId;
    procedure storePersistentConfigId;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;

    procedure beginModification;
    procedure endModification;

    function filename: string;
    function basePath: string;
    procedure loadFromFile(const fname: string);
    procedure saveToFile(const fname: string);

    procedure updateSourcesList;
    procedure activate;
    function inGroup: boolean;
    procedure inGroup(value: boolean);
    function getFormat: TProjectFormat;
    function getProject: TObject;
    function modified: boolean;
    function binaryKind: TProjectBinaryKind;
    function getCommandLine: string;
    function outputFilename: string;
    procedure reload;
    procedure stopCompilation;

    function isSource(const fname: string): boolean;
    function sourcesCount: integer;
    function sourceRelative(index: integer): string;
    function sourceAbsolute(index: integer): string;
    function importsPathCount: integer;
    function importPath(index: integer): string;

    function configurationCount: integer;
    function getActiveConfigurationIndex: integer;
    procedure setActiveConfigurationIndex(index: integer);
    function configurationName(index: integer): string;

    procedure compile;
    function compiled: boolean;
    procedure run(const runArgs: string = '');
    procedure test;
    function targetUpToDate: boolean;

    property json: TJSONObject read fJSON;
    property packageName: string read fPackageName;
    property isSDL: boolean read fIsSdl;
  end;

  // these 9 built types always exist
  TDubBuildType = (plain, debug, release, releaseDebug, releaseNoBounds, unittest,
    docs, ddox, profile, cov, unittestcov);

  // returns true if filename is a valid dub project. Only json format is supported.
  function isValidDubProject(const filename: string): boolean;

  // converts a sdl description to json, returns the json
  function sdl2json(const filename: string): TJSONObject;

  function getDubCompiler: DCompiler;
  procedure setDubCompiler(value: DCompiler);

var
  DubCompiler: DCompiler = dmd;
  DubCompilerFilename: string = 'dmd';
  Lfm: ILifetimeManager = nil;

const
  DubSdlWarning = 'this feature is deactivated in DUB projects with the SDL format';

implementation

var
  dubBuildOptions: TDubBuildOptions;

const

  optFname = 'dubbuild.txt';

  DubBuiltTypeName: array[TDubBuildType] of string = ('plain', 'debug', 'release',
    'release-debug', 'release-nobounds', 'unittest', 'docs', 'ddox', 'profile',
    'cov', 'unittest-cov'
  );

  DubDefaultConfigName = '(default config)';

  dubCmd2Arg: array[TDubCommand] of string = ('build', 'run', 'test');
  dubCmd2PreMsg: array[TDubCommand] of string = ('compiling ', 'running ', 'testing ');
  dubCmd2PostMsg: array[TDubCommand] of string = ('compiled', 'executed', 'tested');

procedure getPackagesLocations(loc: TStringList);
var
  p: string;
  j: TJSONParser;
  m: TMemoryStream;
  a: TJSONArray;
  o: TJSONObject = nil;
  d: TJSONData;
  r: TJSONData;
  i: integer;
begin
  {$IFDEF WINDOWS}
  p := GetEnvironmentVariable('APPDATA') + '\dub\packages\';
  {$ELSE}
  p := GetEnvironmentVariable('HOME') + '/.dub/packages/';
  {$ENDIF}
  if p.dirExists then
    loc.Add(p);
  p += 'local-packages.json';
  if not p.fileExists then
    exit;

  m := TMemoryStream.Create;
  try
    m.LoadFromFile(p);
    j := TJSONParser.Create(m, [joIgnoreTrailingComma, joUTF8]);
    try
      r := j.Parse;
    finally
      j.Free;
    end;
    if r.JSONType = jtArray then
    begin
      a := TJSONArray(r);
      for i := 0 to a.Count-1 do
      begin
        o := a.Objects[i];
        if not o.findAny('path', d) then
          continue;
        p := d.AsString;
        if (p.length <> 0) and (p[p.length] <> DirectorySeparator) then
          p += DirectorySeparator;
        if DirectoryExistsUTF8(p) then
          loc.Add(p);
      end;
    end;
  finally
    m.Free;
    if r.isNotNil then
      r.Free;
  end;
end;

{$REGION TDubLocalPackages -----------------------------------------------------}
constructor TDubLocalPackage.create;
begin
  fVersions := TSemVerList.create;
end;

destructor TDubLocalPackage.destroy;
var I: PtrInt;
begin
  for I := fVersions.Count - 1 downto 0 do
    Dispose(fVersions[I]);
  fVersions.Free;
  inherited Destroy;
end;

procedure TDubLocalPackage.addVersion(const value: string);
var
  v: PSemVer;
  I: PtrInt;
begin
  v := new(PSemVer);
  if value = 'vmaster' then
    v^.init('v0.0.0-master')
  else try
    v^.init(value);
  except
    dispose(v);
    exit;
  end;
  for i := 0 to fVersions.Count-1 do
  begin
    if fVersions[i]^ = v^ then
      exit;
    if (i < fVersions.Count-1) and (fVersions[i+1]^ > v^) and (fVersions[i]^ < v^ ) then
    begin
      fVersions.Insert(i, v);
      exit;
    end;
  end;
  fVersions.Add(v);
end;

function TDubLocalPackage.highest: PSemVer;
begin
  result := fVersions.Last;
end;

function TDubLocalPackage.highestInInterval(constref lo, hi: TSemVer): PSemVer;
var
  I: PtrInt;
begin
  Result := nil;
  for I := 0 to fVersions.Count - 1 do
  begin
    if fVersions[I]^ < Lo then
      Continue;
    if fVersions[I]^ < Hi then
      Result := fVersions[i];
    if (fVersions[I]^ > Hi) then
      Break;
  end;
end;

function TDubLocalPackage.findVersion(constref Value: TSemVer): PSemVer;
var
  I: PtrInt;
begin
  Result := nil;
  for I := fVersions.Count - 1 downto 0 do
    if fVersions[I]^ = Value then
      Exit(fVersions[I]);
end;

class procedure TDubLocalPackages.DeInit;
var
  I: PtrInt;
begin
  for I := High(fLocalPackages) downto 0 do
    fLocalPackages[I].Free;
end;

class procedure TDubLocalPackages.update;
var
  p: TStringList;
  r: TStringList;
  s: string;
  n: string;
  v: string = '';
  i: integer;
  j: integer = 0;
  k: integer;
  d: PDubLocalPackage = nil;
  h: TStringRange = (ptr: nil; pos: 0; len: 0);
  x: string;

begin

  if not assigned(Lfm) then
    Lfm := getLifeTimeManager;
  if not assigned(Lfm) or not (Lfm.getLifetimeStatus = lfsLoaded) then
  begin
    if fDoneFirstUpdate then
      exit;
  end;
  fDoneFirstUpdate := true;

  for i := 0 to high(fLocalPackages) do
    fLocalPackages[i].Free;
  setLength(fLocalPackages, 0);
  r := TStringList.Create;
  getPackagesLocations(r);

  try for k := 0 to r.Count -1 do
  begin
    x := r[k];
    p := TStringList.Create;
    try
      listFolders(p, x);
      for i := 0 to p.Count-1 do
      begin
        j := 0;
        s := p[i];
        h.init(s);
        while true do
        begin
          h.popUntil('-');
          if h.empty then
            break;
          if (h.popFront^.front in ['0'..'9']) or
              h.endsWith('master') then
          begin
            j := h.position;
            break;
          end;
        end;
        if (j = 0) then
          continue;

        n := s[1..j-1];
        n := n.extractFileName;
        if not find(n, d) then
        begin
          setLength(fLocalPackages, length(fLocalPackages) + 1);
          fLocalPackages[high(fLocalPackages)] := TDubLocalPackage.create;
          d := @fLocalPackages[high(fLocalPackages)];
          d^.name := n;
        end;
        v := 'v' + s[j+1 .. length(s)];
        d^.addVersion(v);

      end;
    finally
      p.Free;
    end;
  end;
  finally
    r.Free;
  end;

end;

class function TDubLocalPackages.find(const name: string; out package: PDubLocalPackage): boolean;
var
  i: integer;
begin
  result := false;
  package:= nil;
  for i := 0 to high(fLocalPackages) do
    if fLocalPackages[i].name = name then
  begin
    result := true;
    package := @fLocalPackages[i];
    break;
  end;
end;

class function TDubLocalPackages.find(const name, op: string; constref opVer: TSemVer;
  out package: PDubLocalPackage): PSemVer;
var
  hi: TSemVer;
begin
  result := nil;
  if op = '=' then
  begin
    if find(name, package) then
      result := package^.findVersion(opVer);
  end
  else if op = '>=' then
  begin
    if find(name, package) then
    begin
      result := package^.highest;
      if result^ < opVer then
        result := nil;
    end;
  end
  else if op = '>' then
  begin
    if find(name, package) then
    begin
      result := package^.highest;
      if (result^ < opVer) or (result^ = opVer) then
        result := nil;
    end;
  end
  else if op = '~>' then
  begin
    if find(name, package) then
    begin
      hi := opVer;
      hi.minor := hi.minor + 1;
      hi.patch := 0;
      hi.additional :='';
      result := package^.highestInInterval(opVer, hi);
      result := result;
    end;
  end
  else
  begin
    if find(name, package) then
      result := package^.highest;
  end;
end;
{$ENDREGION}

{$REGION Options ---------------------------------------------------------------}
procedure TDubBuildOptionsBase.setLinkMode(value: TDubLinkMode);
begin
  if fLinkMode = value then
    exit;
  if not (value in [low(TDubLinkMode)..high(TDubLinkMode)]) then
    value := low(TDubLinkMode);
  fLinkMode:=value;
end;

procedure TDubBuildOptionsBase.setCompiler(value: DCompiler);
begin
  fCompiler := value;
  setDubCompiler(fCompiler);
end;

function TDubBuildOptionsBase.getCompiler: DCompiler;
begin
  result := fCompiler;
end;

procedure TDubBuildOptionsBase.assign(source: TPersistent);
var
  opts: TDubBuildOptionsBase;
begin
  if source is TDubBuildOptionsBase then
  begin
    opts := TDubBuildOptionsBase(source);
    parallel:=opts.parallel;
    forceRebuild:=opts.forceRebuild;
    combined:=opts.combined;
    linkMode:=opts.linkMode;
    other:=opts.other;
    dependenciesCheck:=opts.dependenciesCheck;
    compiler:=opts.compiler;
    verbosity:=opts.verbosity;
    archOverride:=opts.archOverride;
    autoFetch:=opts.autoFetch;
    fAutoSelectTestConfig:=opts.fAutoSelectTestConfig;
  end
  else inherited;
end;

procedure TDubBuildOptionsBase.getOpts(options: TStrings);
const
  vb: array[TDubVerbosity] of string = (
  '',           //auto,
  '--vquiet',   //quiet,
  '-v',         //verbose,
  '--vverbose', //veryVerbose,
  '-q',         //onlyWarnAndError,
  '--verror');  //vError
  ao: array [TDubArchOverride] of string = (
  '',
  '--arch=x86',
  '--arch=x86_64'
  );
begin
  if parallel then
    options.Add('--parallel');
  if forceRebuild then
    options.Add('--force');
  if combined then
    options.Add('--combined');
  case linkMode of
    dlmAllAtOnce: options.Add('--build-mode=allAtOnce');
    dlmSingleFile: options.Add('--build-mode=singleFile');
    else ;
  end;
  case dependenciesCheck of
    dcNo: options.Add('--skip-registry=all');
    dcOffline: options.Add('--skip-registry=standard');
    else ;
  end;
  if fVerbosity <> TDubVerbosity.default then
    options.Add(vb[fVerbosity]);
  if fArchOverride <> TDubArchOverride.auto then
    options.Add(ao[fArchOverride]);
  if other.isNotEmpty then
    CommandToList(other, options);
end;

constructor TDubBuildOptions.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fBackup := TDubBuildOptionsBase.Create(nil);
  EntitiesConnector.addObserver(self);
  autoSelectTestConfig := true;
  fname := getDocPath + optFname;
  if fname.fileExists then
    loadFromFile(fname);
end;

destructor TDubBuildOptions.destroy;
begin
  saveToFile(getDocPath + optFname);
  EntitiesConnector.removeObserver(self);
  fBackup.free;
  inherited;
end;

function TDubBuildOptions.optionedWantCategory(): string;
begin
  exit('DUB build');
end;

function TDubBuildOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TDubBuildOptions.optionedWantContainer: TPersistent;
begin
  fBackup.assign(self);
  exit(self);
end;

procedure TDubBuildOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept: fBackup.assign(self);
    oeeCancel: self.assign(fBackup);
    oeeSelectCat:fBackup.assign(self);
    else ;
  end;
end;

function TDubBuildOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION Standard Comp/Obj -----------------------------------------------------}
constructor TDubProject.create(aOwner: TComponent);
begin
  inherited;
  fAsProjectItf := self as ICommonProject;
  fSaveAsUtf8 := true;
  fJSON := TJSONObject.Create();
  fProjectSubject := TProjectSubject.Create;
  fMsgs:= getMessageDisplay;
  fBuildTypes := TStringList.Create;
  fConfigs := TStringList.Create;
  fSrcs := TStringList.Create;
  fSrcs.Sorted:=true;
  fSrcs.Duplicates:=dupIgnore;
  fImportPaths := TStringList.Create;
  fImportPaths.Sorted:=true;
  fImportPaths.Duplicates:=dupIgnore;

  json.Add('name', '');
  endModification;
  subjProjNew(fProjectSubject, self);
  doModified;
  fModified:=false;

  TDubLocalPackages.update;
end;

destructor TDubProject.destroy;
begin
  if not inGroup and fHasLoaded then
    storePersistentConfigId();
  killProcess(fDubProc);
  subjProjClosing(fProjectSubject, self);
  fProjectSubject.free;

  fJSON.Free;
  fBuildTypes.Free;
  fConfigs.Free;
  fSrcs.Free;
  fImportPaths.Free;
  inherited;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICommonProject: project props ---------------------------------------}
procedure TDubProject.activate;
begin
  subjProjFocused(fProjectSubject, fAsProjectItf);
end;

function TDubProject.inGroup: boolean;
begin
  exit(fInGroup);
end;

procedure TDubProject.inGroup(value: boolean);
begin
  fInGroup:=value;
end;


function TDubProject.getFormat: TProjectFormat;
begin
  exit(pfDUB);
end;

function TDubProject.getProject: TObject;
begin
  exit(self);
end;

function TDubProject.modified: boolean;
begin
  exit(fModified);
end;

function TDubProject.filename: string;
begin
  exit(fFilename);
end;

function TDubProject.basePath: string;
begin
  exit(fBasePath);
end;

procedure TDubProject.reload;
begin
  if fFilename.fileExists then
    loadFromFile(fFilename);
end;

procedure TDubProject.loadFromFile(const fname: string);
var
  List: TStringList;
  Stream: TStringStream;
  parser : TJSONParser;
  ext: string;
begin
  fFilename := fname;
  if not FilenameIsAbsolute(fFilename) then
    fFilename := ExpandFileName(fFilename);
  ext := fFilename.extractFileExt.upperCase;
  fBasePath := fFilename.extractFilePath;
  fSaveAsUtf8 := false;
  fIsSdl := false;
  if ext = '.JSON' then
  begin
    List := TStringList.Create;
    List.LoadFromFile(fFileName, TEncoding.UTF8);
    Stream := TStringStream.Create(List.Text, TEncoding.UTF8, False);
    List.Free;
    Stream.Position := 0;
    try
      FreeAndNil(fJSON);
      parser := TJSONParser.Create(Stream, [joUTF8, joIgnoreTrailingComma]);
      try
        try
          fJSON := TJSONObject(parser.Parse);
        except
          if assigned(fJSON) then
            FreeAndNil(fJSON);
          fFilename := '';
        end;
      finally
        parser.Free;
      end;
    finally
      Stream.Free;
    end;
  end
  else if ext = '.SDL' then
  begin
    FreeAndNil(fJSON);
    fJSON := sdl2json(fFilename);
    if fJSON.isNil then
      fFilename := ''
    else
      fIsSdl := true;
  end;

  if not assigned(fJSON) then
  begin
    fHasLoaded := false;
    fJson := TJSONObject.Create(['name','invalid json'])
  end
  else
    fHasLoaded := true;

  updateFields;
  if not inGroup then
    restorePersistentConfigId();

  subjProjChanged(fProjectSubject, self);
  fModified := false;
end;                

procedure TDubProject.saveToFile(const fname: string);
var
  saver: TMemoryStream;
  str: string;
begin
  if fname <> fFilename then
    inGroup(false);
  saver := TMemoryStream.Create;
  try
    fFilename := fname;
    str := fJSON.FormatJSON;
    if fSaveAsUtf8 then
    begin
      saver.WriteDWord($00BFBBEF);
      saver.Position:=saver.Position-1;
    end;
    saver.Write(str[1], str.length);
    saver.SaveToFile(fFilename);
  finally
    saver.Free;
    fModified := false;
  end;
end;

function TDubProject.binaryKind: TProjectBinaryKind;
begin
  exit(fBinKind);
end;

function TDubProject.getCommandLine: string;
var
  str: TStringList;
begin
  result := '';
  str := TStringList.Create;
  try
    str.Add('dub' + exeExt);
    str.Add('build');
    str.Add('--build=' + fBuildTypes[fBuiltTypeIx]);
    if (fConfigs.Count <> 1) and (fConfigs[0] <> DubDefaultConfigName) then
      str.Add('--config=' + fConfigs[fConfigIx]);
    str.Add('--compiler=' + DubCompilerFilename);
    dubBuildOptions.getOpts(str);
    result := str.Text;
  finally
    str.Free;
  end;
end;

function TDubProject.outputFilename: string;
begin
  exit(fOutputFileName);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICommonProject: sources ---------------------------------------------}
function TDubProject.isSource(const fname: string): boolean;
var
  str: string;
begin
  str := fname;
  if str.fileExists then
    str := ExtractRelativepath(fBasePath, str);
  result := fSrcs.IndexOf(str) <> -1;
end;

function TDubProject.sourcesCount: integer;
begin
  exit(fSrcs.Count);
end;

function TDubProject.sourceRelative(index: integer): string;
begin
  exit(fSrcs[index]);
end;

function TDubProject.sourceAbsolute(index: integer): string;
var
  fname: string;
begin
  fname := fSrcs[index];
  if FilenameIsAbsolute(fname) then
    result := fname
  else
    result := expandFilenameEx(fBasePath, fname);
end;

function TDubProject.importsPathCount: integer;
begin
  result := fImportPaths.Count;
end;

function TDubProject.importPath(index: integer): string;
begin
  result := expandFilenameEx(fBasePath, fImportPaths[index]);
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICommonProject: configs ---------------------------------------------}
procedure TDubProject.restorePersistentConfigId;
var
  f: string;
  t: string;
  c: string;
  i: integer;
begin
  f := fBasePath + DirectorySeparator + '.dub' + DirectorySeparator + '.editor_meta_data.ini';
  if f.fileExists then
    with TStringList.Create do
  try
    try
      LoadFromFile(f);
    except
    end;
    t := values['last_dexed_buildType'];
    c := values['last_dexed_config'];
    if t.isNotEmpty and c.isNotEmpty then
      for i := 0 to configurationCount-1 do
        if configurationName(i) = t + ' - ' + c then
    begin
      setActiveConfigurationIndex(i);
      break;
    end;
  finally
    free;
  end;
end;

procedure TDubProject.storePersistentConfigId;
var
  f: string;
  n: string;
  c: string;
  t: string;
  p: integer;
  i: integer;
begin
  i := getActiveConfigurationIndex();
  n := configurationName(i);
  p := Pos(' ', n);
  if (p < 4) and (p + 5 < n.length) then
    exit;

  t := n[1..p-1];
  c := n[p + 3 .. n.length];
  if not DirectoryExists(fBasePath + '.dub') then
    CreateDir(fBasePath + '.dub');
  f := fBasePath + '.dub' + DirectorySeparator + '.editor_meta_data.ini';
  with TStringList.Create do
  try
    values['last_dexed_buildType'] := t;
    values['last_dexed_config'] := c;
    try
      SaveToFile(f);
    except
    end;
  finally
    free;
  end;
end;

function TDubProject.configurationCount: integer;
begin
  exit(fConfigsCount);
end;

function TDubProject.getActiveConfigurationIndex: integer;
begin
  exit(fBuiltTypeIx * fConfigs.Count + fConfigIx);
end;

procedure TDubProject.setActiveConfigurationIndex(index: integer);
begin
  fBuiltTypeIx := index div fConfigs.Count;
  fConfigIx := index mod fConfigs.Count;
  doModified;
  // DUB does not store an active config
  fModified:=false;
end;

function TDubProject.configurationName(index: integer): string;
begin
  result := fBuildTypes[index div fConfigs.Count] + ' - ' +
    fConfigs[index mod fConfigs.Count];
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION ICommonProject: actions ---------------------------------------------}
procedure TDubProject.stopCompilation;
begin
  if fDubProc.isNotNil and fDubProc.Running then
    fDubProc.Terminate(1);
end;

procedure TDubProject.dubProcOutput(proc: TObject);
var
  lst: TStringList;
  str: string;
begin
  lst := TStringList.Create;
  try
    fDubProc.getFullLines(lst);
    for str in lst do
      fMsgs.message(str, fAsProjectItf, amcProj, amkAuto);
  finally
    lst.Free;
  end;
end;

procedure TDubProject.dubProcTerminated(proc: TObject);
var
  n: string;
begin
  dubProcOutput(proc);
  n := shortenPath(filename);
  if fNextTerminatedCommand = dcBuild then
    fCompiled := fDubProc.ExitStatus = 0;
  // note: fCompiled is also used to indicate if there's something produced
  // so the 'or' RHS is there for fNextTerminatedCommand <> dcBuild;
  if fCompiled or (fDubProc.ExitStatus = 0) then
  begin
    fMsgs.message(n + ' has been successfully ' +
      dubCmd2PostMsg[fNextTerminatedCommand], fAsProjectItf, amcProj, amkInf)
  end
  else
  begin
    fMsgs.message(n + ' has not been successfully ' +
      dubCmd2PostMsg[fNextTerminatedCommand], fAsProjectItf, amcProj, amkWarn);
    fMsgs.message(format('error: DUB has returned the status %s',
      [prettyReturnStatus(fDubProc)]), fAsProjectItf, amcProj, amkErr);
  end;
  subjProjCompiled(fProjectSubject, fAsProjectItf, fCompiled);
  SetCurrentDirUTF8(fPreCompilePath);
end;

procedure TDubProject.executeDub(command: TDubCommand; const runArgs: string = '');
var
  olddir: string;
  prjname: string;
  rargs: TStringList;
begin
  if fDubProc.isNotNil and fDubProc.Active then
  begin
    fMsgs.message('the project is already being processed by DUB', fAsProjectItf, amcProj, amkWarn);
    exit;
  end;
  killProcess(fDubProc);
  fCompiled := false;
  if not fFilename.fileExists then
  begin
    dlgOkInfo('The project must be saved before being ' +
      dubCmd2PreMsg[command] + 'by DUB !');
    exit;
  end;
  fNextTerminatedCommand := command;
  fMsgs.clearByData(fAsProjectItf);
  prjname := shortenPath(fFilename);
  fDubProc:= TDexedProcess.Create(nil);
  olddir  := GetCurrentDir;
  try
    subjProjCompiling(fProjectSubject, fAsProjectItf);
    fMsgs.message(dubCmd2PreMsg[command] + prjname, fAsProjectItf, amcProj, amkInf);
    if modified then
      saveToFile(fFilename);
    chDir(fFilename.extractFilePath);
    fDubProc.Executable := 'dub' + exeExt;
    if not dubBuildOptions.showConsole then
    begin
      fDubProc.Options := fDubProc.Options + [poStderrToOutPut, poUsePipes];
      fDubProc.OnReadData:= @dubProcOutput;
      fDubProc.ShowWindow := swoHIDE;
    end
    else
    begin
      fDubProc.Options := fDubProc.Options + [poWaitOnExit, poNewConsole];
    end;
    fDubProc.CurrentDirectory := fFilename.extractFilePath;
    fDubProc.XTermProgram:=consoleProgram;
    fDubProc.Parameters.Add(dubCmd2Arg[command]);
    fDubProc.OnTerminate:= @dubProcTerminated;
    if (command <> dcTest) or not dubBuildOptions.autoSelectTestConfig then
    begin
      fDubProc.Parameters.Add('--build=' + fBuildTypes[fBuiltTypeIx]);
      if (fConfigs.Count <> 1) and (fConfigs[0] <> DubDefaultConfigName) then
        fDubProc.Parameters.Add('--config=' + fConfigs[fConfigIx]);
    end;
    fDubProc.Parameters.Add('--compiler=' + DubCompilerFilename);
    dubBuildOptions.getOpts(fDubProc.Parameters);
    if (command <> dcBuild) and runArgs.isNotEmpty then
    begin
      fDubProc.Parameters.Add('--');
      rargs := TStringList.Create;
      try
        CommandToList(runArgs, rargs);
        fDubProc.Parameters.AddStrings(rargs);
      finally
        rargs.Free;
      end;
    end;
    fDubProc.Execute;
  finally
    SetCurrentDirUTF8(olddir);
  end;
end;

procedure TDubProject.compile;
begin
  fPreCompilePath := GetCurrentDirUTF8;
  executeDub(dcBuild);
end;

function TDubProject.compiled: boolean;
begin
  exit(fCompiled);
end;

procedure TDubProject.run(const runArgs: string = '');
begin
  executeDub(dcRun, runArgs);
end;

procedure TDubProject.test;
begin
  executeDub(dcTest);
end;

function TDubProject.targetUpToDate: boolean;
begin
  // rebuilding is done automatically when the command is 'run'
  result := true;
end;
{$ENDREGION --------------------------------------------------------------------}

{$REGION JSON to internal fields -----------------------------------------------}
function TDubProject.getCurrentCustomConfig: TJSONObject;
var
  confs: TJSONArray;
begin
  result := nil;
  if fJSON.findArray('configurations', confs) and (fConfigIx < confs.Count) then
    result := confs.Objects[fConfigIx];
end;

procedure TDubProject.updatePackageNameFromJson;
var
  value: TJSONData;
begin
  if fJSON.isNil then
    exit;
  if not fJSON.findAny('name', value) then
    fPackageName := ''
  else
      fPackageName := value.AsString;
end;

procedure TDubProject.udpateConfigsFromJson;
var
  i: integer;
  dat: TJSONData;
  arr: TJSONArray = nil;
  item: TJSONObject = nil;
  obj: TJSONObject = nil;
  itemname: string;
begin
  fBuildTypes.Clear;
  fConfigs.Clear;
  if fJSON.isNil then
    exit;
  // the CE interface for dub doesn't make the difference between build type
  //and config, instead each possible combination type + build is generated.
  if fJSON.findArray('configurations', arr) and (arr.Count > 0) then
  begin
    for i:= 0 to arr.Count-1 do
    begin
      item := TJSONObject(arr.Items[i]);
      if item.findAny('name', dat) then
        fConfigs.Add(dat.AsString);
    end;
  end else
  begin
    fConfigs.Add(DubDefaultConfigName);
    // default = what dub set as 'application' or 'library'
    // in this case dexed will pass only the type to DUB: 'DUB --build=release'
  end;

  fBuildTypes.AddStrings(DubBuiltTypeName);
  if fJSON.findObject('buildTypes', obj) then for i := 0 to obj.Count-1 do
  begin
    itemname := obj.Names[i];
    // defaults build types can be overridden
    if fBuildTypes.IndexOf(itemname) <> -1 then
      continue;
    fBuildTypes.Add(itemname);
  end;

  deleteDups(fConfigs);
  deleteDups(fBuildTypes);
  fConfigsCount := fConfigs.Count * fBuildTypes.Count;
end;

procedure TDubProject.updateSourcesList;
begin
  updateSourcesFromJson;
end;

procedure TDubProject.updateSourcesFromJson;
var
  lst: TStringList;
  item: TJSONData;
  conf: TJSONObject;
  arr: TJSONArray;
  i{, j}: integer;
procedure getExclusion(from: TJSONObject);
var
  i: integer;
begin
  if from.findArray('excludedSourceFiles', arr) then
    for i := 0 to arr.Count-1 do
      lst.Add(patchPlateformPath(arr.Strings[i]));
end;
procedure tryAddRelOrAbsFile(const fname: string);
begin
  if not isDlangCompilable(fname.extractFileExt) then
    exit;
  if fname.fileExists and FilenameIsAbsolute(fname) then
  begin
    fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, fname)))
  end
  else if patchPlateformPath(expandFilenameEx(fBasePath, fname)).fileExists then
    fSrcs.Add(patchPlateformPath(fname));
end;
procedure tryAddFromFolder(const pth: string);
var
  abs: string;
begin
  if pth.dirExists then
  begin
    lst.Clear;
    listFiles(lst, pth, true);
    for abs in lst do
      if isDlangCompilable(abs.extractFileExt) then
        fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, abs)));
  end;
end;
var
  pth: string;
  //glb: TRegExpr;
begin
  fSrcs.Clear;
  if not assigned(fJSON) then
    exit;
  lst := TStringList.Create;
  try
    // auto folders & files
    if fJSON.findAny('mainSourceFile', item) then
    begin
      pth := item.AsString;
      if pth.fileExists then
        fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, pth)))
      else if expandFilenameEx(fBasePath, pth).fileExists then
        fSrcs.Add(patchPlateformPath(pth));
    end;
    tryAddFromFolder(fBasePath + 'src');
    tryAddFromFolder(fBasePath + 'source');
    // custom folders
    if fJSON.findArray('sourcePaths', arr) then for i := 0 to arr.Count-1 do
    begin
      pth := TrimRightSet(arr.Strings[i], ['/','\']);
      if pth.dirExists and FilenameIsAbsolute(pth) then
        tryAddFromFolder(pth)
      else
        tryAddFromFolder(expandFilenameEx(fBasePath, pth));
    end;
    // custom files
    if fJSON.findArray('sourceFiles', arr) then for i := 0 to arr.Count-1 do
      tryAddRelOrAbsFile(arr.Strings[i]);
    conf := getCurrentCustomConfig;
    if conf.isNotNil then
    begin
      if conf.findAny('mainSourceFile', item) then
      begin
        pth := item.AsString;
        if pth.fileExists then
          fSrcs.Add(patchPlateformPath(ExtractRelativepath(fBasePath, pth)))
        else if expandFilenameEx(fBasePath, pth).fileExists then
          fSrcs.Add(patchPlateformPath(pth));
      end;
      // custom folders in current config
      if conf.findArray('sourcePaths', arr) then for i := 0 to arr.Count-1 do
      begin
        pth := TrimRightSet(arr.Strings[i], ['/','\']);
        if pth.dirExists and FilenameIsAbsolute(pth) then
          tryAddFromFolder(pth)
        else
          tryAddFromFolder(expandFilenameEx(fBasePath, pth));
      end;
      // custom files in current config
      if conf.findArray('sourceFiles', arr) then for i := 0 to arr.Count-1 do
        tryAddRelOrAbsFile(arr.Strings[i]);
    end;
    // exclusions : not managed anymore because of other IDE features that rely
    // on the full list (scan TODOs, <CPFS>, search in project, etc)
    {lst.Clear;
    getExclusion(fJSON);
      conf := getCurrentCustomConfig;
    if conf.isNotNil then
      getExclusion(conf);
    if lst.Count > 0 then
    begin
      glb := TRegExpr.Create;
      try
        for j := 0 to lst.Count-1 do
        begin
          try
            glb.Expression := globToReg(lst[j]);
            glb.Compile;
            for i := fSrcs.Count-1 downto 0 do
              if glb.Exec(fSrcs[i]) then
                fSrcs.Delete(i);
          except
            continue;
          end;
        end;
      finally
        glb.Free;
      end;
    end;}
  finally
    lst.Free;
  end;
  deleteDups(fSrcs);
end;

function TDubProject.findTargetKindInd(value: TJSONObject): boolean;
var
  tt: TJSONData;
begin
  result := true;
  if value.Find('mainSourceFile').isNotNil then
    fBinKind := executable
  else if value.findAny('targetType', tt) then
  begin
    case tt.AsString of
      'executable': fBinKind := executable;
      'staticLibrary', 'library' : fBinKind := staticlib;
      'dynamicLibrary' : fBinKind := sharedlib;
      'autodetect': result := false;
      else fBinKind := executable;
    end;
  end
  else result := false;
end;

procedure TDubProject.updateTargetKindFromJson;
var
  found: boolean = false;
  conf: TJSONObject;
  src: string;
begin
  fBinKind := executable;
  if fJSON.isNil then exit;
  // note: this is only used to known if output can be launched
  found := findTargetKindInd(fJSON);
  conf := getCurrentCustomConfig;
  if conf.isNotNil then
    found := found or findTargetKindInd(conf);
  if not found then
  begin
    for src in fSrcs do
    begin
      if (src = 'source' + DirectorySeparator + 'app.d')
        or (src = 'src' + DirectorySeparator + 'app.d')
        or (src = 'source' + DirectorySeparator + 'main.d')
        or (src = 'src' + DirectorySeparator + 'main.d')
        or (src = 'source' + DirectorySeparator + fPackageName + DirectorySeparator + 'app.d')
        or (src = 'src' + DirectorySeparator + fPackageName + DirectorySeparator + 'app.d')
        or (src = 'source' + DirectorySeparator + fPackageName + DirectorySeparator + 'main.d')
        or (src = 'src' + DirectorySeparator + fPackageName + DirectorySeparator + 'main.d')
      then
      begin
        fBinKind:= executable;
        break;
      end
      else
        fBinKind:= staticlib;
    end;
  end;
end;

procedure TDubProject.updateImportPathsFromJson;

  procedure addFrom(obj: TJSONObject);
  var
    arr: TJSONArray;
    pth: string;
    i: integer;
  begin
    if obj.findArray('importPaths', arr) then for i := 0 to arr.Count-1 do
    begin
      pth := TrimRightSet(arr.Strings[i], ['/','\']);
      if pth.dirExists and FilenameIsAbsolute(pth) then
        fImportPaths.Add(pth)
      else
        fImportPaths.Add(expandFilenameEx(fBasePath, pth));
    end;
  end;

  // note: dependencies are added as import to allow DCD completion
  // see TDcdWrapper.projChanged()
  procedure addDepsFrom(obj: TJSONObject; const suffix: string = '');
  var
    deps: TJSONObject;
    pck: PDubLocalPackage;
    j: TJSONData;
    p: string;
    s: string;
    v: string;
    n: string;
    o: string;
    r: TStringRange = (ptr: nil; pos: 0; len: 0);
    q: TSemVer;
    u: PSemVer;
    i: integer;
    k: integer;
    c: TJSONObject;
    b: TStringList;
  begin
    if obj.findObject('dependencies' + suffix, deps) then
    begin

      b := TStringList.Create;
      getPackagesLocations(b);

      try for i := 0 to deps.Count-1 do
      begin
        n := deps.Names[i];

        // local path specified
        if deps.findObject(n, c) and c.findAny('path', j) then
        begin
          s := expandFilenameEx(fBasePath, j.AsString);
          if (s + 'source').dirExists then
            fImportPaths.Add(s)
          else if (s + 'src').dirExists then
            fImportPaths.Add(s);
          continue;
        end;

        // Try to fetch if not present at all
        if not TDubLocalPackages.find(n, pck) and dubBuildOptions.autoFetch then
        begin
          with TProcess.Create(nil) do
          try
            Executable := exeFullName('dub' + exeExt);
            Options := Options + [poUsePipes];
            ShowWindow:= swoHIDE;
            Parameters.Add('fetch');
            Parameters.Add(n);
            Execute;
            while Running do ;
            if ExitStatus = 0 then
              TDubLocalPackages.update();
          finally
            free;
          end;
        end;

        if TDubLocalPackages.find(n, pck) then
        begin

          j := deps.Items[i];
          if j.JSONType <> TJSONtype.jtString then
            continue;

          //split version operator and version number
          v := j.AsString;
          r.init(v);
          o := r.takeUntil(['0'..'9']).yield;
          p := r.takeUntil(#0).yield;
          if p = '*' then
          begin
            o := '>=';
            p := '0.0.0';
          end
          else if (p = 'master') or (v = '~master') then
            q.init('v0.0.0-master')
          else
            q.init('v' + p);

          // Finds a match for the version in the local packages list.
          u := TDubLocalPackages.find(n, o, q, pck);

          // Try to fetch the right version if no match
          if not assigned(u) and dubBuildOptions.autoFetch then
          begin
            with TProcess.Create(nil) do
            try
              Executable := exeFullName('dub' + exeExt);
              Options := Options + [poUsePipes];
              ShowWindow:= swoHIDE;
              Parameters.Add('fetch');
              Parameters.Add(n);
              Parameters.Add('--version=' + p);
              Execute;
              while Running do ;
              if ExitStatus = 0 then
              begin
                TDubLocalPackages.update();
                u := TDubLocalPackages.find(n, o, q, pck);
              end;
            finally
              free;
            end;
          end;

          // Set the imports, used in particular by DCD
          if assigned(u) then
          begin
            for k := 0 to b.Count-1 do
            begin
              s := b[k] + n;
              p :=  s + '-' + u^.asString + DirectorySeparator + n + DirectorySeparator;
              if (p + 'source').dirExists then
              begin
                fImportPaths.Add(p + 'source')  ;
                break;
              end
              else if (p + 'src').dirExists then
              begin
                fImportPaths.Add(p + 'src');
                break;
              end;
            end;
          end;
        end;
      end;
      finally
        b.Free;
      end;
    end;
  end;

var
  conf: TJSONObject;
begin
  if fJSON.isNil then
    exit;

  addFrom(fJSON);
  addDepsFrom(fJSON);
  {$IFDEF WINDOWS}
  addDepsFrom(fJSON, '-windows');
  {$ENDIF}
  {$IFDEF LINUX}
  addDepsFrom(fJSON, '-linux');
  {$ENDIF}
  {$IFDEF DARWIN}
  addDepsFrom(fJSON, '-osx');
  {$ENDIF}
  {$IFDEF UNIX}
  addDepsFrom(fJSON, '-posix');
  {$ENDIF}

  conf := getCurrentCustomConfig;
  if conf.isNotNil then
  begin
    addFrom(conf);
    addDepsFrom(conf);
    {$IFDEF WINDOWS}
    addDepsFrom(conf, '-windows');
    {$ENDIF}
    {$IFDEF LINUX}
    addDepsFrom(conf, '-linux');
    {$ENDIF}
    {$IFDEF DARWIN}
    addDepsFrom(conf, '-osx');
    {$ENDIF}
    {$IFDEF UNIX}
    addDepsFrom(conf, '-posix');
    {$ENDIF}
  end;
end;

procedure TDubProject.updateOutputNameFromJson;
var
  conf: TJSONObject;
  item: TJSONData;
  namePart, pathPart: string;
  procedure setFrom(obj: TJSONObject);
  var
    n,p: TJSONData;
  begin
    if obj.findAny('targetPath', p) then
      pathPart := p.AsString;
    if obj.FindAny('targetName', n) then
      namePart := n.AsString;
  end;
begin
  fOutputFileName := '';
  if fJSON.isNil or not fJSON.findAny('name', item) then
    exit;

  namePart := item.AsString;
  pathPart := fBasePath;
  setFrom(fJSON);
  conf := getCurrentCustomConfig;
  if conf.isNotNil then
    setFrom(conf);
  pathPart := TrimRightSet(pathPart, ['/','\']);
  {$IFNDEF WINDOWS}
  if fBinKind in [staticlib, sharedlib] then
    namePart := 'lib' + namePart;
  {$ENDIF}
  fOutputFileName:= pathPart + DirectorySeparator + namePart;
  patchPlateformPath(fOutputFileName);
  fOutputFileName := expandFilenameEx(fBasePath, fOutputFileName);
  case fBinKind of
    executable: fOutputFileName += exeExt;
    staticlib: fOutputFileName += libExt;
    obj: fOutputFileName += objExt;
    sharedlib: fOutputFileName += dynExt;
  end;
end;

procedure TDubProject.updateFields;
begin
  updatePackageNameFromJson;
  udpateConfigsFromJson;
  updateSourcesFromJson;
  updateTargetKindFromJson;
  updateImportPathsFromJson;
  updateOutputNameFromJson;
end;

procedure TDubProject.beginModification;
begin
  fModificationCount += 1;
end;

procedure TDubProject.endModification;
begin
  fModificationCount -=1;
  if fModificationCount <= 0 then
    doModified;
end;

procedure TDubProject.doModified;
begin
  fModificationCount := 0;
  fModified:=true;
  updateFields;
  subjProjChanged(fProjectSubject, fAsProjectItf);
end;
{$ENDREGION}

{$REGION Miscellaneous DUB free functions --------------------------------------}
function sdl2json(const filename: string): TJSONObject;
var
  dub: TProcess;
  str: TStringList;
  jsn: TJSONData;
  prs: TJSONParser;
  old: string;
begin
  result := nil;
  dub := TProcess.Create(nil);
  str := TStringList.Create;
  old := GetCurrentDirUTF8;
  try
    SetCurrentDirUTF8(filename.extractFilePath);
    dub.Executable := 'dub' + exeExt;
    dub.Options := [poUsePipes{$IFDEF WINDOWS}, poNewConsole{$ENDIF}];
    dub.ShowWindow := swoHIDE;
    dub.CurrentDirectory:= filename.extractFilePath;
    dub.Parameters.Add('convert');
    dub.Parameters.Add('-s');
    dub.Parameters.Add('-f');
    dub.Parameters.Add('json');
    dub.Execute;
    processOutputToStrings(dub, str);
    while dub.Running do;
    prs := TJSONParser.Create(str.Text, [joIgnoreTrailingComma, joUTF8]);
    try
      try
        jsn := prs.Parse;
        try
          if jsn.isNotNil then
            result := TJSONObject(jsn.Clone)
          else
            result := nil;
        finally
          jsn.free;
        end;
      finally
        prs.Free
      end;
    except
      result := nil;
    end;
  finally
    SetCurrentDirUTF8(old);
    dub.free;
    str.Free;
  end;
end;

function isValidDubProject(const filename: string): boolean;
var
  maybe: TDubProject;
  ext: string;
begin
  ext := filename.extractFileExt.upperCase;
  if (ext <> '.JSON') and (ext <> '.SDL') then
    exit(false);
  result := true;
  // avoid the project to notify the observers, current project is not replaced
  EntitiesConnector.beginUpdate;
  maybe := TDubProject.create(nil);
  try
    try
      maybe.loadFromFile(filename);
      if maybe.json.isNil or maybe.filename.isEmpty then
        result := false
      else if maybe.json.Find('name').isNil then
        result := false;
    except
      result := false;
    end;
  finally
    maybe.Free;
    EntitiesConnector.endUpdate;
  end;
end;

function getDubCompiler: DCompiler;
begin
  exit(DubCompiler);
end;

procedure setDubCompiler(value: DCompiler);
var
  sel: ICompilerSelector;
begin
  sel := getCompilerSelector;
  DubCompiler := value;
  if not sel.isCompilerValid(DubCompiler) then
    DubCompiler := dmd;
  DubCompilerFilename:=sel.getCompilerPath(DubCompiler);
end;
{$ENDREGION}

initialization
  setDubCompiler(dmd);
  dubBuildOptions:= TDubBuildOptions.create(nil);
finalization
  dubBuildOptions.free;
  TDubLocalPackages.deinit;
end.

