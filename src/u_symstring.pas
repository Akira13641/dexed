unit u_symstring;

{$I u_defines.inc}

interface

uses
  u_observer, sysutils, u_interfaces, u_ceproject, u_synmemo, u_common,
  u_stringrange;

type

  (**
   * Enumerates the symbol kinds, used to index an associative array.
   *)
  TExpandableSymbol = ( ENV_USER, ENV_HOME, ENV_TEMP, CAF, CAP, MEP,
                CFF, CFP, CFR, CI, CL, CPF, CPP, CPO, CPOP, CPR, CPN, CPFS, CPCD,
                CPV, CS);
const

  FirstVariableSymbol = CFF;

type

  (**
   * TSymbolExpander is designed to expand symbolic strings,
   * using the information collected from several observer interfaces.
   *)
  TSymbolExpander = class(IDocumentObserver, IProjectObserver, ISymStringExpander, IMiniExplorerObserver)
  private
    fProj: TNativeProject;
    fProjInterface: ICommonProject;
    fDoc: TDexedMemo;
    fNeedUpdate: boolean;
    fExp: IExplorer;
    fSymbols: array[TExpandableSymbol] of string;
    procedure updateSymbols;

    procedure projNew(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);

    procedure docNew(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);

    procedure mnexDirectoryChanged(const directory: string);

    function singleServiceName: string;
    function expand(const value: string): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Forms, Classes;
var
  symbolExpander: TSymbolExpander;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TSymbolExpander.Create;
begin
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
  fNeedUpdate := true;
  //
  {$IFDEF UNIX}
  fSymbols[ENV_USER] := sysutils.GetEnvironmentVariable('USER');
  fSymbols[ENV_HOME] := sysutils.GetEnvironmentVariable('HOME');
  fSymbols[ENV_TEMP] := sysutils.GetEnvironmentVariable('TMPDIR');
  {$ELSE}
  fSymbols[ENV_USER] := sysutils.GetEnvironmentVariable('USERNAME');
  fSymbols[ENV_HOME] := sysutils.GetEnvironmentVariable('HOMEPATH');
  fSymbols[ENV_TEMP] := sysutils.GetEnvironmentVariable('TEMP');
  {$ENDIF}
  fSymbols[CAF] := Application.ExeName;
  fSymbols[CAP] := fSymbols[CAF].extractFilePath;
end;

destructor TSymbolExpander.Destroy;
begin
  fNeedUpdate := false;
  EntitiesConnector.removeObserver(self);
  inherited;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TSymbolExpander.projNew(project: ICommonProject);
begin
  fProjInterface := project;
  case project.getFormat of
    pfDEXED: fProj := TNativeProject(project.getProject);
    pfDUB: fProj := nil;
  end;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.projClosing(project: ICommonProject);
begin
  if fProjInterface = project then
    fProjInterface := nil;
  if fProj = project.getProject then
    fProj := nil;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.projFocused(project: ICommonProject);
begin
  fProjInterface := project;
  case project.getFormat of
    pfDEXED: fProj := TNativeProject(project.getProject);
    pfDUB: fProj := nil;
  end;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.projChanged(project: ICommonProject);
begin
  fNeedUpdate := true;
end;

procedure TSymbolExpander.projCompiling(project: ICommonProject);
begin
end;

procedure TSymbolExpander.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TSymbolExpander.docNew(document: TDexedMemo);
begin
  fDoc := document;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.docClosing(document: TDexedMemo);
begin
  if document <> fDoc then
    exit;
  fDoc := nil;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.docFocused(document: TDexedMemo);
begin
  if (document.isNotNil) and (fDoc = document) then
    exit;
  fDoc := document;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.docChanged(document: TDexedMemo);
begin
  if document <> fDoc then
    exit;
  fNeedUpdate := true;
end;

procedure TSymbolExpander.mnexDirectoryChanged(const directory: string);
begin
  fNeedUpdate := true;
end;
{$ENDREGION}

{$REGION Symbol things ---------------------------------------------------------}
procedure TSymbolExpander.updateSymbols;
var
  hasNativeProj: boolean;
  hasProjItf: boolean;
  hasDoc: boolean;
  fname: string;
  i: Integer;
  e: TExpandableSymbol;
  str: TStringList;
const
  na = '``';
begin
  if not fNeedUpdate then
    exit;
  fNeedUpdate := false;

  hasNativeProj := fProj.isNotNil;
  hasProjItf := fProjInterface <> nil;
  hasDoc := fDoc.isNotNil;
  if not assigned(fExp) then
    fExp := getExplorer;

  for e := FirstVariableSymbol to high(TExpandableSymbol) do
    fSymbols[e] := na;

  if assigned(fExp) then
    fSymbols[MEP] := fExp.currentLocation;

  // document
  if hasDoc then
  begin
    if not fDoc.fileName.fileExists then
      fDoc.saveTempFile;
    fSymbols[CFF] := fDoc.fileName;
    fSymbols[CFR] := fSymbols[CFF].stripFileExt + exeExt;
    fSymbols[CFP] := fSymbols[CFF].extractFilePath;
    if fDoc.Identifier.isNotEmpty then
      fSymbols[CI] := fDoc.Identifier;
    fSymbols[CL] := fDoc.LineText;
    fSymbols[CS] := fDoc.SelText;
  end;
  // project interface
  if hasProjItf then
  begin
    fname := fProjInterface.filename;
    fSymbols[CPF] := fname;
    fSymbols[CPP] := fSymbols[CPF].extractFilePath;
    fSymbols[CPN] := fSymbols[CPF].extractFileName.stripFileExt;
    fSymbols[CPO] := fProjInterface.outputFilename;
    fSymbols[CPOP]:= fSymbols[CPO].extractFileDir;
    fSymbols[CPR] := fSymbols[CPP];
    if fProjInterface.sourcesCount <> 0 then
    begin
      str := TStringList.Create;
      try
        for i := 0 to fProjInterface.sourcesCount-1 do
        begin
          fname := fProjInterface.sourceAbsolute(i);
          if not isEditable(fname.extractFileExt) then
            continue;
          str.Add(fname);
        end;
        fSymbols[CPFS] := str.Text;
        if str.Count = 1 then
          fSymbols[CPCD] := str[0].extractFileDir
        else
          fSymbols[CPCD] := commonFolder(str);
      finally
        str.Free;
      end;
    end;
  end;
  if hasNativeProj then
  begin
    if fProj.fileName.fileExists then
    begin
      if fProj.version.isNotEmpty then
        fSymbols[CPV] := fProj.version;
      fSymbols[CPR] := expandFilenameEx(fProj.basePath, fProj.RootFolder);
      if fSymbols[CPR].isEmpty then
        fSymbols[CPR] := fSymbols[CPP];
    end;
  end;
  //
  for e := FirstVariableSymbol to high(TExpandableSymbol) do
    if fSymbols[e].isEmpty then
      fSymbols[e] := na;
end;

function TSymbolExpander.singleServiceName: string;
begin
  exit('ISymStringExpander');
end;

function TSymbolExpander.expand(const value: string): string;
var
  rng: TStringRange = (ptr:nil; pos:0; len: 0);
  sym: string;
begin
  Result := '';
  if value.isEmpty then
    exit;
  //
  updateSymbols;
  rng.init(value);
  while true do
  begin
    if rng.empty then
      break;
    Result += rng.takeUntil('<').yield;
    if not rng.empty and (rng.front = '<') then
    begin
      ;
      sym := rng.popFront^.takeUntil('>').yield;
      if not rng.empty and (rng.front = '>') then
      begin
        rng.popFront;
        case sym of
          'ENV_HOME': Result += fSymbols[ENV_HOME];
          'ENV_TEMP': Result += fSymbols[ENV_TEMP];
          'ENV_USER': Result += fSymbols[ENV_USER];
          //
          'AF', 'CAF', 'CoeditApplicationFile': Result += fSymbols[CAF];
          'AP', 'CAP', 'CoeditApplicationPath': Result += fSymbols[CAP];
          'MEP', 'MiniExplorerPath': Result += fSymbols[MEP];
          //
          'CFF', 'CurrentFileFile'      : Result += fSymbols[CFF];
          'CFR', 'CurrentFileRunnable'  : Result += fSymbols[CFR];
          'CFP', 'CurrentFilePath'      : Result += fSymbols[CFP];
          'CI',  'CurrentIdentifier'    : Result += fSymbols[CI];
          'CL',  'CurrentLine'          : Result += fSymbols[CL];
          'CS',  'CurrentSelection'     : Result += fSymbols[CS];
          //
          'CPF', 'CurrentProjectFile'   : Result += fSymbols[CPF];
          'CPFS','CurrentProjectFiles'  : Result += fSymbols[CPFS];
          'CPN', 'CurrentProjectName'   : Result += fSymbols[CPN];
          'CPO', 'CurrentProjectOutput' : Result += fSymbols[CPO];
          'CPOP','CurrentProjectOutputPath' : Result += fSymbols[CPOP];
          'CPP', 'CurrentProjectPath'   : Result += fSymbols[CPP];
          'CPR', 'CurrentProjectRoot'   : Result += fSymbols[CPR];
          'CPCD','CurrentProjectCommonDirectory': Result += fSymbols[CPCD];
          'CPV', 'CurrentProjectVersion': Result += fSymbols[CPV];
          //
          else Result += '<' + sym + '>';
        end;
      end
      else Result += '<' + sym;
    end;
  end;
end;
{$ENDREGION}

initialization
  symbolExpander := TSymbolExpander.Create;

finalization
  symbolExpander.Free;
end.
