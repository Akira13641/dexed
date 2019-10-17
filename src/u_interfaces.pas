unit u_interfaces;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, actnList, menus, process,
  u_synmemo, u_observer;

type

  // describes the project kind. Used as a hint to cast ICommonProject.getProject()
  TProjectFormat = (pfDEXED, pfDUB);

  // describes the binary kind produces when compiling a project
  TProjectBinaryKind = (executable, staticlib, sharedlib, obj);

  (**
   * Common project interface.
   *
   * Each project format has its own dedicated editors.
   * A few common properties allow some generic operations whatever is the format.
   *)
  ICommonProject = interface
  ['ICommonProject']

    // general properties ------------------------------------------------------

      // indicates if the project is owned by a group.
      function inGroup: boolean;
      // flag the project as grouped
      procedure inGroup(value: boolean);
      // in a context of a group, activates the project
      procedure activate;
      // indicates the project format
      function getFormat: TProjectFormat;
      // returns an untyped object that can be casted using getFormat()
      function getProject: TObject;
      // returns the project filename
      function filename: string;
      // loads project from filename
      procedure loadFromFile(const fname: string);
      // saves project to filename
      procedure saveToFile(const fname: string);
      // reloads from filename
      procedure reload;
      // indicates of the project is modified (should be saved or not)
      function modified: boolean;
      // returns the base path used to solve relative locations
      function basePath: string;
      // returns the name of the file produced when a project is compiled
      function outputFilename: string;
      // returns the binary kind produced according to the current configuration
      function binaryKind: TProjectBinaryKind;
      // returns what's gonna be executed in background for this config
      function getCommandLine: string;
      // stops compilation
      procedure stopCompilation;

    // configs -----------------------------------------------------------------

      // returns the count of configuration
      function configurationCount: integer;
      // sets the active configuration
      procedure setActiveConfigurationIndex(index: integer);
      // returns the name of the index-th configuration
      function configurationName(index: integer): string;
      // return the index of the active configration index
      function getActiveConfigurationIndex: integer;

    // project sources ---------------------------------------------------------

      // returns the count of source files for the current config
      function sourcesCount: integer;
      // returns the source absolute filename.
      function sourceAbsolute(index: integer): string;
      // returns the source relative filename.
      function sourceRelative(index: integer): string;
      // returns true if aFilename is a project source.
      function isSource(const aFilename: string): boolean;
      // returns the count of import paths for the current config
      function importsPathCount: integer;
      // returns the import absolute path
      function importPath(index: integer): string;

    // sub routines for the actions --------------------------------------------

      // tries to compile.
      procedure compile;
      // indicates wether last complation was successful.
      function compiled: boolean;
      // tries to execute the project output.
      procedure run(const runArgs: string = '');
      // test the project (only for DUB)
      procedure test;
      // returns true if the target has not to be recompiled
      function targetUpToDate: boolean;

  end;



  (**
   * An implementer declares some actions on demand.
   *)
  IContextualActions = interface(IObserverType)
  ['IContextualActions']
    // declares a context name for the actions
    function contextName: string;
    // action count, called before contextAction()
    function contextActionCount: integer;
    // declares actions, called in loop, from 0 to contextActionCount-1
    function contextAction(index: integer): TAction;
  end;



  (**
   * An implementer is informed about the current file(s).
   *)
  IDocumentObserver = interface(IObserverType)
  ['IDocumentObserver']
    // document has been created (empty, runnable, project source, ...).
    procedure docNew(document: TDexedMemo);
    // document is the document being edited.
    procedure docFocused(document: TDexedMemo);
    // document content has just been modified (edited, saved).
    procedure docChanged(document: TDexedMemo);
    // document is about to be closed.
    procedure docClosing(document: TDexedMemo);
  end;
  (**
   * An implementer informs the IMultiDocObserver about the current file(s)
   *)
  TMultiDocSubject = specialize TCustomSubject<IDocumentObserver>;



  (**
   * An implementer is informed about the current project(s).
   * Usually observer should keep track of two ICommonProject:
   * - the "free standing project" (FSP): the project that's not in a group and
   * that has to be freed manualy in order to be replaced.
   * - the current project, the one that's active) which can be either the FSP
   * or one of the project in the group.
   *)
  IProjectObserver = interface(IObserverType)
  ['IProjectObserver']
    // a project has been created/opened
    procedure projNew(project: ICommonProject);
    // a project has been modified: switches, source name, ...
    procedure projChanged(project: ICommonProject);
    // a project is about to be closed.
    procedure projClosing(project: ICommonProject);
    // a project is focused, it can be inGroup or not
    procedure projFocused(project: ICommonProject);
    // project is about to be compiled, time to lock related actions.
    procedure projCompiling(project: ICommonProject);
    // project compilation is finsihed, related actions can be unlocked.
    procedure projCompiled(project: ICommonProject; success: boolean);
  end;
  (**
   * An implementer informs the IProjectObserver about the current project(s)
   *)
  TProjectSubject = specialize TCustomSubject<IProjectObserver>;



  (**
   * An implementer can expose customizable shortcuts to be edited in a dedicated widget.
   *)
  IEditableShortCut = interface(IObserverType)
  ['IEditableShortCut']
    // a TEditableShortCutSubject will start to collect shortcuts if result.
    function scedWantFirst: boolean;
    // a TEditableShortCutSubject collects the information on the shortcuts while result.
    function scedWantNext(out category, identifier: string; out aShortcut: TShortcut): boolean;
    // a TEditableShortCutSubject sends the possibly modified shortcut.
    procedure scedSendItem(const category, identifier: string; aShortcut: TShortcut);
    // a TEditableShortCutSubject has finished to send the shortcuts.
    procedure scedSendDone;
  end;
  (**
   * An implementer manages its observers shortcuts.
   *)
  TEditableShortCutSubject = specialize TCustomSubject<IEditableShortCut>;

  (**
   * Listen to mini explorer changes. Mostly made to prevent redundant updates
   * of the symbolic string translater.
   *)
  IMiniExplorerObserver = interface(IObserverType)
  ['IMiniExplorerObserver']
    procedure mnexDirectoryChanged(const directory: string);
  end;
  (**
   * Mini-explorer implements this.
   *)
  TMiniExplorerSubject = specialize TCustomSubject<IMiniExplorerObserver>;


  // the option editor uses this value as a hint to display an option container.
  TOptionEditorKind = (
    oekGeneric, // the editor will display the publications of the TPersistent passed in optionedWantContainer
    oekForm,    // the editor will cast the result of optionedWantContainer as a TForm and host this form
    oekControl  // the editor will cast the result of optionedWantContainer as a TControl and host this control
  );
  // event generated by the option editor and passed to an IEditableOptions.
  TOptionEditorEvent = (
    oeeCancel,    // the "cancel" button of the option editor is pressed
    oeeAccept,    // the "accept" button of the option editor is pressed
    oeeChange,    // the properties of the container has changed, only happens if the container is oekGeneric.
    oeeSelectCat  // the container will be displayed.
  );
  (**
   * An implementer can expose options to be edited in a dedicated widget.
   *)
  IEditableOptions = interface(IObserverType)
  ['IEditableOptions']
    // the widget wants the category.
    function optionedWantCategory(): string;
    // the widget wants to know if the options will use a generic editor or a custom form.
    function optionedWantEditorKind: TOptionEditorKind;
    // the widget wants the custom option editor TCustomForm, TWinControl or the TPersistent containing the options.
    function optionedWantContainer: TPersistent;
    // the option editor informs that something has happened.
    procedure optionedEvent(event: TOptionEditorEvent);
    // the option editor wants to know if an editor allows another category to be displayed (not called for oekGeneric).
    function optionedOptionsModified: boolean;
  end;
  (**
   * An implementer displays its observers editable options.
   *)
  TEditableOptionsSubject = specialize TCustomSubject<IEditableOptions>;



  /// describes the message kind, 'amkAuto' implies that an ILogMessageObserver guess the kind.
  TAppMessageKind = (amkAuto, amkBub, amkInf, amkHint, amkWarn, amkErr);
  /// describes the message context. Used by a ILogMessageObserver to filter the messages.
  TAppMessageCtxt = (
    amcAll,         // used as filter
    amcEdit,        // used as filter
    amcProj,        // used as filter
    amcApp,         // used as filter
    amcMisc,        // used as filter
    amcAutoEdit,    // same as amcEdit but the message data is set automatically by the IMessagesDisplay
    amcAutoProj,    // same as amcProj but the message data is set automatically by the IMessagesDisplay
    amcAutoCompile  // same as amcAutoEdit or amcAutoProj but set by the IMessagesDisplay according to what's being compiled.
  );

  TLifetimeStatus = (
    lfsLoading,  // IDE is not ready yet, other services might be nil
    lfsLoaded,   // IDE is 100% working
    lfsExiting  // IDE is quiting, other services will be nil
  );

  (**
   * Single service providing information about the IDE liftetime
   *)
  ILifetimeManager = interface(ISingleService)
    function getLifetimeStatus: TLifetimeStatus;
    function asObject: TObject;
  end;

  (**
   * Single service provided by the messages widget.
   *)
  IMessagesDisplay = interface(ISingleService)
    // displays a message.
    procedure message(const value: string; aData: Pointer; aCtxt: TAppMessageCtxt; aKind: TAppMessageKind);
    // clears the messages related to the context aCtxt.
    procedure clearByContext(aCtxt: TAppMessageCtxt);
    // clears the messages related to the data aData.
    procedure clearByData(aData: Pointer);
  end;



  (**
   * Single service provided by the process-input widget.
   *)
  IProcInputHandler = interface(ISingleService)
    // add an entry to the list of process which can receive an user input.
    procedure addProcess(aProcess: TProcess);
    // removes an entry.
    procedure removeProcess(aProcess: TProcess);
    // indicates the current process.
    function process: TProcess;
  end;



  (**
   * Single service related to the documents as a collection.
   *)
  IMultiDocHandler = interface(ISingleService)
    // returns the count of opened document.
    function documentCount: Integer;
    // returns the nth document.
    function getDocument(index: Integer): TDexedMemo;
    // returns true if the document matching aFilename is already opened.
    function findDocument(const fname: string): TDexedMemo;
    // opens or set the focus on the document matching aFilename.
    procedure openDocument(const fname: string);
    // closes the nth document.
    function closeDocument(index: Integer; promptOnChanged: boolean = true): boolean;
    // closes a particular document.
    function closeDocument(doc: TDexedMemo; promptOnChanged: boolean = true): boolean;
    // conveniance property.
    property document[index: integer]: TDexedMemo read getDocument;
  end;



  (**
   * Single service related to the project groups
   *)
  IProjectGroup = interface(ISingleService)
    // adds a project to the gtoup.
    procedure addProject(project: ICommonProject);
    // opens a group of project.
    procedure openGroup(const fname: string);
    // saves the group to a file.
    procedure saveGroup(const fname: string);
    // closes a group and initialize a new one.
    procedure closeGroup;
    // indicates wether one of the project is modified or if the group is changed.
    function groupModified: boolean;
    // indicates the group filename.
    function groupFilename: string;
    // indicates the count of project in the group.
    function projectCount: integer;
    // indicates the index of the project.
    function getProjectIndex: integer;
    // returns the nth project.
    function getProject(index: Integer): ICommonProject;
    // returns true if the nth project is modified
    function projectModified(index: integer): boolean;
    // tries to find the project named fname.
    function findProject(const fname: string): ICommonProject;
    // selects the nth project of the group.
    procedure setProjectIndex(index: Integer);
    // indicates wether a project is marked for async compilation
    function projectIsAsync(index: integer): boolean;
    // indicates the project index after reloading
    function reloadedProjectIndex: integer;
  end;



  (**
   * Single service related to the expansion of "symbolic strings".
   *)
  ISymStringExpander = interface(ISingleService)
    // expands all the symbols <IDENT> of value in result.
    function expand(const value: string): string;
  end;


  (**
   * Single service related to build-in file explorer.
   *)
  IExplorer = interface(ISingleService)
    // expands the explorer to the folder "location".
    procedure browse(const location: string);
    // returns current folder.
    function currentLocation: string;
  end;


  (**
   * Single service provided by the options editor.
   *)
  IOptionsEditor = interface(ISingleService)
    // Shows the editor. When observer is not nil, its category is selected.
    procedure showOptionEditor(observer: IEditableOptions = nil);
  end;


  DCompiler = (dmd, gdc, gdmd, ldc, ldmd, user1, user2);

  (**
   * Single service provided by the options editor.
   *)
  ICompilerSelector = interface(ISingleService)
    // Indicates wether a D compiler is usable.
    function isCompilerValid(value: DCompiler): boolean;
    // Returns a D compiler exe filename.
    function getCompilerPath(value: DCompiler): string;
    // Fills value with the runtime/phobos import paths for a particular D compiler.
    procedure getCompilerImports(value: DCompiler; paths: TStrings);
  end;

  // Returns a string indicating the which compiler will be used.
  function usingCompilerInfo(value: DCompiler): string;

type

  (**
   * Single service that provides access to the main menu.
   *)
  IMainMenu = interface(ISingleService)
    // adds a main menu entry
    function mnuAdd: TMenuItem;
    // removes a main menu entry
    procedure mnuDelete(value: TMenuItem);
  end;



  (**
   * Single service for DFMT
   *)
  ICodeFormatting = interface(ISingleService)
    // formats the focused editor
    procedure formatCurrent();
  end;


  TDCDCompletionKind = (
    dckClass,
    dckInterface,
    dckStruct,
    dckUnion,
    dckVariable,
    dckMember,
    dckReserved,
    dckFunction,
    dckEnum,
    dckEnum_member,
    dckPackage,
    dckModule,
    dckArray,
    dckAA,
    dckAlias,
    dckTemplate,
    dckMixin
  );



{
  subject primitives:

  A subject cannot necessarly provides all the informations the observers expect.
  It can compose using the following "primitives".
}

  (**
   * TMultiDocSubject primitives.
   *)
  procedure subjDocNew(aSubject: TMultiDocSubject; document: TDexedMemo);      {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjDocClosing(aSubject: TMultiDocSubject; document: TDexedMemo);  {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjDocFocused(aSubject: TMultiDocSubject; document: TDexedMemo);  {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjDocChanged(aSubject: TMultiDocSubject; document: TDexedMemo);  {$IFDEF RELEASE}inline;{$ENDIF}

  (**
   * TProjectSubject primitives.
   *)
  procedure subjProjNew(aSubject: TProjectSubject; project: ICommonProject);     {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjClosing(aSubject: TProjectSubject; project: ICommonProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjFocused(aSubject: TProjectSubject; project: ICommonProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjChanged(aSubject: TProjectSubject; project: ICommonProject); {$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjCompiling(aSubject: TProjectSubject; project: ICommonProject);{$IFDEF RELEASE}inline;{$ENDIF}
  procedure subjProjCompiled(aSubject: TProjectSubject; project: ICommonProject; success: boolean);{$IFDEF RELEASE}inline;{$ENDIF}

  (**
   * TMiniExplorerSubject primitives.
   *)
  procedure subjMnexDirectoryChanged(aSubject: TMiniExplorerSubject; const directory: string); {$IFDEF RELEASE}inline;{$ENDIF}


{
  Service getters:
}
  function getMessageDisplay(var obj: IMessagesDisplay): IMessagesDisplay; inline;
  function getMessageDisplay: IMessagesDisplay; inline;
  function getprocInputHandler: IProcInputHandler; inline;
  function getMultiDocHandler: IMultiDocHandler; inline;
  function getSymStringExpander: ISymStringExpander; inline;
  function getProjectGroup: IProjectGroup; inline;
  function getExplorer: IExplorer; inline;
  function getOptionsEditor: IOptionsEditor; inline;
  function getCompilerSelector: ICompilerSelector; inline;
  function getMainMenu: IMainMenu; inline;
  function getCodeFormatting: ICodeFormatting; inline;
  function getLifeTimeManager: ILifetimeManager; inline;

implementation

{$REGION TMultiDocSubject ----------------------------------------------------}
procedure subjDocNew(aSubject: TMultiDocSubject; document: TDexedMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IDocumentObserver).docNew(document);
end;

procedure subjDocClosing(aSubject: TMultiDocSubject; document: TDexedMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IDocumentObserver).docClosing(document);
end;

procedure subjDocFocused(aSubject: TMultiDocSubject; document: TDexedMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IDocumentObserver).docFocused(document);
end;

procedure subjDocChanged(aSubject: TMultiDocSubject; document: TDexedMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IDocumentObserver).docChanged(document);
end;
{$ENDREGION}

{$REGION TMiniExplorerSubject ------------------------------------------------}
procedure subjMnexDirectoryChanged(aSubject: TMiniExplorerSubject; const directory: string);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IMiniExplorerObserver).mnexDirectoryChanged(directory);
end;
{$ENDREGION}

{$REGION TProjectSubject -----------------------------------------------------}
procedure subjProjNew(aSubject: TProjectSubject; project: ICommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IProjectObserver).ProjNew(project);
end;

procedure subjProjClosing(aSubject: TProjectSubject; project: ICommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IProjectObserver).projClosing(project);
end;

procedure subjProjFocused(aSubject: TProjectSubject; project: ICommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IProjectObserver).projFocused(project);
end;

procedure subjProjChanged(aSubject: TProjectSubject; project: ICommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IProjectObserver).projChanged(project);
end;

procedure subjProjCompiling(aSubject: TProjectSubject; project: ICommonProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IProjectObserver).projCompiling(project);
end;

procedure subjProjCompiled(aSubject: TProjectSubject; project: ICommonProject; success: boolean);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers[i] as IProjectObserver).projCompiled(project, success);
end;
{$ENDREGION}

{$REGION ISingleService getters ----------------------------------------------}
function getMessageDisplay(var obj: IMessagesDisplay): IMessagesDisplay;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('IMessagesDisplay') as IMessagesDisplay;
  exit(obj);
end;

function getMessageDisplay: IMessagesDisplay;
begin
  exit(EntitiesConnector.getSingleService('IMessagesDisplay') as IMessagesDisplay);
end;

function getprocInputHandler(var obj: IProcInputHandler): IProcInputHandler;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('IProcInputHandler') as IProcInputHandler;
  exit(obj);
end;

function getprocInputHandler: IProcInputHandler;
begin
  exit(EntitiesConnector.getSingleService('IProcInputHandler') as IProcInputHandler);
end;

function getMultiDocHandler(var obj: IMultiDocHandler): IMultiDocHandler;
begin
  if obj = nil then
    obj := EntitiesConnector.getSingleService('IMultiDocHandler') as IMultiDocHandler;
  exit(obj);
end;

function getMultiDocHandler: IMultiDocHandler;
begin
  exit(EntitiesConnector.getSingleService('IMultiDocHandler') as IMultiDocHandler);
end;

function getSymStringExpander: ISymStringExpander;
begin
  exit(EntitiesConnector.getSingleService('ISymStringExpander') as ISymStringExpander);
end;

function getProjectGroup: IProjectGroup;
begin
  exit(EntitiesConnector.getSingleService('IProjectGroup') as IProjectGroup);
end;

function getExplorer: IExplorer;
begin
  exit(EntitiesConnector.getSingleService('IExplorer') as IExplorer);
end;

function getOptionsEditor: IOptionsEditor;
begin
  exit(EntitiesConnector.getSingleService('IOptionsEditor') as IOptionsEditor);
end;

function getCompilerSelector: ICompilerSelector;
begin
  exit(EntitiesConnector.getSingleService('ICompilerSelector') as ICompilerSelector);
end;

function getMainMenu: IMainMenu;
begin
  exit(EntitiesConnector.getSingleService('IMainMenu') as IMainMenu);
end;

function getCodeFormatting: ICodeFormatting; inline;
begin
  exit(EntitiesConnector.getSingleService('ICodeFormatting') as ICodeFormatting);
end;

function getLifeTimeManager: ILifetimeManager; inline;
begin
  exit(EntitiesConnector.getSingleService('ILifetimeManager') as ILifetimeManager);
end;
{$ENDREGION}

function usingCompilerInfo(value: DCompiler): string;
const
  c2id: array[DCompiler] of string = ('dmd', 'gdc', 'gdmd', 'ldc', 'ldmd',
    'user1', 'user2');
begin
  result := format('using %s (%s)',
    [getCompilerSelector.getCompilerPath(value), c2id[value]]);
end;

end.
