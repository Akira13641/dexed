unit u_gdb;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, RegExpr, ComCtrls,
  PropEdits, GraphPropEdits, RTTIGrids, Dialogs, ExtCtrls, Menus, Buttons,
  StdCtrls, process, fpjson, typinfo, Unix, ListViewFilterEdit, SynEdit,
  ObjectInspector,
  u_common, u_interfaces, u_widget, u_processes, u_observer, u_synmemo,
  u_sharedres, u_stringrange, u_dsgncontrols, u_dialogs, u_dbgitf,
  u_ddemangle, u_writableComponent, EditBtn, strutils, u_controls;

type

  TAsmSyntax = (intel, att);

  {$IFDEF CPU64}
  TCpuRegister = (rax, rbx, rcx, rdx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13,
    r14, r15, rip);

  const stOffset = 24;
type
  TFpuRegister = (st0, st1, st2, st3, st4, st5, st6, st7);
  {$ENDIF}

  {$IFDEF CPU32}
  TCpuRegister = (eax, ebx, ecx, edx, esi, edi, ebp, esp, eip);

  const stOffset = 16;
type
  TFpuRegister = (st0, st1, st2, st3, st4, st5, st6, st7);
  {$ENDIF}


  TFLAG = (CF, PF, AF, ZF, SF, TF, &IF, DF, &OF);

  const FlagValues: array[TFlag] of word = (1, 4, 16, 64, 128, 256, 512, 1024, 2048);

type

  TFLAGS = set of TFLAG;

  TSegRegister = (CS, SS, DS, ES, FS, GS);
  {$IFDEF CPU64}
  const segOffset = 18;
  const flagOffset = 17;
  {$ELSE}
  const segOffset = 10;
  const flagOffset = 9;
  {$ENDIF}

type

  // aliased to get hex display in object inspector.
  TCpuGprValue = type PtrUInt;

  // aliased to get hex display in object inspector.
  TCPUSegValue = type word;

  // displays a TCpuRegValue in hex
  TCpuRegValueEditor = class(TIntegerProperty)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  // displays a TCPUSegValue in hex
  TCpuSegValueEditor = class(TIntegerProperty)
  public
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  TSetGprEvent = procedure(reg: TCpuRegister; val: TCpuGprValue) of object;

  // Makes a category for the general purpose registers in a object inspector
  TInspectableGPR = class(TPersistent)
  private
    fRegisters: array[TCpuRegister] of TCpuGprValue;
    fSetGprEvent: TSetGprEvent;
    procedure setRegister(index: TCpuRegister; value: TCpuGprValue);
  published
    {$IFDEF CPU64}
    property RAX: TCpuGprValue index TCpuRegister.rax read fRegisters[TCpuRegister.rax] write setRegister;
    property RBX: TCpuGprValue index TCpuRegister.rbx read fRegisters[TCpuRegister.rbx] write setRegister;
    property RCX: TCpuGprValue index TCpuRegister.rcx read fRegisters[TCpuRegister.rcx] write setRegister;
    property RDX: TCpuGprValue index TCpuRegister.rdx read fRegisters[TCpuRegister.rdx] write setRegister;
    property RSI: TCpuGprValue index TCpuRegister.rsi read fRegisters[TCpuRegister.rsi] write setRegister;
    property RDI: TCpuGprValue index TCpuRegister.rdi read fRegisters[TCpuRegister.rdi] write setRegister;
    property RBP: TCpuGprValue index TCpuRegister.rbp read fRegisters[TCpuRegister.rbp] write setRegister;
    property RSP: TCpuGprValue index TCpuRegister.rsp read fRegisters[TCpuRegister.rsp] write setRegister;
    property R8:  TCpuGprValue index TCpuRegister.r8  read fRegisters[TCpuRegister.r8] write setRegister;
    property R9:  TCpuGprValue index TCpuRegister.r9  read fRegisters[TCpuRegister.r9] write setRegister;
    property R10: TCpuGprValue index TCpuRegister.r10 read fRegisters[TCpuRegister.r10] write setRegister;
    property R11: TCpuGprValue index TCpuRegister.r11 read fRegisters[TCpuRegister.r11] write setRegister;
    property R12: TCpuGprValue index TCpuRegister.r12 read fRegisters[TCpuRegister.r12] write setRegister;
    property R13: TCpuGprValue index TCpuRegister.r13 read fRegisters[TCpuRegister.r13] write setRegister;
    property R14: TCpuGprValue index TCpuRegister.r14 read fRegisters[TCpuRegister.r14] write setRegister;
    property R15: TCpuGprValue index TCpuRegister.r15 read fRegisters[TCpuRegister.r15] write setRegister;
    property RIP: TCpuGprValue index TCpuRegister.rip read fRegisters[TCpuRegister.rip] write setRegister;
    {$ELSE}
    property EAX: TCpuGprValue index TCpuRegister.eax read fRegisters[TCpuRegister.eax] write setRegister;
    property EBX: TCpuGprValue index TCpuRegister.ebx read fRegisters[TCpuRegister.ebx] write setRegister;
    property ECX: TCpuGprValue index TCpuRegister.ecx read fRegisters[TCpuRegister.ecx] write setRegister;
    property EDX: TCpuGprValue index TCpuRegister.edx read fRegisters[TCpuRegister.edx] write setRegister;
    property ESI: TCpuGprValue index TCpuRegister.esi read fRegisters[TCpuRegister.esi] write setRegister;
    property EDI: TCpuGprValue index TCpuRegister.edi read fRegisters[TCpuRegister.edi] write setRegister;
    property EBP: TCpuGprValue index TCpuRegister.ebp read fRegisters[TCpuRegister.ebp] write setRegister;
    property ESP: TCpuGprValue index TCpuRegister.esp read fRegisters[TCpuRegister.esp] write setRegister;
    property EIP: TCpuGprValue index TCpuRegister.eip read fRegisters[TCpuRegister.eip] write setRegister;
    {$ENDIF}
  public
    constructor create(eventGPR: TSetGprEvent);
    procedure setInspectableRegister(index: TCpuRegister; value: PtrUInt);
  end;

  TSetSsrEvent = procedure(reg: TSegRegister; val: TCPUSegValue) of object;

  // Makes a category for the segment registers in a object inspector
  TInspectableSSR = class(TPersistent)
  private
    fRegisters: array[TSegRegister] of TCPUSegValue;
    fSetSsrEvent: TSetSsrEvent;
    procedure setRegister(index: TSegRegister; value: TCPUSegValue);
  published
    property CS: TCPUSegValue index TSegRegister.CS read fRegisters[TSegRegister.CS] write setRegister;
    property SS: TCPUSegValue index TSegRegister.SS read fRegisters[TSegRegister.SS] write setRegister;
    property DS: TCPUSegValue index TSegRegister.DS read fRegisters[TSegRegister.DS] write setRegister;
    property ES: TCPUSegValue index TSegRegister.ES read fRegisters[TSegRegister.ES] write setRegister;
    property FS: TCPUSegValue index TSegRegister.FS read fRegisters[TSegRegister.FS] write setRegister;
    property GS: TCPUSegValue index TSegRegister.GS read fRegisters[TSegRegister.GS] write setRegister;
  public
    constructor create(eventSSR: TSetSsrEvent);
    procedure setInspectableRegister(index: TSegRegister; value: TCPUSegValue);
  end;

  TSetFlagEvent = procedure(val: PtrUint) of object;

  TSetFprEvent = procedure(reg: TFpuRegister; val: extended) of object;

  // Makes a category for the floating point unit registers in a object inspector
  TInspectableFPR = class(TPersistent)
  private
    fRegisters: array[TFpuRegister] of extended;
    fSetFprEvent: TSetFprEvent;
    procedure setRegister(index: TFpuRegister; value: extended);
  published
    property ST0: extended index TFpuRegister.st0 read fRegisters[TFpuRegister.st0] write setRegister;
    property ST1: extended index TFpuRegister.st1 read fRegisters[TFpuRegister.st1] write setRegister;
    property ST2: extended index TFpuRegister.st2 read fRegisters[TFpuRegister.st2] write setRegister;
    property ST3: extended index TFpuRegister.st3 read fRegisters[TFpuRegister.st3] write setRegister;
    property ST4: extended index TFpuRegister.st4 read fRegisters[TFpuRegister.st4] write setRegister;
    property ST5: extended index TFpuRegister.st5 read fRegisters[TFpuRegister.st5] write setRegister;
    property ST6: extended index TFpuRegister.st6 read fRegisters[TFpuRegister.st6] write setRegister;
    property ST7: extended index TFpuRegister.st7 read fRegisters[TFpuRegister.st7] write setRegister;
  public
    constructor create(event: TSetFprEvent);
    procedure setInspectableRegister(index: TFpuRegister; value: extended);
  end;

  // Makes a category for the SSE registers in a object inspector
  TInspectableSSE = class(TPersistent)
    // interpretation is a problem:
    // 4 int ? 2 double ? 4 single ? ...
  end;

  // Stores the registers content, to be displayable in an object inspector.
  TInspectableCPU = class(TPersistent)
  private
    fFullFlags: PtrUint;
    fFlags: TFlags;
    fSetFlagEvent: TSetFlagEvent;
    fGpr: TInspectableGPR;
    fFpr: TInspectableFPR;
    fSsr: TInspectableSSR;
    procedure setFlag(value: TFlags);
  published
    property CPU: TInspectableGPR read fGpr;
    property FPU: TInspectableFPR read fFpr;
    property SEG: TInspectableSSR read fSsr;
    property FLAGS: TFlags read fFlags write setFlag;
  public
    constructor create(setGprEvent: TSetGprEvent; setSsrEvent: TSetSsrEvent;
      setFlagEvent: TSetFlagEvent; setFprEvent: TSetFprEvent);
    destructor destroy; override;
    procedure setInspectableFlags(value: PtrUint);
  end;

  // Represents an item in the call stack
  TStackItem = class(TCollectionItem)
  strict private
    fFilename: string;
    fFname: string;
    fAddress: PtrUInt;
    fLine: integer;
  public
    procedure setProperties(addr: PtrUint; fname, nme: string; lne: integer);
    property address: ptruint read fAddress;
    property filename: string read fFilename;
    property line: integer read fLine;
    property name: string read fFname;
  end;

  // The call stack
  TStackItems = class
  strict private
    fItems: TCollection;
  public
    constructor create;
    destructor destroy; override;
    procedure assignToList(list: TListView);
    procedure addItem(addr: PtrUint; fname, nme: string; lne: integer);
    procedure clear;
  end;

  // serializable breakpoint
  TPersistentBreakPoint = class(TCollectionItem)
  strict private
    fFilename: string;
    fLine: integer;
    fKind: TBreakPointKind;
  published
    property filename: string read fFilename write fFilename;
    property line: integer read fLine write fLine;
    property kind: TBreakPointKind read fKind write fKind;
  end;

  // allow to retrieve the breakpoints even if source is not openened.
  TPersistentBreakPoints = class(TWritableLfmTextComponent)
  strict private
    fItems: TCollection;
    procedure setItems(value: TCollection);
    function getItem(index: integer): TPersistentBreakPoint;
    function find(const fname: string; line: integer; kind: TBreakPointKind): boolean;
  published
    property items: TCollection read fItems write setItems;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function count: integer;
    procedure clearFile(const fname: string);
    function deleteItem(const fname: string; line: integer; kind: TBreakPointKind): boolean;
    function addItem(const fname: string; line: integer; kind: TBreakPointKind): boolean;
    property item[index: integer]: TPersistentBreakPoint read getItem; default;
  end;

  // Makes a category for the shortcuts in the option editor.
  TDebugShortcuts = class(TPersistent)
  private
    fStart, fStop, fPause, fContinue, fStep, fStepOver, fStack, fRegs,
      fVariables, fRepeatCustomEval: TShortCut;
  published
    property start: TShortCut read fStart write fStart;
    property stop: TShortCut read fStop write fStop;
    property pause: TShortCut read fPause write fPause;
    property continue: TShortcut read fContinue write fContinue;
    property step: TShortCut read fStep write fStep;
    property stepOver: TShortCut read fStepOver write fStepOver;
    property updateStack: TShortCut read fStack write fStack;
    property updateRegisters: TShortCut read fRegs write fRegs;
    property updateVariables: TShortCut read fVariables write fVariables;
    property repeatCustomEval: TShortCut read fRepeatCustomEval write fRepeatCustomEval;
  public
    procedure assign(source: TPersistent); override;
  end;

  TDlangBreakpoint = (
    onAssertError,
    onAssertErrorMsg,
    onUnittestErrorMsg,
    onRangeError,
    onFinalizeError,
    onHiddenFuncError,
    onOutOfMemoryError,
    onInvalidMemoryOperationError,
    onSwitchError,
    onUnicodeError,
    _d_throwc,
    _d_throwdwarf,
    _d_assertm,
    _d_assert,
    _d_assert_msg,
    _d_array_bounds,
    _d_arraybounds,
    _d_switch_error
  );

  TDlangBreakpoints = set of TDlangBreakpoint;

  TDebugOptionsBase = class(TWritableLfmTextComponent)
  private
    fAutoDisassemble: boolean;
    fAutoGetThreads: boolean;
    fAutoDemangle: boolean;
    fAutoGetCallStack: boolean;
    fAutoGetRegisters: boolean;
    fAutoGetVariables: boolean;
    fCommandsHistory: TStringList;
    fCustomEvalHistory: TStringList;
    fIgnoredSignals: TStringList;
    fShowGdbOutput: boolean;
    fShowOutput: boolean;
    fShowRawMiOutput: boolean;
    fShortcuts: TDebugShortcuts;
    fAsmSyntax: TAsmSyntax;
    fKeepRedirectedStreams: boolean;
    fStopAllThreadsOnBreak: boolean;
    fDlangBreakpoints: TDlangBreakpoints;
    procedure setIgnoredSignals(value: TStringList);
    procedure setCommandsHistory(value: TStringList);
    procedure setCustomEvalHistory(value: TStringList);
    procedure setShortcuts(value: TDebugShortcuts);
    procedure cleanInvalidHistoryEntries;
  published
    property asmSyntax: TAsmSyntax read fAsmSyntax write fAsmSyntax;
    property autoDisassemble: boolean read fAutoDisassemble write fAutoDisassemble;
    property autoDemangle: boolean read fAutoDemangle write fAutoDemangle;
    property autoGetCallStack: boolean read fAutoGetCallStack write fAutoGetCallStack;
    property autoGetRegisters: boolean read fAutoGetRegisters write fAutoGetRegisters;
    property autoGetVariables: boolean read fAutoGetVariables write fAutoGetVariables;
    property autoGetThreads: boolean read fAutoGetThreads write fAutoGetThreads;
    property commandsHistory: TStringList read fCommandsHistory write setCommandsHistory;
    property coreBreakingSymbols: TDlangBreakpoints read fDlangBreakpoints write fDlangBreakpoints;
    property customEvalHistory: TStringList read fCustomEvalHistory write setCustomEvalHistory;
    property ignoredSignals: TStringList read fIgnoredSignals write setIgnoredSignals;
    property keepRedirectedStreams: boolean read fKeepRedirectedStreams write fKeepRedirectedStreams default false;
    property shortcuts: TDebugShortcuts read fShortcuts write setShortcuts;
    property showGdbOutput: boolean read fShowGdbOutput write fShowGdbOutput;
    property showRawMiOutput: boolean read fShowRawMiOutput write fShowRawMiOutput;
    property showOutput: boolean read fShowOutput write fShowOutput;
    property stopAllThreadsOnBreak: boolean read fStopAllThreadsOnBreak write fStopAllThreadsOnBreak;
  protected
    procedure beforeSave; override;
    procedure afterLoad; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(source: TPersistent); override;
  end;

  TDebugOptions = class(TDebugOptionsBase, IEditableOptions)
  private
    FonChangesApplied: TNotifyEvent;
    fBackup: TDebugOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    property onChangesApplied: TNotifyEvent read FonChangesApplied write FonChangesApplied;
  end;

  TGdbState = (gsNone, gsRunning, gsPaused);

  TAddWatchPointKind = (wpkRead, wpkWrite, wpkReadWrite);

  // Persistent command line & environment of the inferior.
  TDebugeeOption = class(TCollectionItem)
  strict private
    fQueryArguments: boolean;
    fFname: string;
    fWorkingDir: TPathname;
    fAgruments: TStringList;
    fEnvPaths: TStringList;
    procedure setOptions(value: TStringList);
    procedure setEnvPaths(value: TStringList);
  published
    property environmentPaths: TStringList read fEnvPaths write fEnvPaths;
    property filename: string read fFname write fFname;
    property arguments: TStringList read fAgruments write setOptions;
    property queryArguments: boolean read fQueryArguments write fQueryArguments default false;
    property target: string read fFname;
    property workingDirectory: TPathname read fWorkingDir write fWorkingDir;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  end;

  // Store for the command line & environment of the inferior.
  TDebugeeOptions = class(TWritableLfmTextComponent)
  strict private
    fProjects: TCollection;
    procedure setProjects(value: TCollection);
    function getProjectByIndex(index: integer): TDebugeeOption;
    function getProjectByFile(const fname: string): TDebugeeOption;
    procedure cleanup;
  protected
    procedure beforeSave; override;
  published
    property projects: TCollection read fProjects write setProjects;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property projectByIndex[index: integer]: TDebugeeOption read getProjectByIndex;
    property projectByFile[const fname: string]: TDebugeeOption read getProjectByFile; default;
  end;

  TGdbEvalKind = (gekSelectedVar, gekDerefSelectedVar, gekCustom);

  { TGdbWidget }
  TGdbWidget = class(TDexedWidget, IProjectObserver, IDocumentObserver, IDebugger)
    btnContinue: TDexedToolButton;
    btnEval: TDexedToolButton;
    btnVariables: TDexedToolButton;
    btnNext: TDexedToolButton;
    btnOver: TDexedToolButton;
    btnPause: TDexedToolButton;
    btnReg: TDexedToolButton;
    btnStack: TDexedToolButton;
    btnStop: TDexedToolButton;
    btnStart: TDexedToolButton;
    btnWatch: TDexedToolButton;
    button4: TDexedToolButton;
    Edit1: TComboBox;
    GroupBox3: TGroupBox;
    lstThreads: TListView;
    mnuEvalDeref: TMenuItem;
    mnuEvalSelected: TMenuItem;
    mnuEvalCustom: TMenuItem;
    mnuNextMachine: TMenuItem;
    mnuStepMachine: TMenuItem;
    mnuStep: TPopupMenu;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    mnuNext: TPopupMenu;
    mnuEval: TPopupMenu;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    lstVariables: TListView;
    lstCallStack: TListView;
    mnuReadW: TMenuItem;
    mnuWriteW: TMenuItem;
    mnuReadWriteW: TMenuItem;
    mnuSelProj: TMenuItem;
    mnuSelRunnable: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    btnSendCom: TSpeedButton;
    cpuViewer: TTIPropertyGrid;
    mnuProjRunnable: TPopupMenu;
    mnuWatch: TPopupMenu;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    lstAsm: TListView;
    TabSheet5: TTabSheet;
    dbgeeOptsEd: TTIPropertyGrid;
    varListFlt: TListViewFilterEdit;
    procedure btnContClick(Sender: TObject);
    procedure btnEvalClick(Sender: TObject);
    procedure btnVariablesClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnOverClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnRegClick(Sender: TObject);
    procedure btnSendComClick(Sender: TObject);
    procedure btnStackClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnWatchClick(Sender: TObject);
    procedure dbgeeOptsEdEditorFilter(Sender: TObject;
      aEditor: TPropertyEditor; var aShow: boolean);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstCallStackDblClick(Sender: TObject);
    procedure lstThreadsDblClick(Sender: TObject);
    procedure mnuEvalDerefClick(Sender: TObject);
    procedure mnuEvalCustomClick(Sender: TObject);
    procedure mnuEvalSelectedClick(Sender: TObject);
    procedure mnuReadWClick(Sender: TObject);
    procedure mnuReadWriteWClick(Sender: TObject);
    procedure mnuSelProjClick(Sender: TObject);
    procedure mnuSelRunnableClick(Sender: TObject);
    procedure mnuWriteWClick(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure varListFltChange(Sender: TObject);
  protected
    procedure setToolBarFlat(value: boolean); override;
  private
    fEvalKind: TGdbEvalKind;
    fSynchronizedDocuments: TStringList;
    fSynchronizingBreakpoints: boolean;
    fSyms: ISymStringExpander;
    fExe: string;
    fOutputName: string;
    fInputName: string;
    fShowFromCustomCommand: boolean;
    fGdbState: TGdbState;
    fSubj: TDebugObserverSubject;
    fDoc: TDexedMemo;
    fDbgRunnable: boolean;
    fProj: ICommonProject;
    fJson: TJsonObject;
    fLog: TStringList;
    fDocHandler: IMultiDocHandler;
    fMsg: IMessagesDisplay;
    fGdb: TDexedProcess;
    fOutput: TFileStream;
    fInput: TFileStream;
    fInspState: TInspectableCPU;
    fStackItems: TStackItems;
    fCatchPause: boolean;
    fSilentPause: boolean;
    fOptions: TDebugOptions;
    fAddWatchPointKind: TAddWatchPointKind;
    fBreakPoints: TPersistentBreakPoints;
    fMenu: TMenuItem;
    fLastFilename: string;
    fLastFunction: string;
    fLastOffset: string;
    fLastLine: string;
    fLastEvalStuff: string;
    fCommandProcessed: boolean;
    fDebugeeOptions: TDebugeeOptions;
    procedure continueDebugging;
    procedure waitCommandProcessed;
    procedure clearDisplays;
    procedure updateMenu;
    procedure optionsChangesApplied(sender: TObject);
    procedure disableEditor;
    procedure setState(value: TGdbState);
    procedure updateButtonsState;
    procedure startDebugging;
    procedure killGdb;
    procedure updateDebugeeOptionsEditor;
    procedure deleteRedirectedIO;
    // GDB output processors
    procedure gdboutQuiet(sender: TObject);
    procedure gdboutJsonize(sender: TObject);
    procedure interpretJson;
    // GDB commands & actions
    procedure gdbCommand(aCommand: string; gdbOutProcessor: TNotifyEvent = nil);
    procedure infoRegs;
    procedure infoStack;
    procedure infoVariables;
    procedure infoThreads;
    procedure infoAsm(const fname: string);
    procedure evalStuff(const stuff: string);
    procedure sendCustomCommand;
    procedure setGpr(reg: TCpuRegister; val: TCpuGprValue);
    procedure setFpr(reg: TFpuRegister; val: extended);
    procedure setSsr(reg: TSegRegister; val: TCPUSegValue);
    procedure setFlag(val: PtrUint);
    procedure readOutput;
    //
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    //
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
    //
    function running: boolean;
    function singleServiceName: string;
    procedure addBreakPoint(const fname: string; line: integer;
      kind: TBreakPointKind = bpkBReak);
    procedure removeBreakPoint(const fname: string; line: integer;
      kind: TBreakPointKind = bpkBreak);
    procedure removeBreakPoints(const fname: string);
    procedure executeFromShortcut(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

const
  SR_exec = 0;
  SR_access_watchpoint_trigger = 1;
  SR_location_reached = 2;
  SR_syscall_return = 3;
  SR_vfork = 4;
  SR_syscall_entry = 5;
  SR_watchpoint_trigger= 6;
  SR_read_watchpoint_trigger = 8;
  SR_breakpoint_hit = 9;
  SR_end_stepping_range = 10;
  SR_function_finished = 11;
  SR_fork = 13;
  SR_solib_event = 14;

type
  // Perfect static hash-set that detect a GDB stop reason
  stopReasons = record
  public
  private
    const fWords: array [0..15] of string =
    (
      'exec', 'access-watchpoint-trigger', 'location-reached', 'syscall-return',
      'vfork', 'syscall-entry', 'watchpoint-trigger', '', 'read-watchpoint-trigger',
      'breakpoint-hit', 'end-stepping-range', 'function-finished', '', 'fork',
      'solib-event', ''
    );
    const fHasEntry: array [0..15] of boolean =
    (
      true, true, true, true, true, true, true, false, true, true, true, true,
      false, true, true, false);
    const fCoeffs: array[0..255] of Byte =
    (
      3, 89, 191, 109, 130, 168, 79, 148, 40, 57, 142, 200, 143, 109, 144, 128,
      90, 119, 44, 16, 170, 160, 252, 48, 142, 79, 188, 18, 124, 157, 218, 14,
      108, 236, 45, 97, 190, 119, 37, 44, 103, 121, 20, 36, 149, 200, 122, 188,
      222, 195, 201, 15, 30, 183, 145, 110, 21, 186, 66, 71, 229, 247, 179, 169,
      169, 212, 55, 0, 44, 189, 223, 250, 253, 23, 140, 204, 155, 114, 139, 39,
      189, 35, 218, 30, 222, 168, 40, 203, 20, 208, 146, 226, 122, 200, 28, 223,
      116, 208, 151, 27, 16, 253, 107, 207, 71, 102, 215, 69, 202, 175, 103, 240,
      179, 198, 173, 120, 47, 48, 199, 52, 203, 207, 21, 80, 103, 33, 254, 46,
      218, 25, 47, 131, 221, 239, 44, 33, 165, 73, 143, 121, 73, 23, 76, 159, 199,
      172, 144, 236, 161, 249, 178, 45, 49, 157, 246, 43, 227, 145, 139, 161, 185,
      15, 68, 254, 112, 132, 133, 5, 41, 149, 116, 82, 57, 156, 193, 250, 73, 108,
      126, 1, 44, 151, 211, 197, 23, 225, 119, 247, 59, 20, 225, 241, 232, 192,
      241, 1, 7, 12, 73, 160, 157, 14, 203, 109, 9, 17, 43, 20, 14, 174, 233, 108,
      254, 204, 78, 224, 16, 15, 133, 251, 254, 204, 191, 12, 0, 131, 19, 252,
      104, 178, 231, 176, 22, 234, 104, 181, 167, 17, 103, 23, 156, 197, 249, 237,
      109, 53, 170, 237, 57, 126, 48, 119, 175, 238, 141, 188
    );
    class function hash(const w: string): Byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): shortint; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

implementation
{$R *.lfm}

const optFname = 'gdbcommander.txt';
const bpFname = 'breakpoints.txt';
const prjFname = 'projectsgdboptions.txt';

{$REGION TDebugOption --------------------------------------------------------}
procedure TDebugShortcuts.assign(source: TPersistent);
var
  src: TDebugShortcuts;
begin
  if source is TDebugShortcuts then
  begin
    src := TDebugShortcuts(source);
    fStart    := src.fStart;
    fStop     := src.fStop;
    fPause    := src.fPause;
    fContinue := src.fContinue;
    fStep     := src.fStep;
    fStepOver := src.fStepOver;
    fStack    := src.fStack;
    fRegs     := src.fRegs;
    fVariables:= src.fVariables;
    fRepeatCustomEval:=src.fRepeatCustomEval;
  end
  else inherited;
end;

constructor TDebugOptionsBase.create(aOwner: TComponent);
var
  d: TDlangBreakpoint;
begin
  inherited;
  fAutoDemangle := true;
  fAutoGetCallStack:= true;
  fAutoGetRegisters:= true;
  fAutoGetVariables:= true;
  fAutoDisassemble:= true;
  fAutoGetThreads:=true;
  fShowGdbOutput:=true;
  fStopAllThreadsOnBreak:= true;
  fIgnoredSignals := TStringList.Create;
  fIgnoredSignals.Duplicates:= dupIgnore;
  fIgnoredSignals.Sorted:=true;
  fCommandsHistory := TStringList.Create;
  fCommandsHistory.Duplicates:= dupIgnore;
  fCommandsHistory.Sorted:=true;
  fShortcuts := TDebugShortcuts.Create;
  fCustomEvalHistory := TstringList.Create;
  fCustomEvalHistory.Duplicates:= dupIgnore;
  fCustomEvalHistory.Sorted:=true;
  for d in [low(TDlangBreakpoint) .. high(TDlangBreakpoint)] do
    include(fDlangBreakpoints, d);
end;

destructor TDebugOptionsBase.destroy;
begin
  fIgnoredSignals.Free;
  fCommandsHistory.Free;
  fCustomEvalHistory.Free;
  fShortcuts.Free;
  inherited;
end;

procedure TDebugOptionsBase.cleanInvalidHistoryEntries;
var
  i: integer;
begin
  for i := fCommandsHistory.Count-1 downto 0 do
    if fCommandsHistory[i].length > 128 then
      fCommandsHistory.Delete(i);
end;

procedure TDebugOptionsBase.beforeSave;
begin
  cleanInvalidHistoryEntries;
end;

procedure TDebugOptionsBase.afterLoad;
begin
  cleanInvalidHistoryEntries;
end;

procedure TDebugOptionsBase.setIgnoredSignals(value: TStringList);
begin
  fIgnoredSignals.Assign(value);
end;

procedure TDebugOptionsBase.setCommandsHistory(value: TStringList);
begin
  fCommandsHistory.Assign(value);
end;

procedure TDebugOptionsBase.setCustomEvalHistory(value: TStringList);
begin
  fCustomEvalHistory.Assign(value);
end;

procedure TDebugOptionsBase.setShortcuts(value: TDebugShortcuts);
begin
  fShortcuts.assign(value);
end;

procedure TDebugOptionsBase.assign(source: TPersistent);
var
  src: TDebugOptionsBase;
begin
  if source is TDebugOptionsBase then
  begin
    src := TDebugOptionsBase(source);
    fAsmSyntax:=src.fAsmSyntax;
    fAutoDemangle:=src.fAutoDemangle;
    fAutoDisassemble:=src.fAutoDisassemble;
    fAutoGetThreads:=src.fAutoGetThreads;
    fAutoGetCallStack:=src.fAutoGetCallStack;
    fAutoGetRegisters:=src.fAutoGetRegisters;
    fAutoGetVariables:=src.autoGetVariables;
    fShowGdbOutput:=src.fShowGdbOutput;
    fShowOutput:=src.fShowOutput;
    fShowRawMiOutput:=src.fShowRawMiOutput;
    fStopAllThreadsOnBreak:= src.fStopAllThreadsOnBreak;
    fIgnoredSignals.Assign(src.fIgnoredSignals);
    fCommandsHistory.Assign(src.fCommandsHistory);
    fShortcuts.assign(src.fShortcuts);
    fKeepRedirectedStreams := src.fKeepRedirectedStreams;
  end
  else inherited;
end;

constructor TDebugOptions.create(aOwner: TComponent);
var
  fname: string;
begin
  inherited;
  fBackup := TDebugOptionsBase.create(self);
  EntitiesConnector.addObserver(self);
  fShowGdbOutput:=false;
  fShowOutput:= true;
  fAutoDemangle:= true;
  fAutoGetCallStack:= true;
  fAutoGetRegisters:= true;
  fAutoGetVariables:= true;
  fname := getDocPath + optFname;
  if fname.fileExists then
    loadFromFile(fname);
end;

destructor TDebugOptions.destroy;
begin
  saveToFile(getDocPath + optFname);
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TDebugOptions.optionedWantCategory(): string;
begin
  exit('Debugger');
end;

function TDebugOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekGeneric);
end;

function TDebugOptions.optionedWantContainer: TPersistent;
begin
  exit(self);
end;

procedure TDebugOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeSelectCat: fBackup.assign(self);
    oeeCancel: assign(fBackup);
    oeeAccept:
    begin
      fBackup.assign(self);
      if assigned(FonChangesApplied) then
        FonChangesApplied(self);
    end;
  end;
end;

function TDebugOptions.optionedOptionsModified: boolean;
begin
  exit(false);
end;
{$ENDREGION}

{$REGION TPersistentBreakpoints ------------------------------------------------}
constructor TPersistentBreakPoints.create(aOwner: TComponent);
var
  fname: string;
begin
  Inherited;
  fItems := TCollection.Create(TPersistentBreakPoint);
  fname := getDocPath + bpFname;
  if fname.fileExists then
    loadFromFile(fname);
end;

destructor TPersistentBreakPoints.destroy;
begin
  saveToFile(getDocPath + bpFname);
  fItems.Free;
  inherited;
end;

procedure TPersistentBreakPoints.setItems(value: TCollection);
begin
  fItems.Assign(value);
end;

function TPersistentBreakPoints.getItem(index: integer): TPersistentBreakPoint;
begin
  exit(TPersistentBreakPoint(fItems.Items[index]));
end;

function TPersistentBreakPoints.count: integer;
begin
  exit(fItems.Count);
end;

function TPersistentBreakPoints.find(const fname: string; line: integer; kind: TBreakPointKind): boolean;
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  result := false;
  for i := 0 to fItems.Count-1 do
  begin
    b := item[i];
    if (b.filename = fname) and (b.line = line) and (b.kind = kind) then
      exit(true);
  end;
end;

function TPersistentBreakPoints.addItem(const fname: string; line: integer; kind: TBreakPointKind): boolean;
var
  b: TPersistentBreakPoint;
begin
  result := false;
  if not find(fname, line, kind) then
  begin
    b := TPersistentBreakPoint(fItems.Add);
    b.filename:=fname;
    b.line:=line;
    b.kind:=kind;
    result := true;
  end;
end;

function TPersistentBreakPoints.deleteItem(const fname: string; line: integer; kind: TBreakPointKind): boolean;
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  result := false;
  for i := fItems.Count-1 downto 0 do
  begin
    b := item[i];
    if (b.filename = fname) and (b.line = line) and (b.kind = kind) then
    begin
      result := true;
      fItems.Delete(i);
      break;
    end;
  end;
end;

procedure TPersistentBreakPoints.clearFile(const fname: string);
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  for i:= fItems.Count-1 downto 0 do
  begin
    b := item[i];
    if b.filename = fname then
      fItems.Delete(i);
  end;
end;
{$ENDREGION}

{$REGION TStackItem/TStackItems ------------------------------------------------}
procedure TStackItem.setProperties(addr: PtrUint; fname, nme: string; lne: integer);
begin
  fAddress:=addr;
  fLine:=lne;
  fFilename:=fname;
  fFname:= nme;
end;

constructor TStackItems.create;
begin
  fItems := TCollection.Create(TStackItem);
end;

destructor TStackItems.destroy;
begin
  fItems.Free;
  inherited;
end;

procedure TStackItems.assignToList(list: TListView);
var
  i: integer;
  litm: TListItem;
  sitm: TStackItem;
begin
  list.Clear;
  for i:= 0 to fItems.Count-1 do
  begin
    litm := list.Items.Add;
    sitm := TStackItem(fItems.Items[i]);
    litm.Caption := sitm.name;
    {$IFDEF CPU64}
    litm.SubItems.Add(format('0x%.16X', [sitm.address]));
    {$ELSE}
    litm.SubItems.Add(format('0x%.8X', [sitm.address]));
    {$ENDIF}
    litm.SubItems.Add(shortenPath(sitm.filename));
    litm.SubItems.Add(sitm.line.ToString);
    litm.Data:=sitm;
  end;
end;

procedure TStackItems.addItem(addr: PtrUint; fname, nme: string; lne: integer);
begin
  TStackItem(fItems.Add).setProperties(addr, fname, nme, lne);
end;

procedure TStackItems.clear;
begin
  fItems.Clear;
end;
{$ENDREGION}

{$REGION TInspectableCPU -------------------------------------------------------}
function TCpuRegValueEditor.GetValue: ansistring;
begin
  {$IFDEF CPU64}
  result := '0x' + IntToHex(GetInt64Value, 16);
  {$ELSE}
  result := '0x' + IntToHex(GetOrdValue, 8);
  {$ENDIF}
end;

procedure TCpuRegValueEditor.SetValue(const NewValue: ansistring);
begin
  try
    {$IFDEF CPU64}
    SetInt64Value(StrToQWord(NewValue));
    {$ELSE}
    SetOrdValue(StrToInt(NewValue));
    {$ENDIF}
  except
  end;
end;

function TCpuSegValueEditor.GetValue: ansistring;
begin
  result := '0x' + IntToHex(GetOrdValue, 4);
end;

procedure TCpuSegValueEditor.SetValue(const NewValue: ansistring);
begin
  try
    SetOrdValue(StrToInt(NewValue));
  except
  end;
end;

constructor TInspectableGPR.create(eventGPR: TSetGprEvent);
begin
  fSetGprEvent:=eventGPR;
end;

procedure TInspectableGPR.setInspectableRegister(index: TCpuRegister; value: PtrUInt);
begin
  fRegisters[index] := value;
end;

procedure TInspectableGPR.setRegister(index: TCpuRegister; value: TCpuGprValue);
begin
  fSetGprEvent(index, value);
  fRegisters[index] := value;
end;

constructor TInspectableSSR.create(eventSSR: TSetSsrEvent);
begin
  fSetSsrEvent:=eventSSR;
end;

procedure TInspectableSSR.setInspectableRegister(index: TSegRegister; value: TCPUSegValue);
begin
  fRegisters[index] := value;
end;

procedure TInspectableSSR.setRegister(index: TSegRegister; value: TCPUSegValue);
begin
  fSetSsrEvent(index, value);
  fRegisters[index] := value;
end;

constructor TInspectableFPR.create(event: TSetFprEvent);
begin
  fSetFprEvent:=event;
end;

procedure TInspectableFPR.setInspectableRegister(index: TFpuRegister; value: extended);
begin
  fRegisters[index] := value;
end;

procedure TInspectableFPR.setRegister(index: TFpuRegister; value: extended);
begin
  fSetFprEvent(index, value);
  fRegisters[index] := value;
end;

constructor TInspectableCPU.create(setGprEvent: TSetGprEvent; setSsrEvent: TSetSsrEvent;
  setFlagEvent: TSetFlagEvent; setFprEvent: TSetFprEvent);
begin
  fSetFlagEvent:=setFlagEvent;
  fGpr := TInspectableGPR.create(setGprEvent);
  fSsr := TInspectableSSR.create(setSsrEvent);
  fFpr := TInspectableFPR.create(setFprEvent);
end;

destructor TInspectableCPU.destroy;
begin
  fGpr.Free;
  fFPr.Free;
  fSSr.Free;
  inherited;
end;

procedure TInspectableCPU.setInspectableFlags(value: PtrUint);
var
  flg: TFlag;
begin
  if fFullFlags = value then
    exit;
  fFullFlags:=value;
  fFlags:= [];
  for flg in TFlag do
    if (value and FlagValues[flg]) >= FlagValues[flg] then
      fFlags += [flg];
end;

procedure TInspectableCPU.setFlag(value: TFlags);
var
  flg: TFlag;
begin
  if fFlags = value then
    exit;
  for flg in TFlag do
    if (flg in value) <> (flg in fFlags) then
      fFullFlags:= fFullFlags xor FlagValues[flg];
  fFlags := value;
  fSetFlagEvent(fFullFlags);
end;
{$ENDREGION}

{$REGION StopReasons -----------------------------------------------------------}
{$IFDEF DEBUG}{$PUSH}{$R-}{$ENDIF}
class function stopReasons.hash(const w: string): Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $F;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function stopReasons.match(const w: string): shortint;
var
  h: Byte;
begin
  result := -1;
  if (length(w) < 4) or (length(w) > 25) then
    exit;
  h := hash(w);
  if fHasEntry[h] and (fWords[h] = w) then
    result := h;
end;
{$ENDREGION}

{$REGION TDebugeeOption ------------------------------------------------------}
constructor TDebugeeOption.Create(ACollection: TCollection);
begin
  inherited create(ACollection);
  fAgruments := TStringList.Create;
  fEnvPaths := TStringList.Create;
  fAgruments.Delimiter:= ' ';
end;

destructor TDebugeeOption.Destroy;
begin
  fAgruments.Free;
  fEnvPaths.Free;
  inherited;
end;

procedure TDebugeeOption.setOptions(value: TStringList);
begin
  fAgruments.Assign(value);
end;

procedure TDebugeeOption.setEnvPaths(value: TStringList);
begin
  fEnvPaths.Assign(value);
end;

constructor TDebugeeOptions.Create(AOwner: TComponent);
var
  fname: string;
begin
  inherited create(AOwner);
  fProjects := TCollection.Create(TDebugeeOption);
  fname := getDocPath + prjFname;
  if fname.fileExists then
    loadFromFile(fname);
end;

destructor TDebugeeOptions.Destroy;
begin
  saveToFile(getDocPath + prjFname);
  fProjects.Free;
  inherited;
end;

procedure TDebugeeOptions.cleanup;
var
  i: integer;
  p: TDebugeeOption;
begin
  for i:= fProjects.Count-1 downto 0 do
  begin
    p := projectByIndex[i];
    if not p.filename.fileExists or p.filename.isEmpty then
      fProjects.Delete(i)
    else if (p.arguments.Count = 0) and (p.environmentPaths.Count = 0) and
      (p.workingDirectory = '') and (p.queryArguments = false) then
        fProjects.Delete(i);
  end;
end;

procedure TDebugeeOptions.beforeSave;
begin
  cleanup;
end;

procedure TDebugeeOptions.setProjects(value: TCollection);
begin
  fProjects.Assign(value);
end;

function TDebugeeOptions.getProjectByIndex(index: integer): TDebugeeOption;
begin
  exit(TDebugeeOption(fProjects.Items[index]));
end;

function TDebugeeOptions.getProjectByFile(const fname: string): TDebugeeOption;
var
  i: integer;
begin
  for i := 0 to fProjects.Count-1 do
  begin
    result := projectByIndex[i];
    if result.filename = fname then
      exit;
  end;
  result := TDebugeeOption(fProjects.Add);
  result.filename:=fname;
end;
{$ENDREGION}

{$REGION Common/standard comp --------------------------------------------------}
constructor TGdbWidget.create(aOwner: TComponent);
begin
  inherited;
  EntitiesConnector.addObserver(self);
  EntitiesConnector.addSingleService(self);
  fSyms := getSymStringExpander;
  fDocHandler:= getMultiDocHandler;
  fMsg:= getMessageDisplay;
  fLog := TStringList.Create;
  fInspState := TInspectableCPU.Create(@setGpr, @setSsr, @setFlag, @setFpr);
  fJson := TJsonObject.Create;
  fStackItems := TStackItems.create;
  fSubj:= TDebugObserverSubject.Create;
  fOptions:= TDebugOptions.create(self);
  fOptions.onChangesApplied:=@optionsChangesApplied;
  fDebugeeOptions:= TDebugeeOptions.Create(self);
  Edit1.Items.Assign(fOptions.commandsHistory);
  fAddWatchPointKind := wpkWrite;
  fBreakPoints := TPersistentBreakPoints.create(self);
  fSynchronizedDocuments := TStringList.Create;

  TListViewCopyMenu.create(lstCallStack);
  TListViewCopyMenu.create(lstAsm);
  TListViewCopyMenu.create(lstVariables);
  TListViewCopyMenu.create(lstThreads);

  cpuViewer.DefaultItemHeight := scaleY(22, 96);
  dbgeeOptsEd.DefaultItemHeight:= cpuViewer.DefaultItemHeight;

  Case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnSendCom, 'ACCEPT');
      AssignPng(varListFlt.Glyph, 'FILTER_CLEAR');
    end;
    iss24:
    begin
      AssignPng(btnSendCom, 'ACCEPT24');
      AssignPng(varListFlt.Glyph, 'FILTER_CLEAR24');
    end;
    iss32:
    begin
      AssignPng(btnSendCom, 'ACCEPT32');
      AssignPng(varListFlt.Glyph, 'FILTER_CLEAR32');
    end;
  end;

  updateMenu;
  updateButtonsState;
end;

destructor TGdbWidget.destroy;
begin
  fInput.free;
  fOutput.Free;
  fOptions.commandsHistory.Assign(edit1.Items);
  fOptions.Free;
  fLog.Free;
  killGdb;
  fInspState.Free;
  fJson.Free;
  fStackItems.Free;
  fSynchronizedDocuments.Free;
  EntitiesConnector.removeObserver(self);
  fSubj.free;
  inherited;
end;

procedure TGdbWidget.setToolBarFlat(value: boolean);
begin
  inherited setToolBarFlat(value);
  btnSendCom.Flat:=value;
  varListFlt.flat := value;
end;

procedure TGdbWidget.updateMenu;
var
  mnu: IMainMenu;
  itm: TMenuItem;
  bmp: TBitmap;
begin
  mnu := getMainMenu;
  if not assigned(mnu) then
    exit;

  if fMenu.isNil then
  begin
    fMenu := mnu.mnuAdd;
    fMenu.Caption:='Debugger';
  end;
  fMenu.Clear;

  bmp := TBitmap.Create;

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.start;
  itm.Caption:='Start';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=0;
  fMenu.Add(itm);
  btnStart.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.stop;
  itm.Caption:='Stop';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=1;
  fMenu.Add(itm);
  btnStop.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.pause;
  itm.Caption:='Pause';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=2;
  fMenu.Add(itm);
  btnPause.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.continue;
  itm.Caption:='Continue';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=3;
  fMenu.Add(itm);
  btnContinue.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.step;
  itm.Caption:='Step';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=4;
  fMenu.Add(itm);
  btnNext.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.stepOver;
  itm.Caption:='Step over';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=5;
  fMenu.Add(itm);
  btnOver.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.Caption:= '-';
  itm.Tag:=-1;
  fMenu.Add(itm);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.updateRegisters;
  itm.Caption:='Update registers';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=6;
  fMenu.Add(itm);
  btnReg.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.updateStack;
  itm.Caption:='Update call stack';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=7;
  fMenu.Add(itm);
  btnStack.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.updateVariables;
  itm.Caption:='Update the variables';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=8;
  fMenu.Add(itm);
  btnVariables.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  itm := TMenuItem.Create(fMenu);
  itm.ShortCut:=fOptions.shortcuts.repeatCustomEval;
  itm.Caption:='Repeat last evaluation command';
  itm.OnClick:= @executeFromShortcut;
  itm.Tag:=9;
  fMenu.Add(itm);
  btnEval.toBitmap(bmp);
  itm.Bitmap.Assign(bmp);
  itm.ImageIndex:= fMenu.GetImageList.Add(bmp, nil);

  bmp.Free;
end;

procedure TGdbWidget.clearDisplays;
begin
  lstVariables.Clear;
  lstCallStack.Clear;
  lstThreads.Clear;
  lstAsm.Clear;
  cpuViewer.Clear;
  cpuViewer.TIObject := nil;
end;

procedure TGdbWidget.optionsChangesApplied(sender: TObject);
begin
  updateMenu;
end;

procedure TGdbWidget.executeFromShortcut(sender: TObject);
begin
  case TMenuItem(sender).Tag of
    0: begin showWidget; btnStart.Click; end;
    1: btnStop.Click;
    2: btnPause.Click;
    3: btnContinue.Click;
    4: btnNext.Click;
    5: btnOver.Click;
    6: begin showWidget; btnReg.Click; end;
    7: begin showWidget; btnStack.Click; end;
    8: begin showWidget; btnVariables.Click; end;
    9: evalStuff(fLastEvalStuff);
  end;
end;
{$ENDREGION}

{$REGION IProjectObserver ----------------------------------------------------}
procedure TGdbWidget.projNew(project: ICommonProject);
begin
  fProj := project;
end;

procedure TGdbWidget.projChanged(project: ICommonProject);
begin
  if fProj <> project then
    exit;
end;

procedure TGdbWidget.projClosing(project: ICommonProject);
begin
  if fProj <> project then
    exit;
  fProj := nil;
  updateDebugeeOptionsEditor;
end;

procedure TGdbWidget.projFocused(project: ICommonProject);
begin
  fProj := project;
  updateDebugeeOptionsEditor;
end;

procedure TGdbWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TGdbWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;
{$ENDREGION}

{$REGION IDocumentObserver ---------------------------------------------------}
procedure TGdbWidget.docNew(document: TDexedMemo);
begin
end;

procedure TGdbWidget.docFocused(document: TDexedMemo);
var
  i: integer;
  b: TPersistentBreakPoint;
begin
  fDoc := document;
  if fGdbState = gsNone then
    updateDebugeeOptionsEditor;
  if (fDoc.fileName <> '<new document>') then
  begin
    fSynchronizingBreakpoints:= true;
    if fSynchronizedDocuments.IndexOf(document.fileName) = -1 then
    begin
      fSynchronizedDocuments.Add(document.fileName);
      for i:= 0 to fBreakPoints.count-1 do
      begin
        b := fBreakPoints.item[i];
        if b.filename = fDoc.fileName then
          fDoc.addBreakpoint(b.line);
      end;
    end;
    fSynchronizingBreakpoints:= false;
  end;
end;

procedure TGdbWidget.docChanged(document: TDexedMemo);
begin
end;

procedure TGdbWidget.docClosing(document: TDexedMemo);
var
  i: integer;
begin
  if fDoc <> document then
    exit;
  i := fSynchronizedDocuments.IndexOf(fDoc.fileName);
  if i <> -1 then
    fSynchronizedDocuments.delete(i);
  fDoc := nil;
  if fGdbState = gsNone then
    updateDebugeeOptionsEditor;
end;
{$ENDREGION}

{$REGION Unsorted Debugging things ---------------------------------------------}
function TGdbWidget.running: boolean;
begin
  if assigned(fGdb) then
    exit(fGdb.Running)
  else
    exit(false);
end;

function TGdbWidget.singleServiceName: string;
begin
  exit('IDebugger');
end;

procedure TGdbWidget.killGdb;
begin
  if not assigned(fGdb) then
    exit;
  if fGdb.Running then
    fGdb.Terminate(0);
  FreeAndNil(fGdb);
  updateDebugeeOptionsEditor;
end;

procedure TGdbWidget.waitCommandProcessed;
begin
  while not fCommandProcessed do
    application.ProcessMessages;
end;

procedure TGdbWidget.addBreakPoint(const fname: string; line: integer;
  kind: TBreakPointKind = bpkBreak);
var
  r: boolean;
  a: boolean = false;
begin
  if fSynchronizingBreakpoints then
    exit;
  if assigned(fBreakPoints) then
    a := fBreakPoints.addItem(fname, line, kind);
  if not a or fGdb.isNil or not fGdb.Running then
    exit;
  r := fGdbState = gsRunning;
  if r then
  begin
    fSilentPause := true;
    gdbCommand('-exec-interrupt --all', @gdboutJsonize);
    waitCommandProcessed;
    fSilentPause := false;
  end;
  gdbCommand(format('-break-insert --source %s --line %d', [fname, line]) + #10);
  if r then
  begin
    fSilentPause := true;
    gdbCommand('-exec-continue --all', @gdboutJsonize);
    waitCommandProcessed;
    fSilentPause := false;
  end;
end;

procedure TGdbWidget.removeBreakPoint(const fname: string; line: integer;
  kind: TBreakPointKind = bpkBreak);
var
  r: boolean;
  d: boolean = false;
  a: TJSONArray;
  o: TJSONObject;
  t: TJSONObject;
  v: TJSONData;
  i: integer;
begin
  if assigned(fBreakPoints) then
    d := fBreakPoints.deleteItem(fname, line, kind);
  if not d or fGdb.isNil or not fGdb.Running then
    exit;

  r := fGdbState = gsRunning;
  if r then
  begin
    fSilentPause := true;
    gdbCommand('-exec-interrupt --all', @gdboutJsonize);
    waitCommandProcessed;
    fSilentPause := false;
  end;
  gdbCommand('-break-list', @gdboutJsonize);
  waitCommandProcessed;

  if fJson.findObject('BreakpointTable', t) and t.findArray('body', a) then
    for i := 0 to a.Count-1 do
  begin
    o := a.Objects[i];
    if o.findAny('fullname', v) and (v.AsString <> fname) then
      continue;
    if o.findAny('line', v) and (v.AsInteger <> line) then
      continue;
    if o.findAny('type', v) then
    begin
      if (v.AsString = 'breakpoint') and (kind <> bpkBreak) then
        continue
      else if (v.AsString = 'watchpoint') and (kind <> bpkWatch) then
        continue;
    end;
    if o.findAny('number', v) then
      gdbCommand('-break-delete ' + v.AsString);
  end;

  if r then
  begin
    fSilentPause := true;
    gdbCommand('-exec-continue --all', @gdboutJsonize);
    waitCommandProcessed;
    fSilentPause := false;
  end;
end;

procedure TGdbWidget.removeBreakPoints(const fname: string);
begin
  fBreakPoints.clearFile(fname);
end;

procedure TGdbWidget.setState(value: TGdbState);
begin
  if fGdbState = value then
    exit;
  fGdbState:=value;
  updateButtonsState;
end;

procedure TGdbWidget.updateButtonsState;
begin
  case fGdbState of
    gsNone:
    begin
      btnStart.Enabled:=true;
      btnStop.Enabled:=false;
      btnPause.Enabled:=false;
      btnContinue.Enabled:=false;
      btnNext.Enabled:=false;
      btnOver.Enabled:=false;
      btnReg.Enabled:=false;
      btnVariables.Enabled:=false;
      btnStack.Enabled:=false;
      btnWatch.Enabled:=false;
      clearDisplays;
    end;
    gsPaused:
    begin
      btnStart.Enabled:=false;
      btnStop.Enabled:=true;
      btnPause.Enabled:=false;
      btnContinue.Enabled:=true;
      btnNext.Enabled:=true;
      btnOver.Enabled:=true;
      btnReg.Enabled:=true;
      btnVariables.Enabled:=true;
      btnStack.Enabled:=true;
      btnWatch.Enabled:=true;
    end;
    gsRunning:
    begin
      btnStart.Enabled:=false;
      btnStop.Enabled:=true;
      btnPause.Enabled:=true;
      btnContinue.Enabled:=false;
      btnNext.Enabled:=false;
      btnOver.Enabled:=false;
      btnReg.Enabled:=false;
      btnVariables.Enabled:=false;
      btnStack.Enabled:=false;
      btnWatch.Enabled:=false;
    end;
  end;
end;

procedure TGdbWidget.mnuSelProjClick(Sender: TObject);
begin
  fDbgRunnable := false;
  mnuSelRunnable.Checked:=false;
  updateDebugeeOptionsEditor;
end;

procedure TGdbWidget.mnuSelRunnableClick(Sender: TObject);
begin
  fDbgRunnable := true;
  mnuSelProj.Checked:=false;
  updateDebugeeOptionsEditor;
end;

procedure TGdbWidget.mnuWriteWClick(Sender: TObject);
begin
  fAddWatchPointKind := wpkWrite;
  mnuReadW.Checked:=false;
  mnuReadWriteW.Checked:=false;
end;

procedure TGdbWidget.PageControl2Change(Sender: TObject);
begin
  // workaround LCL bug, "cannot focus..." due to caret in filter
  varListFlt.Enabled := PageControl2.PageIndex = 0
end;

procedure TGdbWidget.varListFltChange(Sender: TObject);
var
  i: integer;
begin
  if varListFlt.Filter = '' then
    exit;
  for i:= 0 to lstVariables.Items.Count-1 do
    if AnsiContainsText(lstVariables.Items[i].Caption, varListFlt.Filter) then
    begin
      lstVariables.ItemIndex:=i;
      lstVariables.Selected.MakeVisible(false);
      break;
    end;
end;

procedure TGdbWidget.disableEditor;
begin
  cpuViewer.ItemIndex:=-1;
end;

procedure TGdbWidget.startDebugging;
var
  str: string = '';
  gdb: string;
  i: integer;
  b: TPersistentBreakPoint;
  o: TDebugeeOption;
const
  asmFlavorStr: array[TAsmSyntax] of string = ('intel','att');
begin
  clearDisplays;
  if not fDbgRunnable and (fProj = nil) then
  begin
    dlgOkInfo('No project to debug', 'GDB commander');
    exit;
  end;
  if fDbgRunnable and fDoc.isNil then
  begin
    dlgOkInfo('No runnable to debug', 'GDB commander');
    exit;
  end;
  if not fDbgRunnable and (fProj.binaryKind <> executable) then
  begin
    dlgOkInfo('The project cannot be debugged because it does not output an executable', 'GDB commander');
    exit;
  end;
  if not fDbgRunnable then
    fExe := fProj.outputFilename
  else
    fExe := fDoc.fileName.stripFileExt + exeExt;
  //
  if (fExe = '/') or not fExe.fileExists then
  begin
    if fDbgRunnable then
      dlgOkInfo('Either the runnable is not compiled or it cannot be found' +
        LineEnding + 'Note that the runnable option "outputFolder" is not supported by this widget.' +
        LineEnding + LineEnding + 'Expected target: ' + fExe, 'GDB commander')
    else
      dlgOkInfo('The project binary is missing, cannot debug.' +
        LineEnding + LineEnding + 'Expected target: ' + fExe, 'GDB commander');
    exit;
  end;
  //
  fOutputName := fExe + '.inferiorout';
  fInputName  := fExe + '.inferiorin';
  FreeAndNil(fInput);
  FreeAndNil(fOutput);
  //
  gdb := exeFullName('gdb');
  if not gdb.fileExists then
  begin
    dlgOkInfo('Cannot debug, GDB is missing', 'GDB commander');
    exit;
  end;
  //
  if fInputName.fileExists then
    deletefile(fInputName);
  fInput:= TFileStream.Create(fInputName, fmCreate or fmShareExclusive);
  subjDebugStart(fSubj, self as IDebugger);
  case fDbgRunnable of
    true: o := fDebugeeOptions.projectByFile[fDoc.fileName];
    false:o := fDebugeeOptions.projectByFile[fProj.fileName];
  end;
  fLastFunction := '';
  // gdb process
  killGdb;
  fGdb := TDexedProcess.create(nil);
  fGdb.Executable:= gdb;
  fgdb.Options:= [poUsePipes, poStderrToOutPut];
  fgdb.Parameters.Add(fExe);
  fgdb.Parameters.Add('--interpreter=mi');
  fGdb.OnReadData:= @gdboutQuiet;
  fGdb.OnTerminate:= @gdboutJsonize;
  fgdb.execute;
  // file:line breakpoints
  for i:= 0 to fBreakPoints.Count-1 do
  begin
    b := fBreakPoints[i];
    case b.kind of
      bpkBreak:
      begin
        str := format('-break-insert --source %s --line %d', [b.filename, b.line]) + #10;
        fGdb.Input.Write(str[1], str.length);
      end;
      bpkWatch: {TODO-cGDB: put watchpoint from persistent};
    end;
  end;
  gdbCommand('set disassembly-flavor ' + asmFlavorStr[fOptions.asmSyntax]);
  // break on druntime exceptions + any throw'
  if (onAssertError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onAssertError');
  if (onAssertErrorMsg in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onAssertErrorMsg');
  if (onUnittestErrorMsg in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onUnittestErrorMsg');
  if (onRangeError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onRangeError');
  if (onFinalizeError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onFinalizeError');
  if (onHiddenFuncError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onHiddenFuncError');
  if (onOutOfMemoryError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onOutOfMemoryError');
  if (onInvalidMemoryOperationError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onInvalidMemoryOperationError');
  if (onSwitchError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onSwitchError');
  if (onUnicodeError in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function onUnicodeError');
  if (_d_throwc in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_throwc');
  if (_d_throwdwarf in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_throwdwarf');
  if (_d_assertm in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_assertm');
  if (_d_assert in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_assert');
  if (_d_assert_msg in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_assert_msg');
  if (_d_array_bounds in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_array_bounds');
  if (_d_arraybounds in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_arraybounds');
  if (_d_switch_error in fOptions.coreBreakingSymbols) then
    gdbCommand('-break-insert --function _d_switch_error');

  gdbCommand('-gdb-set mi-async on');
  if fOptions.stopAllThreadsOnBreak then
    gdbCommand('-gdb-set non-stop off')
  else
    gdbCommand('-gdb-set non-stop on');
  fGdb.OnReadData := @gdboutJsonize;
  cpuViewer.TIObject := fInspState;
  cpuViewer.RefreshPropertyValues;
  // inferior options
  if o.environmentPaths.Count <> 0 then
  begin
    str := '';
    for i:= 0 to o.environmentPaths.Count-1 do
      str += o.environmentPaths[i] + ' ';
    str := fSyms.expand(str[1..str.length-1]);
    gdbCommand('-environment-path ' + str);
  end;
  if DirectoryExists(fSyms.expand(o.workingDirectory)) then
    gdbCommand('-environment-cd ' + fSyms.expand(o.workingDirectory));
  if (o.arguments.Count <> 0) or (o.queryArguments) then
  begin
    str := '';
    if o.queryArguments and not InputQuery('Command line arguments', 'GDB commander', str) then
      str := ''
    else
      str += ' ';
    for i := 0 to o.arguments.Count-1 do
      str += o.arguments[i] + ' ';
    str := fSyms.expand(str);
  end;
  gdbCommand('-exec-arguments '+ str + '> ' + fOutputName + '< ' + fInputName);
  // non-MI command "run" has the same problem as https://sourceware.org/bugzilla/show_bug.cgi?id=18077
  gdbCommand('-exec-run');
  setState(gsRunning);
end;

procedure TGdbWidget.updateDebugeeOptionsEditor;
var
  nme: string = '';
  opt: TDebugeeOption;
begin
  dbgeeOptsEd.ItemIndex:=-1;
  dbgeeOptsEd.TIObject := nil;
  if not fDbgRunnable then
  begin
    if fProj <> nil then
      nme := fProj.filename;
  end
  else
  begin
    if fDoc.isNotNil then
      nme := fDoc.filename;
  end;
  if nme.fileExists then
  begin
    opt := fDebugeeOptions.projectByFile[nme];
    dbgeeOptsEd.TIObject := opt;
  end;
end;

procedure TGdbWidget.deleteRedirectedIO;
begin
  if fOptions.keepRedirectedStreams then
    exit;
  if fOutputName.fileExists then
    deleteFile(fOutputName);
  if fInputName.fileExists then
    deleteFile(fInputName);
end;
{$ENDREGION}

{$REGION GDB output processors -------------------------------------------------}
procedure parseGdbout(const str: string; var json: TJSONObject);

  procedure parseProperty(node: TJSONObject; r: PStringRange); forward;
  procedure parseProperty(node: TJSONArray; r: PStringRange); forward;

  procedure parseCLI(node: TJSONObject; r: PStringRange);
  var
    lne: TStringRange;
    msg: string = '';
  begin
    if r^.front = '"' then
      r^.popFront;
    while true do
    begin
      lne := r^.takeUntil(['\', '"']);
      if (r^.empty) then
        break
      else if r^.front = '\' then
      begin
        r^.popFront;
        if r^.front = 'n' then
        begin
          r^.popFront;
          node.Arrays['CLI'].Add(msg + lne.yield);
          msg := '';
        end else
          msg += lne.yield;
      end
      else if r^.front = '"' then
      begin
        r^.popFront;
        if r^.front = #10 then
        begin
          r^.popFront;
          break;
        end;
      end;
    end;
  end;

  procedure parseProperty(node: TJSONArray; r: PStringRange);
  var
    c: char;
  begin
    while true do
    begin
      if r^.empty then
        exit;
      c := r^.front;
      case c of
        'a'..'z', 'A'..'Z', '_':
        begin
          r^.takeUntil('=').yield;
          r^.popFront;
        end;
        '"':
        begin
          r^.popFront;
          node.Strings[node.Count] := r^.takeUntil('"').yield;
          r^.popFront;
        end;
        '{':
        begin
          r^.popFront;
          node.Objects[node.Count] := TJSONObject.Create;
          parseProperty(node.Objects[node.Count-1], r);
        end;
        ']':
        begin
          r^.popFront;
          exit;
        end;
        ',': r^.popFront;
        #10:
        begin
          r^.popFront;
          exit;
        end;
      end;
    end;
  end;

  procedure parseProperty(node: TJSONObject; r: PStringRange);
  var
    idt: string = '';
    v: string;
    c: char;
  begin
    while true do
    begin
      if r^.empty then
        exit;
      c := r^.front;
      case c of
        ',':
        begin
          r^.popFront;
        end;
        'a'..'z', 'A'..'Z', '_':
        begin
          idt := r^.takeUntil('=').yield;
          r^.popFront;
        end;
        '"':
        begin
          v := '';
          r^.popFront;
          while true do
          begin
            v += r^.takeUntil(['"','\']).yield;
            if r^.front = '\' then
            begin
              v += '\';
              r^.popFront;
              if r^.front = '"' then
              begin
                r^.popFront;
                v += '"';
              end;
            end else
              break;
          end;
          node.Strings[idt] := v;
          r^.popFront;
        end;
        '{':
        begin
          r^.popFront;
          node.Objects[idt] := TJSONObject.Create;
          parseProperty(node.Objects[idt], r);
        end;
        '[':
        begin
          r^.popFront;
          node.Arrays[idt] := TJSONArray.Create;
          parseProperty(node.Arrays[idt], r);
        end;
        '}', ']':
        begin
          r^.popFront;
          exit;
        end;
        ' ', #9:
          r^.popFront;
        #10:
        begin
          r^.popFront;
          exit;
        end;
      end;
    end;
  end;

var
  rng: TStringRange = (ptr: nil; pos: 0; len: 0);
begin
  json.Clear;
  if str.length = 0 then
    exit;
  rng.init(str);
  json.Arrays['OUT'] := TJSONArray.Create;
  json.Arrays['CLI'] := TJSONArray.Create;
  while true do
  begin
    if rng.empty then
      exit;
    case rng.front of
      // event
      '*':
      begin
        parseProperty(json, rng.popUntil(',')^.popFront);
      end;
      // command answer (can be a simple '^done')
      '^':
      begin
        parseProperty(json, rng.popUntil([',', #10]));
      end;
      // what would be output in a console by gdb
      '~':
      begin
        parseCLI(json, rng.popFront);
      end;
      // internal gdb messages
      '&':
      begin
        parseCLI(json, rng.popFront);
      end;
      // async notify / status / out stream when remote (@)
      '=', '+','@':
      begin
        rng.popUntil(#10);
        if not rng.empty then
          rng.popFront;
      end
      else
      begin
        rng.popUntil(#10);
        if not rng.empty then
          rng.popFront;
      end;
    end;
  end;
end;

procedure TGdbWidget.interpretJson;

  procedure selectAsmInstr;
  var
    itm: TListItem = nil;
  begin
    if lstAsm.items.findCaption(fLastOffset, itm) then
    begin
      itm.Selected:=true;
      itm.MakeVisible(false);
    end;
  end;

  procedure autoGetStuff;
  begin
    if fOptions.autoGetCallStack then
      infoStack;
    if fOptions.autoGetRegisters then
      infoRegs;
    if fOptions.autoGetVariables then
      infoVariables;
    if fOptions.autoGetThreads then
      infoThreads;
    selectAsmInstr;
  end;

var
  r: shortint;
  i,j: integer;
  val: TJSONData;
  obj: TJSONObject;
  arr: TJSONArray;
  k: TListItem;
  // common data
  nme: string = '';
  reason: string;
  addr: PtrUint = 0;
  func:string = '';
  line: integer = -1;
  // registers data
  number: integer = 0;
  // signal data
  sigmean: string;
  signame: string;
  brkreason: TDebugBreakReason;
  // FPU
  fpustr: string;
  fFpuExtended: extended;
  fFpuRaw: array[0..9] of Byte absolute fFpuExtended;
begin

  if fJson.findAny('reason', val) then
  begin
    reason := val.AsString;
    r := stopReasons.match(reason);
    if r <> -1 then
    begin
      case r of
        SR_breakpoint_hit:
          brkreason := dbBreakPoint;
        SR_watchpoint_trigger, SR_access_watchpoint_trigger, SR_read_watchpoint_trigger:
          brkreason:= dbWatch;
        else
          brkreason := dbStep;
      end;
      if brkreason = dbWatch then
      begin
        if fJson.findObject('wpt', obj) and obj.findAny('exp', val) then
        begin
          if lstVariables.items.findCaption(val.AsString, k) then
          begin
            lstVariables.ItemIndex:=k.index;
            k.MakeVisible(false);
          end;
        end;
      end;
      if fJson.findObject('frame', obj) then
      begin
        if obj.FindAny('addr', val) then
          fLastOffset:=val.AsString;
        if obj.FindANy('fullname', val) then
          fLastFilename := val.AsString;
        if obj.findAny('line', val) then
        begin
          line := val.AsInteger;
          fLastLine := val.AsString;
        end;
        if obj.findAny('func', val) then
        begin
          if fOptions.autoDisassemble and (val.AsString <> fLastFunction) then
            infoAsm(fLastFilename);
          fLastFunction := val.AsString;
        end;
        if fDocHandler.findDocument(fLastFilename).isNil and fLastFilename.fileExists then
          fDocHandler.openDocument(fLastFilename);
        setState(gsPaused);
        autoGetStuff;
        readOutput;
        application.BringToFront;
        subjDebugBreak(fSubj, fLastFilename, line, brkreason);
      end;
    end

    else if reason = 'watchpoint-scope' then
    begin
      gdbCommand('continue', @gdboutJsonize);
    end

    else if reason = 'signal-received' then
    begin
      signame := 'unknown signal';
      sigmean := 'unknown meaning';
      if fJson.findAny('signal-name', val) then
        signame := val.AsString;
      if (fOptions.ignoredSignals.Count <> 0) and
        (fOptions.ignoredSignals.IndexOf(signame) <> -1) then
      begin
        continueDebugging;
        exit;
      end;
      if fJson.findAny('signal-meaning', val) then
        sigmean := val.AsString;
      if fJson.findObject('frame', obj) then
      begin
        if obj.findAny('addr', val) then
          fLastOffset:=val.AsString;
        if obj.findAny('fullname', val) then
          fLastFilename := val.AsString;
        if obj.findAny('line', val) then
          line := val.AsInteger;
        if obj.findAny('func', val) then
        begin
          if fOptions.autoDisassemble and (val.AsString <> fLastFunction) then
            infoAsm(fLastFilename);
          fLastFunction := val.AsString;
        end;
      end;
      if fCatchPause then
      begin
        fCatchPause := false;
        if  fDocHandler.findDocument(fLastFilename).isNil and fLastFilename.fileExists then
          fDocHandler.openDocument(fLastFilename);
        autoGetStuff;
        setState(gsPaused);
        readOutput;
        subjDebugBreak(fSubj, fLastFilename, line, dbSignal);
      end
      else if not fSilentPause then
      begin
        if dlgYesNo(format('The signal %s (%s) was received on line %d of file %s .'
        + LineEnding + 'Do you wish to pause execution ?', [signame, sigmean, line, fLastFilename]),
        'Unexpected signal received') = mrNo then
        begin
          gdbCommand('continue', @gdboutJsonize);
          setState(gsRunning);
        end
        else
        begin
          if not fDocHandler.findDocument(fLastFilename).isNil and fLastFilename.fileExists then
            fDocHandler.openDocument(fLastFilename);
          autoGetStuff;
          setState(gsPaused);
          readOutput;
          subjDebugBreak(fSubj, fLastFilename, line, dbSignal);
        end;
      end;
    end

    else if (reason = 'exited-normally') or (reason = 'exited-signalled')
      or (reason = 'exited')
    then
    begin
      application.BringToFront;
      readOutput;
      if not fOptions.showGdbOutput then
        fMsg.message('debugging terminated: ' + reason, nil, amcMisc, amkInf);
      setState(gsNone);
      subjDebugStop(fSubj);
      deleteRedirectedIO;
      updateDebugeeOptionsEditor;
      killGdb;
    end;
  end;

  if fJson.findAny('msg', val) then
    fMsg.message(val.AsString, nil, amcMisc, amkAuto);

  if fJson.findArray('register-values', arr) then
  begin
    for i := 0 to arr.Count-1 do
    begin
      obj := TJSONObject(arr.Objects[i]);
      if obj.isNil then
        break;
      if obj.findAny('number', val) then
        number := val.AsInteger;
      if obj.findAny('value', val) then
      case number of
        0..integer(high(TCpuRegister)):
        begin
          fInspState.CPU.setInspectableRegister
            (TCpuRegister(number), {$IFDEF CPU64}val.AsQWord{$ELSE}val.AsInteger{$ENDIF});
        end;
        flagOffset:
        begin
          fInspState.setInspectableFlags({$IFDEF CPU64}val.AsInt64{$ELSE}val.AsInteger{$ENDIF});
        end;
        segOffset..segOffset+5:
        begin
          fInspState.SEG.setInspectableRegister
            (TSegRegister(number - segOffset), val.AsInteger);
        end;
        stOffset..stOffset+7:
        begin
          fpustr := val.AsString;
          fpustr := fpustr[3..fpustr.length];
          if fpustr.length < 20 then
          while fpustr.length < 20 do
            fpustr += '0';
          fFpuRaw[9] := StrToInt('$' + fpustr[1..2]);
          fFpuRaw[8] := StrToInt('$' + fpustr[3..4]);
          fFpuRaw[7] := StrToInt('$' + fpustr[5..6]);
          fFpuRaw[6] := StrToInt('$' + fpustr[7..8]);
          fFpuRaw[5] := StrToInt('$' + fpustr[9..10]);
          fFpuRaw[4] := StrToInt('$' + fpustr[11..12]);
          fFpuRaw[3] := StrToInt('$' + fpustr[13..14]);
          fFpuRaw[2] := StrToInt('$' + fpustr[15..16]);
          fFpuRaw[1] := StrToInt('$' + fpustr[17..18]);
          fFpuRaw[0] := StrToInt('$' + fpustr[19..20]);
          fInspState.FPU.setInspectableRegister
            (TFpuRegister(number - stOffset), fFpuExtended);
        end;
      end;
      // TODO-cGDB: get SSE registers
    end;
    cpuViewer.RefreshPropertyValues;
  end;

  if fJson.findArray('stack', arr) then
  begin
    fStackItems.clear;
    lstCallStack.Clear;
    for i := 0 to arr.Count-1 do
    begin
      obj := arr.Objects[i];
      if obj.isNil then
        break;
      val := obj.Find('fullname');
      if val.isNotNil then
        fLastFilename:= val.AsString;
      val := obj.Find('func');
      if val.isNotNil then
      begin
        if fOptions.autoDemangle then
          func:= demangle(val.AsString)
        else
          func := val.AsString;
      end;
      val := obj.Find('addr');
      if val.isNotNil then
        addr := val.AsInt64;
      val := obj.Find('line');
      if val.isNotNil then
        line := val.AsInteger;
      fStackItems.addItem(addr, fLastFilename, func, line);
    end;
    fStackItems.assignToList(lstCallStack);
  end;

  val := fJson.Find('variables');
  if val.isNil then
    val := fJson.Find('locals');
  if val.isNotNil and (val.JSONType = jtArray) then
  begin
    j := lstVariables.ItemIndex;
    lstVariables.BeginUpdate;
    lstVariables.Clear;
    arr := TJSONArray(val);
    for i := 0 to arr.Count-1 do
    begin
      val := arr.Items[i];
      if val.JSONType <> jtObject then
        continue;
      obj := TJSONObject(val);
      val := obj.Find('name');
      if val.isNil then
        continue;
      nme := val.AsString;
      val := obj.Find('value');
      if val.isNil then
        continue;
      lstVariables.AddItem(nme, nil);
      with lstVariables.Items[lstVariables.Items.Count-1] do
        SubItems.Add(val.AsString);
    end;
    if (j <> -1) and (j < lstVariables.Items.Count) then
      lstVariables.ItemIndex := j;
    lstVariables.EndUpdate;
  end;

  if fJson.findArray('asm_insns', arr) then
  begin
    lstAsm.BeginUpdate;
    lstAsm.Clear;
    for i := 0 to arr.Count-1 do
    begin
      obj := arr.Objects[i];
      val := obj.Find('address');
      if val.isNotNil then
        nme := val.AsString;
      //val := obj.Find('func-name');
      //val := obj.Find('offset');
      val := obj.Find('inst');
      if val.isNotNil then
      begin
        lstAsm.AddItem(nme, nil);
        if nme = fLastOffset then
          lstAsm.Selected := lstAsm.Items[lstAsm.Items.Count-1];
        if fOptions.autoDemangle then
          lstAsm.Items[lstAsm.Items.Count-1].SubItems.Add(demangle(val.AsString))
        else
          lstAsm.Items[lstAsm.Items.Count-1].SubItems.Add(val.AsString);
      end;
    end;
    if lstAsm.Selected.isNotNil then
      lstAsm.Selected.MakeVisible(false);
    lstAsm.EndUpdate;
    selectAsmInstr;
  end;

  if fJson.findArray('threads', arr) then
  begin
    lstThreads.BeginUpdate;
    lstThreads.Clear;
    for i := 0 to arr.Count-1 do
    begin
      obj := arr.Objects[i];
      if obj.findAny('id', val) then
      begin
        lstThreads.AddItem(val.AsString, nil);
        k := lstThreads.Items[lstThreads.Items.Count-1];
        if obj.findAny('state', val) then
          k.SubItems.Add(val.AsString);
        if obj.findAny('core', val) then
          k.SubItems.Add(val.AsString);
        val := obj.Find('frame');
        if val.isNotNil and (val.JSONType = jtObject) then
        begin
          obj := TJSONObject(val);
          if obj.findAny('func', val) then
            if fOptions.autoDemangle then
              k.SubItems.Add(demangle(val.AsString))
            else
              k.SubItems.Add(demangle(val.AsString));
          if obj.findAny('addr', val) then
            k.SubItems.Add(val.AsString);
          if obj.findAny('fullname', val) then
            k.SubItems.Add(val.AsString);
          if obj.findAny('line', val) then
            k.SubItems.Add(val.AsString);
        end;
      end;
    end;
    lstThreads.EndUpdate;
  end;

  if fOptions.showGdbOutput or fShowFromCustomCommand then
  begin
    fShowFromCustomCommand := false;
    if fJson.findArray('CLI', arr) then
      for i := 0 to arr.Count-1 do
        fMsg.message(arr.Strings[i], nil, amcMisc, amkAuto);
  end;

end;

procedure TGdbWidget.gdboutJsonize(sender: TObject);
var
  str: string;
begin
  if fMsg = nil then
    exit;

  fLog.Clear;
  fGdb.getFullLines(fLog);
  if fOptions.showRawMiOutput then
    for str in fLog do
      fMsg.message(str, nil, amcMisc, amkAuto);

  fCommandProcessed := true;

  if flog.Text.isEmpty then
    exit;

  parseGdbout(fLog.Text, fJson);
  interpretJson;
end;

procedure TGdbWidget.readOutput;
var
  str: TMemoryStream;
  lst: TStringList;
  lne: string;
begin
  if (fGdbState = gsNone) or not fOptions.showOutput then
    exit;
  if fOutput.isNil and fOutputName.fileExists then
  try
    fOutput := TFileStream.Create(fOutputName, 0);
  except
    if fOutput.isNotNil then
      FreeAndNil(fOutput);
  end;
  if fOutput.isNil then
    exit;
  str := TMemoryStream.Create;
  lst := TStringList.Create;
  try
    str.size := fOutput.Size - fOutput.Position;
    fOutput.Read(str.Memory^, str.Size);
    lst.LoadFromStream(str);
    for lne in lst do
      fMsg.message(lne, nil, amcMisc, amkBub);
  finally
    lst.Free;
    str.Free;
  end;
end;

procedure TGdbWidget.gdboutQuiet(sender: TObject);
begin
  fCommandProcessed := true;
  fGdb.StdoutEx.Clear;
  fGdb.OnReadData:=@gdboutJsonize;
end;
{$ENDREGION}

{$REGION GDB commands & actions ------------------------------------------------}
procedure TGdbWidget.gdbCommand(aCommand: string; gdbOutProcessor: TNotifyEvent = nil);
begin
  if fGdb.isNil or not fGdb.Running then
    exit;
  fCommandProcessed := false;
  aCommand += #10;
  if assigned(gdbOutProcessor) then
    fGdb.OnReadData := gdbOutProcessor;
  fGdb.Input.Write(aCommand[1], aCommand.length);
end;

procedure TGdbWidget.infoRegs;
begin
  disableEditor;
  gdbCommand('-data-list-register-values r', @gdboutJsonize);
end;

procedure TGdbWidget.infoStack;
begin
  gdbCommand('-stack-list-frames', @gdboutJsonize);
end;

procedure TGdbWidget.infoVariables;
begin
  gdbCommand('-stack-list-variables --skip-unavailable --all-values');
end;

procedure TGdbWidget.infoThreads;
begin
  gdbCommand('-thread-info');
end;

procedure TGdbWidget.infoAsm(const fname: string);
var
  cmd: string;
begin
  if not fname.fileExists or (fLastLine = '-1') or (fLastLine = '0') then
    exit;
  cmd := format('-data-disassemble -f %s -l %s -n -1 -- 0', [fname, fLastLine]);
  //cmd := format('-data-disassemble -s %s -e $pc -- 0', [fLastOffset]);
  gdbCommand(cmd, @gdboutJsonize);
end;

procedure TGdbWidget.evalStuff(const stuff: string);
begin
  fLastEvalStuff := stuff;
  gdbCommand('-data-evaluate-expression "' + stuff + '"');
end;

procedure TGdbWidget.continueDebugging;
begin
  gdbCommand('-exec-continue --all', @gdboutJsonize);
  if assigned(fGdb) and fgdb.Running then
  begin
    setState(gsRunning);
    subjDebugContinue(fSubj);
  end;
end;

procedure TGdbWidget.btnStartClick(Sender: TObject);
begin
  startDebugging;
end;

procedure TGdbWidget.btnContClick(Sender: TObject);
begin
  continueDebugging;
end;

procedure TGdbWidget.btnEvalClick(Sender: TObject);
var
  e: string = '';
begin
  if fGdb.isNil or not fGdb.Running then
    exit;
  case fEvalKind of
    gekCustom:
    begin
      if fOptions.customEvalHistory.Count = 0 then
        fOptions.customEvalHistory.Add('<enter a custom expression to evaluate>');
      e := InputComboEx('Evaluate', 'Expression', fOptions.customEvalHistory, true);
      if not e.isBlank then
        fOptions.customEvalHistory.Add(e);
    end;
    gekSelectedVar:
      if lstVariables.ItemIndex <> -1 then
        e := lstVariables.Items[lstVariables.ItemIndex].Caption;
    gekDerefSelectedVar:
      if lstVariables.ItemIndex <> -1 then
        e := '*' + lstVariables.Items[lstVariables.ItemIndex].Caption;
  end;
  if not e.isBlank then
    evalStuff(e);
end;

procedure TGdbWidget.btnVariablesClick(Sender: TObject);
begin
  infoVariables;
end;

procedure TGdbWidget.btnNextClick(Sender: TObject);
const
  cmd: array[boolean] of string = ('-exec-step','-exec-step-instruction');
begin
  gdbCommand(cmd[mnuNextMachine.Checked], @gdboutJsonize);
  if assigned(fGdb) and fgdb.Running then
    setState(gsRunning);
end;

procedure TGdbWidget.btnOverClick(Sender: TObject);
const
  cmd: array[boolean] of string = ('-exec-next','-exec-next-instruction');
begin
  gdbCommand(cmd[mnuStepMachine.Checked], @gdboutJsonize);
  if assigned(fGdb) and fgdb.Running then
    setState(gsRunning);
end;

procedure TGdbWidget.btnPauseClick(Sender: TObject);
begin
  if assigned(fGdb) and fGdb.Running then
    fCatchPause:=true;
  gdbCommand('-exec-interrupt --all', @gdboutJsonize);
end;

procedure TGdbWidget.btnRegClick(Sender: TObject);
begin
  infoRegs;
end;

procedure TGdbWidget.btnStackClick(Sender: TObject);
begin
  infoStack;
end;

procedure TGdbWidget.btnStopClick(Sender: TObject);
begin
  gdbCommand('kill', @gdboutJsonize);
  subjDebugStop(fSubj);
  setState(gsNone);
  deleteRedirectedIO;
  updateDebugeeOptionsEditor;
  killGdb;
end;

procedure TGdbWidget.btnWatchClick(Sender: TObject);
const
  cmd: array[TAddWatchPointKind] of string = (
    '-break-watch -r ','-break-watch ','-break-watch -a ');
var
  nme: string;
begin
  if lstVariables.ItemIndex = -1 then
    exit;
  nme := lstVariables.Items[lstVariables.ItemIndex].Caption;
  gdbCommand(cmd[fAddWatchPointKind] + nme);
end;

procedure TGdbWidget.dbgeeOptsEdEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  aShow := aEditor.GetName <> 'filename';
end;

procedure TGdbWidget.btnSendComClick(Sender: TObject);
begin
  sendCustomCommand;
end;

procedure TGdbWidget.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = byte(#13) then
    sendCustomCommand;
end;

procedure TGdbWidget.lstCallStackDblClick(Sender: TObject);
var
  itm: TStackItem;
  nme: string;
  doc: TDexedMemo;
begin
  if lstCallStack.Selected.isNil or lstCallStack.Selected.Data.isNil then
    exit;
  itm := TStackItem(lstCallStack.Selected.Data);
  nme := itm.filename;
  if not nme.fileExists then
    exit;
  fDocHandler.openDocument(nme);
  doc := fDocHandler.findDocument(nme);
  if doc.isNotNil then
    doc.CaretY:= itm.line;
  {gdbCommand('-stack-select-frame ' + intToStr(lstCallStack.ItemIndex));
  if fOptions.autoGetVariables then
    infoVariables;
  if fOptions.autoGetRegisters then
    infoRegs;}
end;

procedure TGdbWidget.lstThreadsDblClick(Sender: TObject);
var
  lne: integer;
  nme: string;
  doc: TDexedMemo = nil;
begin
  if (lstThreads.Selected.isNil) or (lstThreads.Selected.SubItems.Count < 6) then
    exit;
  lne := StrToIntDef(lstThreads.Selected.SubItems[5], -1);
  nme := lstThreads.Selected.SubItems[4];
  if not nme.fileExists or (lne = -1) then
    exit;
  fDocHandler.openDocument(nme);
  doc := fDocHandler.findDocument(nme);
  if doc.isNotNil then
    doc.CaretY:= lne;
end;

procedure TGdbWidget.mnuEvalDerefClick(Sender: TObject);
begin
  fEvalKind := gekDerefSelectedVar;
  mnuEvalSelected.Checked:=false;
  mnuEvalCustom.Checked:=false;
  mnuEvalDeref.Checked:=true;
end;

procedure TGdbWidget.mnuEvalCustomClick(Sender: TObject);
begin
  fEvalKind := gekCustom;
  mnuEvalSelected.Checked:=false;
  mnuEvalCustom.Checked:=true;
  mnuEvalDeref.Checked:=false;
end;

procedure TGdbWidget.mnuEvalSelectedClick(Sender: TObject);
begin
  fEvalKind := gekSelectedVar;
  mnuEvalSelected.Checked:=true;
  mnuEvalCustom.Checked:=false;
  mnuEvalDeref.Checked:=false;
end;

procedure TGdbWidget.mnuReadWClick(Sender: TObject);
begin
  fAddWatchPointKind := wpkRead;
  mnuWriteW.Checked:=false;
  mnuReadWriteW.Checked:=false;
end;

procedure TGdbWidget.mnuReadWriteWClick(Sender: TObject);
begin
  fAddWatchPointKind := wpkReadWrite;
  mnuReadW.Checked:=false;
  mnuWriteW.Checked:=false;
end;

procedure TGdbWidget.sendCustomCommand;
var
  cmd: string;
begin
  cmd := edit1.Text;
  if cmd.isEmpty then
    exit;
  if edit1.Items.IndexOf(cmd) = -1 then
    edit1.Items.Add(cmd);
  cmd := fSyms.expand(cmd);
  if (cmd.length > 1) and (cmd[1] = '>') and assigned(fInput) then
  begin
    cmd := cmd[2..cmd.length] + #10;
    fInput.Write(cmd[1], cmd.length);
    {$IFDEF UNIX}
    fpfsync(fInput.Handle);
    {$ELSE}
    FlushFileBuffers(fInput.Handle);
    {$ENDIF}
    sleep(100);
    readOutput;
  end
  else
  begin
    fShowFromCustomCommand := true;
    gdbCommand(cmd, @gdboutJsonize);
  end;
  edit1.Text := '';
end;

procedure TGdbWidget.setGpr(reg: TCpuRegister; val: TCpuGprValue);
const
  spec = 'set $%s = 0x%X';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TCpuRegister), integer(reg)), val]);
  gdbCommand(cmd);
end;

procedure TGdbWidget.setSsr(reg: TSegRegister; val: TCPUSegValue);
const
  spec = 'set $%s = 0x%X';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TSegRegister), integer(reg)), val]);
  gdbCommand(cmd);
end;

procedure TGdbWidget.setFpr(reg: TFpuRegister; val: extended);
const
  spec = 'set $%s = %.18g';
var
  cmd : string;
begin
  cmd := format(spec, [GetEnumName(typeinfo(TFpuRegister), integer(reg)), val]);
  gdbCommand(cmd);
end;

procedure TGdbWidget.setFlag(val: PtrUint);
const
  spec = 'set $eflags = 0x%X';
var
  cmd: string;
begin
  cmd := format(spec, [val]);
  gdbCommand(cmd);
end;
{$ENDREGION}

initialization
  RegisterPropertyEditor(TypeInfo(TCpuGprValue), nil, '', TCpuRegValueEditor);
  RegisterPropertyEditor(TypeInfo(TCpuSegValue), nil, '', TCpuSegValueEditor);
end.

