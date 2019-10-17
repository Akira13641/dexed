unit u_dbgitf;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, u_observer;

type

  TBreakPointKind = (
    bpkNone,  // nothing
    bpkBreak, // break point
    bpkWatch  // watch point
  );

  (**
   * IEDebugObserver can call any of the method during debugging
   *)
  IDebugger = interface(ISingleService)
    function running: boolean;
    procedure addBreakPoint(const fname: string; line: integer; kind: TBreakPointKind = bpkBreak);
    procedure removeBreakPoint(const fname: string; line: integer; kind: TBreakPointKind = bpkBreak);
    procedure removeBreakPoints(const fname: string);
  end;

  // Enumerates th e reason why debuging breaks.
  TDebugBreakReason = (
    dbUnknown,      // ?
    dbBreakPoint,   // a break point is reached.
    dbSignal,       // an unexpected signal is emitted.
    dbStep,         // step to this line
    dbWatch         // watchpoint reached
  );
  (**
   * An implementer is informed about a debuging session.
   *)
  IDebugObserver = interface(IObserverType)
  ['IDebugObserver']
    // a debugging session starts. The IDebugger can be stored for the session.
    procedure debugStart(debugger: IDebugger);
    // a debugging session terminates. Any pointer to a IDebugger becomes invalid.
    procedure debugStop;
    // a break happens when code in fname at line is executed.
    procedure debugBreak(const fname: string; line: integer; reason: TDebugBreakReason);
    // debugging continue
    procedure debugContinue;
  end;

  (**
   * An implementer notifies is observer about a debuginf session.
   *)
  TDebugObserverSubject = specialize TCustomSubject<IDebugObserver>;

  // TDebugObserverSubject primitives
  procedure subjDebugStart(subj: TDebugObserverSubject; dbg: IDebugger);
  procedure subjDebugStop(subj: TDebugObserverSubject);
  procedure subjDebugContinue(subj: TDebugObserverSubject);
  procedure subjDebugBreak(subj: TDebugObserverSubject; const fname: string;
    line: integer; reason: TDebugBreakReason);


implementation

procedure subjDebugStart(subj: TDebugObserverSubject; dbg: IDebugger);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as IDebugObserver).debugStart(dbg);
end;

procedure subjDebugStop(subj: TDebugObserverSubject);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as IDebugObserver).debugStop;
end;

procedure subjDebugBreak(subj: TDebugObserverSubject; const fname: string;
    line: integer; reason: TDebugBreakReason);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as IDebugObserver).debugBreak(fname, line, reason);
end;

procedure subjDebugContinue(subj: TDebugObserverSubject);
var
  i: integer;
begin
  for i:= 0 to subj.observersCount-1 do
    (subj.observers[i] as IDebugObserver).debugContinue;
end;

end.

