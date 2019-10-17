unit u_processes;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, process, asyncprocess, pipes;

type

  {
    The standard process wrapper that used accross the applicaton.

    This class solves several issues encountered when using TProcess and TAsyncProcess:

    -   "OnTerminate" event is never called under Linux.
        FIX: a timer perdiodically check the process and call the event accordingly.
    -   "OnReadData" event is not usable to read full output lines.
        FIX: the output is accumulated in a TMemoryStream which allows to keep data
        at the left of an unterminated line when a buffer is available.
    -   When StdErr is redirect to "Output", both streams can be blended.
        FIX: the option is deactivated on execution, a flag is set, and the error
        stream is always appended after the output.

    The member "Output" is not usable anymore. Instead:

    -   "getFullLines()" can be used in "OnReadData" or after the execution to fill
        a string list.
    -   "StdoutEx" can be used to read the raw output. It allows to seek, which
        overcomes another limitation of the basic process classes.
  }
  TDexedProcess = class(TASyncProcess)
  private
    fErrToOut: boolean;
    fRealOnTerminate: TNotifyEvent;
    fRealOnReadData: TNotifyEvent;
    fStdoutEx: TMemoryStream;
    fStderrEx: TMemoryStream;
    fTerminateChecker: TTimer;
    fDoneTerminated: boolean;
    fHasRead: boolean;
    procedure checkTerminated(sender: TObject);
    procedure setOnTerminate(value: TNotifyEvent);
    procedure setOnReadData(value: TNotifyEvent);
  protected
    procedure internalDoOnReadData(sender: TObject); virtual;
    procedure internalDoOnTerminate(sender: TObject); virtual;
  published
    property OnTerminate write setOnTerminate;
    property OnReadData write setOnReadData;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure execute; override;
    // reads TProcess.Output and StdErr in their "Ex" versions.
    procedure fillOutputStack;
    // fills list with the full lines contained in StdoutEx
    procedure getFullLines(list: TStrings; consume: boolean = true);
    // access to a flexible copy of TProcess.Output
    property StdoutEx: TMemoryStream read fStdoutEx;
    // access to a flexible copy of TProcess.Error
    property StdErrEx: TMemoryStream read fStderrEx;
    // indicates if an output buffer is read
    property hasRead: boolean read fHasRead;
    // indicates if OnTerminated was called
    property doneTerminated: boolean read fDoneTerminated;
  end;

  {
    OnReadData is only called if no additional buffers are passed
    during a timeout.
  }
  TAutoBufferedProcess = class(TDexedProcess)
  private
    fNewBufferChecker: TTimer;
    fNewBufferTimeOut: Integer;
    fPreviousSize: Integer;
    procedure newBufferCheckerChecks(sender: TObject);
    procedure setTimeout(value: integer);
  protected
    procedure internalDoOnReadData(sender: TObject); override;
    procedure internalDoOnTerminate(sender: TObject); override;
  public
    constructor create(aOwner: TComponent); override;
    procedure execute; override;
    property timeOut: integer read fNewBufferTimeOut write setTimeout;
  end;

  procedure killProcess(var proc: TDexedProcess);

  function prettyReturnStatus(proc: TProcess): string;

implementation

procedure killProcess(var proc: TDexedProcess);
begin
  if proc = nil then
    exit;
  if proc.Running then
    proc.Terminate(0);
  proc.Free;
  proc := nil;
end;

function prettyReturnStatus(proc: TProcess): string;
var
  s: integer;
  {$IFDEF UNIX}
  u: integer;
  {$ENDIF}
begin
  result := '';
  s := proc.ExitStatus;
  {$IFDEF UNIX}
  if s > 255 then
  begin
    u := s div 256;
    result := intToStr(u) + ' (Program-defined exit status)';
  end
  else if s > 127 then
  begin
    u := s - 128;
    if s > 128 then
    case u of
      0: result := '128 (Invalid argument to exit)';
      1: result := '1 (SIGHUP)';
      2: result := '2 (SIGINT)';
      3: result := '3 (SIGQUIT)';
      4: result := '4 (SIGILL)';
      5: result := '4 (SIGTRAP)';
      6: result := '6 (SIGABRT)';
      7: result := '7 (SIGEMT)';
      8: result := '8 (SIGFPE)';
      9: result := '9 (SIGKILL)';
      10: result := '10 (SIGBUS)';
      11: result := '11 (SIGSEGV)';
      12: result := '12 (SIGSYS)';
      13: result := '13 (SIGPIPE)';
      14: result := '14 (SIGALRM)';
      15: result := '15 (SIGTERM)';
      16: result := '16 (SIGUSR1)';
      17: result := '17 (SIGUSR2)';
      18: result := '18 (SIGCHLD)';
      19: result := '19 (SIGPWR)';
      20: result := '20 (SIGWINCH)';
      21: result := '21 (SIGURG)';
      22: result := '22 (SIGPOLL)';
      23: result := '23 (SIGSTOP)';
      24: result := '24 (SIGTSTP)';
      25: result := '25 (SIGCONT)';
      26: result := '26 (SIGTTIN)';
      27: result := '27 (SIGTTOU)';
      28: result := '28 (SIGVTALRM)';
      29: result := '29 (SIGPROF)';
      30: result := '30 (SIGXCPU)';
      31: result := '31 (SIGXFSZ)';
      32: result := '32 (SIGWAITING)';
      33: result := '33 (SIGLWP)';
      34: result := '34 (SIGAIO)';
    end;
  end;
  {$ENDIF}
  if result = '' then
    result := intToStr(s) + ' (undeterminated meaning)';
end;

constructor TDexedProcess.create(aOwner: TComponent);
begin
  inherited;
  fStdoutEx := TMemoryStream.Create;
  fStderrEx := TMemoryStream.Create;
  FTerminateChecker := TTimer.Create(nil);
  FTerminateChecker.Interval := 50;
  fTerminateChecker.OnTimer := @checkTerminated;
  fTerminateChecker.Enabled := false;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
end;

destructor TDexedProcess.destroy;
begin
  FTerminateChecker.Free;
  fStdoutEx.Free;
  fStderrEx.Free;
  inherited;
end;

procedure TDexedProcess.Execute;
begin
  fHasRead := false;
  fStdoutEx.Clear;
  fStderrEx.Clear;
  fDoneTerminated := false;
  fErrToOut := poStderrToOutPut in Options;
  if fErrToOut then
    Options := Options - [poStderrToOutPut];
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
  fTerminateChecker.Enabled := true;
  inherited;
end;

procedure TDexedProcess.fillOutputStack;

  procedure fill(inStr: TInputPipeStream; outStr: TMemoryStream);
  var
    s: integer;
    c: integer;
  begin
    s := outStr.size;
    while (inStr <> nil) and (inStr.NumBytesAvailable > 0) do
    begin
      outStr.SetSize(s + 1024);
      c := inStr.Read((outStr.Memory + s)^, 1024);
      s += c;
    end;
    outStr.SetSize(s);
  end;

begin
  if not (poUsePipes in Options) then
    exit;

  fill(Output, StdoutEx);
  fill(Stderr, stderrEx);
end;

procedure TDexedProcess.getFullLines(list: TStrings; consume: boolean = true);
var
  stored: Integer;
  lastTerm: Integer;
  toread: Integer;
  buff: Byte = 0;
  str: TMemoryStream;
begin
  stored := fStdoutEx.Position;
  if not Running then
  begin
    // stderr has been read in its own stream
    // preventing interleaving, now put it at the end...
    if (poStderrToOutPut in Options) or fErrToOut then
    begin
      fStdoutEx.Position:=fStdoutEx.Size;
      fStdoutEx.Write(fStderrEx.Memory^, fStderrEx.Size);
      if consume then
        fStderrEx.Clear;
    end;
    fStdoutEx.Position:=stored;
    list.LoadFromStream(fStdoutEx);
    if consume then
      fStdoutEx.Clear;
  end else
  begin
    lastTerm := fStdoutEx.Position;
    while fStdoutEx.Read(buff, 1) = 1 do
      if buff = 10 then lastTerm := fStdoutEx.Position;
    fStdoutEx.Position := stored;
    if lastTerm <> stored then
    begin
      str := TMemoryStream.Create;
      try
        toread := lastTerm - stored;
        str.SetSize(toRead);
        fStdoutEx.Read(str.Memory^, toread);
        list.LoadFromStream(str);
      finally
        str.Free;
      end;
    end;
  end;
end;

procedure TDexedProcess.setOnTerminate(value: TNotifyEvent);
begin
  fRealOnTerminate := value;
  TAsyncProcess(self).OnTerminate := @internalDoOnTerminate;
end;

procedure TDexedProcess.setOnReadData(value: TNotifyEvent);
begin
  fRealOnReadData := value;
  TAsyncProcess(self).OnReadData := @internalDoOnReadData;
end;

procedure TDexedProcess.internalDoOnReadData(sender: TObject);
begin
  fHasRead := true;
  fillOutputStack;
  if fRealOnReadData <> nil then
    fRealOnReadData(self);
end;

procedure TDexedProcess.internalDoOnTerminate(sender: TObject);
begin
  fHasRead := false;
  fTerminateChecker.Enabled := false;
  if fDoneTerminated then exit;
  fDoneTerminated := true;

  // restore if same proc is called again,
  // self.execute will exclude the option.
  if fErrToOut then
    Options := Options + [poStderrToOutPut];

  // note: made to fix a leak in the process used by the linter
  // onTerminate is sometimes determined by an internal timer
  // and not the base method of TAsyncProcess (which usually unhooks)
  //UnhookPipeHandle;
  //UnhookProcessHandle;

  fillOutputStack;
  if fRealOnTerminate <> nil then
    fRealOnTerminate(self);
end;

procedure TDexedProcess.checkTerminated(sender: TObject);
begin
  if Running then
    exit;
  fTerminateChecker.Enabled := false;
  internalDoOnTerminate(self);
end;

constructor TAutoBufferedProcess.create(aOwner: TComponent);
begin
  inherited;
  fNewBufferTimeOut := 1000;
  fNewBufferChecker := TTimer.Create(self);
  fNewBufferChecker.Enabled:= false;
  fNewBufferChecker.Interval:= fNewBufferTimeOut;
  fNewBufferChecker.OnTimer:= @newBufferCheckerChecks;
end;

procedure TAutoBufferedProcess.setTimeout(value: integer);
begin
  if fNewBufferTimeOut = value then
    exit;
  fNewBufferTimeOut := value;
  fNewBufferChecker.Interval:= fNewBufferTimeOut;
end;

procedure TAutoBufferedProcess.execute;
begin
  fPreviousSize := fStdoutEx.Size;
  fNewBufferChecker.Enabled:=true;
  inherited;
end;

procedure TAutoBufferedProcess.newBufferCheckerChecks(sender: TObject);
begin
  if fStdoutEx.Size = fPreviousSize then
  begin
    if assigned(fRealOnReadData) then
      fRealOnReadData(self);
  end;
  fPreviousSize := fStdoutEx.Size;
end;

procedure TAutoBufferedProcess.internalDoOnReadData(sender: TObject);
begin
  fillOutputStack;
end;

procedure TAutoBufferedProcess.internalDoOnTerminate(sender: TObject);
begin
  fNewBufferChecker.Enabled:=false;
  inherited;
end;

end.

