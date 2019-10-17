unit u_observer;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Contnrs, u_common;

type

  (**
   * interface for a single service (many to one relation).
   * A service is valid during the whole application life-time and
   * is mostly designed to avoid messy uses clauses or to limit
   * the visibility of the implementer methods.
   *)
  ISingleService = interface
    function singleServiceName: string;
  end;

  TServiceList = class(specialize TStringHashMap<TObject>);

  (**
   * Manages the connections between the observers and their subjects in the
   * whole program.
   *)
  TEntitiesConnector = class
  private
    fObservers: TObjectList;
    fSubjects: TObjectList;
    fServices: TServiceList;
    fUpdatesCount: Integer;
    procedure tryUpdate;
    procedure updateEntities;
    function getIsUpdating: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    // forces the update, fixes begin/add pair error or if immediate update is needed.
    procedure forceUpdate;
    // entities will be added in bulk, must be followed by an enUpdate().
    procedure beginUpdate;
    // entities has ben added in bulk
    procedure endUpdate;
    // add/remove entities, update is automatic
    procedure addObserver(observer: TObject);
    procedure addSubject(subject: TObject);
    procedure removeObserver(observer: TObject);
    procedure removeSubject(subject: TObject);
    // allow to register a single service provider.
    procedure addSingleService(provider: TObject);
    // allow to retrieve a single service provider based on its interface name
    function getSingleService(const serviceName: string): TObject;
    // should be tested before forceUpdate()
    property isUpdating: boolean read getIsUpdating;
  end;

  (**
   * Interface for a subject. Basically designed to hold a list of observer
   *)
  ISubject = interface
    // an observer is proposed. anObserver is not necessarly compatible.
    procedure addObserver(observer: TObject);
    // anObserver must be removed.
    procedure removeObserver(observer: TObject);
    // optionally implemented to trigger all the methods of the observer interface.
  end;

  (**
   * Base type used as constraint for an interface that contains
   * the methods called by a ISubject.
   *)
  IObserverType = interface
  end;

  (**
   * Standard implementation of an ISubject.
   * Any descendant automatically adds itself to the EntitiesConnector.
   *)
  generic TCustomSubject<T:IObserverType> = class(ISubject)
  protected
    fObservers: TObjectList;
    // test for a specific interface when adding an observer.
    function acceptObserver(observer: TObject): boolean;
    function getObserversCount: Integer;
    function getObserver(index: Integer): TObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    procedure addObserver(observer: TObject);
    procedure removeObserver(observer: TObject);
    //
    property observersCount: Integer read getObserversCount;
    property observers[index: Integer]: TObject read getObserver; default;
  end;

var
  EntitiesConnector: TEntitiesConnector = nil;

implementation

uses
  LCLProc;

{$REGION TEntitiesConnector --------------------------------------------------}
constructor TEntitiesConnector.Create;
begin
  fObservers := TObjectList.Create(False);
  fSubjects := TObjectList.Create(False);
  fServices := TServiceList.Create();
end;

destructor TEntitiesConnector.Destroy;
begin
  fObservers.Free;
  fSubjects.Free;
  fServices.Free;
  inherited;
end;

function TEntitiesConnector.getIsUpdating: boolean;
begin
  exit(fUpdatesCount > 0);
end;

procedure TEntitiesConnector.tryUpdate;
begin
  if fUpdatesCount <= 0 then
    updateEntities;
end;

procedure TEntitiesConnector.forceUpdate;
begin
  updateEntities;
end;

procedure TEntitiesConnector.updateEntities;
var
  i, j: Integer;
begin
  fUpdatesCount := 0;
  for i := 0 to fSubjects.Count - 1 do
  begin
    if not (fSubjects[i] is ISubject) then
      continue;
    for j := 0 to fObservers.Count - 1 do
    begin
      if fSubjects[i] <> fObservers[j] then
        (fSubjects[i] as ISubject).addObserver(fObservers[j]);
    end;
  end;
end;

procedure TEntitiesConnector.beginUpdate;
begin
  fUpdatesCount += 1;
end;

procedure TEntitiesConnector.endUpdate;
begin
  fUpdatesCount -= 1;
  tryUpdate;
end;

procedure TEntitiesConnector.addObserver(observer: TObject);
begin
  if fObservers.IndexOf(observer) <> -1 then
    exit;
  fObservers.Add(observer);
  tryUpdate;
end;

procedure TEntitiesConnector.addSubject(subject: TObject);
begin
  if (subject as ISubject) = nil then
    exit;
  if fSubjects.IndexOf(subject) <> -1 then
    exit;
  fSubjects.Add(subject);
  tryUpdate;
end;

procedure TEntitiesConnector.removeObserver(observer: TObject);
var
  i: Integer;
begin
  fObservers.Remove(observer);
  for i := 0 to fSubjects.Count - 1 do
    if fSubjects[i] <> nil then
      (fSubjects[i] as ISubject).removeObserver(observer);
  tryUpdate;
end;

procedure TEntitiesConnector.removeSubject(subject: TObject);

begin
  fSubjects.Remove(subject);
  tryUpdate;
end;

procedure TEntitiesConnector.addSingleService(provider: TObject);
begin
  if not (provider is ISingleService) then
    exit;
  fServices.insert((provider as ISingleService).singleServiceName, provider);
end;

function TEntitiesConnector.getSingleService(const serviceName: string): TObject;
begin
  Result := nil;
  if not fServices.GetValue(serviceName, result) then
    result := nil;
end;
{$ENDREGION}

{$REGION TCustomSubject ------------------------------------------------------}
constructor TCustomSubject.Create;
begin
  fObservers := TObjectList.Create(False);
  EntitiesConnector.addSubject(Self);
end;

destructor TCustomSubject.Destroy;
begin
  EntitiesConnector.removeSubject(Self);
  fObservers.Free;
  Inherited;
end;

function TCustomSubject.acceptObserver(observer: TObject): boolean;
begin
  exit(observer is T);
end;

function TCustomSubject.getObserversCount: Integer;
begin
  exit(fObservers.Count);
end;

function TCustomSubject.getObserver(index: Integer): TObject;
begin
  exit(fObservers.Items[index]);
end;

procedure TCustomSubject.addObserver(observer: TObject);
begin
  if not acceptObserver(observer) then
    exit;
  if fObservers.IndexOf(observer) <> -1 then
    exit;
  fObservers.Add(observer);
end;

procedure TCustomSubject.removeObserver(observer: TObject);
begin
  fObservers.Remove(observer);
end;
{$ENDREGION}

initialization
  EntitiesConnector := TEntitiesConnector.Create;
  EntitiesConnector.beginUpdate;

finalization
  EntitiesConnector.Free;
  EntitiesConnector := nil;
end.
