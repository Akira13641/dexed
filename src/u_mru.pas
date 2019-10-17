unit u_mru;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils,
  u_interfaces, u_observer, u_synmemo, u_common;

type

  (**
   * 'Most Recently Used' list for strings.
   *)
  TMruList = class(TStringList)
  private
    fMaxCount: Integer;
    fObj: TObject;
  protected
    fChecking: boolean;
    procedure clearOutOfRange;
    procedure setMaxCount(value: Integer);
    function checkItem(const value: string): boolean; virtual;
    procedure Put(index: Integer; const value: string); override;
    procedure InsertItem(index: Integer; const value: string); override;
  published
    property maxCount: Integer read fMaxCount write setMaxCount;
  public
    constructor create; virtual;
    procedure Insert(index: Integer; const value: string); override;
    property objectTag: TObject read fObj write fObj;
  end;

  (**
   * MRU list for filenames.
   *)
  TMRUFileList = class(TMruList)
  protected
    function checkItem(const value: string): boolean; override;
  public
    constructor create; override;
    procedure assign(source: TPersistent); override;
  end;

  (**
   * MRU list for D/text files.
   * Insertion is automatic (IDocumentObserver).
   *)
  TMRUDocumentList = class(TMRUFileList, IDocumentObserver)
  private
    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);
  public
    constructor create; override;
    destructor destroy; override;
  end;

  (**
   * MRU list for the ceodit projects.
   * Insertion is automatic (IProjectObserver).
   *)
  TMRUProjectList = class(TMRUFileList, IProjectObserver)
  private
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
  public
    constructor create; override;
    destructor destroy; override;
  end;

  (**
   * MRU list for the ceodit projects group.
   * Managed manually since only 1 group exists.
   *)
  TMRUProjectsGroupList = class(TMRUFileList)
  end;

implementation

constructor TMruList.Create;
begin
  fMaxCount := 10;
end;

procedure TMruList.clearOutOfRange;
begin
  while Count > fMaxCount do
    delete(Count-1);
end;

procedure TMruList.setMaxCount(value: Integer);
begin
  if value < 0 then
    value := 0;
  if fMaxCount = value then
    exit;
  fMaxCount := value;
  clearOutOfRange;
end;

function TMruList.checkItem(const value: string): boolean;
var
  i: integer;
begin
  i := indexOf(value);
  if i = -1 then
    exit(true);
  if i = 0 then
    exit(false);
  if Count < 2 then
    exit(false);
  exchange(i, i-1);
  exit( false);
end;

procedure TMruList.Put(index: Integer; const value: string);
begin
  if not (checkItem(value)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TMruList.InsertItem(index: Integer; const value: string);
begin
  if not (checkItem(value)) then
    exit;
  inherited;
  clearOutOfRange;
end;

procedure TMruList.Insert(index: Integer; const value: string);
begin
  if not (checkItem(value)) then
    exit;
  inherited;
  clearOutOfRange;
end;

constructor TMRUFileList.create;
begin
  inherited;
  {$IFDEF WINDOWS}
  CaseSensitive := true;
  {$ENDIF}
end;

procedure TMRUFileList.assign(source: TPersistent);
var
  i: Integer;
begin
  inherited;
  for i := Count-1 downto 0 do
    if not Strings[i].fileExists then
      Delete(i);
end;

function TMRUFileList.checkItem(const value: string): boolean;
begin
  exit( inherited checkItem(value) and value.fileExists);
end;

constructor TMRUDocumentList.create;
begin
  inherited;
  EntitiesConnector.addObserver(self);
end;

destructor TMRUDocumentList.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TMRUDocumentList.docNew(document: TDexedMemo);
begin
end;

procedure TMRUDocumentList.docFocused(document: TDexedMemo);
begin
end;

procedure TMRUDocumentList.docChanged(document: TDexedMemo);
begin
end;

procedure TMRUDocumentList.docClosing(document: TDexedMemo);
begin
  if document.fileName.fileExists and not document.isTemporary then
    Insert(0, document.fileName);
end;

constructor TMRUProjectList.create;
begin
  inherited;
  EntitiesConnector.addObserver(self);
end;

destructor TMRUProjectList.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TMRUProjectList.projNew(project: ICommonProject);
begin
end;

procedure TMRUProjectList.projFocused(project: ICommonProject);
begin
end;

procedure TMRUProjectList.projChanged(project: ICommonProject);
begin
end;

procedure TMRUProjectList.projCompiling(project: ICommonProject);
begin
end;

procedure TMRUProjectList.projCompiled(project: ICommonProject; success: boolean);
begin
end;

procedure TMRUProjectList.projClosing(project: ICommonProject);
var
  fname: string;
begin
  if project = nil then
    exit;

  fname := project.filename;
  if fname.fileExists then
    Insert(0, fname);
end;

end.
