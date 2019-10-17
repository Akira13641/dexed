unit u_lcldragdrop;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ShellCtrls,
  u_common, u_ceproject, u_dubproject, u_interfaces,
  u_dialogs, u_projutils;

type

  TDDHandler = class(TObject, IProjectObserver)
  private
    fProj: ICommonProject;
    fFreeProj: ICommonProject;
    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);
    //
    function getFilename(src: TObject): string;
  public
    constructor create;
    destructor destroy; override;
    procedure DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
  end;

function ddHandler: TDDHandler;

implementation

uses
  u_observer;

var
  fDdHandler: TDDHandler = nil;

constructor TDDHandler.create;
begin
  EntitiesConnector.addObserver(self);
end;

destructor TDDHandler.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TDDHandler.projNew(project: ICommonProject);
begin
  fProj := project;
  if not fProj.inGroup then
    fFreeProj := fProj;
end;

procedure TDDHandler.projChanged(project: ICommonProject);
begin
end;

procedure TDDHandler.projClosing(project: ICommonProject);
begin
  fProj := nil;
  if project = fFreeProj then
    fFreeProj := nil;
end;

procedure TDDHandler.projFocused(project: ICommonProject);
begin
  fProj := project;
  if not fProj.inGroup then
    fFreeProj := fProj
  else if fFreeProj = project then
    fFreeProj := nil;
end;

procedure TDDHandler.projCompiling(project: ICommonProject);
begin
end;

procedure TDDHandler.projCompiled(project: ICommonProject; success: boolean);
begin
end;

function TDDHandler.getFilename(src: TObject): string;
var
  lst: TShellListView;
  trv: TTreeView;
begin
  result := '';
  if src.isNil then exit;
  // from mini-explorer
  if src is TShellListView then
  begin
    lst := TShellListView(src);
    if lst.Selected.isNotNil then
      result := lst.GetPathFromItem(lst.Selected);
  end
  // from CE/DUB project inspector
  else if src is TTreeView then
  begin
    trv := TTreeView(src);
    if trv.Selected.isNotNil then
    begin
      result := trv.Selected.Text;
      if not result.fileExists and assigned(fProj) then
        result := fProj.filename.extractFilePath + result;
    end;
  end;
  {$IFNDEF WINDOWS}
  if (result.length > 1) and (result[2] = '/') then
    result := result[2..result.length];
  {$ENDIF}
end;

procedure TDDHandler.DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  fname: string;
begin
  fname := getFilename(Source);
  Accept := fname.fileExists and not fname.dirExists;
end;

procedure TDDHandler.DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  fname: string;
  fmt: TProjectFileFormat;
begin
  if Source.isNil then exit;
  fname := getFilename(Source);
  if not fname.fileExists then exit;

  fmt := projectFormat(fname);
  if fmt in [pffDexed, pffDub] then
  begin
    if assigned(fFreeProj) then
    begin
      if fFreeProj.modified and (dlgFileChangeClose(fFreeProj.filename, UnsavedProj) = mrCancel) then
        exit;
      fFreeProj.getProject.Free;
    end;
    if fmt = pffDexed then
      TNativeProject.create(nil)
    else
      TDubProject.create(nil);
    fProj.loadFromFile(fname);
    fProj.activate;
  end
  else getMultiDocHandler.openDocument(fname);
end;

function ddHandler: TDDHandler;
begin
  if fDdHandler.isNil then
    fDdHandler:= TDDHandler.create;
  result := fDdHandler;
end;

finalization
  ddHandler.free;
end.

