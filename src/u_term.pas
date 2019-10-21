unit u_term;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, LCLType,
  ActnList, LMessages,
  u_widget, TerminalCtrls, u_interfaces, u_writableComponent, u_observer,
  u_common, u_synmemo;

type

  TTerminalShortcuts = class(TPersistent)
  private
    fCopy: TShortCut;
    fPaste: TShortCut;
  published
    property copy: TShortCut read fCopy write fCopy;
    property paste: TShortCut read fPaste write fPaste;
  public
    constructor create;
    procedure assign(source: TPersistent); override;
  end;

  // Terminal options
  TTerminalOptionsBase = class(TWritableLfmTextComponent)
  private
    fBackgroundColor: TColor;
    fForegroundColor: TColor;
    fSelectedColor: TColor;
    fFollowEditors: boolean;
    fFollowProjects: boolean;
    fFollowExplorer: boolean;
    fScrollbackLines: longword;
    fFont: TFont;
    fShortcuts: TTerminalShortcuts;
    procedure setFont(value: TFont);
    procedure setShortcuts(value: TTerminalShortcuts);
  public
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure assign(value: TPersistent); override;
  published
    property backgroundColor: TColor read fBackgroundColor write fBackgroundColor;
    property foregroundColor: TColor read fForegroundColor write fForegroundColor;
    property selectedColor: TColor read fSelectedColor write fSelectedColor;
    property font: TFont read fFont write setFont;
    property followEditors: boolean read fFollowEditors write fFollowEditors;
    property followProjects: boolean read fFollowProjects write fFollowProjects;
    property followExplorer: boolean read fFollowExplorer write fFollowExplorer;
    property scrollbackLines: longword read fScrollbackLines write fScrollbackLines default 512;
    property shortcuts: TTerminalShortcuts read fShortcuts write fShortcuts;
  end;

  // Editable and reversible Terminal options
  TTerminalOptions = class(TTerminalOptionsBase, IEditableOptions)
  private
    fBackup: TTerminalOptionsBase;
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure applyChanges;
  end;

  { TTermWidget }

  TTermWidget = class(TDexedWidget, IDocumentObserver, IProjectObserver, IMiniExplorerObserver)
    procedure ContentPaint(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
  private
    fTerm: TTerminal;
    fOpts: TTerminalOptions;
    fLastCd: string;
    fNeedApplyChanges: boolean;

    procedure docNew(document: TDexedMemo);
    procedure docFocused(document: TDexedMemo);
    procedure docChanged(document: TDexedMemo);
    procedure docClosing(document: TDexedMemo);

    procedure mnexDirectoryChanged(const directory: string);

    procedure projNew(project: ICommonProject);
    procedure projChanged(project: ICommonProject);
    procedure projClosing(project: ICommonProject);
    procedure projFocused(project: ICommonProject);
    procedure projCompiling(project: ICommonProject);
    procedure projCompiled(project: ICommonProject; success: boolean);

  protected

    procedure DoShow; override;
    procedure SetVisible(Value: boolean); override;

  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation
{$R *.lfm}

const
  optFname = 'terminal.txt';

constructor TTerminalShortcuts.create;
begin
  fCopy := KeyToShortCut(word(char('C')), [ssCtrl]);
  fPaste:= KeyToShortCut(word(char('V')), [ssCtrl]);
end;

procedure TTerminalShortcuts.assign(source: TPersistent);
var
  s: TTerminalShortcuts;
begin
  if source is TTerminalShortcuts then
  begin
    s := TTerminalShortcuts(source);
    fCopy := s.fCopy;
    fPaste:= s.fPaste;
  end
  else inherited;
end;

constructor TTerminalOptionsBase.create(AOwner: TComponent);
begin
  inherited;
  fFont := TFont.Create;
  fBackgroundColor:= clWhite;
  fForegroundColor:= clBlack;
  fSelectedColor:= clBlack;
  fFont.Name:= 'Monospace';
  fFont.Size:= 12;
  fScrollbackLines:=512;
  fShortcuts := TTerminalShortcuts.create;
end;

destructor TTerminalOptionsBase.destroy;
begin
  fFont.Free;
  fShortcuts.Free;
  inherited;
end;

procedure TTerminalOptionsBase.setFont(value: TFont);
begin
  fFont.Assign(value);
end;

procedure TTerminalOptionsBase.setShortcuts(value: TTerminalShortcuts);
begin
  fShortcuts.assign(value);
end;

procedure TTerminalOptionsBase.assign(value: TPersistent);
var
  s: TTerminalOptionsBase;
begin
  if value is TTerminalOptionsBase then
  begin
    s := TTerminalOptionsBase(value);
    fBackgroundColor:=s.fbackgroundColor;
    fForegroundColor:=s.fForegroundColor;
    fSelectedColor:=s.fSelectedColor;
    followEditors:=s.fFollowEditors;
    fFont.BeginUpdate;
    fFont.Height:=fFont.Height+1;
    fFont.Height:=fFont.Height-1;
    fFont.Assign(s.font);
    fFont.EndUpdate;
    fShortcuts.assign(s.fShortcuts);
    fScrollbackLines := s.fScrollbackLines;
  end
  else inherited;
end;

constructor TTerminalOptions.Create(AOwner: TComponent);
begin
  inherited;
  fBackup := TTerminalOptionsBase.Create(self);
end;

procedure TTerminalOptions.applyChanges;
var
  w: TTermWidget;
begin
  w := TTermWidget(owner);
  w.fTerm.backgroundColor:= backgroundColor;
  w.fTerm.foregroundColor:= foregroundColor;
  w.fTerm.selectedColor:= selectedColor;
  w.fTerm.Font.BeginUpdate;
  w.fTerm.Font.Assign(fFont);
  // force the change: assigning does always trigger TTerminal.FontChanged.
  w.fTerm.Font.Size := w.fTerm.Font.Size +1;
  w.fTerm.Font.Size := w.fTerm.Font.Size -1;
  w.fTerm.Font.endUpdate;
  w.fTerm.scrollbackLines:=fScrollbackLines;
end;

function TTerminalOptions.optionedWantCategory(): string;
begin
  result := 'Terminal';
end;

function TTerminalOptions.optionedWantEditorKind: TOptionEditorKind;
begin
  result := oekGeneric;
end;

function TTerminalOptions.optionedWantContainer: TPersistent;
begin
  result := self;
end;

procedure TTerminalOptions.optionedEvent(event: TOptionEditorEvent);
begin
  case event of
    oeeAccept:
    begin
      fBackup.assign(self);
      applyChanges;
    end;
    oeeCancel:
    begin
      self.assign(fBackup);
      applyChanges;
    end;
    oeeChange:
    begin
      applyChanges;
    end;
    else ;
  end;
end;

function TTerminalOptions.optionedOptionsModified: boolean;
begin
  result := false;
end;

constructor TTermWidget.create(aOwner: TComponent);
var
  f: string;
begin
  inherited;

  toolbarVisible:=false;
  fTerm := TTerminal.Create(self);
  fTerm.Align:= alClient;
  fTerm.BorderSpacing.Around:=4;
  fterm.Parent := self;

  fOpts:= TTerminalOptions.Create(self);

  f := getDocPath + optFname;
  if f.fileExists then
    fOpts.loadFromFile(f);

  EntitiesConnector.addObserver(fOpts);
end;

destructor TTermWidget.destroy;
begin
  fOpts.saveToFile(getDocPath + optFname);
  EntitiesConnector.removeObserver(fOpts);
  inherited;
end;

procedure TTermWidget.DoShow;
begin
  inherited;
  fNeedApplyChanges := true;
end;

procedure TTermWidget.SetVisible(Value: boolean);
begin
  inherited;
  if Value then
    fNeedApplyChanges := true;
end;

procedure TTermWidget.ContentPaint(Sender: TObject);
begin
  if not fNeedApplyChanges then
    exit;
  fNeedApplyChanges:=false;
  fOpts.applyChanges;
end;

procedure TTermWidget.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  s: TShortCut;
begin
  Handled := false;
  s := KeyToShortCut(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
  if s = fOpts.shortcuts.copy then
  begin
    fTerm.copyToClipboard();
    handled := true;
  end
  else if s = fOpts.shortcuts.paste then
  begin
    fTerm.pasteFromClipboard();
    handled := true;
  end;
end;

procedure TTermWidget.mnexDirectoryChanged(const directory: string);
begin
  if fOpts.followExplorer and directory.dirExists and
    not SameText(directory, fLastCd) then
  begin
    fLastCd := directory;
    fTerm.Restart;
    fNeedApplyChanges := true;
    fOpts.applyChanges;
    fTerm.Command('cd ' + directory);
  end;
end;

procedure TTermWidget.docNew(document: TDexedMemo);
begin
end;

procedure TTermWidget.docFocused(document: TDexedMemo);
var
  s: string;
begin
  s := document.fileName.extractFileDir;
  if fOpts.followEditors and s.fileExists and not SameText(s, fLastCd) then
  begin
    fLastCd := s;
    fTerm.Restart;
    fNeedApplyChanges := true;
    fOpts.applyChanges;
    fTerm.Command('cd ' + s);
  end;
end;

procedure TTermWidget.docChanged(document: TDexedMemo);
begin
end;

procedure TTermWidget.docClosing(document: TDexedMemo);
begin
end;

procedure TTermWidget.projNew(project: ICommonProject);
begin
end;

procedure TTermWidget.projChanged(project: ICommonProject);
begin
end;

procedure TTermWidget.projClosing(project: ICommonProject);
begin
end;

procedure TTermWidget.projFocused(project: ICommonProject);
var
  s: string;
begin
  s := project.fileName.extractFileDir;
  if fOpts.followProjects and s.dirExists and not SameText(s, fLastCd) then
  begin
    fLastCd := s;
    fTerm.Restart;
    fNeedApplyChanges := true;
    fOpts.applyChanges;
    fTerm.Command('cd ' + s);
  end;
end;

procedure TTermWidget.projCompiling(project: ICommonProject);
begin
end;

procedure TTermWidget.projCompiled(project: ICommonProject; success: boolean);
begin
end;

end.

