unit u_cdbcmd;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  process, Menus, StdCtrls, u_widget, u_project, u_interfaces, u_observer,
  asyncprocess, ComCtrls, Buttons, u_common;

type
  TCECdbWidget = class(TDexedWidget, ICEProjectObserver)
    btnGo: TSpeedButton;
    btnStep: TSpeedButton;
    btnDisasm: TSpeedButton;
    btnStop: TSpeedButton;
    btnStart: TSpeedButton;
    txtCdbCmd: TEdit;
    lstCdbOut: TListView;
    Panel1: TPanel;
    procedure btnDisasmClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure txtCdbCmdKeyPress(Sender: TObject; var Key: char);
  private
    fCdbProc: TAsyncProcess;
    fProject: TCEProject;
    procedure cdbOutput(Sender: TObject);
    procedure cdbTerminate(Sender: TObject);
    procedure cdbOutputToGui;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure projNew(aProject: TCEProject);
    procedure projClosing(aProject: TCEProject);
    procedure projFocused(aProject: TCEProject);
    procedure projChanged(aProject: TCEProject);
    procedure projCompiling(aProject: TCEProject);
  end;

implementation

{$R *.lfm}

uses
  u_symstring;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCECdbWidget.Create(aOwner: TComponent);
begin
  inherited;
  Enabled := exeInSysPath('cdb');
  if Enabled then
    EntitiesConnector.addObserver(self);
end;

destructor TCECdbWidget.Destroy;
begin
  if Enabled then
  begin
    killProcess(fCdbProc);
    EntitiesConnector.removeObserver(self);
  end;
  inherited;
end;

{$ENDREGION --------------------------------------------------------------------}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCECdbWidget.projNew(aProject: TCEProject);
begin
  fProject := aProject;
end;

procedure TCECdbWidget.projClosing(aProject: TCEProject);
begin
  if fProject <> aProject then
    exit;
  fProject := nil;
end;

procedure TCECdbWidget.projFocused(aProject: TCEProject);
begin
  fProject := aProject;
end;

procedure TCECdbWidget.projChanged(aProject: TCEProject);
begin
end;

procedure TCECdbWidget.projCompiling(aProject: TCEProject);
begin
end;

{$ENDREGION --------------------------------------------------------------------}

procedure TCECdbWidget.btnStartClick(Sender: TObject);
var
  outname: string;
begin
  lstCdbOut.Clear;
  if fProject = nil then
    exit;
  outname := fProject.outputFilename;
  if not fileExists(outname) then
    exit;
  //
  killProcess(fCdbProc);
  fCdbProc := TAsyncProcess.Create(nil);
  fCdbProc.Executable := 'cdb';
  fCdbProc.Parameters.Add('-c');
  fCdbProc.Parameters.Add('"l+*;.lines"');
  fCdbProc.Parameters.Add(outname);
  fCdbProc.CurrentDirectory := extractFilePath(outname);
  fCdbProc.Options := [poNoConsole, poStderrToOutPut, poUsePipes];
  fCdbProc.OnReadData := @cdbOutput;
  fCdbProc.OnTerminate := @cdbTerminate;
  //
  fCdbProc.Execute;
end;

procedure TCECdbWidget.btnStepClick(Sender: TObject);
const
  cmd = 'p'#13#10;
begin
  if fCdbProc = nil then
    exit;
  fCdbProc.Input.Write(cmd[1], length(cmd));
end;

procedure TCECdbWidget.btnGoClick(Sender: TObject);
const
  cmd = 'g'#13#10;
begin
  if fCdbProc = nil then
    exit;
  fCdbProc.Input.Write(cmd[1], length(cmd));
end;

procedure TCECdbWidget.btnDisasmClick(Sender: TObject);
const
  cmd = 'u'#13#10;
begin
  if fCdbProc = nil then
    exit;
  fCdbProc.Input.Write(cmd[1], length(cmd));
end;

procedure TCECdbWidget.btnStopClick(Sender: TObject);
const
  cmd = 'q'#13#10;
begin
  if fCdbProc <> nil then
    fCdbProc.Input.Write(cmd[1], length(cmd));
  killProcess(fCdbProc);
end;

procedure TCECdbWidget.txtCdbCmdKeyPress(Sender: TObject; var Key: char);
var
  inp: string;
  cmd: string;
begin
  if (fCdbProc = nil) or (key <> #13) then
    exit;
  //
  cmd := symbolExpander.get(txtCdbCmd.Text);
  inp := cmd + LineEnding;
  fCdbProc.Input.Write(inp[1], length(inp));
  //
  inp := lstCdbOut.Items.Item[lstCdbOut.Items.Count - 1].Caption;
  inp += cmd;
  lstCdbOut.Items.Item[lstCdbOut.Items.Count - 1].Caption := inp;
  //
  txtCdbCmd.Text := '';
end;

procedure TCECdbWidget.cdbOutputToGui;
var
  lst: TStringList;
  str: string;
begin
  if fCdbProc = nil then
    exit;
  //
  lst := TStringList.Create;
  try
    processOutputToStrings(fCdbProc, lst);
    for str in lst do
      lstCdbOut.AddItem(str, nil);
    lstCdbOut.Items[lstCdbOut.Items.Count - 1].MakeVisible(True);
  finally
    lst.Free;
  end;
end;

procedure TCECdbWidget.cdbOutput(Sender: TObject);
begin
  cdbOutputToGui;
end;

procedure TCECdbWidget.cdbTerminate(Sender: TObject);
begin
  cdbOutputToGui;
  killProcess(fCdbProc);
end;

end.
