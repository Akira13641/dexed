unit u_newdubproj;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ExtCtrls, Menus, Buttons, process,
  u_common, u_sharedres, u_stringrange;

type

  TCeNewDubProject = class(TForm)
    btnAccept: TSpeedButton;
    btnCancel: TSpeedButton;
    edAuthor: TEdit;
    edName: TEdit;
    edLic: TEdit;
    edCopyR: TEdit;
    edDeps: TEdit;
    edDescr: TEdit;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    lblValidName: TLabel;
    lblValidDir: TLabel;
    lblValidDeps: TLabel;
    pnlFooter: TPanel;
    ssgsdg: TBoundLabel;
    edDir: TDirectoryEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
  private
    deps: array of string;
    function canCreate: boolean;
    function isDirValid: boolean;
    function isNameValid: boolean;
    function areDepsValid: boolean;
    procedure updateBtnAccept;
  public
    constructor create(aOwner: TComponent); override;
  end;

var
  createdNewProject: string;

implementation
{$R *.lfm}

constructor TCeNewDubProject.create(aOwner: TComponent);
begin
  createdNewProject := '';
  inherited;
  case GetIconScaledSize of
    iss16:
    begin
      AssignPng(btnCancel, 'CANCEL');
      AssignPng(btnAccept, 'ACCEPT');
    end;
    iss24:
    begin
      AssignPng(btnCancel, 'CANCEL24');
      AssignPng(btnAccept, 'ACCEPT24');
    end;
    iss32:
    begin
      AssignPng(btnCancel, 'CANCEL32');
      AssignPng(btnAccept, 'ACCEPT32');
    end;
  end;
  width := ScaleX(350, 96);
  height:= ScaleY(400, 96);
end;

function TCeNewDubProject.isDirValid: boolean;
begin
  lblValidDir.Caption := '';
  result := edDir.Directory.isEmpty or (edDir.Directory.dirExists
    and not FileIsInDirectory('dub.json', edDir.Directory)
    and not FileIsInDirectory('dub.sdl', edDir.Directory)
    and not FileIsInDirectory('package.json', edDir.Directory)
    and not FileIsInDirectory('package.sdl', edDir.Directory));
  if not result then
    lblValidDir.Caption :=
      'ERROR: Invalid directory or directory already contains a DUB package';
end;

function TCeNewDubProject.isNameValid: boolean;
var
  s: string;
begin
  lblValidName.Caption := '';
  result := true;
  s := edName.Text;
  if not s.isEmpty then
    with TStringRange.create(s) do
      result := popWhile(['a'..'z', 'A'..'Z', '0'..'9', '-', '_'])^.empty;
  if not result then
    lblValidName.Caption :=
      'ERROR: Invalid name. Name must be empty of made of alphanumeric chars, "-" and "_"';
end;

function TCeNewDubProject.areDepsValid: boolean;
var
  s: string;
  d: string;
  r: TStringRange = (ptr:nil; pos:0; len:0);
begin
  setLength(deps, 0);
  lblValidDeps.Caption := '';
  result := true;
  s := edDeps.Text;
  if s.isEmpty then
    exit;
  r.init(s);
  while not r.empty do
  begin
    r.popWhile([' ']);
    if not r.empty then
      if not (r.front in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
    begin
      result := false;
      lblValidDeps.Caption := 'ERROR: Invalid char found in dependency identifier.';
      break;
    end;
    if r.empty then
      break;
    d := r.takeWhile(['a'..'z', 'A'..'Z', '0'..'9', '-', '_']).yield;
    if not d.isEmpty then
    begin
      setLength(deps, length(deps) + 1);
      deps[high(deps)] := d;
    end;
    lblValidDeps.Caption := lblValidDeps.Caption + 'dep: ' + d + ' ';
  end;
end;

function TCeNewDubProject.canCreate: boolean;
begin
  result := isDirValid and isNameValid and areDepsValid;
end;

procedure TCeNewDubProject.updateBtnAccept;
begin
  btnAccept.Enabled := canCreate;
end;

procedure TCeNewDubProject.edNameChange(Sender: TObject);
begin
  updateBtnAccept;
end;

procedure TCeNewDubProject.btnCancelClick(Sender: TObject);
begin
  ModalResult:= mrCancel;
end;

procedure TCeNewDubProject.btnAcceptClick(Sender: TObject);
var
  p: TProcess;
  s: string;
  i: integer;
  o: TStringList;
begin
  p := TProcess.Create(nil);
  o := TStringList.Create;
  try
    p.Executable:= 'dub'+ exeExt;
    p.Options:= p.Options + [poUsePipes, poNoConsole];
    p.ShowWindow:= swoHIDE;
    p.Parameters.Add('init');
    p.Execute;
    // format
    processOutputToStrings(p, o);
    s := 'json'#10;
    p.Input.Write(s[1], s.length);
    // directory
    processOutputToStrings(p, o);
    s := edDir.Directory + #10;
    p.Input.Write(s[1], s.length);
    // name
    processOutputToStrings(p, o);
    s := edName.Text + #10;
    p.Input.Write(s[1], s.length);
    // description
    processOutputToStrings(p, o);
    s := edDescr.Text + #10;
    p.Input.Write(s[1], s.length);
    // author
    processOutputToStrings(p, o);
    s := edAuthor.Text + #10;
    p.Input.Write(s[1], s.length);
    // license
    processOutputToStrings(p, o);
    s := edLic.Text + #10;
    p.Input.Write(s[1], s.length);
    // copyright
    processOutputToStrings(p, o);
    s := edCopyR.Text + #10;
    p.Input.Write(s[1], s.length);
    // deps
    for i := 0 to high(deps) do
    begin
      processOutputToStrings(p, o);
      s := deps[i];
      p.Input.Write(s[1], s.length);
    end;
    processOutputToStrings(p, o);
    p.Input.WriteByte(10);
    p.CloseInput;
  finally
    p.Free;
    o.Free;
  end;
  createdNewProject := edDir.Directory + DirectorySeparator + 'dub.json';
  ModalResult := mrOK;
end;

end.

