unit u_diff;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  SynEdit, SynHighlighterDiff, process,
  u_common, StdCtrls, ExtCtrls, Buttons;

type
  TDiffViewer = class(TForm)
    btnIgnore: TBitBtn;
    btnAccept: TBitBtn;
    btnCancel: TBitBtn;
    editor: TSynEdit;
    diffHl: TSynDiffSyn;
    lbl: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private
  public
    constructor construct(ed: TSynEdit; const fname1, fname2: string);
  end;

implementation
{$R *.lfm}

constructor TDiffViewer.construct(ed: TSynEdit; const fname1, fname2: string);
var
  p: TProcess;
  r: TStringList;
begin
  inherited create(nil);

  editor.Gutter.LineNumberPart.Visible:=false;

  if ed.isNotNil then
    editor.Font.Assign(ed.Font);

  p := TProcess.Create(self);
  try
    p.Executable:= 'diff' + exeExt;

    lbl.Caption:= 'The file: "' + fname2 + '" has been modified by another program.'
      + LineEnding + 'Use the following diff to decide if the content should be '
      + 'reloaded.';

    if exeInSysPath(p.Executable) then
    begin
      p.Parameters.Add('-u');
      p.Parameters.Add(fname1);
      p.Parameters.Add(fname2);
      p.Options:= [poUsePipes];
      p.ShowWindow:= swoHIDE;
      p.Execute;

      r := TStringList.Create;
      try
        processOutputToStrings(p,r);
        editor.Lines.Assign(r);
      finally
        r.Free;
      end;

      while p.Running do
        sleep(1);
    end
    else editor.Lines.Add('(The "diff" tool cannot be found)');
  finally
    p.Free;
  end
end;

end.

