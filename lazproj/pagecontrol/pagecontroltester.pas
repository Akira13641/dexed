unit pagecontroltester;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ce_controls, ce_sharedres;

type
  TForm1 = class(TForm)
  private
    fPageControl: TCEPageControl;
    procedure pageControlChanged(sender: TObject);
    procedure pagePaint(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation
{$R *.lfm}

constructor TForm1.create(aOwner: TComponent);
begin
  inherited;
  fPageControl := TCEPageControl.Create(self);
  fPageControl.Parent := self;
  fPageControl.Align  := alClient;
  fPageControl.onChanged:=@pageControlChanged;

  AssignPng(fPageControl.closeButton.Glyph, 'DOCUMENT_DELETE');
  AssignPng(fPageControl.addButton.Glyph, 'DOCUMENT_ADD');
  AssignPng(fPageControl.moveLeftButton.Glyph, 'DOCUMENT_BACK');
  AssignPng(fPageControl.moveRightButton.Glyph, 'DOCUMENT_NEXT');
  AssignPng(fPageControl.splitButton.Glyph, 'SPLITTER');
end;

procedure TForm1.pageControlChanged(sender: TObject);
var
  page: TCEPage;
begin
  page := fPageControl.currentPage;
  if assigned(page) then
  begin
    if page.OnPaint = nil then
    begin
      page.OnPaint := @pagePaint;
      page.Repaint;
    end;
    if page.Caption = '' then
      page.Caption := format('<created index %d>', [page.index]);
  end;
end;

procedure TForm1.pagePaint(sender: TObject);
var
  page: TCEPage = nil;
begin
  page := TCEPage(sender);
  if assigned(page) then
  begin
    page.Canvas.Clear;
    page.Canvas.Font.Size := 22;
    page.Canvas.TextOut(10, 10, format('current index %d', [page.index]));
  end;
end;

end.

