unit u_dsgncontrols;
{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, buttons, graphics,
  Menus, LMessages, LCLType, Toolwin;

type

  (**
   * Toolbutton with methods to load the glyph from the shared resources
   *)
  TDexedToolButton = class(TToolButton)
  private
    fResourceName: string;
    fScaledSeparator: boolean;
    fPng: TPortableNetworkGraphic;
    fDPng: TPortableNetworkGraphic;
    function  findResourceWithSize(value: integer): boolean;
    procedure setResourceName(const value: string);
    procedure setScaledSeparator(value: boolean);
    procedure setToolBar(value: TToolbar);
  protected
    procedure reloadPng;
    procedure Paint; override;
    procedure SetEnabled(Value: Boolean); override;
  published
    property resourceName: string read fResourceName write setResourceName;
    property scaledSeparator: boolean read fScaledSeparator write setScaledSeparator;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure toBitmap(value: TBitmap);
  end;

  TToolBarScaling = (auto, force16, force24, force32);

  (**
   * Toolbar with design-time support for TCEToolbutton
   *)
  TDexedToolBar = class(TToolBar)
  protected
    fDesignMenu: TPopupMenu;
    fToolBarScaling: TToolBarScaling;
    procedure dsgnAdd(style: TToolButtonStyle);
    procedure dsgnAddButton(sender: TObject);
    procedure dsgnAddDivider(sender: TObject);
    procedure dsgnAddSeparator(sender: TObject);
    procedure dsgnAddDropdown(sender: TObject);
    procedure dsgnAddCheckbutton(sender: TObject);
    procedure setScaling(value: TToolBarScaling);
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
  published
    property ButtonHeight stored false;
    property ButtonWidth stored false;
    property Scaling: TToolBarScaling read fToolBarScaling write setScaling stored false;
  end;

  procedure register;

implementation

procedure register;
begin
  RegisterComponents('dexed', [TDexedToolBar, TDexedToolButton]);
end;

constructor TDexedToolButton.Create(TheOwner: TComponent);
begin
  inherited;
  fPng := TPortableNetworkGraphic.Create;
  fDPng := TPortableNetworkGraphic.Create;
  AutoSize := true;
end;

destructor TDexedToolButton.Destroy;
begin
  fPng.FreeImage;
  fPng.Free;
  fDPng.FreeImage;
  fDPng.Free;
  inherited;
end;

procedure TDexedToolButton.setToolBar(value: TToolbar);
begin
  FToolBar := value;
end;

function TDexedToolButton.findResourceWithSize(value: integer): boolean;
var
  n: string;
begin
  n := resourceName + IntToStr(value);
  result := FindResource(HINSTANCE, PChar(n), PChar(RT_RCDATA)) <> 0;
end;

procedure TDexedToolButton.reloadPng;
var
  i: integer;
  j: integer;
  p: PRGBAQuad;
  g: byte;
  n: string;
  h: integer;
begin
  if csDesigning in ComponentState then
    exit;

  fPng.FreeImage;
  fDPng.FreeImage;

  n := resourceName;
  h := FToolBar.ButtonHeight;

  if n.IsEmpty then
    exit;

  if (h > 50) and findResourceWithSize(32) then
    n += '32'
  else if (h > 30) and findResourceWithSize(24) then
    n += '24';

  fPng.LoadFromResourceName(HINSTANCE, n);
  fDPng.LoadFromResourceName(HINSTANCE, n);

  if fDpng.PixelFormat = pf32bit then
    for i:= 0 to fDPng.Height-1 do
  begin
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}{$R-}
    p := PRGBAQuad(fDPng.ScanLine[i]);
    {$POP}
    for j:= 0 to fDPng.Width-1 do
    begin
      g := (p^.Red div 5) + (p^.Green div 100) * 70 + (p^.Blue div 11);
      p^.Green:=g;
      p^.Blue:=g;
      p^.Red:=g;
      p += 1;
    end;
  end;
end;

procedure TDexedToolButton.setResourceName(const value: string);
begin
  if fResourceName = value then
    exit;
  fResourceName := value;
  if csDesigning in ComponentState then
    exit;
  if Style in [tbsButton, tbsDropDown] then
    reloadPng;
  if assigned(fToolBar) then
    FToolBar.Repaint;
end;

procedure TDexedToolButton.setScaledSeparator(value: boolean);
begin
  if fScaledSeparator = value then
    exit;
  fScaledSeparator:=value;
  // store ratio if true
end;

procedure TDexedToolButton.SetEnabled(Value: Boolean);
var
  old: boolean;
begin
  old := Enabled;
  inherited;
  if (old <> Enabled) and assigned(fToolBar) then
    FToolBar.Repaint;
end;

procedure TDexedToolButton.toBitmap(value: TBitmap);
begin
  value.Assign(fPng);
end;

procedure TDexedToolButton.Paint;
var
  rc: TRect;
  x, y: integer;
begin
  inherited;
  if (fResourceName <> '') and (style in [tbsButton, tbsDropDown, tbsCheck]) then
  begin
    rc := ClientRect;
    if Style = tbsDropDown then
      rc.Right := rc.left + FToolBar.ButtonWidth;
    x := ((rc.Right - rc.Left) - fPng.width) div 2;
    y := ((rc.Bottom - rc.Top) - fPng.Height) div 2;
    if Enabled then
      Canvas.Draw(x, y, fPng)
    else
      Canvas.Draw(x, y, fDPng);
  end;
end;

constructor TDexedToolBar.Create(TheOwner: TComponent);
var
  item: TMenuItem;
begin
  inherited;
  if csDesigning in ComponentState then
  begin
    fDesignMenu := TPopupMenu.Create(nil);
    fDesignMenu.Name:= 'CEToolbarDsgnMenu';
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add button';
    item.OnClick:= @dsgnAddButton;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add separator';
    item.OnClick:= @dsgnAddSeparator;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add divider';
    item.OnClick:= @dsgnAddDivider;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add check';
    item.OnClick:= @dsgnAddCheckbutton;
    fDesignMenu.Items.Add(item);
    item := TMenuItem.Create(fDesignMenu);
    item.Caption:= 'add dropdown';
    item.OnClick:= @dsgnAddDropdown;
    fDesignMenu.Items.Add(item);
  end;
  borderSpacing.Left := 2;
  borderSpacing.Top := 2;
  borderSpacing.Right := 2;
  borderSpacing.Bottom := 0;
  EdgeInner:= esNone;
  EdgeOuter:= esNone;
  Flat := false;
  Transparent := true;
end;

destructor TDexedToolBar.Destroy;
begin
  if csDesigning in ComponentState then
    fDesignMenu.Free;
  inherited;
end;

procedure TDexedToolBar.Loaded;
begin
  inherited;
  setScaling(auto);
end;

procedure TDexedToolBar.setScaling(value: TToolBarScaling);
var
  i: integer;
begin
  fToolBarScaling := value;
  if ((fToolBarScaling = TToolBarScaling.auto) and (ScaleY(16, 96) >= 32)) or
     (fToolBarScaling = TToolBarScaling.force32) then
  begin
    height := 60;
    ButtonHeight := 56;
    ButtonWidth := 56;
  end else
  if ((fToolBarScaling = TToolBarScaling.auto) and (ScaleY(16, 96) >= 24)) or
     (fToolBarScaling = TToolBarScaling.force24) then
  begin
    height := 44;
    ButtonHeight := 42;
    ButtonWidth := 42;
  end
  else
  begin
    height :=  30;
    ButtonHeight := 28;
    ButtonWidth := 28;
  end;

  for i := 0 to ControlCount-1 do
    if Controls[i] is TDexedToolButton then
      TDexedToolButton(Controls[i]).reloadPng;

  Repaint;
end;

procedure TDexedToolBar.CMDesignHitTest(var Message: TCMDesignHitTest);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    exit;
  if Message.Keys <> MK_RBUTTON then
    exit;
  Message.Result := 0;
  fDesignMenu.PopUp(Mouse.CursorPos.x,Mouse.CursorPos.y);
end;

procedure TDexedToolBar.dsgnAdd(style: TToolButtonStyle);
var
  button: TDexedToolButton;
  str: string = '';
  i: integer = 0;
begin
  button := TDexedToolButton.Create(owner);
  while true do
  begin
    str := format('button%d',[i]);
    if owner.FindComponent(str) = nil then
      break;
    i += 1;
  end;
  button.Name:= str;
  button.Style := style;
  InsertControl(button);
  ButtonList.add(button);
  button.setToolBar(self);
  if style = tbsDivider then
    width := 16;
end;

procedure TDexedToolBar.dsgnAddButton(sender: TObject);
begin
  dsgnAdd(tbsButton);
end;

procedure TDexedToolBar.dsgnAddDivider(sender: TObject);
begin
  dsgnAdd(tbsDivider);
end;

procedure TDexedToolBar.dsgnAddSeparator(sender: TObject);
begin
  dsgnAdd(tbsSeparator);
end;

procedure TDexedToolBar.dsgnAddCheckbutton(sender: TObject);
begin
  dsgnAdd(tbsCheck);
end;

procedure TDexedToolBar.dsgnAddDropdown(sender: TObject);
begin
  dsgnAdd(tbsDropDown);
end;

initialization
  RegisterClasses([TDexedToolBar, TDexedToolButton]);
end.

