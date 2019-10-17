unit u_sharedres;

interface

uses
  Classes, Controls, Buttons, Graphics;

procedure AssignPng(ctrl: TPersistent; const resName: string);

implementation

var
  png: TPortableNetworkGraphic;

procedure AssignPng(ctrl: TPersistent; const resName: string);
begin
  try
    png.LoadFromResourceName(HINSTANCE, resName);
    if ctrl is TCustomBitBtn then
      TCustomBitBtn(ctrl).Glyph.Assign(png)
    else if ctrl is TCustomSpeedButton then
      TCustomSpeedButton(ctrl).Glyph.Assign(png)
    else if ctrl is TBitmap then
      TBitmap(ctrl).Assign(png);
  except
  end;
end;

initialization
  png := TPortableNetworkGraphic.Create;
  //{$I ../src/u_icons.inc}
finalization
  png.Free;
end.

