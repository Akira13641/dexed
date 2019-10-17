unit u_inspectors;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Dialogs, PropEdits, GraphPropEdits, Graphics, typinfo,
  LCLType, u_common;

type

  TCustomPathType = (ptFile, ptFolder);

  // base class for a property representing a path
  // additionaly to the text field, a dialog can be opened
  // to select the directory or the file.
  TCustomPathEditor = class(TStringPropertyEditor)
  private
    fType: TCustomPathType;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TPathnameEditor = class(TCustomPathEditor)
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
  end;

  TFilenameEditor = class(TCustomPathEditor)
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer); override;
  end;

  TActionInEditor = class(TPropertyEditor)
    constructor Create(Hook:TPropertyEditorHook; APropCount:Integer); override;
    function GetAttributes: TPropertyAttributes; override;
    function IsReadOnly: boolean; override;
    function GetVisualValue: ansistring; override;
    procedure Edit; override;
  end;

  TListDrawValueProc = procedure(const CurValue: ansistring; Index: integer;
    ACanvas: TCanvas; const ARect:TRect; AState: TPropEditDrawState) of object;

  TColorEditor = class(TColorPropertyEditor)
    procedure ListDrawValue(const CurValue: ansistring; Index: integer;
      ACanvas: TCanvas; const ARect:TRect; AState: TPropEditDrawState); override;
  end;

implementation

procedure TColorEditor.ListDrawValue(const CurValue: ansistring; Index: integer;
  ACanvas: TCanvas; const ARect:TRect; AState: TPropEditDrawState);

  function ColorToBorderColor(AColor: TColorRef): TColor;
  type
    TColorQuad = record
      Red,
      Green,
      Blue,
      Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or
       (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else
      if pedsInEdit in AState then
      begin
        if pedsSelected in AState then
          Result := clWindow
        else
         Result := TColor(AColor);
      end else
      begin
        if pedsSelected in AState then
          Result := clHighlight
        else
         Result := clWindow;
      end;
  end;
var
  vRight, vBottom: Integer;
  vOldPenColor, vOldBrushColor: TColor;
  vOldPenStyle: TPenStyle;
  noFill: Boolean;
  //proc: TListDrawValueProc;
  Style : TTextStyle;
  OldColor : TColor;
  rc: TRect;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left - 2;
  vBottom:=ARect.Bottom-2;
  with ACanvas do
  begin
    // save off things
    vOldPenStyle := Pen.Style;
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;

    // frame things
    if pedsInEdit in AState then
    begin
      if pedsSelected in AState then
        Brush.Color := clWindow
      else
        Brush.Color := ACanvas.Brush.Color;
    end
    else
    begin
      if pedsSelected in AState then
        Brush.Color := clHighlightText
      else
       Brush.Color := clWindow;
    end;
    Pen.Color := Brush.Color;
    Pen.Style := psSolid;
    FillRect(ARect);
    Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

    // set things up and do the work
    noFill := CurValue = 'clNone';
    if noFill then
      Brush.Color := clWindow
    else
      Brush.Color := StringToColorDef(CurValue,clNone);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, vBottom - 1);
    if noFill then
    begin
      Line(ARect.Left + 1, ARect.Top + 1, vRight - 2, vBottom - 2);
      Line(ARect.Left + 1, vBottom - 2, vRight - 2, ARect.Top + 1);
    end;

    // restore the things we twiddled with
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
    Pen.Style := vOldPenStyle;
  end;

  //TMethod(proc).Code:= @TPropertyEditor.ListDrawValue;
  //TMethod(proc).Data:= self;
  //proc(CurValue, Index, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),AState);

  rc := Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom);

  FillChar(Style{%H-},SizeOf(Style),0);
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlCenter;
    Opaque := false;
    Clipping := True;
    ShowPrefix := True;
    WordBreak := False;
    SingleLine := True;
    SystemFont := true;
  end;
  If (pedsInComboList in AState) and not (pedsInEdit in AState)
  then begin
    OldColor := ACanvas.Brush.Color;
    If pedsSelected in AState then begin
      ACanvas.Brush.Color := clHighlight;
      ACanvas.Font.Color := clHighlightText;
    end
    else begin
      ACanvas.Brush.Color := clwhite{clWindow};
      ACanvas.Font.Color := clWindowText;
    end;
    ACanvas.FillRect(rc);
    ACanvas.Brush.Color := OldColor;
  end;

  ACanvas.TextRect(rc, rc.Left+2, rc.Top, CurValue, Style);
end;

function TCustomPathEditor.GetAttributes: TPropertyAttributes;
begin
  exit( inherited GetAttributes() + [paDialog]);
end;

procedure TCustomPathEditor.Edit;
var
  newValue: string;
begin
  case fType of
    ptFile:
      with TOpenDialog.create(nil) do try
        InitialDir := GetValue.extractFileName;
        FileName := GetValue;
        if Execute then
          SetValue(FileName.normalizePath);
      finally
        free;
      end;
    ptFolder:
      if SelectDirectory(GetPropInfo^.Name, GetValue, newValue) then
        SetValue(newValue);
  end;
end;

constructor TPathnameEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited;
  fType := ptFolder;
end;

constructor TFilenameEditor.Create(Hook: TPropertyEditorHook; APropCount: Integer);
begin
  inherited;
  fType := ptFile;
end;

constructor TActionInEditor.Create(Hook:TPropertyEditorHook; APropCount:Integer);
begin
  inherited;
end;

function TActionInEditor.GetAttributes: TPropertyAttributes;
begin
  exit([paReadOnly, paDialog]);
end;

function TActionInEditor.IsReadOnly: boolean;
begin
  exit(true);
end;

function TActionInEditor.GetVisualValue: ansistring;
begin
  exit('(click)');
end;

procedure TActionInEditor.Edit;
begin
  SetOrdValue(not GetOrdValue);
  Modified;
end;

initialization
  RegisterPropertyEditor(TypeInfo(TPathname), nil, '', TPathnameEditor);
  RegisterPropertyEditor(TypeInfo(TFilename), nil, '', TFilenameEditor);
  RegisterPropertyEditor(TypeInfo(TEditEvent), nil, '', TActionInEditor);
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TColorEditor);
end.

