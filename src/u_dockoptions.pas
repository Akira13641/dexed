unit u_dockoptions;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, AnchorDocking, AnchorDockOptionsDlg, XMLPropStorage,
  AnchorDockStr, Forms, Controls, u_observer, u_interfaces;

type

  (**
   * The option frame to edit the docking option, displayed in the option form.
   *)
  TDockOptionsEditor = class(TAnchorDockOptionsFrame, IEditableOptions)
  private
    fBackup: TXMLConfigStorage;
    //
    function optionedWantCategory(): string;
    function optionedWantEditorKind: TOptionEditorKind;
    function optionedWantContainer: TPersistent;
    procedure optionedEvent(event: TOptionEditorEvent);
    function optionedOptionsModified: boolean;
    //
    procedure doChanged(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

var
  DockOptionsEditor: TDockOptionsEditor;
  DockOptionContainer: TScrollBox;

constructor TDockOptionsEditor.Create(TheOwner: TComponent);
begin
  inherited;
  fBackup := TXMLConfigStorage.Create('', False);
  Master := AnchorDocking.DockMaster;
  //
  HeaderAlignLeftTrackBar.OnChange := @doChanged;
  HeaderAlignTopTrackBar.OnChange := @doChanged;
  DragThresholdTrackBar.OnChange := @doChanged;
  SplitterWidthTrackBar.OnChange := @doChanged;
  //
  ShowHeaderCaptionCheckBox.OnChange := @doChanged;
  HideHeaderCaptionForFloatingCheckBox.OnChange := @doChanged;
  FlattenHeadersCheckBox.OnChange := @doChanged;
  FilledHeadersCheckBox.OnChange := @doChanged;
  //
  HeaderStyleComboBox.OnChange := @doChanged;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TDockOptionsEditor.Destroy;
begin
  fBackup.Free;
  inherited;
end;

function TDockOptionsEditor.optionedWantCategory(): string;
begin
  exit('Docking')
end;

function TDockOptionsEditor.optionedWantEditorKind: TOptionEditorKind;
begin
  exit(oekControl);
end;

function TDockOptionsEditor.optionedWantContainer: TPersistent;
begin
  LoadFromMaster;
  fBackup.Clear;
  DockMaster.SaveSettingsToConfig(fBackup);
  SaveToMaster;
  exit(DockOptionContainer);
end;

procedure TDockOptionsEditor.optionedEvent(event: TOptionEditorEvent);
begin
  // restores
  if event = oeeCancel then
  begin
    DockMaster.LoadSettingsFromConfig(fBackup);
    LoadFromMaster;
    fBackup.Clear;
    DockMaster.SaveSettingsToConfig(fBackup);
  end
  // accept and new backup
  else if event = oeeAccept then
  begin
    SaveToMaster;
    fBackup.Clear;
    DockMaster.SaveSettingsToConfig(fBackup);
  end
  // reload
  else if event = oeeSelectCat then
  begin
    SaveToMaster;
    fBackup.Clear;
    DockMaster.SaveSettingsToConfig(fBackup);
  end
  //change
  else
  begin
    doChanged(nil);
  end;
end;

function TDockOptionsEditor.optionedOptionsModified: boolean;
begin
  exit(false);
end;

procedure TDockOptionsEditor.doChanged(Sender: TObject);
var
  hasHeaders: boolean;
begin
  DragThresholdLabel.Caption := adrsDragThreshold +
    ' (' + IntToStr(DragThresholdTrackBar.Position) + ')';
  HeaderAlignTopLabel.Caption := adrsHeaderAlignTop +
    ' (' + IntToStr(HeaderAlignTopTrackBar.Position) + ')';
  HeaderAlignLeftLabel.Caption := adrsHeaderAlignLeft +
    ' (' + IntToStr(HeaderAlignLeftTrackBar.Position) + ')';
  SplitterWidthLabel.Caption := adrsSplitterWidth +
    ' (' + IntToStr(SplitterWidthTrackBar.Position) + ')';
  //
  hasHeaders:=ShowHeaderCheckBox.Checked;
  ShowHeaderCaptionCheckBox.Enabled:=HasHeaders;
  HideHeaderCaptionForFloatingCheckBox.Enabled:=HasHeaders;
  FlattenHeadersCheckBox.Enabled:=HasHeaders;
  FilledHeadersCheckBox.Enabled:=HasHeaders;
  //
  DockMaster.HeaderFilled := FilledHeadersCheckBox.Checked;
  DockMaster.HeaderFlatten:= FlattenHeadersCheckBox.Checked;
  DockMaster.ShowHeaderCaption:= ShowHeaderCaptionCheckBox.Checked;
  DockMaster.HideHeaderCaptionFloatingControl:= HideHeaderCaptionForFloatingCheckBox.Checked;
  //
  SaveToMaster;
end;

initialization
  DockOptionsEditor := TDockOptionsEditor.Create(nil);
  DockOptionContainer:= TScrollBox.Create(nil);
  DockOptionsEditor.Parent := DockOptionContainer;
  DockOptionContainer.Align:= alClient;
  DockOptionsEditor.Align:= alClient;

finalization
  DockOptionsEditor.Free;
  DockOptionContainer.Free;
end.
