unit u_widget;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, LGHelpers, LGVector, Forms, Controls, ExtCtrls, ActnList, Menus,
  AnchorDocking, u_interfaces, u_dsgncontrols, u_common;

type

  (**
   * Base type for an UI module.
   *)
  PTDexedWidget = ^TDexedWidget;

  TDexedWidget = class;

  TWidgetDockingState =
  (
    wdsUndocked,  // from docked to undocked
    wdsDocked,    // from undocked to docked
    wdsRedocked   // docked from a site to another
  );

  TWidgetDockingChangedEvent = procedure(sender: TDexedWidget; newState: TWidgetDockingState) of object;

  { TDexedWidget }

  TDexedWidget = class(TForm, IContextualActions)
    toolbar: TDexedToolBar;
    Content: TPanel;
    Back: TPanel;
    contextMenu: TPopupMenu;
  private
    fUpdating: boolean;
    fDelayDur: Integer;
    fLoopInter: Integer;
    fUpdaterAuto: TTimer;
    fUpdaterDelay: TTimer;
    fImperativeUpdateCount: Integer;
    fLoopUpdateCount: Integer;
    fOnDockingChanged: TWidgetDockingChangedEvent;
    procedure setDelayDur(value: Integer);
    procedure setLoopInt(value: Integer);
    procedure updaterAutoProc(Sender: TObject);
    procedure updaterLatchProc(Sender: TObject);
  protected
    fIsDockable: boolean;
    fIsModal: boolean;
    fToolBarFlat: boolean;
    fToolBarVisible: boolean;
    fOldSiteParent: TWinControl;
    // TODO-cdocking: find a better way to detect that the docking state changed
    procedure Resize; override;
    // a descendant overrides to implement a periodic update.
    procedure updateLoop; virtual;
    // a descendant overrides to implement an imperative update.
    procedure updateImperative; virtual;
    // a descendant overrides to implement a delayed update.
    procedure updateDelayed; virtual;
    //
    function contextName: string; virtual;
    function contextActionCount: integer; virtual;
    function contextAction(index: integer): TAction; virtual;
    //
    function getIfModal: boolean;
    //
    procedure setToolBarVisible(value: boolean); virtual;
    procedure setToolBarFlat(value: boolean); virtual;
  published
    property updaterByLoopInterval: Integer read fLoopInter write setLoopInt;
    property updaterByDelayDuration: Integer read fDelayDur write setDelayDur;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    // prevent closing when 'locked' is checked in the header context menu
    function closeQuery: boolean; override;
    // restarts the wait period to the delayed update event.
    // if not re-called during 'updaterByDelayDuration' ms then
    // 'UpdateByDelay' is called once.
    procedure beginDelayedUpdate;
    // prevent any pending update.
    procedure stopDelayedUpdate;
    // calls immediattly any pending delayed update.
    procedure forceDelayedUpdate;

    // increments the imperative updates count.
    procedure beginImperativeUpdate;
    // decrements the imperative updates count and call updateImperative() if the
    // counter value is equal to zero.
    procedure endImperativeUpdate;
    // calls updateImperative() immediatly
    procedure forceImperativeUpdate;

    // increment a flag used to indicate if updateLoop has to be called
    procedure IncLoopUpdate;

    procedure showWidget;

    // returns true if one of the three updater is processing.
    property updating: boolean read fUpdating;
    // true by default, allow a widget to be docked.
    property isDockable: boolean read fIsDockable;
    // not if isDockable, otherwise a the widget is shown as modal form.
    property isModal: boolean read getIfModal;

    property toolbarFlat: boolean read fToolBarFlat write setToolBarFlat;
    property toolbarVisible: boolean read fToolBarVisible write setToolBarVisible;
    property onDockingChanged: TWidgetDockingChangedEvent read fOnDockingChanged write fOnDockingChanged;
  end;

  (**
   * TDexedWidget list.
   *)
  TWidgetList = specialize TGVector<TDexedWidget>;

  function CompareWidgCaption(constref Item1, Item2: TDexedWidget): SizeInt;

implementation
{$R *.lfm}

uses
  u_observer;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TDexedWidget.create(aOwner: TComponent);
var
  i: Integer;
  itm: TmenuItem;
begin
  inherited;
  fToolBarVisible := true;
  fIsDockable := true;
  fUpdaterAuto := TTimer.Create(self);
  fUpdaterAuto.Interval := 0;
  fUpdaterAuto.OnTimer := @updaterAutoProc;
  fUpdaterDelay := TTimer.Create(self);

  updaterByLoopInterval := 70;
  updaterByDelayDuration := 500;

  for i := 0 to contextActionCount-1 do
  begin
    itm := TMenuItem.Create(self);
    itm.Action := contextAction(i);
    contextMenu.Items.Add(itm);
  end;
  PopupMenu := contextMenu;

  EntitiesConnector.addObserver(self);
end;

destructor TDexedWidget.destroy;
begin
  EntitiesConnector.removeObserver(self);
  inherited;
end;

function TDexedWidget.closeQuery: boolean;
begin
  result := inherited;
  if fIsDockable and (not DockMaster.AllowDragging) and not
    (DockMaster.GetAnchorSite(self).GetTopParent = DockMaster.GetAnchorSite(self)) then
      result := false;
end;

function TDexedWidget.getIfModal: boolean;
begin
  if isDockable then
    result := false
  else
    result := fIsModal;
end;

procedure TDexedWidget.showWidget;
var
  win: TControl;
begin
  if isDockable then
  begin
    win := DockMaster.GetAnchorSite(self);
    if win <> nil then
    begin
      win.Show;
      win.BringToFront;
    end;
  end
  else
  begin
    if isModal then ShowModal else
    begin
      Show;
      BringToFront;
    end;
  end;
end;

procedure TDexedWidget.setToolBarVisible(value: boolean);
begin
  if fToolBarVisible = value then
    exit;
  toolbar.Visible := value;
  fToolBarVisible := value;
end;

procedure TDexedWidget.setToolBarFlat(value: boolean);
begin
  if fToolBarFlat = value then
    exit;
  toolbar.Flat := value;
  fToolBarFlat := value;
end;

procedure TDexedWidget.Resize;
var
  n: TWinControl = nil;
  s: TAnchorDockHostSite;
begin
  inherited;
  s := DockMaster.GetAnchorSite(self);
  if s.isNotNil then
    n := s.Parent;
  if fOldSiteParent <> n then
  begin
    if fOldSiteParent.isNil and n.isNotNil and assigned(fOnDockingChanged) then
      fOnDockingChanged(self, wdsDocked)
    else if fOldSiteParent.isNotNil and n.isNil and assigned(fOnDockingChanged) then
      fOnDockingChanged(self, wdsUndocked)
    else
      fOnDockingChanged(self, wdsRedocked);
    fOldSiteParent := n;
  end;
end;
{$ENDREGION}

{$REGION IContextualActions---------------------------------------------------}
function TDexedWidget.contextName: string;
begin
  result := '';
end;

function TDexedWidget.contextActionCount: integer;
begin
  result := 0;
end;

function TDexedWidget.contextAction(index: integer): TAction;
begin
  result := nil;
end;
{$ENDREGION}

{$REGION Updaters---------------------------------------------------------------}
procedure TDexedWidget.setDelayDur(value: Integer);
begin
  if value < 100 then value := 100;
  if fDelayDur = value then exit;
  fDelayDur := value;
  fUpdaterDelay.Interval := fDelayDur;
end;

procedure TDexedWidget.setLoopInt(value: Integer);
begin
  if fLoopInter = value then
    exit;
  fLoopInter := value;
  fUpdaterAuto.Interval := fLoopInter;
  if value = 0 then
  begin
    fUpdaterAuto.Enabled:= false;
    fLoopUpdateCount := 0;
  end
  else fUpdaterAuto.Enabled:= true;
end;

procedure TDexedWidget.IncLoopUpdate;
begin
  inc(fLoopUpdateCount);
end;

procedure TDexedWidget.beginImperativeUpdate;
begin
  Inc(fImperativeUpdateCount);
end;

procedure TDexedWidget.endImperativeUpdate;
begin
  Dec(fImperativeUpdateCount);
  if fImperativeUpdateCount > 0 then exit;
  fUpdating := true;
  updateImperative;
  fUpdating := false;
  fImperativeUpdateCount := 0;
end;

procedure TDexedWidget.forceImperativeUpdate;
begin
  fUpdating := true;
  updateImperative;
  fUpdating := false;
  fImperativeUpdateCount := 0;
end;

procedure TDexedWidget.beginDelayedUpdate;
begin
  fUpdaterDelay.Enabled := false;
  fUpdaterDelay.Enabled := true;
  fUpdaterDelay.OnTimer := @updaterLatchProc;
end;

procedure TDexedWidget.stopDelayedUpdate;
begin
  fUpdaterDelay.OnTimer := nil;
end;

procedure TDexedWidget.forceDelayedUpdate;
begin
  updaterLatchProc(nil);
end;

procedure TDexedWidget.updaterAutoProc(Sender: TObject);
begin
  fUpdating := true;
  if fLoopUpdateCount > 0 then
  	updateLoop;
  fLoopUpdateCount := 0;
  fUpdating := false;
end;

procedure TDexedWidget.updaterLatchProc(Sender: TObject);
begin
  fUpdating := true;
  updateDelayed;
  fUpdating := false;
  fUpdaterDelay.OnTimer := nil;
end;

procedure TDexedWidget.updateLoop;
begin
end;

procedure TDexedWidget.updateImperative;
begin
end;

procedure TDexedWidget.updateDelayed;
begin
end;
{$ENDREGION}

{$REGION TWidgetList----------------------------------------------------------}
function CompareWidgCaption(constref Item1, Item2: TDexedWidget): SizeInt;
begin
  result := AnsiCompareStr(Item1.Caption, Item2.Caption);
end;
{$ENDREGION}

end.
