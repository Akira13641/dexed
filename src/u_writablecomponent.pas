unit u_writableComponent;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, u_common, typinfo, fpjson, jsonparser, fpjsonrtti;

type

  (**
   * The ancestor of classes which can be saved or reloaded to/from a file.
   * It's used each time some options or data have to
   * persist from a cession to another, independently from the centralized
   * system provided by the ISessionOptionObserver/Subject mechanism.
   *
   * The descendants overrides customLoadFromFile and customSaveToFile
   * to save/load to/from a specific format.
   *)
  TCustomWritableComponent = class(TComponent)
  protected
    fFilename: string;
    fHasLoaded: boolean;
    fHasSaved: boolean;
    procedure setFilename(const value: string); virtual;
    procedure beforeLoad; virtual;
    procedure beforeSave; virtual;
    procedure afterLoad; virtual;
    procedure afterSave; virtual;
    procedure customLoadFromFile(const fname: string); virtual; abstract;
    procedure customSaveToFile(const fname: string); virtual; abstract;
  public
    procedure saveToFile(const fname: string); virtual;
    procedure loadFromFile(const fname: string); virtual;
    //
    property Filename: string read fFilename write setFilename;
    property hasLoaded: boolean read fHasLoaded;
    property hasSaved: boolean read fHasSaved;
  end;

  (**
   * The ancestor of classes which can be saved or reloaded to/from
   * a LFM text file.
   * By default, reading errors are skipped and no exception is raised.
   *)
  TWritableLfmTextComponent = class(TCustomWritableComponent)
  protected
    procedure customLoadFromFile(const fname: string); override;
    procedure customSaveToFile(const fname: string); override;
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean); virtual;
    procedure readerError(Reader: TReader; const Message: string;
      var Handled: Boolean); virtual;
  end;

  (**
   * The ancestor of classes which can be saved or reloaded to/from
   * a JSON file.
   * By default, reading errors are skipped and no exception is raised.
   *)
  TWritableJsonComponent = class(TCustomWritableComponent)
  protected
    procedure propertyError(Sender : TObject; AObject : TObject; Info : PPropInfo;
      value : TJSONData; Error : Exception; Var doContinue : Boolean); virtual;
    procedure restoreProperty(Sender : TObject; AObject : TObject; Info : PPropInfo;
      value : TJSONData; Var Handled : Boolean); virtual;
    procedure customLoadFromFile(const fname: string); override;
    procedure customSaveToFile(const fname: string); override;
  end;

implementation

{$REGION TCustomWritableComponent ----------------------------------------------}
procedure TCustomWritableComponent.beforeSave;
begin
end;

procedure TCustomWritableComponent.beforeLoad;
begin
end;

procedure TCustomWritableComponent.afterLoad;
begin
end;

procedure TCustomWritableComponent.afterSave;
begin
end;

procedure TCustomWritableComponent.setFilename(const value: string);
begin
  fFilename := value;
end;

procedure TCustomWritableComponent.saveToFile(const fname: string);
begin
  fHasSaved := true;
  beforeSave;
  try
    customSaveToFile(fname);
  except
    fHasSaved := false;
  end;
  setFilename(fname);
  afterSave;
end;

procedure TCustomWritableComponent.loadFromFile(const fname: string);
begin
  fHasLoaded := true;
  beforeLoad;
  setFilename(fname);
  try
    customLoadFromFile(fname);
  except
    fHasLoaded := false;
  end;
  afterLoad;
end;
{$ENDREGION}

{$REGION TWritableLfmTextComponent ---------------------------------------------}
procedure TWritableLfmTextComponent.customSaveToFile(const fname: string);
begin
  saveCompToTxtFile(self, fname);
end;

procedure TWritableLfmTextComponent.customLoadFromFile(const fname: string);
begin
  loadCompFromTxtFile(self, fname, @readerPropNoFound, @readerError);
end;

procedure TWritableLfmTextComponent.readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Handled := true;
  Skip := true;
end;

procedure TWritableLfmTextComponent.readerError(Reader: TReader; const Message: string;
      var Handled: Boolean);
begin
  Handled := true;
  fHasLoaded := false;
end;
{$ENDREGION}

{$REGION TWritableJsonComponent ------------------------------------------------}
procedure TWritableJsonComponent.propertyError(Sender : TObject; AObject : TObject; Info : PPropInfo;
      value : TJSONData; Error : Exception; Var doContinue : Boolean);
begin
  doContinue := true;
end;

procedure TWritableJsonComponent.restoreProperty(Sender : TObject; AObject : TObject; Info : PPropInfo;
      value : TJSONData; Var Handled : Boolean);
begin
  Handled := true;
end;

procedure TWritableJsonComponent.customSaveToFile(const fname: string);
var
  file_str: TMemoryStream;
  json_str: TJSONStreamer;
  json_dat: TJSONStringType;
begin
  file_str := TMemoryStream.Create;
  json_str := TJSONStreamer.Create(nil);
  try
    json_dat := json_str.ObjectToJSONString(self);
    file_str.Write(json_dat[1], length(json_dat));
    file_str.SaveToFile(fname);
  finally
    file_str.Free;
    json_str.Free;
  end;
end;

procedure TWritableJsonComponent.customLoadFromFile(const fname: string);
var
  file_str: TMemoryStream;
  json_str: TJSONDeStreamer;
  json_dat: TJSONStringType;
begin
  file_str := TMemoryStream.Create;
  json_str := TJSONDeStreamer.Create(nil);
  try
    json_str.OnPropertyError:= @propertyError;
    json_str.OnRestoreProperty := @restoreProperty;
    file_str.LoadFromFile(fname);
    setLength(json_dat, file_str.Size);
    file_str.Read(json_dat[1], length(json_dat));
    json_str.JSONToObject(json_dat, self);
  finally
    file_str.Free;
    json_str.Free;
  end;
end;
{$ENDREGION}

end.
