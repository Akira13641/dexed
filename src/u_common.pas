unit u_common;

{$I u_defines.inc}

interface

uses

  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Windows, JwaTlHelp32, registry,
  {$ELSE}
  ExtCtrls,
  {$ENDIF}
  {$IFNDEF CEBUILD}
  forms, ComCtrls,
  {$ENDIF}
  LazFileUtils, FileUtil, process, asyncprocess, LGHelpers, LGHashMap, LGHashSet, LCLIntf, strutils,
  fpjson, graphics;

const
  exeExt = {$IFDEF WINDOWS} '.exe' {$ELSE} ''   {$ENDIF};
  objExt = {$IFDEF WINDOWS} '.obj' {$ELSE} '.o' {$ENDIF};
  libExt = {$IFDEF WINDOWS} '.lib' {$ELSE} '.a' {$ENDIF};
  dynExt = {$IFDEF WINDOWS} '.dll' {$ENDIF} {$IFDEF LINUX}'.so'{$ENDIF} {$IFDEF DARWIN}'.dylib'{$ENDIF};

type

  TIndentationMode = (imNone, imSpaces, imTabs, imMixed);

  THasMain = (mainNo, mainYes, mainDefaultBehavior);

  // desscibes leading whites of a line
  TIndentComposition = record
    // num spaces
    numS: integer;
    // num tabs
    numT: integer;
  end;

  // HashMap for TValue by string
  generic TStringHashMap<TValue> = class(specialize TGHashMapLP<String, TValue>);

  // HashSet for any object
  generic TObjectHashSet<TValue: TObject> = class(specialize TGHashSetLP<TValue>);

  // Used instead of TStringList when the usage would mostly be ".IndexOf"
  TStringHashSet = class(specialize TGHashSetLP<String>);

  // aliased to get a custom prop inspector
  TPathname = type string;
  TFilename = type string;
  TEditEvent = type boolean;

  // sugar for classes
  TObjectHelper = class helper for TObject
    function isNil: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function isNotNil: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  // sugar for pointers
  TPointerHelper = type helper for Pointer
    function isNil: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function isNotNil: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  // sugar for strings
  TDexedStringHelper = type helper(TStringHelper) for string
    function isEmpty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function isNotEmpty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function isBlank: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function extractFileName: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    function extractFileExt: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    function extractFilePath: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    function extractFileDir: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    function stripFileExt: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    function fileExists: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function dirExists: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function upperCase: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    function length: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function toIntNoExcept(default: integer = -1): integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function toInt: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function normalizePath: string; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TStringsHelper = class helper for TStrings
    // Same as text but without the additional line terminator.
    function strictText: string; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TJSONObjectHelper = class helper for TJSONObject
    function findObject(const key: TJSONStringType; out value: TJSONObject): boolean;
    function findArray(const key: TJSONStringType; out value: TJSONArray): boolean;
    function findAny(const key: TJSONStringType; out value: TJSONData): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TListItemsHelper = class helper for TListItems
    function findCaption(const value: string; out res: TListItem): boolean; overload;
  end;

  (**
   *  TProcess with assign() 'overriden'.
   *)
  TProcessEx = class helper for TProcess
  public
    procedure Assign(value: TPersistent);
  end;

  (**
   * CollectionItem used to store a shortcut.
   *)
  TPersistentShortcut = class(TCollectionItem)
  private
    fShortcut: TShortCut;
    fActionName: string;
  published
    property shortcut: TShortCut read fShortcut write fShortcut;
    property actionName: string read fActionName write fActionName;
  public
    procedure assign(value: TPersistent); override;
  end;

  TIconScaledSize = (iss16, iss24, iss32);

  (**
   * Indicates the ideal icon size depending on the system DPI settings
   *)
  function GetIconScaledSize: TIconScaledSize;

  (**
   * Save a component with a readable aspect.
   *)
  procedure saveCompToTxtFile(value: TComponent; const fname: string);

  (**
   * Load a component. Works in pair with saveCompToTxtFile().
   *)
  procedure loadCompFromTxtFile(value: TComponent; const fname: string;
    notFoundClbck: TPropertyNotFoundEvent = nil; errorClbck: TReaderError = nil);

  (**
   * Converts a relative path to an absolute path.
   *)
  function expandFilenameEx(const basePath, fname: string): string;

  (**
   * Patches the directory separators from a string.
   * This is used to ensure that a project saved on a platform can be loaded
   * on another one.
   *)
  function patchPlateformPath(const path: string): string;
  procedure patchPlateformPaths(const paths: TStrings);

  (**
   * Patches the file extension from a string.
   * This is used to ensure that a project saved on a platform can be loaded
   * on another one. Note that the ext which are handled are specific to coedit projects.
   *)
  function patchPlateformExt(const fname: string): string;

  (**
   * Returns aFilename without its extension.
   *)
  function stripFileExt(const fname: string): string;

  (**
   * Returns an unique object identifier, based on its heap address.
   *)
  function uniqueObjStr(const value: TObject): string;

  (**
   * Reduces a filename if its length is over the threshold defined by charThresh.
   * Even if the result is not usable anymore, it avoids any "visually-overloaded" MRU menu.
   *)
  function shortenPath(const path: string; thresh: Word = 60): string;

  (**
   * Returns the user data dir.
   *)
  function getUserDataPath: string;

  (**
   * Returns the folder where Dexed stores the data, the cache, the settings.
   *)
  function getDocPath: string;

  (**
   * Fills aList with the names of the files located in aPath.
   *)
  procedure listFiles(list: TStrings; const path: string; recursive: boolean = false);

  (**
   * Fills aList with the names of the folders located in aPath.
   *)
  procedure listFolders(list: TStrings; const path: string);

  (**
   * Returns true if aPath contains at least one sub-folder.
   *)
  function hasFolder(const path: string): boolean;

  (**
   * Fills aList with the system drives.
   *)
  procedure listDrives(list: TStrings);

  (**
   * If aPath ends with an asterisk then fills aList with the names of the files located in aPath.
   * Returns true if aPath was 'asterisk-ifyed'.
   *)
  function listAsteriskPath(const path: string; list: TStrings; exts: TStrings = nil): boolean;

  (**
   * Lets the shell open a file.
   *)
  function shellOpen(fname: string; wait: boolean = true): boolean;

  (**
   * Returns true if anExeName can be spawn without its full path.
   *)
  function exeInSysPath(fname: string): boolean;

  (**
   * Returns the full path to anExeName. Works if exeInSysPath() returns true.
   *)
  function exeFullName(fname: string): string;

  (**
   * Clears then fills aList with aProcess output stream.
   *)
  procedure processOutputToStrings(process: TProcess; list: TStrings);

  (**
   * Copy available process output to a stream.
   *)
  procedure processOutputToStream(process: TProcess; output: TMemoryStream);

  (**
   * Terminates and frees aProcess.
   *)
  procedure killProcess(var process: TAsyncProcess);

  (**
   * Ensures that the i/o process pipes are not redirected if it waits on exit.
   *)
  procedure ensureNoPipeIfWait(process: TProcess);

  (**
   * Returns true if ExeName is already running.
   *)
  function AppIsRunning(const fname: string):Boolean;

  (**
   * Returns the length of the line ending in fname.
   *)
  function getLineEndingLength(const fname: string): byte;

  (**
   * Returns the length of the line ending for the current platform.
   *)
  function getSysLineEndLen: byte;

  (**
   * Returns the common folder of the file names stored in aList.
   *)
  function commonFolder(const files: TStringList): string;

  (**
   * Returns true if ext matches a file extension whose type is highlightable (D)
   *)
  function hasDlangSyntax(const ext: string): boolean;

  (**
   * Returns true if ext matches a file extension whose type is highlightable (C/C++)
   *)
  function hasCppSyntax(const ext: string): boolean;

  (**
   * Returns true if ext matches a file extension whose type can be passed as source.
   *)
  function isDlangCompilable(const ext: string): boolean;

  (**
   * Returns true if ext matches a file extension whose type is editable in Dexed.
   *)
  function isEditable(const ext: string): boolean;

  (**
   * Returns true if str starts with a semicolon or a double slash.
   * This is used to disable TStringList items in several places
   *)
  function isStringDisabled(const str: string): boolean;

  (**
   * Indicates wether str is only made of blank characters
   *)
  function isBlank(const str: string): boolean;

  (**
   * Converts a global match expression to a regular expression.
   * Limitation: Windows style, negation of set not handled [!a-z] [!abc]
   *)
  function globToReg(const glob: string ): string;

  (**
   * Removes duplicate items in strings
   *)
  procedure deleteDups(strings: TStrings);

  procedure tryRaiseFromStdErr(proc: TProcess);

  // Converts all leading whites to spaces. Tabs takes width * spaces.
  function leadingTabsToSpaces(const value: string; width: integer): string;

  // Converts all leading whites to tabs. Fails if width doesn't fivide number of spaces.
  function leadingSpacesToTabs(const value: string; width: integer): string;

  // Converts the delta between two calls to GetTickCount64 to a string indicating a duration.
  function formatTicksAsDuration(ticks: UInt64): string;

var
  // additional directories to find background tools
  additionalPath: string;
  // terminal used under posix
  consoleProgram: string;


implementation

function GetIconScaledSize: TIconScaledSize;
var
  h : integer;
begin
  result := TIconScaledSize.iss16;
  h := ScaleY(16, 96);
  if h >= 24 then
    result := TIconScaledSize.iss24;
  if h >= 32 then
    result := TIconScaledSize.iss32;
end;

procedure TPersistentShortcut.assign(value: TPersistent);
var
  src: TPersistentShortcut;
begin
  if value is TPersistentShortcut then
  begin
    src := TPersistentShortcut(value);
    fActionName := src.fActionName;
    fShortcut := src.fShortcut;
  end
  else inherited;
end;

function TObjectHelper.isNil: boolean;
begin
  exit(self = nil);
end;

function TObjectHelper.isNotNil: boolean;
begin
  exit(self <> nil);
end;

function TPointerHelper.isNil: boolean;
begin
  exit(self = nil);
end;

function TPointerHelper.isNotNil: boolean;
begin
  exit(self <> nil);
end;

function TDexedStringHelper.isEmpty: boolean;
begin
  exit(self = '');
end;

function TDexedStringHelper.isNotEmpty: boolean;
begin
  exit(self <> '');
end;

function TDexedStringHelper.isBlank: boolean;
begin
  exit(u_common.isBlank(self));
end;

function TDexedStringHelper.extractFileName: string;
begin
  exit(sysutils.extractFileName(self));
end;

function TDexedStringHelper.extractFileExt: string;
begin
  exit(sysutils.extractFileExt(self));
end;

function TDexedStringHelper.extractFilePath: string;
begin
  exit(sysutils.extractFilePath(self));
end;

function TDexedStringHelper.extractFileDir: string;
begin
  exit(sysutils.extractFileDir(self));
end;

function TDexedStringHelper.stripFileExt: string;
begin
  exit(u_common.stripFileExt(self));
end;

function TDexedStringHelper.fileExists: boolean;
begin
  exit(sysutils.FileExists(self));
end;

function TDexedStringHelper.dirExists: boolean;
begin
  exit(sysutils.DirectoryExists(self));
end;

function TDexedStringHelper.upperCase: string;
begin
  exit(sysutils.upperCase(self));
end;

function TDexedStringHelper.length: integer;
begin
  exit(system.length(self));
end;

function TDexedStringHelper.toInt: integer;
begin
  exit(strToInt(self));
end;

function TDexedStringHelper.toIntNoExcept(default: integer = -1): integer;
begin
  exit(StrToIntDef(self, default));
end;

function TDexedStringHelper.normalizePath: string;
begin
  exit(TrimFilename(self));
end;

function TStringsHelper.strictText: string;
begin
  result := self.Text;
  setLength(result, result.length - self.LineBreak.length);
end;


function TJSONObjectHelper.findObject(const key: TJSONStringType; out value: TJSONObject): boolean;
var
  v: TJSONData;
begin
  v := self.Find(key);
  if v.isNotNil then
  begin
    result := v.JSONType = jtObject;
    if result then
      value := TJSONObject(v);
  end
  else
    result := false;
end;

function TJSONObjectHelper.findArray(const key: TJSONStringType; out value: TJSONArray): boolean;
var
  v: TJSONData;
begin
  v := self.Find(key);
  if v.isNotNil then
  begin
    result := v.JSONType = jtArray;
    if result then
      value := TJSONArray(v);
  end
  else
    result := false;
end;

function TJSONObjectHelper.findAny(const key: TJSONStringType; out value: TJSONData): boolean;
begin
  value := self.Find(key);
  result := value.isNotNil;
end;

function TListItemsHelper.findCaption(const value: string; out res: TListItem): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to self.Count-1 do
  begin
    res := self.Item[i];
    if res.Caption = value then
      exit(true);
  end;
end;

procedure TProcessEx.Assign(value: TPersistent);
var
  src: TProcess;
begin
  if value is TProcess then
  begin
    src := TProcess(value);
    PipeBufferSize := src.PipeBufferSize;
    Active := src.Active;
    Executable := src.Executable;
    Parameters := src.Parameters;
    ConsoleTitle := src.ConsoleTitle;
    CurrentDirectory := src.CurrentDirectory;
    Desktop := src.Desktop;
    Environment := src.Environment;
    Options := src.Options;
    Priority := src.Priority;
    StartupOptions := src.StartupOptions;
    ShowWindow := src.ShowWindow;
    WindowColumns := src.WindowColumns;
    WindowHeight := src.WindowHeight;
    WindowLeft := src.WindowLeft;
    WindowRows := src.WindowRows;
    WindowTop := src.WindowTop;
    WindowWidth := src.WindowWidth;
    FillAttribute := src.FillAttribute;
    XTermProgram := src.XTermProgram;
  end
  else inherited;
end;

procedure saveCompToTxtFile(value: TComponent; const fname: string);
var
  str1, str2: TMemoryStream;
begin
  str1 := TMemoryStream.Create;
  str2 := TMemoryStream.Create;
  try
    str1.WriteComponent(value);
    str1.Position := 0;
    ObjectBinaryToText(str1,str2);
    ForceDirectories(fname.extractFilePath);
    str2.SaveToFile(fname);
  finally
    str1.Free;
    str2.Free;
  end;
end;

procedure loadCompFromTxtFile(value: TComponent; const fname: string;
  notFoundClbck: TPropertyNotFoundEvent = nil; errorClbck: TReaderError = nil);
var
  str1, str2: TMemoryStream;
  rdr: TReader;
begin
  str1 := TMemoryStream.Create;
  str2 := TMemoryStream.Create;
  try
    str1.LoadFromFile(fname);
    str1.Position := 0;
    ObjectTextToBinary(str1, str2);
    str2.Position := 0;
    try
      rdr := TReader.Create(str2, 4096);
      try
        rdr.OnPropertyNotFound := notFoundClbck;
        rdr.OnError := errorClbck;
        rdr.ReadRootComponent(value);
      finally
        rdr.Free;
      end;
    except
    end;
  finally
    str1.Free;
    str2.Free;
  end;
end;

function expandFilenameEx(const basePath, fname: string): string;
var
  curr: string = '';
begin
  if FilenameIsAbsolute(fname) then
    exit(fname);
  getDir(0, curr);
  try
    if (curr <> basePath) and basePath.dirExists then
      chDir(basePath);
    result := expandFileName(fname);
  finally
    chDir(curr);
  end;
end;

function patchPlateformPath(const path: string): string;
  function patchProc(const src: string; const invalid: char): string;
  var
    i: Integer;
    dir: string;
  begin
    dir := ExtractFileDrive(src);
    if dir.length > 0 then
      result := src[dir.length+1..src.length]
    else
      result := src;
    i := pos(invalid, result);
    if i <> 0 then
    begin
      repeat
        result[i] := directorySeparator;
        i := pos(invalid,result);
      until
        i = 0;
    end;
    result := dir + result;
  end;
begin
  result := path;
  {$IFDEF WINDOWS}
  result := patchProc(result, '/');
  {$ELSE}
  result := patchProc(result, '\');
  {$ENDIF}
end;

procedure patchPlateformPaths(const paths: TStrings);
var
  i: Integer;
  str: string;
begin
  for i:= 0 to paths.Count-1 do
  begin
    str := paths[i];
    paths[i] := patchPlateformPath(str);
  end;
end;

function patchPlateformExt(const fname: string): string;
var
  ext, newext: string;
begin
  ext := fname.extractFileExt;
  newext := '';
  {$IFDEF MSWINDOWS}
  case ext of
    '.so':    newext := '.dll';
    '.dylib': newext := '.dll';
    '.a':     newext := '.lib';
    '.o':     newext := '.obj';
    else      newext := ext;
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  case ext of
    '.dll':   newext := '.so';
    '.dylib': newext := '.so';
    '.lib':   newext := '.a';
    '.obj':   newext := '.o';
    '.exe':   newext := '';
    else      newext := ext;
  end;
  {$ENDIF}
  {$IFDEF DARWIN}
  case ext of
    '.dll': newext := '.dylib';
    '.so':  newext := '.dylib';
    '.lib': newext := '.a';
    '.obj': newext := '.o';
    '.exe': newext := '';
    else    newext := ext;
  end;
  {$ENDIF}
  result := ChangeFileExt(fname, newext);
end;

function stripFileExt(const fname: string): string;
begin
  if Pos('.', fname) > 1 then
    exit(ChangeFileExt(fname, ''))
  else
    exit(fname);
end;

function uniqueObjStr(const value: TObject): string;
begin
  {$PUSH}{$HINTS OFF}{$WARNINGS OFF}{$R-}
  exit( format('%.8X',[NativeUint(value)]));
  {$POP}
end;

function shortenPath(const path: string; thresh: Word = 60): string;
var
  i: NativeInt;
  sepCnt: integer = 0;
  drv: string;
  pth1: string;
begin
  if path.length <= thresh then
    exit(path);

  drv := extractFileDrive(path);
  i := path.length;
  while(i <> drv.length+1) do
  begin
    Inc(sepCnt, Byte(path[i] = directorySeparator));
    if sepCnt = 2 then
      break;
    Dec(i);
  end;
  pth1 := path[i..path.length];
  exit(format('%s%s...%s', [drv, directorySeparator, pth1]));
end;

function getUserDataPath: string;
begin
  {$IFDEF WINDOWS}
  result := sysutils.GetEnvironmentVariable('APPDATA');
  {$ENDIF}
  {$IFDEF LINUX}
  result := sysutils.GetEnvironmentVariable('HOME') + '/.config';
  {$ENDIF}
  {$IFDEF DARWIN}
  result := sysutils.GetEnvironmentVariable('HOME') + '/Library/Application Support';
  {$ENDIF}
  if not DirectoryExists(result) then
    raise Exception.Create('dexed failed to retrieve the user data folder');
  if result[result.length] <> DirectorySeparator then
    result += directorySeparator;
end;

function getDocPath: string;
var
  o: string;
  n: string;
begin
  o := getUserDataPath + 'Coedit' + directorySeparator;
  n := getUserDataPath + 'dexed' + directorySeparator;
  if not n.dirExists and o.dirExists then
    CopyDirTree(o, n);
  result := n;
end;

function isFolder(sr: TSearchRec): boolean;
begin
  result := (sr.Name <> '.') and  (sr.Name <> '..' ) and  (sr.Name <> '' ) and
    (sr.Attr and faDirectory = faDirectory);
end;

procedure listFiles(list: TStrings; const path: string; recursive: boolean = false);
var
  sr: TSearchrec;
procedure tryAdd;
begin
  if sr.Attr and faDirectory <> faDirectory then
    list.Add(path+ directorySeparator + sr.Name);
end;
begin
  if findFirst(path + directorySeparator + '*', faAnyFile, sr) = 0 then
  try
    repeat
      tryAdd;
      if recursive then if isFolder(sr) then
        listFiles(list, path + directorySeparator + sr.Name, recursive);
    until
      findNext(sr) <> 0;
  finally
    sysutils.FindClose(sr);
  end;
end;

procedure listFolders(list: TStrings; const path: string);
var
  sr: TSearchrec;
begin
  if findFirst(path + '*', faAnyFile, sr) = 0 then
  try
    repeat if isFolder(sr) then
      list.Add(path + sr.Name);
    until findNext(sr) <> 0;
  finally
    sysutils.FindClose(sr);
  end;
end;

function hasFolder(const path: string): boolean;
var
  sr: TSearchrec;
  res: boolean;
begin
  res := false;
  if findFirst(path + directorySeparator + '*', faDirectory, sr) = 0 then
  try
    repeat if isFolder(sr) then
    begin
      res := true;
      break;
    end;
    until findNext(sr) <> 0;
  finally
    sysutils.FindClose(sr);
  end;
  result := res;
end;

function listAsteriskPath(const path: string; list: TStrings; exts: TStrings = nil): boolean;
var
  pth, ext, fname: string;
  files: TStringList;
begin
  result := false;
  if path.isEmpty then
    exit;

  if path[path.length] = '*' then
  begin
    pth := path[1..path.length-1];
    if pth[pth.length] in ['/', '\'] then
      pth := pth[1..pth.length-1];
    if not pth.dirExists then exit(false);
    //
    files := TStringList.Create;
    try
      listFiles(files, pth, true);
      for fname in files do
      begin
        if exts = nil then
          list.Add(fname)
        else
        begin
          ext := fname.extractFileExt;
          if exts.IndexOf(ext) <> -1 then
            list.Add(fname);
        end;
      end;
    finally
      files.Free;
    end;
    exit(true);
  end;
  exit(false);
end;

procedure listDrives(list: TStrings);
{$IFDEF WINDOWS}
var
  drv: char;
  ltr, nme: string;
  OldMode : Word;
  {$ENDIF}
begin
  {$IFDEF WINDOWS}
  setLength(nme, 255);
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    for drv := 'A' to 'Z' do
    begin
      try
        ltr := drv + ':\';
        if not GetVolumeInformation(PChar(ltr), PChar(nme), 255, nil, nil, nil, nil, 0) then
          continue;
        case GetDriveType(PChar(ltr)) of
           DRIVE_REMOVABLE, DRIVE_FIXED, DRIVE_REMOTE: list.Add(ltr);
        end;
      except
        // SEM_FAILCRITICALERRORS: exception is sent to application.
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
  {$ELSE}
  list.Add('//');
  {$ENDIF}
end;

function shellOpen(fname: string; wait: boolean = true): boolean;
begin
  {$IFDEF WINDOWS}
  result := ShellExecute(0, 'OPEN', PChar(fname), nil, nil, SW_SHOW) > 32;
  {$ELSE}
  with TProcess.Create(nil) do
  try
    {$IFDEF LINUX}
    Executable := 'xdg-open';
    if (fname.length > 1) and (fname[1..2] = '//') then
      fname := fname[2..fname.length];
    {$ELSE}
    {$IFDEF DARWIN}
    Executable := 'open';
    {$ENDIF}
    {$ENDIF}
    Parameters.Add(fname);
    Execute;
    if wait then
      while Running do
        sleep(1);
  finally
    result := ExitStatus = 0;
    Free;
  end;
  {$ENDIF}
end;

function exeInSysPath(fname: string): boolean;
begin
  exit(exeFullName(fname) <> '');
end;

function exeFullName(fname: string): string;
var
  ext: string;
  env: string;
begin
  ext := fname.extractFileExt;
  if ext.isEmpty then
    fname += exeExt;
  //full path already specified
  if fname.fileExists and (not fname.extractFileName.fileExists) then
    exit(fname);
  //
  env := sysutils.GetEnvironmentVariable('PATH');
  // maybe in current dir
  if fname.fileExists then
    env += PathSeparator + GetCurrentDir;
  if additionalPath.isNotEmpty then
    env += PathSeparator + additionalPath;
  {$IFNDEF CEBUILD}
  if Application.isNotNil then
    env += PathSeparator + ExtractFileDir(application.ExeName.ExtractFilePath);
  {$ENDIF}
  exit(ExeSearch(fname, env));
end;

procedure processOutputToStrings(process: TProcess; list: TStrings);
var
  str: TMemoryStream;
  sum: Integer = 0;
  cnt: Integer;
  buffSz: Integer;
begin
  if not (poUsePipes in process.Options) then
    exit;

  str := TMemoryStream.Create;
  try
    buffSz := process.PipeBufferSize;
    // temp fix: messages are cut if the TAsyncProcess version is used on simple TProcess.
    if process is TAsyncProcess then
    begin
      while process.Output.NumBytesAvailable <> 0 do
      begin
        str.SetSize(sum + buffSz);
        cnt := process.Output.Read((str.Memory + sum)^, buffSz);
        sum += cnt;
      end;
    end else
    begin
      repeat
        str.SetSize(sum + buffSz);
        cnt := process.Output.Read((str.Memory + sum)^, buffSz);
        sum += cnt;
      until
        cnt = 0;
    end;
    str.Size := sum;
    list.LoadFromStream(str);
  finally
    str.Free;
  end;
end;

procedure processOutputToStream(process: TProcess; output: TMemoryStream);
var
  sum, cnt: Integer;
const
  buffSz = 2048;
begin
  if not (poUsePipes in process.Options) then
    exit;

  sum := output.Size;
  while process.Output.NumBytesAvailable <> 0 do
  begin
    output.SetSize(sum + buffSz);
    cnt := process.Output.Read((output.Memory + sum)^, buffSz);
    sum += cnt;
  end;
  output.SetSize(sum);
  output.Position := sum;
end;

procedure killProcess(var process: TAsyncProcess);
begin
  if process.isNil then
    exit;
  if process.Running then
    process.Terminate(0);
  process.Free;
  process := nil;
end;

procedure ensureNoPipeIfWait(process: TProcess);
begin
  if not (poWaitonExit in process.Options) then
    exit;
  process.Options := process.Options - [poStderrToOutPut, poUsePipes];
end;

function getLineEndingLength(const fname: string): byte;
var
  value: char = #0;
  le: string = LineEnding;
begin
  result := le.length;
  if not fileExists(fname) then
    exit;
  with TMemoryStream.Create do
  try
    LoadFromFile(fname);
    while true do
    begin
      if Position = Size then
        exit;
      read(value,1);
      if value = #10 then
        exit(1);
      if value = #13 then
        exit(2);
    end;
  finally
    Free;
  end;
end;

function getSysLineEndLen: byte;
begin
  {$IFDEF WINDOWS}
  exit(2);
  {$ELSE}
  exit(1);
  {$ENDIF}
end;

function countFolder(fname: string): integer;
var
  parent: string;
begin
  result := 0;
  while true do
  begin
    parent := fname.extractFileDir;
    if parent = fname then
      exit;
    fname := parent;
    result += 1;
  end;
end;

function formatTicksAsDuration(ticks: UInt64): string;
var
  ms: Uint64;
  sc: Uint64;
  mn: Uint64;
begin
  mn := ticks div 60000;
  ticks -= mn * 60000;
  sc := ticks div 1000;
  ticks -= sc * 1000;
  ms := ticks;
  result := format('%d minutes, %d seconds and %d msecs', [mn, sc, ms]);
end;

//TODO-cfeature: make it working with relative paths
function commonFolder(const files: TStringList): string;
var
  i,j,k: integer;
  sink: TStringList;
  dir: string;
  cnt: integer;
begin
  result := '';
  if files.Count = 0 then exit;
  sink := TStringList.Create;
  try
    sink.Assign(files);
    for i := sink.Count-1 downto 0 do
      if (not sink[i].fileExists) and (not sink[i].dirExists) then
        sink.Delete(i);
    // folders count
    cnt := 256;
    for dir in sink do
    begin
      k := countFolder(dir);
      if k < cnt then
        cnt := k;
    end;
    for i := sink.Count-1 downto 0 do
    begin
      while (countFolder(sink[i]) <> cnt) do
        sink[i] := sink[i].extractFileDir;
    end;
    // common folder
    while true do
    begin
      for i := sink.Count-1 downto 0 do
      begin
        dir := sink[i].extractFileDir;
        j := sink.IndexOf(dir);
        if j = -1 then
          sink[i] := dir
        else if j <> i then
          sink.Delete(i);
      end;
      if sink.Count < 2 then
        break;
    end;
    if sink.Count = 0 then
      result := ''
    else
      result := sink[0];
  finally
    sink.free;
  end;
end;

{$IFDEF WINDOWS}
function internalAppIsRunning(const ExeName: string): integer;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := 0;
  while integer(ContinueLoop) <> 0 do
    begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeName))) then
      begin
      Inc(Result);
      // SendMessage(Exit-Message) possible?
      end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
  CloseHandle(FSnapshotHandle);
end;
{$ENDIF}

{$IFDEF LINUX}
function internalAppIsRunning(const ExeName: string): integer;
var
  proc: TProcess;
  lst: TStringList;
begin
  Result := 0;
  proc := tprocess.Create(nil);
  proc.Executable := 'ps';
  proc.Parameters.Add('-C');
  proc.Parameters.Add(ExeName);
  proc.Options := [poUsePipes, poWaitonexit];
  try
    proc.Execute;
    lst := TStringList.Create;
    try
      lst.LoadFromStream(proc.Output);
      Result := Pos(ExeName, lst.Text);
    finally
      lst.Free;
    end;
  finally
    proc.Free;
  end;
end;
{$ENDIF}

{$IFDEF DARWIN}
function internalAppIsRunning(const ExeName: string): integer;
var
  proc: TProcess;
  lst: TStringList;
begin
  Result := 0;
  proc := tprocess.Create(nil);
  proc.Executable := 'pgrep';
  proc.Parameters.Add(ExeName);
  proc.Options := [poUsePipes, poWaitonexit];
  try
    proc.Execute;
    lst := TStringList.Create;
    try
      lst.LoadFromStream(proc.Output);
      Result := StrToIntDef(Trim(lst.Text), 0);
    finally
      lst.Free;
    end;
  finally
    proc.Free;
  end;
end;
{$ENDIF}

function AppIsRunning(const fname: string): boolean;
begin
  Result:= internalAppIsRunning(fname) > 0;
end;

function hasDlangSyntax(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.d', '.di': result := true;
  end;
end;

function hasCppSyntax(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.c', '.h', '.cc', '.cpp', '.hpp': result := true;
  end;
end;

function isDlangCompilable(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.d', '.di', '.dd', '.obj', '.o', '.a', '.lib': result := true;
  end;
end;

function isEditable(const ext: string): boolean;
begin
  result := false;
  case ext of
    '.d', '.di', '.dd', '.lst', '.md', '.txt', '.map' , '.sx': result := true;
  end;
end;

function isStringDisabled(const str: string): boolean;
begin
  result := false;
  if str.isEmpty then
    exit;
  if str[1] = ';' then
    result := true
  else if (str.length > 1) and (str[1..2] = '//') then
    result := true;
end;

function isBlank(const str: string): boolean;
var
  c: char;
begin
  result := true;
  for c in str do
    if not (c in [#9, ' ']) then
      exit(false);
end;

function globToReg(const glob: string ): string;
  procedure quote(var r: string; c: char);
  begin
    if not (c in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) then
      r += '\';
    r += c;
  end;
var
  i: integer = 0;
  b: integer = 0;
begin
  result := '^';
  while i < length(glob) do
  begin
    i += 1;
    case glob[i] of
      '*': result += '.*';
      '?': result += '.';
      '[', ']': result += glob[i];
      '{':
        begin
          b += 1;
          result += '(';
        end;
      '}':
        begin
          b -= 1;
          result += ')';
        end;
      ',':
        begin
          if b > 0 then
            result += '|'
          else
            quote(result, glob[i]);
        end;
      else
        quote(result, glob[i]);
    end;
  end;
end;

procedure tryRaiseFromStdErr(proc: TProcess);
var
  str: string = '';
begin
  if (proc.ExitStatus <> 0) and (poUsePipes in proc.Options) and not
    (poStderrToOutPut in proc.Options) then with TStringList.Create do
  try
    LoadFromStream(proc.Stderr);
    Insert(0, format('%s crashed with code: %d',
      [shortenPath(proc.Executable), proc.ExitStatus]));
    str += ReplaceStr(Text, #0, #10);
    str += 'parameters:'#10;
    str += proc.Parameters.Text;
    str += 'stderr:'#10;
    raise Exception.Create(str);
  finally
    free;
  end;
end;

procedure deleteDups(strings: TStrings);
var
  i,j: integer;
begin
  for i := strings.Count-1 downto 0 do
  begin
    j := strings.IndexOf(strings[i]);
    if (j <> -1) and (j < i) then
      strings.Delete(i);
  end;
end;

function leadingTabsToSpaces(const value: string; width: integer): string;
var
  p: integer;
  b: string;
  c: TIndentComposition;
  u: char;
begin
  assert(width > 0);

  p := 1;
  c.numS := 0;
  c.numT := 0;
  while p < value.length do
  begin
    u := value[p];
    if u = ' ' then
      c.numS += 1
    else if u = #9 then
      c.numT += 1
    else
      break;
    p += 1;
  end;
  if p <> 1 then
  begin
    setLength(b, c.numT * width + c.numS);
    FillChar(b[1], b.length, ' ');
    result := b + value[p .. value.length];
  end
  else result := value;
end;

function leadingSpacesToTabs(const value: string; width: integer): string;
var
  p: integer;
  c: TIndentComposition;
  i: integer;
begin
  assert(width > 0);

  result := '';
  if value = '' then
    exit;

  p := 1;
  while true do
  begin

    if p > value.length then
      break;
    if not (value[p] in [#9, ' ']) then
      break;

    c.numS := 0;
    c.numT := 0;

    while (p < value.length) and (value[p] = ' ') do
    begin
      c.numS += 1;
      p += 1;
    end;
    c.numT := c.numS div width;
    c.numS -= c.numT * width;

    for i := 0 to c.numT-1 do
      result += #9;
    for i := 0 to c.numS-1 do
      result += ' ';

    c.numT := 0;
    while (p < value.length) and (value[p] = #9) do
    begin
      c.numT += 1;
      p += 1;
    end;

    for i := 0 to c.numT-1 do
      result += #9;

    if p >= value.length then
      break;

  end;
  result += value[p .. value.length];
end;

{$IFDEF DEBUG}
initialization

  assert(leadingTabsToSpaces('', 2)      = '');
  assert(leadingTabsToSpaces(' start', 2)      = ' start');
  assert(leadingTabsToSpaces('start', 2)      = 'start');
  assert(leadingTabsToSpaces('start ', 2)      = 'start ');
  assert(leadingTabsToSpaces('start '#9, 2)      = 'start '#9);
  assert(leadingTabsToSpaces('  '#9'  '#9'start', 2)  = '        start');
  assert(leadingTabsToSpaces('  '#9#9'start', 2)      = '      start');
  assert(leadingTabsToSpaces(#9' ', 4)      = '     ');

  assert(leadingSpacesToTabs('', 2)      = '');
  assert(leadingSpacesToTabs('start', 2)      = 'start');
  assert(leadingSpacesToTabs('start ', 2)      = 'start ');
  assert(leadingSpacesToTabs('start '#9, 2)      = 'start '#9);
  assert(leadingSpacesToTabs('  '#9'  '#9'start', 2)  = #9#9#9#9'start');
  assert(leadingSpacesToTabs('  '#9#9'start', 2)      = #9#9#9'start');
  assert(leadingSpacesToTabs(#9' ', 4)      = #9' ');
  assert(leadingSpacesToTabs(' '#9, 4)      = ' '#9);
  assert(leadingSpacesToTabs(#9'   ' , 2)   = #9#9' ');

{$ENDIF}
end.
