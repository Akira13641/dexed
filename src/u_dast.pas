unit u_dast;

{$I u_defines.inc}

interface

uses
  dynlibs, sysutils, u_common;

type

  TAstHandle = type NativeInt;

  TAstNotification = procedure(param: Pointer); cdecl;

  {$Z1}
  TSerializationFormat = (json, pas);

  // Returns an ID to a D AST. clbsk is the proc called with param when scanning is achieved.
  TNewAst     = function(param: Pointer; clbck: TAstNotification): TAstHandle; cdecl;
  // Deletes the D AST identified by hdl.
  TDeleteAst  = procedure(hdl: TAstHandle); cdecl;

  // Uses D AST identified by hsdl to parse filename.
  TScanFile   = procedure(hdl: TAstHandle; filename: PChar); cdecl;
  // Uses D AST identified by hsdl to parse the buffer of length len.
  TScanBuffer = procedure(hdl: TAstHandle; buffer: PByte; len: NativeUint); cdecl;

  // Returns the name of the module identified by hdl.
  TModuleName = function(hdl: TAstHandle): PChar; cdecl;
  // Returns the list of the declarations in the module identified by hdl.
  TSymbolList = function(hdl: TAstHandle; var len: NativeUint ; fmt: TSerializationFormat): PByte; cdecl;

var
  newAST: TNewAst;
  deleteAST: TDeleteAst;
  scanFile: TScanFile;
  scanBuffer: TScanBuffer;
  moduleName: TModuleName;
  symbolList: TSymbolList;
  dastAvailable: boolean;


implementation

var
  dastHdl: TLibHandle = 0;
  fname: string;

initialization

  fname := exeFullName('cedast' + dynExt);
  if FileExists(fname) then
    dastHdl := LoadLibrary(fname);
  if dastHdl <> NilHandle then
  begin
    newAST := TNewAst(GetProcAddress(dastHdl, 'newAst'));
    deleteAST := TDeleteAst(GetProcAddress(dastHdl, 'deleteAst'));
    scanFile := TScanFile(GetProcAddress(dastHdl, 'scanFile'));
    scanBuffer := TScanBuffer(GetProcAddress(dastHdl, 'scanBuffer'));
    moduleName := TModuleName(GetProcAddress(dastHdl, 'moduleName'));
    symbolList := TSymbolList(GetProcAddress(dastHdl, 'symbolList'));
    //
    dastAvailable := assigned(newAST) and assigned(deleteAST) and assigned(scanFile)
      and assigned(scanBuffer) and assigned(moduleName) and assigned(symbolList);
  end;

finalization
  {$IFDEF RELEASE}
  if dastHdl <> NilHandle then
    UnloadLibrary(dastHdl);
  {$ENDIF}
end.

