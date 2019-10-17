unit u_dlangmaps;

{$I u_defines.inc}

interface


type

  (**
   * Perfect static hash-map that detects the D2 "special" keywords such as
   * __LINE__ or __FILE__.
   *)
  specialKeywordsMap = record
  private
    const fWords: array [0..15] of string =
    (
      '__VERSION__', '', '__FILE_FULL_PATH__', '__TIME__', '__FILE__', '__VENDOR__',
      '', '__DATE__', '__FUNCTION__', '__LINE__', '__EOF__', '__MODULE__',
      '__PRETTY_FUNCTION__', '', '', '__TIMESTAMP__'
    );
    const fHasEntry: array [0..15] of boolean =
    (
      true, false, true, true, true, true, false, true, true, true, true, true,
      true, false, false, true
    );
    const fCoeffs: array[0..255] of Byte =
    (
      162, 105, 225, 180, 180, 12, 125, 73, 237, 109, 3, 67, 160, 192, 35, 42,
      131, 170, 41, 106, 103, 53, 105, 74, 29, 64, 247, 248, 184, 146, 172, 142,
      239, 232, 158, 168, 29, 243, 40, 241, 255, 85, 184, 38, 44, 242, 193, 222,
      86, 131, 181, 101, 161, 209, 115, 124, 91, 118, 188, 67, 172, 115, 24, 221,
      142, 99, 17, 30, 231, 80, 185, 182, 185, 55, 4, 23, 152, 63, 126, 37, 158,
      36, 28, 235, 65, 220, 243, 62, 169, 129, 127, 76, 149, 232, 21, 119, 134,
      144, 20, 89, 103, 65, 109, 12, 95, 200, 41, 14, 52, 25, 56, 228, 4, 227,
      86, 113, 77, 158, 46, 246, 90, 25, 210, 214, 149, 219, 219, 27, 95, 203,
      43, 21, 191, 94, 216, 113, 100, 222, 245, 224, 127, 174, 214, 44, 78, 89,
      213, 184, 73, 77, 236, 131, 46, 90, 58, 171, 34, 215, 201, 104, 138, 251,
      54, 103, 75, 235, 12, 149, 49, 19, 128, 72, 138, 224, 73, 174, 151, 50,
      152, 32, 135, 238, 132, 34, 3, 230, 201, 166, 31, 119, 50, 155, 125, 103,
      133, 250, 253, 218, 48, 167, 207, 107, 235, 53, 214, 213, 49, 8, 13, 247,
      37, 251, 21, 43, 34, 108, 162, 160, 133, 199, 169, 218, 189, 1, 128, 17,
      67, 186, 55, 2, 23, 23, 133, 114, 240, 176, 124, 127, 217, 231, 129, 220,
      250, 17, 136, 92, 191, 172, 16, 137, 23, 109, 37, 191, 74, 218
    );
    class function hash(const w: string): Byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  (**
   * Perfect static hash-map that detects the 'straight' D2 keywords plus a few
   * exception for the library types related to the strings and registry-wide integers.
   *)
  keywordsMap = record
  private
    const fWords: array [0..511] of string =
    (
      'double', '', '', '', '', 'volatile', 'synchronized', '', 'wchar', '', '',
      '', '', '', 'goto', '', 'assert', '', '', 'void', '', '', '', 'override',
      'pure', '', '', '', '', '', '', 'delegate', '', '', 'super', '', 'case',
      '', '', '', 'pragma', '', '', '', 'string', '', 'debug', '', '', '', '',
      '', 'module', '', '', '', '', '', '', '', '', '', '', 'immutable', '',
      'template', 'dstring', '', '__parameters', '', '', '', '', '__vector', '',
      '', '', '', '', '', 'invariant', '', 'unittest', '', '', 'protected', '',
      '', 'break', 'alias', '', '', '', '', '', '', '', '', '', 'wstring', '',
      '', 'private', 'final', '', 'false', '', 'catch', 'float', '', '', '', '',
      '', '', '', '', '', '', '', '', '', '', 'align', '', '', '', '', '', '',
      'ptrdiff_t', '', '', '', '', '', '', 'delete', '', '', '', '', '', '', '',
      'do', '', 'mixin', '', 'ireal', '', '', '', '', 'static', 'extern', '', '',
      'null', '', '', 'creal', '', '', 'typeid', '', 'idouble', '', '', '', 'try',
      '', '', '', 'finally', '', 'is', '', 'cdouble', '', 'in', '', '', '', '',
      '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '',
      'scope', '', '', 'package', '', '', '', '', '', 'interface', '', '', 'macro',
      '', '', '', '', '', '', '', '', '', 'default', '', '', '', '', '', 'out',
      '', '', '', '', 'size_t', '', '', '', '', 'new', 'int', '', '', '', '', '',
      '', '', '', 'this', '', '', '', '', '', '', '', 'public', '', '', '',
      'continue', '', '', '', 'body', '', '', '', '', '', '', 'ifloat', '', '',
      '', '', 'version', '', '', 'deprecated', '', '', '', 'cfloat', '', 'uint',
      'function', '', '', '', '', 'short', '', 'with', 'typeof', '', '', '', '',
      '', '', '', '', '', '', '', 'import', '', '', '', '', '', '', '', '',
      '__traits', '', '', '', '', '', 'export', '', '', '', '', '', '', '', '',
      '', '', '', '', '', '', 'throw', 'ushort', '', '', '', '', '', '', '', '',
      '', 'asm', '', '', '', '', '', 'byte', '', '', '', '', '', 'abstract',
      'union', 'if', '', 'true', '', 'typedef', '', '', '', '', '', '', '', '',
      '', '', '', '', '', '', '', 'enum', '', '', 'const', '', '', '', '', '', '',
      '', 'bool', '', '', '', '', '', '', 'ubyte', 'else', 'long', '', '', 'for',
      '', '', '', 'inout', '', '', '', '', '', '', '', 'auto', '', '', '', '', '',
      '', 'cent', '', '', '', '', '', '', '', '', 'class', '', '', 'cast', '', '',
      '', '', '', 'struct', '', 'foreach', '', '', '', 'ulong', '', '', '__gshared',
      '', 'while', 'ref', '', '', '', '', '', '', '', '', 'char', 'return', '',
      'foreach_reverse', 'lazy', '', '', 'ucent', '', '', '', 'nothrow', '', '',
      '', '', '', '', '', 'switch', '', '', 'dchar', '', '', '', 'shared', '', '',
      '', 'real', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
    );
    const fHasEntry: array [0..511] of boolean =
    (
      true, false, false, false, false, true, true, false, true, false, false,
      false, false, false, true, false, true, false, false, true, false, false,
      false, true, true, false, false, false, false, false, false, true, false,
      false, true, false, true, false, false, false, true, false, false, false,
      true, false, true, false, false, false, false, false, true, false, false,
      false, false, false, false, false, false, false, false, true, false, true,
      true, false, true, false, false, false, false, true, false, false, false,
      false, false, false, true, false, true, false, false, true, false, false,
      true, true, false, false, false, false, false, false, false, false, false,
      true, false, false, true, true, false, true, false, true, true, false, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, true, false, false, false, false, false, false, true, false, false,
      false, false, false, false, true, false, false, false, false, false, false,
      false, true, false, true, false, true, false, false, false, false, true,
      true, false, false, true, false, false, true, false, false, true, false,
      true, false, false, false, true, false, false, false, true, false, true,
      false, true, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, false, false, false, true, false, false, true, false, false, false,
      false, false, true, false, false, true, false, false, false, false, false,
      false, false, false, false, true, false, false, false, false, false, true,
      false, false, false, false, true, false, false, false, false, true, true,
      false, false, false, false, false, false, false, false, true, false, false,
      false, false, false, false, false, true, false, false, false, true, false,
      false, false, true, false, false, false, false, false, false, true, false,
      false, false, false, true, false, false, true, false, false, false, true,
      false, true, true, false, false, false, false, true, false, true, true, false,
      false, false, false, false, false, false, false, false, false, false, true,
      false, false, false, false, false, false, false, false, true, false, false,
      false, false, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, true, true, false, false,
      false, false, false, false, false, false, false, true, false, false, false,
      false, false, true, false, false, false, false, false, true, true, true,
      false, true, false, true, false, false, false, false, false, false, false,
      false, false, false, false, false, false, false, false, true, false, false,
      true, false, false, false, false, false, false, false, true, false, false,
      false, false, false, false, true, true, true, false, false, true, false,
      false, false, true, false, false, false, false, false, false, false, true,
      false, false, false, false, false, false, true, false, false, false, false,
      false, false, false, false, true, false, false, true, false, false, false,
      false, false, true, false, true, false, false, false, true, false, false,
      true, false, true, true, false, false, false, false, false, false, false,
      false, true, true, false, true, true, false, false, true, false, false,
      false, true, false, false, false, false, false, false, false, true, false,
      false, true, false, false, false, true, false, false, false, true, false,
      false, false, false, false, false, false, false, false, false, false, false,
      false, false, false, false
    );
    // 100017a
    const fCoeffs: array[0..255] of Byte =
    (
      93, 12, 147, 37, 246, 76, 204, 47, 77, 0, 217, 84, 225, 244, 62, 63, 81, 2,
      46, 137, 104, 245, 184, 87, 229, 148, 69, 207, 24, 10, 239, 172, 27, 34, 60,
      251, 113, 66, 175, 29, 10, 1, 158, 38, 157, 120, 224, 173, 11, 199, 49, 173,
      88, 229, 213, 191, 217, 177, 90, 19, 83, 212, 97, 12, 136, 154, 243, 105,
      97, 29, 94, 226, 71, 60, 28, 245, 38, 212, 156, 116, 254, 70, 207, 211, 93,
      67, 32, 42, 149, 101, 98, 4, 83, 160, 228, 128, 231, 188, 100, 178, 22, 172,
      198, 218, 13, 166, 45, 54, 49, 152, 14, 123, 232, 223, 86, 10, 62, 46, 220,
      55, 161, 22, 210, 86, 14, 79, 8, 28, 66, 67, 84, 116, 159, 144, 37, 46, 199,
      218, 233, 188, 207, 168, 89, 64, 245, 3, 6, 199, 144, 165, 216, 145, 141, 70,
      69, 20, 149, 252, 119, 75, 153, 97, 14, 196, 74, 48, 91, 145, 70, 90, 59, 69,
      92, 252, 233, 161, 169, 155, 9, 28, 234, 103, 172, 225, 164, 49, 161, 95, 81,
      201, 217, 217, 58, 119, 169, 230, 11, 8, 137, 65, 165, 159, 4, 243, 225, 236,
      178, 209, 133, 35, 68, 222, 237, 114, 64, 158, 72, 66, 151, 208, 169, 232, 83,
      229, 157, 233, 123, 135, 65, 187, 161, 100, 217, 63, 124, 36, 108, 198, 2,
      103, 156, 241, 140, 163, 128, 196, 45, 166, 41, 61, 19, 139, 25, 115, 72, 175
    );
    class function hash(const w: string): Word; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    class function match(const w: string): boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;


implementation

{$IFDEF DEBUG}{$PUSH}{$R-}{$ENDIF}
class function specialKeywordsMap.hash(const w: string): Byte;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $F;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function specialKeywordsMap.match(const w: string): boolean;
var
  h: Byte;
begin
  result := false;
  if (length(w) < 7) or (length(w) > 19) then
    exit;
  h := hash(w);
  if fHasEntry[h] then
    result := fWords[h] = w;
end;

{$IFDEF DEBUG}{$PUSH}{$R-}{$ENDIF}
class function keywordsMap.hash(const w: string): Word;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to length(w) do
    Result += fCoeffs[Byte(w[i])];
  Result := Result and $1FF;
end;
{$IFDEF DEBUG}{$POP}{$ENDIF}

class function keywordsMap.match(const w: string): boolean;
var
  h: Word;
begin
  result := false;
  if (length(w) < 2) or (length(w) > 15) then
    exit;
  h := hash(w);
  if fHasEntry[h] then
    result := fWords[h] = w;
end;

end.

