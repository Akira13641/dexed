unit u_dlangutils;

{$I u_defines.inc}

interface

uses
  SysUtils;

type
  TCharSet = set of Char;

const
  stringPostfixes: TCharSet = ['c', 'w', 'd'];
  stringPrefixes: TCharSet = ['r', 'x', 'q', '"'];
  stringStopChecks: TCharSet = ['\', '"'];
  charStopChecks: TCharSet = ['\', #39];
  symbChars: TCharSet = [';', '{', '}', '(', ')', '[', ']', ',', '.', ':', '?', '$', '"', #39];
  hexaChars: TCharSet = ['0'..'9', 'a'..'f', 'A'..'F', '_'];

function isWhite(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isSpace(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isAlpha(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isNumber(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isBit(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isAlNum(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isHex(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isSymbol(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isOperator1(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isOperator2(const s: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isOperator3(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
function isOperator4(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
function isStringPostfix(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isFirstIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}


function readLine(var reader: PChar; var position: Integer): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

function readUntil(var reader: PChar; var position: Integer; const aDelim: Char): boolean; overload;    {$IFNDEF DEBUG}inline;{$ENDIF}
function readUntil(var reader: PChar; var position: Integer; const aDelim: string): boolean; overload;  {$IFNDEF DEBUG}inline;{$ENDIF}

function readUntilAmong(var reader: PChar; var position: Integer; const aDelim: TCharSet): boolean;     {$IFNDEF DEBUG}inline;{$ENDIF}

function readWhile(var reader: PChar; var position: Integer; const aDelim: Char): boolean;  overload;   {$IFNDEF DEBUG}inline;{$ENDIF}
function readWhile(var aReader: PChar; var position: Integer; const aDelim: TCharSet): boolean; overload;{$IFNDEF DEBUG}inline;{$ENDIF}

function readDelim(var reader: PChar; var position: Integer; const aDelim: Char): boolean; overload;      {$IFNDEF DEBUG}inline;{$ENDIF}
function readDelim(var reader: PChar; var position: Integer; const aDelim: string): boolean; overload;    {$IFNDEF DEBUG}inline;{$ENDIF}
function readDelim(var reader: PChar; var position: Integer; const aDelims: TCharSet): boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

function tryReadDelim(var reader: PChar; var position: Integer; const aDelim: Char): boolean; overload;   {$IFNDEF DEBUG}inline;{$ENDIF}
function tryReadDelim(var reader: PChar; var position: Integer; const aDelim: string): boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

implementation

function isWhite(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in [#0..#32]);
end;

function isSpace(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in [#9,' ']);
end;

function isAlpha(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit((c in ['a'..'z']) or (c in ['A'..'Z']));
end;

function isNumber(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in ['0'..'9']);
end;

function isBit(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit((c in ['0'..'1']));
end;

function isAlNum(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(isAlpha(c) or isNumber(c));
end;

function isHex(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(isNumber(c) or (c in ['A'..'F']) or (c in ['a'..'f']));
end;

function isSymbol(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in [';', '{', '}', '(', ')', '[', ']', ',', '.', ':' , '"', #39, '?', '$', #35]);
end;

function isOperator1(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in ['/', '*', '-', '+', '%', '>', '<', '=', '!', '&', '|', '^', '~']);
end;

function isOperator2(const s: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := false;
  case s[1] of
    '.': result := s[2] = '.';
    '*': result := s[2] = '=';

    '>': result := s[2] in ['>', '='];
    '<': result := s[2] in ['<', '=', '>'];
    '=': result := s[2] in ['=', '>'];
    '!': result := s[2] in ['=', '>', '<'];

    '+': result := s[2] in ['+', '='];
    '-': result := s[2] in ['-', '='];
    '/': result := s[2] in ['='];
    '%': result := s[2] in ['='];
    '~': result := s[2] in ['='];

    '&': result := s[2] in ['&', '='];
    '|': result := s[2] in ['|', '='];
    '^': result := s[2] in ['^', '='];
  end;
end;

function isOperator3(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := false;
  case s[1] of
    '.': result := (s[2] = '.')   and (s[3] = '.');
    '^': result := (s[2] = '^')   and (s[3] = '=');
    '>': result := (s[2] = '>')   and (s[3] in ['>', '=']);
    '<': result := ((s[2] = '<')  and (s[3] in ['<', '=']))
                  or (s[2] = '>') and (s[3] = '=');
    '!': result := ((s[2] = '<')  and (s[3] in ['>', '=']))
                  or ((s[2] = '>')and (s[3] = '='));
  end;
end;

function isOperator4(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (s = '>>>=') or (s = '!<>=');
end;

function isStringPostfix(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in stringPostfixes);
end;

function isIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit((not isSymbol(c)) and (not isOperator1(c)) and (not isWhite(c)));
end;

function isFirstIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(isIdentifier(c) and (not isNumber(c)));
end;

function readLine(var reader: PChar; var position: Integer): boolean;
begin
  result := true;
  while reader^ <> #10 do
  begin
    inc(reader);
    inc(position);
  end;
end;

function readUntil(var reader: PChar; var position: Integer; const aDelim: Char): boolean;
begin
  while reader^ <> aDelim do
  begin
    if reader^ = #10 then
      exit(false);
    inc(reader);
    inc(position);
  end;
  inc(reader);
  inc(position);
  exit(true);
end;

function readUntil(var reader: PChar; var position: Integer; const aDelim: string): boolean;
begin
  while reader[0..length(aDelim)-1] <> aDelim do
  begin
    if reader^ = #10 then
      exit(false);
    inc(reader);
    inc(position);
  end;
  inc(reader, length(aDelim));
  inc(position, length(aDelim));
  exit(true);
end;

function readUntilAmong(var reader: PChar; var position: Integer; const aDelim: TCharSet): boolean;
begin
  while not (reader^ in aDelim) do
  begin
    if reader^ = #10 then
      exit(false);
    inc(reader);
    inc(position);
  end;
  exit(true);
end;

function readWhile(var reader: PChar; var position: Integer; const aDelim: Char): boolean;
begin
  result := false;
  while reader^ = aDelim do
  begin
    inc(reader);
    inc(position);
    result := true;
  end;
end;

function readWhile(var aReader: PChar; var position: Integer; const aDelim: TCharSet): boolean;
begin
  result := false;
  while aReader^ in aDelim do
  begin
    inc(aReader);
    inc(position);
    result := true;
  end;
end;


function readDelim(var reader: PChar; var position: Integer; const aDelim: Char): boolean;
begin
  if reader^ <> aDelim then
    exit(false);
  inc(reader);
  inc(position);
  exit(true);
end;

function readDelim(var reader: PChar; var position: Integer; const aDelims: TCharSet): boolean;
begin
  if not (reader^ in aDelims) then
    exit(false);
  inc(reader);
  inc(position);
  exit(true);
end;

function readDelim(var reader: PChar; var position: Integer; const aDelim: string): boolean;
var
  i: Integer;
begin
  for i := 1 to length(aDelim) do
  begin
    if reader^ = #10 then
      exit(false);
    if reader^ <> aDelim[i] then
      exit(false);
    inc(reader);
    inc(position);
  end;
  exit(true);
end;

function tryReadDelim(var reader: PChar; var position: Integer; const aDelim: Char): boolean;
var
  savedReader: PChar;
  savedPos: Integer;
begin
  savedReader := reader;
  savedPos := position;
  if readDelim(reader, position, aDelim) then
      exit(true);
  reader := savedReader;
  position := savedPos;
  exit(false);
end;

function tryReadDelim(var reader: PChar; var position: Integer; const aDelim: string): boolean;
var
  savedReader: PChar;
  savedPos: Integer;
begin
  savedReader := reader;
  savedPos := position;
  if readDelim(reader, position, aDelim) then
    exit(true);
  reader := savedReader;
  position := savedPos;
  exit(false);
end;

end.
