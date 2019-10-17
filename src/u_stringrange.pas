unit u_stringrange;

{$I u_defines.inc}

interface

uses
  SysUtils;

type

  PStringRange = ^TStringRange;

  (**
   * Iterator specialized for strings.
   *
   * This structure allows to easily scan strings.
   * Most of the operations can be chained because the functions
   * return either a pointer to a TStringRange (in this case this is always
   * the "Self") or a new TStringRange (in this case this is always a copy).
   *
   * This is based on a more generic work which tries to implement some kind
   * of "D" ranges in Object Pascal (see https://github.com/Basile-z/ArrayOps).
   * Even if Object Pascal doesn't provide the expressivness required to mimic
   * D ranges, a few good stuff are still possible.
   *)
  TStringRange = record
  private
    ptr: PChar;
    pos: integer;
    len: integer;

  public

    // returns a new range initialized with a string.
    class function create(const str: string): TStringRange; static;
    // returns a new range initialized from a pointer.
    class function create(const pchr: PChar; length: integer): TStringRange; static;

    // initializes the range with a string.
    function init(const str: string): PStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}
    // initialized the range from a pointer.
    function init(const pchr: PChar; length: integer): PStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}

    // advances.
    function popFront: PStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances N times
    function popFrontN(n: integer): PStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}
    // returns the current element.
    function front: char; {$IFNDEF DEBUG}inline;{$ENDIF}
    // indicates wether the range is consumed.
    function empty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

    // yields the state of the range to a string.
    function yield: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    // returns a copy.
    function save: TStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}
    // resets the range.
    function reset: PStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}


    // advances the range while the front is in value, returns a copy.
    function takeWhile(value: TSysCharSet): TStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range while the front is equal to value, returns a copy.
    function takeWhile(value: Char): TStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range until the front is in value, returns a copy.
    function takeUntil(value: TSysCharSet): TStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range until the front is equal to value, returns a copy.
    function takeUntil(value: Char): TStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range while the front is in value.
    function popWhile(value: TSysCharSet): PStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range while the front is equal to value.
    function popWhile(value: Char): PStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range until the front is in value.
    function popUntil(value: TSysCharSet): PStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    // advances the range until the front is equal to value.
    function popUntil(value: Char): PStringRange; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

    // advances the range until the beginning of the next line.
    function popLine: PStringRange; {$IFNDEF DEBUG}inline;{$ENDIF}

    // returns the next word.
    function nextWord: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    // returns the next line.
    function nextLine: string; {$IFNDEF DEBUG}inline;{$ENDIF}
    // indicates wether the range starts with value.
    function startsWith(const value: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    // indicates wether the range starts with value.
    function startsWith(var value: TStringRange): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    // indicates wether the range ends with value.
    function endsWith(const value: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}


    // read-only position
    function position: integer;
  end;

implementation

{$IFDEF DEBUG}
var
  t1: string;
  r1: TStringRange;
{$ENDIF}

class function TStringRange.create(const str: string): TStringRange;
begin
  result.ptr := nil;
  result.pos := 0;
  result.len := 0;
  if str = '' then
    exit;
  result.ptr := @str[1];
  result.pos := 0;
  result.len := length(str);
end;

class function TStringRange.create(const pchr: PChar; length: integer): TStringRange;
begin
  result.ptr := pchr;
  result.pos := 0;
  result.len := length;
end;

function TStringRange.init(const str: string): PStringRange;
begin
  Result := @self;
  ptr := nil;
  pos := 0;
  len := 0;
  if str = '' then
    exit;
  ptr := @str[1];
  pos := 0;
  len := length(str);
end;

function TStringRange.position: integer;
begin
  Result := pos;
end;

function TStringRange.init(const pchr: PChar; length: integer): PStringRange;
begin
  ptr := pchr;
  pos := 0;
  len := length;
  Result := @self;
end;

function TStringRange.popFront: PStringRange;
begin
  pos += 1;
  Result := @self;
end;

function TStringRange.popFrontN(n: integer): PStringRange;
begin
  pos += n;
  Result := @self;
end;

function TStringRange.front: char;
begin
  result := (ptr + pos)^;
end;

function TStringRange.empty: boolean;
begin
  result := pos >= len;
end;

function TStringRange.yield: string;
begin
  Result := ptr[pos .. len-1];
end;

function TStringRange.save: TStringRange;
begin
  Result.len:= len;
  Result.pos:= pos;
  Result.ptr:= ptr;
end;

function TStringRange.reset: PStringRange;
begin
  pos := 0;
  Result := @Self;
end;

function TStringRange.takeWhile(value: TSysCharSet): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or not (front in value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.takeWhile(value: Char): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or not (front = value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.takeUntil(value: TSysCharSet): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or (front in value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.takeUntil(value: Char): TStringRange;
begin
  Result.ptr := ptr + pos;
  Result.pos := 0;
  Result.len := 0;
  while true do
  begin
    if empty or (front = value) then
      break;
    Result.len += 1;
    popFront;
  end;
end;

function TStringRange.popWhile(value: TSysCharSet): PStringRange;
begin
  while true do
  begin
    if empty or not (front in value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popWhile(value: Char): PStringRange;
begin
  while true do
  begin
    if empty or not (front = value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popUntil(value: TSysCharSet): PStringRange;
begin
  while true do
  begin
    if empty or (front in value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popUntil(value: Char): PStringRange;
begin
  while true do
  begin
    if empty or (front = value) then
      break;
    popFront;
  end;
  Result := @self;
end;

function TStringRange.popLine: PStringRange;
begin
  popUntil(#10);
  if not empty then
    popFront;
  Result := @self;
end;

function TStringRange.nextWord: string;
const
  blk = [#0 .. #32];
begin
  Result := popWhile(blk)^.takeUntil(blk).yield;
end;

function TStringRange.nextLine: string;
const
  lsp = [#10, #13];
begin
  Result := popWhile(lsp)^.takeUntil(lsp).yield;
end;

function TStringRange.startsWith(const value: string): boolean;
begin
  Result := false;
  if len - pos >= length(value) then
    Result := ptr[pos .. pos + length(value)-1] = value;
end;

function TStringRange.startsWith(var value: TStringRange): boolean;
var
  p0, p1: integer;
begin
  p0 := pos;
  p1 := value.pos;
  Result := true;
  while not empty and not value.empty do
  begin
    if front <> value.front then
    begin
      Result := false;
      break;
    end;
    popFront;
    value.popFront;
  end;
  pos := p0;
  value.pos := p1;
end;

function TStringRange.endsWith(const value: string): boolean;
begin
  if empty then
    Result := false
  else
    Result := ptr[pos .. length(value)-1] = value;
end;

{$IFDEF DEBUG}
begin
  t1 := '~>0.6.0';
  r1.init(t1);
  assert(r1.takeUntil(['0'..'9']).yield = '~>');
  assert(r1.takeUntil(#0).yield = '0.6.0');
  t1 := '>=0.8.0-alpha.1';
  r1.init(t1);
  assert(r1.takeUntil(['0'..'9']).yield = '>=');
  assert(r1.takeUntil(#0).yield = '0.8.0-alpha.1');
{$ENDIF}
end.

