unit u_dlang;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, u_dlangutils, u_dlangmaps;


type

  (**
   * Represents the pointer in a source file.
   * Automatically updates the line and the column.
   *)
  TReaderHead = object
  private
    fLineIndex: Integer;
    fColumnIndex: Integer;
    fAbsoluteIndex: Integer;
    fReaderHead: PChar;
    fPreviousLineColum: Integer;
    fBegColumnIndex: Integer;
    fBegLineIndex: Integer;
    fBegOffset: Integer;
    function getColAndLine: TPoint;
  public
    constructor Create(const text: PChar; const colAndLine: TPoint);
    procedure setReader(const text: PChar; const colAndLine: TPoint);
    //
    function Next: PChar;
    function previous: PChar;
    procedure saveBeginning;
    //
    property AbsoluteIndex: Integer read fAbsoluteIndex;
    property LineIndex: Integer read fLineIndex;
    property ColumnIndex: Integer read fColumnIndex;
    property LineAnColumn: TPoint read getColAndLine;
    property SavedLine: Integer read fBegLineIndex;
    property SavedColumn: Integer read fBegColumnIndex;
    property SavedOffset: Integer read fBegOffset;
    //
    property head: PChar read fReaderHead;
  end;

  TLexTokenKind = (ltkIllegal, ltkChar, ltkComment, ltkIdentifier, ltkKeyword,
    ltkNumber, ltkOperator, ltkString, ltkSymbol, ltkWhite);

const
  LexTokenKindString: array[TLexTokenKind] of string =
   ('Illegal   ',
    'Character ',
    'Comment   ',
    'Identifier',
    'Keyword   ',
    'Number    ',
    'Operator  ',
    'String    ',
    'Symbol    ',
    'White     ');

type

  (**
   * Lexer token.
   *)
  PLexToken = ^TLexToken;

  TLexToken = record
    offset: integer;
    position: TPoint;
    kind: TLexTokenKind;
    Data: string;
  end;

  TLexOption = (lxoNoComments, lxoNoWhites);
  TLexOptions = set of TLexOption;

  TLexFoundEvent = procedure(const token: PLexToken; out stop: boolean) of Object;

  (**
   * List of lexer tokens.
   *)
  TLexTokenList = class(TFPList)
  private
    function getToken(index: integer): PLexToken;
  public
    procedure Clear;
    procedure addToken(value: PLexToken);
    procedure saveToFile(const fname: string);
    property token[index: integer]: PLexToken read getToken; default;
  end;

  TLexTokenEnumerator = class
    fList: TLexTokenList;
    fIndex: Integer;
    function GetCurrent: PLexToken;
    function MoveNext: Boolean;
    property Current: PLexToken read GetCurrent;
  end;

  (**
   * Lexer error.
   *)
  PLexError = ^TLexError;

  TLexError = record
    position: TPoint;
    msg: string;
  end;

  (**
   * Error list.
   *)
  TLexErrorList = class(TFPList)
  private
    function getError(index: integer): TLexError;
  public
    procedure Clear;
    procedure addError(value: PLexError);
    property error[index: integer]: TLexError read getError;
  end;

  TLexErrorEnumerator = class
    fList: TLexErrorList;
    fIndex: Integer;
    function GetCurrent: TLexError;
    function MoveNext: Boolean;
    property Current: TLexError read GetCurrent;
  end;

operator enumerator(list: TLexTokenList): TLexTokenEnumerator;
operator enumerator(list: TLexErrorList): TLexErrorEnumerator;

(**
 * Lexes text and fills list with the TLexToken found.
 *)
procedure lex(const text: string; list: TLexTokenList; clbck: TLexFoundEvent = nil; Options: TLexOptions = []);

(**
 * Outputs the module name from a tokenized D source.
 *)
function getModuleName(list: TLexTokenList): string;

(**
 * Fills a list with all the modules imported in a tokenized D source.
 *)
procedure getImports(list: TLexTokenList; imports: TStrings);

(**
 * Compares two TPoints.
 *)
operator = (lhs: TPoint; rhs: TPoint): boolean;
operator > (lhs: TPoint; rhs: TPoint): boolean;
operator < (lhs: TPoint; rhs: TPoint): boolean;
operator <= (lhs: TPoint; rhs: TPoint): boolean;

implementation

{$REGION TReaderHead -----------------------------------------------------------}
operator = (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit((lhs.y = rhs.y) and (lhs.x = rhs.x));
end;

operator > (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit((lhs.y > rhs.y) or ((lhs.y = rhs.y) and (lhs.x > rhs.x)));
end;

operator < (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit(rhs > lhs);
end;

operator <= (lhs: TPoint; rhs: TPoint): boolean;
begin
  exit((lhs = rhs) or (lhs < rhs));
end;

constructor TReaderHead.Create(const text: PChar; const colAndLine: TPoint);
begin
  setReader(text, colAndLine);
end;

procedure TReaderHead.setReader(const text: PChar; const colAndLine: TPoint);
begin
  fLineIndex := colAndLine.y;
  fColumnIndex := colAndLine.x;
  fReaderHead := text;
  while (LineAnColumn <> colAndLine) do
    Next;
  //
  // editor not 0 based ln index
  if fLineIndex = 0 then
    fLineIndex := 1;
end;

function TReaderHead.getColAndLine: TPoint;
begin
  exit(Point(fColumnIndex, fLineIndex));
end;

function TReaderHead.Next: PChar;
begin
  if (fReaderHead^ = #10) then
  begin
    Inc(fLineIndex);
    fPreviousLineColum := fColumnIndex;
    fColumnIndex := -1;
  end;
  Inc(fReaderHead);
  Inc(fAbsoluteIndex);
  Inc(fColumnIndex);
  exit(fReaderHead);
end;

function TReaderHead.previous: PChar;
begin
  Dec(fReaderHead);
  Dec(fColumnIndex);
  Dec(fAbsoluteIndex);
  if (fReaderHead^ = #10) then
  begin
    Dec(fLineIndex);
    fColumnIndex:= fPreviousLineColum;
  end;
  exit(fReaderHead);
end;

procedure TReaderHead.saveBeginning;
begin
  fBegColumnIndex:= fColumnIndex;
  fBegLineIndex:= fLineIndex;
  fBegOffset:= fAbsoluteIndex;
end;

{$ENDREGION}

{$REGION Lexing ----------------------------------------------------------------}
function TLexTokenList.getToken(index: integer): PLexToken;
begin
  Result := PLexToken(Items[index]);
end;

procedure TLexTokenList.Clear;
begin
  while Count > 0 do
  begin
    Dispose(PLexToken(Items[Count - 1]));
    Delete(Count - 1);
  end;
end;

procedure TLexTokenList.addToken(value: PLexToken);
begin
  add(Pointer(value));
end;

procedure TLexTokenList.saveToFile(const fname: string);
var
  tok: PLexToken;
  i: integer;
begin
  with TStringList.Create do
  try
    for i:= 0 to self.count-1 do
    begin
      tok := getToken(i);
      add(format('line %.5d - col %.3d (%.8d): (%s): %s',
        [tok^.position.Y, tok^.position.X, tok^.offset,
          LexTokenKindString[tok^.kind], tok^.Data]));
    end;
  finally
    SaveToFile(fname);
    free;
  end;
end;

function TLexTokenEnumerator.GetCurrent: PLexToken;
begin
  exit(fList.token[fIndex]);
end;

function TLexTokenEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  exit(fIndex < fList.Count);
end;

operator enumerator(list: TLexTokenList): TLexTokenEnumerator;
begin
  Result := TLexTokenEnumerator.Create;
  Result.fList := list;
  Result.fIndex := -1;
end;


procedure lex(const text: string; list: TLexTokenList; clbck: TLexFoundEvent = nil; Options: TLexOptions = []);
var
  reader: TReaderHead;
  identifier: string;
  nestedCom: integer;
  rstring: boolean;
  decSet: boolean;
  expSet: boolean;
  noComment: boolean;

  procedure addToken(aTk: TLexTokenKind);
  var
    ptk: PLexToken;
  begin
    if (aTk = ltkComment) and (noComment) then
      exit;
    ptk := new(PLexToken);
    ptk^.kind := aTk;
    ptk^.position.X := reader.SavedColumn;
    ptk^.position.Y := reader.SavedLine;
    ptk^.offset := reader.savedOffset;
    ptk^.Data := identifier;
    list.Add(ptk);
  end;

  function isOutOfBound: boolean;
  begin
    result := reader.AbsoluteIndex >= length(text);
    if result and (identifier <> '') then
      addToken(ltkIllegal);
  end;

  function callBackDoStop: boolean;
  begin
    Result := False;
    if clbck <> nil then
      clbck(list[list.Count-1], Result);
  end;

begin

  if text = '' then exit;

  noComment := lxoNoComments in Options;

  reader.Create(@text[1], Point(0, 0));
  while (True) do
  begin

    if isOutOfBound then
      exit;

    identifier := '';

    // skip blanks
    if isWhite(reader.head^) then
    begin
      reader.saveBeginning;
      while isWhite(reader.head^) do
      begin
        if isOutOfBound then
          exit;
        reader.Next;
      end;
      if not (lxoNoWhites in Options) then
        addToken(ltkWhite);
    end;

    // line comment
    if (reader.head^ = '/') then
    begin
      if (reader.Next^ = '/') then
      begin
        reader.saveBeginning;
        if isOutOfBound then
          exit;
        while (reader.head^ <> #10) do
        begin
          if not noComment then
            identifier += reader.head^;
          reader.Next;
          if isOutOfBound then
            exit;
        end;
        if not noComment then
        begin
          addToken(ltkComment);
          if callBackDoStop then
            exit;
        end;
        reader.Next;
        continue;
      end
      else
        reader.previous;
    end;

    // block comments
    if (reader.head^ = '/') then
    begin
      if (reader.Next^ = '*') then
      begin
        reader.saveBeginning;
        if isOutOfBound then
          exit;
        reader.Next;
        while (reader.head^ <> '/') or ((reader.head - 1)^ <> '*') do
        begin
          if not noComment then
            identifier += reader.head^;
          reader.Next;
          if isOutOfBound then
            exit;
        end;
        if not noComment then
        begin
          addToken(ltkComment);
          if callBackDoStop then
            exit;
        end;
        reader.Next;
        continue;
      end
      else
        reader.previous;
    end;

    // nested block comments
    if (reader.head^ = '/') then
    begin
      if (reader.Next^ = '+') then
      begin
        reader.saveBeginning;
        nestedCom := 1;
        if isOutOfBound then
          exit;
        repeat
          while ((reader.head^ <> '+') or (reader.head^ <> '/')) and
                ((reader.next^ <> '/') or (reader.head^ <> '+')) do
          begin
            if isOutOfBound then
              exit;
            if not noComment then
              identifier += reader.head^;
            if ((reader.head-1)^ = '/') and (reader.head^ = '+') then
            begin
              nestedCom += 1;
              break;
            end;
            if ((reader.head-1)^ = '+') and (reader.head^ = '/') then
            begin
              nestedCom -= 1;
              break;
            end;
            if isOutOfBound then
              exit;
          end;
        until nestedCom = 0;
        if not noComment then
        begin
          addToken(ltkComment);
          if callBackDoStop then
            exit;
        end;
        reader.Next;
        continue;
      end
      else
        reader.previous;
    end;

    // double quoted or raw strings
    rstring := false;
    if (reader.head^ in ['r', 'x']) then
    begin
      rstring := reader.head^ = 'r';
      if not (reader.Next^ = '"') then
        reader.previous;
    end;
    if (reader.head^ = '"') then
    begin
      reader.saveBeginning;
      reader.Next;
      if isOutOfBound then
        exit;
      if (reader.head^ = '"') then
      begin
        if isStringPostfix(reader.Next^) then
          reader.Next;
        addToken(ltkString);
        rstring := false;
        if callBackDoStop then
          exit;
        continue;
      end;
      while (True) do
      begin
        if reader.head^ = '\' then
        begin
          identifier += reader.head^;
          reader.Next;
          if isOutOfBound then
            exit;
          if rstring then
            continue;
          identifier += reader.head^;
          reader.Next;
          if isOutOfBound then
            exit;
          continue;
        end;
        if (reader.head^ = '"') then
        begin
          reader.Next;
          if isOutOfBound then
            exit;
          break;
        end
        else
        begin
          identifier += reader.head^;
          reader.Next;
          if isOutOfBound then
            exit;
        end;
      end;
      if isStringPostfix(reader.head^) then
      begin
        identifier += reader.head^;
        reader.Next;
      end;
      addToken(ltkString);
      rstring := false;
      if callBackDoStop then
        exit;
      continue;
    end;

    // back quoted strings
    if (reader.head^ = '`') then
    begin
      reader.Next;
      reader.saveBeginning;
      if isOutOfBound then
        exit;
      while (reader.head^ <> '`') do
      begin
        identifier += reader.head^;
        reader.Next;
        if isOutOfBound then
          exit;
      end;
      if isStringPostfix(reader.Next^) then
        reader.Next;
      if isOutOfBound then
        exit;
      addToken(ltkString);
      if callBackDoStop then
        exit;
      continue;
    end;

    // token string
    if (reader.head^ = 'q') then
    begin
      if (reader.Next^ = '{') then
      begin
        reader.saveBeginning;
        reader.Next;
        if isOutOfBound then
          exit;
        identifier := 'q{';
        addToken(ltkSymbol);
        if callBackDoStop then
          exit;
        continue;
      end else
        reader.previous;
    end;

    // char literals
    if (reader.head^ = #39) then
    begin
      reader.Next;
      reader.saveBeginning;
      if isOutOfBound then
        exit;
      while true do
      begin
        if reader.head^ = '\' then
        begin
          identifier += reader.head^;
          reader.Next;
          identifier += reader.head^;
          if isOutOfBound then
            exit;
          reader.Next;
          if isOutOfBound then
            exit;
        end;
        if isOutOfBound then
          exit;
        if reader.head^ = #39 then
          break;
        identifier += reader.head^;
        reader.Next;
      end;
      reader.Next;
      if isOutOfBound then
        exit;
      addToken(ltkChar);
      if callBackDoStop then
        exit;
      continue;
    end;

    // binary and hex literals
    if (reader.head^ = '0') then
    begin
      reader.saveBeginning;
      if reader.Next^ in ['b', 'B'] then
      begin
        identifier := '0' + reader.head^;
        while reader.Next^ in ['0','1','_'] do
          identifier += reader.head^;
        if  (reader.head[0..1] = 'LU') or
            (reader.head[0..1] = 'Lu') or
            (reader.head[0..1] = 'UL') or
            (reader.head[0..1] = 'uL') then
        begin
          identifier += reader.head[0..1];
          reader.Next;
          reader.Next;
        end else
        if reader.head^ in ['L','u','U'] then
        begin
          identifier += reader.head^;
          reader.Next;
        end;
        if isWhite(reader.head^) or isOperator1(reader.head^) or
          isSymbol(reader.head^) then
        begin
          addToken(ltkNumber);
          if callBackDoStop then
            exit;
          continue;
        end
        else
        begin
          while true do
          begin
            if isWhite(reader.head^) or isOperator1(reader.head^) or
              isSymbol(reader.head^) or isOutOfBound then
            begin
              addToken(ltkIllegal);
              break;
              if callBackDoStop then
                exit;
            end;
            identifier += reader.head^;
            reader.Next;
          end;
          continue;
        end;
      end
      else if reader.head^ in ['x', 'X'] then
      begin
        identifier := '0' + reader.head^;
        reader.Next;
        expSet := reader.head^ in ['p','P'];
        decSet := reader.head^ = '.';
        if not (expSet or decSet) then
          reader.previous;
        while reader.Next^ in ['0'..'9', 'a'..'f', 'A'..'F', '_'] do
          identifier += reader.head^;
        decSet := reader.head^ = '.';
        expSet := reader.head^ in ['p','P'];
        if (not expSet and decSet) then
          while reader.Next^ in ['0'..'9', 'a'..'f', 'A'..'F', '_'] do
            identifier += reader.head^
        else if (expSet) then
          while reader.Next^ in ['0'..'9', '_'] do
            identifier += reader.head^;
        if not expSet then expSet:= reader.head^ in ['p','P'];
        if (expSet) then
          while reader.Next^ in ['0'..'9', '_'] do
            identifier += reader.head^;
        if  (reader.head[0..1] = 'LU') or
            (reader.head[0..1] = 'Lu') or
            (reader.head[0..1] = 'UL') or
            (reader.head[0..1] = 'Li') or
            (reader.head[0..1] = 'fi') or
            (reader.head[0..1] = 'uL') then
        begin
          identifier += reader.head[0..1];
          reader.Next;
          reader.Next;
        end else
        if reader.head^ in ['L','u','U', 'i', 'f'] then
        begin
          identifier += reader.head^;
          reader.Next;
        end;
        if isWhite(reader.head^) or isOperator1(reader.head^) or
          isSymbol(reader.head^) then
        begin
          addToken(ltkNumber);
          if callBackDoStop then
            exit;
          continue;
        end
        else
        begin
          while true do
          begin
            if isWhite(reader.head^) or isOperator1(reader.head^) or
              isSymbol(reader.head^) or isOutOfBound then
            begin
              addToken(ltkIllegal);
              break;
              if callBackDoStop then
                exit;
            end;
            identifier += reader.head^;
            reader.Next;
          end;
          continue;
        end;
      end
      else
        reader.previous;
    end;

    // check float literal starting with dec separator
    decSet := false;
    expSet := false;
    if (reader.head^= '.') then
    begin
      if isNumber(reader.Next^) then
        decSet := true
      else
        reader.previous;
    end;

    // decimal number literals
    if isNumber(reader.head^) then
    begin
      reader.saveBeginning;
      if decSet then
        identifier:= '.';
      identifier += reader.head^;
      while isNumber(reader.Next^) or (reader.head^ = '_') do
      begin
        if isOutOfBound then
          exit;
        identifier += reader.head^;
      end;
      if decSet and (reader.head^ = '.') then
      begin
        addToken(ltkNumber);
        if callBackDoStop then
          exit;
        continue;
      end;
      if (reader.head^ = '.') then
      begin
        decSet := true;
        identifier += reader.head^;
      end;
      expSet := reader.head^ in ['e','E'];
      if expSet then identifier += reader.head^;
      if decSet then while isNumber(reader.Next^) or (reader.head^ = '_') do
      begin
        if isOutOfBound then
          exit;
        identifier += reader.head^;
      end;
      if not expSet then
      begin
        expSet := reader.head^ in ['e','E'];
        if expSet then identifier += reader.head^;
      end;
      if expSet then while isNumber(reader.Next^) or (reader.head^ = '_') do
      begin
        if isOutOfBound then
          exit;
        identifier += reader.head^;
      end;
      if  (reader.head[0..1] = 'LU') or
          (reader.head[0..1] = 'Lu') or
          (reader.head[0..1] = 'UL') or
          (reader.head[0..1] = 'Li') or
          (reader.head[0..1] = 'fi') or
          (reader.head[0..1] = 'uL') then
      begin
        identifier += reader.head[0..1];
        reader.Next;
        reader.Next;
      end else
      if reader.head^ in ['L','u','U', 'i', 'f'] then
      begin
        identifier += reader.head^;
        reader.Next;
      end;
      if isWhite(reader.head^) or isOperator1(reader.head^) or
        isSymbol(reader.head^) then
      begin
        addToken(ltkNumber);
        if callBackDoStop then
          exit;
        continue;
      end
      else
      begin
        while true do
        begin
          if isWhite(reader.head^) or isOperator1(reader.head^) or
            isSymbol(reader.head^) or isOutOfBound then
          begin
            addToken(ltkIllegal);
            break;
            if callBackDoStop then
              exit;
          end;
          identifier += reader.head^;
          reader.Next;
        end;
        continue;
      end;
    end;

    // symbols
    if isSymbol(reader.head^) then
    begin
      reader.saveBeginning;
      identifier += reader.head^;
      if (reader.head^ = '}') and ((reader.head + 1)^  in stringPostfixes) and not
        isIdentifier((reader.head + 2)^) then
          reader.Next;
      reader.Next;
      addToken(ltkSymbol);
      if callBackDoStop then
        exit;
      if isOutOfBound then
        exit;
      continue;
    end;

    // operators
    if isOperator1(reader.head^) then
    begin
      reader.saveBeginning;
      while isOperator1(reader.head^) do
      begin
        if isOutOfBound then
          exit;
        identifier += reader.head^;
        reader.next;
        if length(identifier) = 4 then
          break;
      end;
      if length(identifier) = 4 then
      begin
        if isOperator4(identifier) then
        begin
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
        if isOperator3(identifier[1..3]) then
        begin
          setLength(identifier, 3);
          reader.previous;
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
        if isOperator2(identifier[1..2]) then
        begin
          setLength(identifier, 2);
          reader.previous;
          reader.previous;
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
        if isOperator1(identifier[1]) then
        begin
          setLength(identifier, 1);
          reader.previous;
          reader.previous;
          reader.previous;
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
      end;
      if length(identifier) = 3 then
      begin
        if isOperator3(identifier) then
        begin
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
        if isOperator2(identifier[1..2]) then
        begin
          setLength(identifier, 2);
          reader.previous;
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
        if isOperator1(identifier[1]) then
        begin
          setLength(identifier, 1);
          reader.previous;
          reader.previous;
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
      end;
      if length(identifier) = 2 then
      begin
        if isOperator2(identifier) then
        begin
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
        if isOperator1(identifier[1]) then
        begin
          setLength(identifier, 1);
          reader.previous;
          addToken(ltkOperator);
          if callBackDoStop then
            exit;
          continue;
        end;
      end;
      if (length(identifier) = 1) and isOperator1(identifier[1]) then
      begin
        addToken(ltkOperator);
        if callBackDoStop then
          exit;
        continue;
      end;
    end;

    // identifiers and keywords
    if isFirstIdentifier(reader.head^) then
    begin
      reader.saveBeginning;
      while isIdentifier(reader.head^) do
      begin
        identifier += reader.head^;
        reader.Next;
        if isOutOfBound then
          exit;
      end;
      if keywordsMap.match(identifier) then
        addToken(ltkKeyword)
      else if specialKeywordsMap.match(identifier) then
        addToken(ltkKeyword)
      else
        addToken(ltkIdentifier);
      if callBackDoStop then
        exit;
      continue;
    end;

    // illegal
    while not isWhite(reader.head^) or not isSymbol(reader.head^) or
      not isOperator1(reader.head^) do
    begin
      if isOutOfBound then
        break;
      reader.Next;
    end;
    {$IFDEF DEBUG}
    identifier += ' (unrecognized lexer input)';
    {$ENDIF}
    addToken(ltkIllegal);

  end;
end;
{$ENDREGION}

{$REGION Utils}
function TLexErrorList.getError(index: integer): TLexError;
begin
  Result := PLexError(Items[index])^;
end;

procedure TLexErrorList.Clear;
begin
  while Count > 0 do
  begin
    Dispose(PLexError(Items[Count - 1]));
    Delete(Count - 1);
  end;
end;

procedure TLexErrorList.addError(value: PLexError);
begin
  add(Pointer(value));
end;

function TLexErrorEnumerator.GetCurrent: TLexError;
begin
  exit(fList.error[fIndex]);
end;

function TLexErrorEnumerator.MoveNext: Boolean;
begin
  Inc(fIndex);
  exit(fIndex < fList.Count);
end;

operator enumerator(list: TLexErrorList): TLexErrorEnumerator;
begin
  Result := TLexErrorEnumerator.Create;
  Result.fList := list;
  Result.fIndex := -1;
end;

function getModuleName(list: TLexTokenList): string;
var
  ltk: PLexToken;
  mtok: boolean = false;
begin
  Result := '';
  for ltk in list do
  begin
    if mtok then
    begin
      case ltk^.kind of
        ltkIdentifier, ltkKeyword:
          Result += ltk^.Data;
        ltkSymbol:
          case ltk^.Data of
            '.': Result += ltk^.Data;
            ';': exit;
          end;
      end;
    end
    else
    if ltk^.kind = ltkKeyword then
      if ltk^.Data = 'module' then
        mtok := True;
  end;
end;

procedure getImports(list: TLexTokenList; imports: TStrings);
var
  i: integer;
  imp: boolean = false;
  tok: PLexToken;
  itm: string = '';
begin
  for i:= 0 to list.Count-1 do
  begin
    tok := list[i];
    if (tok^.kind = ltkKeyword) and (tok^.Data = 'import') then
    begin
      imp := true;
      continue;
    end;
    if not imp then
      continue;
    //
    if (tok^.Data = '=') then
      itm := ''
    else if (tok^.Data = ';') or (tok^.Data = ':') or (tok^.Data = ',') then
    begin
      if (length(itm) <> 0) and (imports.IndexOf(itm) = -1) then
        imports.Add(itm);
      itm := '';
      if (tok^.Data = ';') or (tok^.Data = ':') then
        imp := false;
    end else
      itm += tok^.Data;
  end;
end;
{$ENDREGION}
end.

