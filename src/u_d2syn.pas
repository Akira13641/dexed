unit u_d2syn;

{$I u_defines.inc}

interface

uses
  Classes, SysUtils, Graphics,
  SynEditHighlighter, SynEditHighlighterFoldBase, SynEditTypes,
  u_dlangutils,u_dlangmaps;

type

  TTokenKind = (tkCommt, tkIdent, tkKeywd, tkStrng, tkBlank, tkSymbl, tkNumbr,
    tkDDocs, tkSpecK, tkError, tkAsmbl, tkAttri, tkLost,  tkTypes);

  TRangeKind = (rkString1, rkString2, rkBlockCom1, rkBlockCom2,
    rkBlockDoc1, rkBlockDoc2, rkAsm);

  TRangeKinds = set of TRangeKind;

  // defines the ranges which can be folded
  TFoldKinds = set of (fkBrackets, fkComments1, fkComments2, fkStrings, fkRegion,
    fkDDoc);

  // internal class used to keep trace of the useful informations of the previous line
  TSynD2SynRange = class(TSynCustomHighlighterRange)
  private
    namedRegionCount: Integer;
    nestedCommentsCount: Integer;
    rangeKinds: TRangeKinds;
    // double quoted multi-line string prefixed with 'r':
    // => don't skip '"' following '\'
    rString: boolean;
  public
    procedure Assign(source: TSynCustomHighlighterRange); override;
    function Compare(range: TSynCustomHighlighterRange): integer; override;
    procedure Clear; override;
    //
    procedure copyFrom(source: TSynD2SynRange);
  end;

	TSynD2Syn = class (TSynCustomFoldHighlighter)
	private
    fWhiteAttrib: TSynHighlighterAttributes;
		fNumbrAttrib: TSynHighlighterAttributes;
		fSymblAttrib: TSynHighlighterAttributes;
		fIdentAttrib: TSynHighlighterAttributes;
		fCommtAttrib: TSynHighlighterAttributes;
		fStrngAttrib: TSynHighlighterAttributes;
    fKeywdAttrib: TSynHighlighterAttributes;
    fDDocsAttrib: TSynHighlighterAttributes;
    fAsblrAttrib: TSynHighlighterAttributes;
    fSpeckAttrib: TSynHighlighterAttributes;
    fErrorAttrib: TSynHighlighterAttributes;
    fAttriAttrib: TSynHighlighterAttributes;
    fLost_Attrib: TSynHighlighterAttributes;
    fTypesAttrib: TSynHighlighterAttributes;
    fLineBuf: string;
    fTokStart, fTokStop: Integer;
    fTokKind: TTokenKind;
    fCurrRange: TSynD2SynRange;
    fFoldKinds: TFoldKinds;
    fAttribLut: array[TTokenKind] of TSynHighlighterAttributes;
    fPhobosStyleType: boolean;
    procedure doAttribChange(sender: TObject); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure doChanged; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setFoldKinds(value: TFoldKinds); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setWhiteAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setNumbrAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setSymblAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setIdentAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setCommtAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setStrngAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setKeywdAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setDDocsAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setAsblrAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setSpeckAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setErrorAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setAttriAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setTypesAttrib(value: TSynHighlighterAttributes); {$IFNDEF DEBUG}inline;{$ENDIF}
  protected
    function GetRangeClass: TSynCustomHighlighterRangeClass; override;
    function GetIdentChars: TSynIdentChars; override;
	published
    property phobosStyleType: boolean read fPhobosStyleType write fPhobosStyleType stored true;
    property foldKinds:   TFoldKinds read fFoldKinds write setFoldKinds stored true;
    property whites:      TSynHighlighterAttributes read fWhiteAttrib write setWhiteAttrib stored true;
    property numbers:     TSynHighlighterAttributes read fNumbrAttrib write setNumbrAttrib stored true;
    property symbols:     TSynHighlighterAttributes read fSymblAttrib write setSymblAttrib stored true;
    property identifiers: TSynHighlighterAttributes read fIdentAttrib write setIdentAttrib stored true;
    property comments:    TSynHighlighterAttributes read fCommtAttrib write setCommtAttrib stored true;
    property strings:     TSynHighlighterAttributes read fStrngAttrib write setStrngAttrib stored true;
    property keywords:    TSynHighlighterAttributes read fKeywdAttrib write setKeywdAttrib stored true;
    property ddoc:        TSynHighlighterAttributes read fDDocsAttrib write setDDocsAttrib stored true;
    property inlineAsm:   TSynHighlighterAttributes read fAsblrAttrib write setAsblrAttrib stored true;
    property special:     TSynHighlighterAttributes read fSpeckAttrib write setSpeckAttrib stored true;
    property errors:      TSynHighlighterAttributes read fErrorAttrib write setErrorAttrib stored true;
    property attributes:  TSynHighlighterAttributes read fAttriAttrib write setAttriAttrib stored true;
    property types:       TSynHighlighterAttributes read fTypesAttrib write setTypesAttrib stored true;
    property DefaultFilter stored false;
    property enabled stored false;
	public
		constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    procedure setLine(const NewValue: string; LineNumber: Integer); override;
    procedure next; override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function GetEol: Boolean; override;
    procedure SetRange(value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
	end;

implementation

procedure TSynD2SynRange.Assign(source: TSynCustomHighlighterRange);
var
  rng: TSynD2SynRange;
begin
  inherited;
  if source is TSynD2SynRange then
  begin
    rng := TSynD2SynRange(source);
    rangeKinds := rng.rangeKinds;
    namedRegionCount := rng.namedRegionCount;
  end;
end;

function TSynD2SynRange.Compare(range: TSynCustomHighlighterRange): integer;
var
  src_t: TSynD2SynRange;
const
  cmpRes: array[boolean] of integer = (-1, 1);
begin
  result := inherited Compare(range);
  assert(range <> nil);
  if result <> 0 then exit;
  //
  if range is TSynD2SynRange then
  begin
    src_t := TSynD2SynRange(range);
    if src_t.rangeKinds <> rangeKinds then exit(1);
    if src_t.rString <> rString then exit(1);
    if src_t.nestedCommentsCount <> nestedCommentsCount then
      exit(cmpRes[src_t.nestedCommentsCount > nestedCommentsCount]);
    if src_t.namedRegionCount <> namedRegionCount then
      exit(cmpRes[src_t.namedRegionCount > namedRegionCount]);
  end;
end;

procedure TSynD2SynRange.Clear;
begin
  inherited;
  nestedCommentsCount := 0;
  namedRegionCount := 0;
  rangeKinds := [];
  rString := false;
end;

procedure TSynD2SynRange.copyFrom(source: TSynD2SynRange);
begin
  if assigned(source) then
  begin
    nestedCommentsCount := source.nestedCommentsCount;
    namedRegionCount := source.namedRegionCount;
    rangeKinds := source.rangeKinds;
    rString := source.rString;
  end;
end;

constructor TSynD2Syn.create(aOwner: TComponent);
begin
	inherited create(aOwner);
  SetSubComponent(true);

  DefaultFilter:= 'D source|*.d|D interface|*.di';

  fFoldKinds := [fkBrackets,fkRegion];

  WordBreakChars := WordBreakChars - ['@'];

  fWhiteAttrib := TSynHighlighterAttributes.Create('White','White');
  fNumbrAttrib := TSynHighlighterAttributes.Create('Numbr','Numbr');
  fSymblAttrib := TSynHighlighterAttributes.Create('Symbl','Symbl');
  fIdentAttrib := TSynHighlighterAttributes.Create('Ident','Ident');
  fCommtAttrib := TSynHighlighterAttributes.Create('Commt','Commt');
  fStrngAttrib := TSynHighlighterAttributes.Create('Strng','Strng');
  fKeywdAttrib := TSynHighlighterAttributes.Create('Keywd','Keywd');
  fDDocsAttrib := TSynHighlighterAttributes.Create('DDocs','DDocs');
  fAsblrAttrib := TSynHighlighterAttributes.Create('Asblr','Asblr');
  fSpeckAttrib := TSynHighlighterAttributes.Create('Speck','Speck');
  fErrorAttrib := TSynHighlighterAttributes.Create('Error','Error');
  fAttriAttrib := TSynHighlighterAttributes.Create('Attri','Attri');
  fTypesAttrib := TSynHighlighterAttributes.Create('Types','Types');
  fLost_Attrib := TSynHighlighterAttributes.Create('Lost','Lost');

  fNumbrAttrib.Foreground := $000079F2;
  fSymblAttrib.Foreground := clMaroon;
  fIdentAttrib.Foreground := clBlack;
  fCommtAttrib.Foreground := clGreen;
  fStrngAttrib.Foreground := clBlue;
  fKeywdAttrib.Foreground := clNavy;
  fAsblrAttrib.Foreground := clGray;
  fSpeckAttrib.Foreground := clNavy;
  fAttriAttrib.Foreground := clNavy;
  fLost_Attrib.Foreground := clLime;
  fDDocsAttrib.Foreground := clTeal;
  fTypesAttrib.Foreground := clBlack;

  fLost_Attrib.Background := clBlack;

  fCommtAttrib.Style := [fsItalic];
  fKeywdAttrib.Style := [fsBold];
  fAsblrAttrib.Style := [fsBold];
  fSpeckAttrib.Style := [fsBold];
  fAttriAttrib.Style := [fsBold];
  fLost_Attrib.Style := [fsBold];
  fTypesAttrib.Style := [fsBold];

  fErrorAttrib.Foreground:= fIdentAttrib.Foreground;
  fErrorAttrib.FrameStyle:= slsWaved;
  fErrorAttrib.FrameColor:= clRed;
  fErrorAttrib.FrameEdges:= sfeBottom;

  AddAttribute(fWhiteAttrib);
  AddAttribute(fNumbrAttrib);
  AddAttribute(fSymblAttrib);
  AddAttribute(fIdentAttrib);
  AddAttribute(fCommtAttrib);
  AddAttribute(fStrngAttrib);
  AddAttribute(fKeywdAttrib);
  AddAttribute(fDDocsAttrib);
  AddAttribute(fAsblrAttrib);
  AddAttribute(fSpeckAttrib);
  AddAttribute(fErrorAttrib);
  AddAttribute(fAttriAttrib);
  AddAttribute(fTypesAttrib);

  fAttribLut[TTokenKind.tkident] := fIdentAttrib;
  fAttribLut[TTokenKind.tkBlank] := fWhiteAttrib;
  fAttribLut[TTokenKind.tkCommt] := fCommtAttrib;
  fAttribLut[TTokenKind.tkKeywd] := fKeywdAttrib;
  fAttribLut[TTokenKind.tkNumbr] := fNumbrAttrib;
  fAttribLut[TTokenKind.tkStrng] := fStrngAttrib;
  fAttribLut[TTokenKind.tksymbl] := fSymblAttrib;
  fAttribLut[TTokenKind.tkDDocs] := fDDocsAttrib;
  fAttribLut[TTokenKind.tkSpecK] := fSpeckAttrib;
  fAttribLut[TTokenKind.tkError] := fErrorAttrib;
  fAttribLut[TTokenKind.tkAsmbl] := fAsblrAttrib;
  fAttribLut[TTokenKind.tkAttri] := fAttriAttrib;
  fAttribLut[TTokenKind.tkTypes] := fTypesAttrib;
  fAttribLut[TTokenKind.tkLost]  := fLost_Attrib;

  SetAttributesOnChange(@doAttribChange);
  fTokStop := 1;
  next;
end;

destructor TSynD2Syn.destroy;
begin
  fLost_Attrib.Free;
  fCurrRange.Free;
  inherited;
end;

procedure TSynD2Syn.doChanged;
begin
  BeginUpdate;
    fUpdateChange := true;
  EndUpdate;
end;

procedure TSynD2Syn.setFoldKinds(value: TFoldKinds);
begin
  fFoldKinds := value;
  DoFoldConfigChanged(Self);
  doChanged;
end;

procedure TSynD2Syn.Assign(Source: TPersistent);
var
  srcsyn: TSynD2Syn;
begin
  inherited;
  if Source is TSynD2Syn then
  begin
    srcsyn := TSynD2Syn(Source);
    foldKinds := srcsyn.foldKinds;
    fPhobosStyleType:=srcsyn.fPhobosStyleType;
  end;
end;

function TSynD2Syn.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  result := TSynD2SynRange;
end;

function TSynD2Syn.GetIdentChars: TSynIdentChars;
begin
  result := ['_', 'A'..'Z', 'a'..'z', '0'..'9'];
end;

procedure TSynD2Syn.doAttribChange(sender: TObject);
begin
  doChanged;
end;

procedure TSynD2Syn.setWhiteAttrib(value: TSynHighlighterAttributes);
begin
  fWhiteAttrib.Assign(value);
end;

procedure TSynD2Syn.setNumbrAttrib(value: TSynHighlighterAttributes);
begin
  fNumbrAttrib.Assign(value);
end;

procedure TSynD2Syn.setSymblAttrib(value: TSynHighlighterAttributes);
begin
  fSymblAttrib.Assign(value);
end;

procedure TSynD2Syn.setIdentAttrib(value: TSynHighlighterAttributes);
begin
  fIdentAttrib.Assign(value);
end;

procedure TSynD2Syn.setCommtAttrib(value: TSynHighlighterAttributes);
begin
  fCommtAttrib.Assign(value);
end;

procedure TSynD2Syn.setStrngAttrib(value: TSynHighlighterAttributes);
begin
  fStrngAttrib.Assign(value);
end;

procedure TSynD2Syn.setKeywdAttrib(value: TSynHighlighterAttributes);
begin
  fKeywdAttrib.Assign(value);
end;

procedure TSynD2Syn.setDDocsAttrib(value: TSynHighlighterAttributes);
begin
  fDDocsAttrib.Assign(value);
end;

procedure TSynD2Syn.setAsblrAttrib(value: TSynHighlighterAttributes);
begin
  fAsblrAttrib.Assign(value);
end;

procedure TSynD2Syn.setSpeckAttrib(value: TSynHighlighterAttributes);
begin
  fSpeckAttrib.Assign(value);
end;

procedure TSynD2Syn.setErrorAttrib(value: TSynHighlighterAttributes);
begin
  fErrorAttrib.Assign(value);
end;

procedure TSynD2Syn.setAttriAttrib(value: TSynHighlighterAttributes);
begin
  fAttriAttrib.Assign(value);
end;

procedure TSynD2Syn.setTypesAttrib(value: TSynHighlighterAttributes);
begin
  fTypesAttrib.Assign(value);
end;

procedure TSynD2Syn.setLine(const NewValue: string; LineNumber: Integer);
begin
  inherited;
  // note: the end of line is marked with a #10
  // usually a TSynCustomFoldHighlighter rather tests 'length(line)' in 'Next()'
  // but since the scanner works line by line, #10 is guaranteed not to
  // appear at all excepted when added implictly, like here.
  fLineBuf := NewValue + #10;
  fTokStop := 1;
  next;
end;

procedure TSynD2Syn.next;
var
  reader: PChar = nil;
label
  _notRawStrng, _notDotFloat;

procedure readerReset; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  fTokStop := fTokStart;
  reader := @fLineBuf[fTokStop];
end;

function readerNext: PChar; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  Inc(reader);
  Inc(fTokStop);
  exit(reader);
end;

function readerPrev: PChar; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  Dec(reader);
  Dec(fTokStop);
  exit(reader);
end;

begin

  fTokStart := fTokStop;
  fTokStop  := fTokStart;

  // EOL
  if fTokStop > length(fLineBuf) then exit;
  readerReset;

  // script line
  if LineIndex = 0 then if fTokStart = 1 then
    if readDelim(reader, fTokStop, '#!') then
    begin
      fTokKind := tkCommt;
      readLine(reader, fTokStop);
      exit;
    end else readerReset;

  // spaces
  if (isWhite(reader^)) then
  begin
    fTokKind := tkBlank;
    while(true) do
    begin
      if isWhite(reader^) then
        readerNext;
      exit;
    end;
  end;

  if not assigned(fCurrRange) then
    fCurrRange := TSynD2SynRange.Create(nil);

  // line comments / region beg-end
  if (fCurrRange.rangeKinds = []) or (fCurrRange.rangeKinds = [rkAsm])
      then if readDelim(reader, fTokStop, '//') then
  begin
    fTokKind := tkCommt;
    if readDelim(reader, fTokStop, '/') then
      fTokKind := tkDDocs;
    readLine(reader, fTokStop);
    if (fkRegion in fFoldKinds) and (fTokStop - fTokStart > 4) then
    begin
      while isWhite(reader^) do
      begin
        Dec(reader);
        Dec(fTokStop);
      end;
      Dec(reader, 3);
      Dec(fTokStop, 3);
      if reader[0..3] = '---+' then
      begin
        fCurrRange.namedRegionCount += 1;
        StartCodeFoldBlock(nil);
      end
      else if (reader[0..3] = '----') and (fCurrRange.namedRegionCount > 0) then
      begin
        EndCodeFoldBlock();
        fCurrRange.namedRegionCount -= 1;
      end;
      readLine(reader, fTokStop);
    end;
    exit;
  end else readerReset;

  // block comments 1
  if (fCurrRange.rangeKinds = []) or (fCurrRange.rangeKinds = [rkAsm]) then
    if readDelim(reader, fTokStop, '/*') then
  begin
    fTokKind := tkCommt;
    if readDelim(reader, fTokStop, '*') then
      if readDelim(reader, fTokStop, '/') then exit
        else fTokKind := tkDDocs;
    if readUntil(reader, fTokStop, '*/') then
      exit;
    if fTokKind = tkDDocs then
      fCurrRange.rangeKinds += [rkBlockDoc1]
    else
      fCurrRange.rangeKinds += [rkBlockCom1];
    readLine(reader, fTokStop);
    if (fTokKind = tkCommt) then
      StartCodeFoldBlock(nil, fkComments1 in fFoldKinds)
    else if (fTokKind = tkDDocs) then
      StartCodeFoldBlock(nil, fkDDoc in fFoldKinds);
    exit;
  end else readerReset;
  if (rkBlockCom1 in fCurrRange.rangeKinds) or (rkBlockDoc1 in fCurrRange.rangeKinds) then
  begin
    if (rkBlockDoc1 in fCurrRange.rangeKinds) then fTokKind := tkDDocs
      else fTokKind := tkCommt;
    if readUntil(reader, fTokStop, '*/') then
    begin
      if (fTokKind = tkCommt) then
        EndCodeFoldBlock(fkComments1 in fFoldKinds)
      else if (fTokKind = tkDDocs) then
        EndCodeFoldBlock(fkDDoc in fFoldKinds);
      fCurrRange.rangeKinds -= [rkBlockDoc1, rkBlockCom1];
      exit;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // block comments 2
  if (fCurrRange.rangeKinds = []) or (fCurrRange.rangeKinds = [rkAsm]) then
    if readDelim(reader, fTokStop, '/+') then
  begin
    fTokKind := tkCommt;
    if readDelim(reader, fTokStop, '+') then
      if readDelim(reader, fTokStop, '/') then
        exit
      else
        fTokKind := tkDDocs;
    inc(fCurrRange.nestedCommentsCount);
    while (reader^ <> #10) and (fCurrRange.nestedCommentsCount > 0) do
    begin
      if readUntilAmong(reader, fTokStop, ['+', '/']) then
      begin
        if readDelim(reader, fTokStop, ['+', '/']) then
        begin
          if ((reader-1)^ = '/') and (reader^ = '+') then
          begin
            inc(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
          if ((reader-1)^ = '+') and (reader^ = '/') then
          begin
            dec(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
        end else readerNext;
      end;
    end;
    if (fCurrRange.nestedCommentsCount > 0) then
    begin
      if fTokKind = tkDDocs then
        fCurrRange.rangeKinds += [rkBlockDoc2]
      else
        fCurrRange.rangeKinds += [rkBlockCom2];
      if (fTokKind = tkCommt) then
        StartCodeFoldBlock(nil, fkComments2 in fFoldKinds)
      else if (fTokKind = tkDDocs) then
        StartCodeFoldBlock(nil, fkDDoc in fFoldKinds);
    end;
    exit;
  end else readerReset;
  if (rkBlockCom2 in fCurrRange.rangeKinds) or (rkBlockDoc2 in fCurrRange.rangeKinds) then
  begin
    if (rkBlockDoc2 in fCurrRange.rangeKinds) then fTokKind := tkDDocs
      else fTokKind := tkCommt;
    while (reader^ <> #10) and (fCurrRange.nestedCommentsCount > 0) do
    begin
      if readUntilAmong(reader, fTokStop, ['+', '/']) then
      begin
        if readDelim(reader, fTokStop, ['+', '/']) then
        begin
          if ((reader-1)^ = '/') and (reader^ = '+') then
          begin
            inc(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
          if ((reader-1)^ = '+') and (reader^ = '/') then
          begin
            dec(fCurrRange.nestedCommentsCount);
            readerNext;
            continue;
          end;
        end else readerNext;
      end;
    end;
    if fCurrRange.nestedCommentsCount = 0 then
    begin
      if (fTokKind = tkCommt) then
        EndCodeFoldBlock(fkComments2 in fFoldKinds)
      else if (fTokKind = tkDDocs) then
        EndCodeFoldBlock(fkDDoc in fFoldKinds);
      fCurrRange.rangeKinds -= [rkBlockDoc2, rkBlockCom2];
    end;
    exit;
  end;

  // double quoted strings | raw double quoted strings
  if (fCurrRange.rangeKinds = []) and readDelim(reader, fTokStop, stringPrefixes) then
  begin
    if readerPrev^ in ['r','x','q'] then
    begin
      fCurrRange.rString := reader^ = 'r';
      if not (readerNext^ = '"') then
      begin
        fCurrRange.rString := false;
        readerPrev;
        goto _notRawStrng;
      end;
    end;
    readerNext;
    fTokKind := tkStrng;
    while(true) do
    begin
      if not readUntilAmong(reader, fTokStop, stringStopChecks) then break;
      if (reader^ = '\') then
      begin
        readerNext;
        if reader^ <> #10 then
        begin
          if fCurrRange.rString then continue;
          readerNext;
        end;
      end
      else if reader^ = '"' then
      begin
        readerNext;
        readDelim(reader, fTokStop, stringPostfixes);
        fCurrRange.rString := false;
        exit;
      end;
    end;
    fCurrRange.rangeKinds += [rkString1];
    StartCodeFoldBlock(nil, fkStrings in fFoldKinds);
    exit;
  end else _notRawStrng: readerReset;
  if rkString1 in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    while(true) do
    begin
      if not readUntilAmong(reader, fTokStop, stringStopChecks) then break;
      if reader^ = '\' then
      begin
        readerNext;
        if reader^ <> #10 then
        begin
          if fCurrRange.rString then continue;
          readerNext;
        end;
      end
      else if reader^ = '"' then
      begin
        readerNext;
        fCurrRange.rangeKinds -= [rkString1];
        readDelim(reader, fTokStop, stringPostfixes);
        fCurrRange.rString := false;
        EndCodeFoldBlock(fkStrings in fFoldKinds);
        exit;
      end
      else break;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // backticks strings
  if (fCurrRange.rangeKinds = []) and readDelim(reader, fTokStop, '`') then
  begin
    fTokKind := tkStrng;
    if readUntil(reader, fTokStop, '`') then
    begin
      readDelim(reader, fTokStop, stringPostfixes);
      exit;
    end;
    fCurrRange.rangeKinds += [rkString2];
    readLine(reader, fTokStop);
    StartCodeFoldBlock(nil, fkStrings in fFoldKinds);
    exit;
  end else readerReset;
  if rkString2 in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    if readUntil(reader, fTokStop, '`') then
    begin
      fCurrRange.rangeKinds -= [rkString2];
      EndCodeFoldBlock(fkStrings in fFoldKinds);
      readDelim(reader, fTokStop, stringPostfixes);
      exit;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // token string
  if readDelim(reader, fTokStop, 'q{') then
  begin
    fTokKind := tkSymbl;
    StartCodeFoldBlock(nil, fkBrackets in fFoldKinds);
    exit;
  end else readerReset;

  // char literals
  if (fCurrRange.rangeKinds = []) and readDelim(reader, fTokStop, #39) then
  begin
    fTokKind := tkStrng;
    while true do
    begin
      if reader^ = '\' then
      begin
        readerNext;
        if reader^ = #10 then
          exit;
        readerNext;
      end;
      if reader^ = #10 then
        exit;
      if reader^ = #39 then
        break;
      readerNext;
    end;
    readerNext;
    exit;
  end else readerReset;

  // bin & hex literals
  if reader^ = '0' then if readerNext^ in ['b','B', 'x', 'X'] then
  begin
    fTokKind:= tkNumbr;
    readerNext;
    if (reader-1)^ in ['b','B'] then
      readWhile(reader, fTokStop, ['0','1','_'])
    else
      readWhile(reader, fTokStop, hexaChars + ['.']);
    // exponent, sign tokenized later as op then value as number
    if reader^ in ['P','p'] then
    begin
      readerNext;
      exit;
    end;
         if not tryReadDelim(reader, fTokStop, 'uL')
    then if not tryReadDelim(reader, fTokStop, 'UL')
    then if not tryReadDelim(reader, fTokStop, 'Lu')
    then if not tryReadDelim(reader, fTokStop, 'LU')
    then if reader^ in ['U', 'L', 'u', 'i'] then
      readerNext;
    if not isWhite(reader^) and not isOperator1(reader^) and
      not isSymbol(reader^) then
    begin
      fTokKind:= tkError;
      readUntilAmong(reader, fTokStop, [#0..#32] + symbChars);
    end;
    exit;
  end
  else readerPrev;

  // int and float literals
  if (reader^ = '.') or (isNumber(reader^)) then
  begin
    if (reader^= '.') then
    begin
      readerPrev;
      if reader^= '.' then
      begin
        readerNext;
        goto _notDotFloat;
      end else
        readerNext;
      readerNext;
      if not isNumber(reader^) then
      begin
        readerPrev;
        goto _notDotFloat;
      end else
        readerPrev;
    end;
    fTokKind:= tkNumbr;
    if reader^ <> '.' then
      while isNumber(readerNext^) or (reader^ = '_') do (*!*);
    if reader^ = '.' then
    begin
      if isNumber(readerNext^) then
      begin
        while isNumber(reader^) or (reader^ = '_') do
          readerNext;
      end else
        readerPrev;
    end;
    if reader^= '.' then
    begin
      readerNext;
      // .init .min .max etc.
      if not isNumber(reader^) then
      begin
        readerPrev;
        exit;
      end;
      readerPrev;
    end;
    // exponent, sign tokenized later as op then value as number
    if reader^ in ['E','e'] then
    begin
      readerNext;
      exit;
    end;
    // try valid suffixes
         if not tryReadDelim(reader, fTokStop, 'uL')
    then if not tryReadDelim(reader, fTokStop, 'UL')
    then if not tryReadDelim(reader, fTokStop, 'Lu')
    then if not tryReadDelim(reader, fTokStop, 'LU')
    then if not tryReadDelim(reader, fTokStop, 'fi')
    then if not tryReadDelim(reader, fTokStop, 'Fi')
    then if not tryReadDelim(reader, fTokStop, 'Li')
    then if reader^ in ['U','L','u', 'i', 'f','F'] then
      readerNext;
    if not isWhite(reader^) and not isOperator1(reader^) and
      (not isSymbol(reader^) or (reader^ = '.')) then
    begin
      fTokKind:= tkError;
      readUntilAmong(reader, fTokStop, [#0..#32] + symbChars - ['.']);
    end;
    if (fTokStop - fTokStart = 10) and (fLineBuf[fTokStart..fTokStop-1] = '4815162342') then
      fTokKind:=tkLost;
    exit;
  end;
  _notDotFloat:

  // symbols
  if isSymbol(reader^) then
  begin
    fTokKind := tkSymbl;
    case reader^ of
      '{': StartCodeFoldBlock(nil, fkBrackets in fFoldKinds);
      '}':
      begin
        EndCodeFoldBlock(fkBrackets in fFoldKinds);
        if (reader^ = '}') and (rkAsm in fCurrRange.rangeKinds) then
          fCurrRange.rangeKinds -= [rkAsm]; ;
        if ((reader+1)^ in stringPostfixes) and not isIdentifier((reader+2)^) then
          readerNext;
      end;
    end;
    readerNext;
    exit;
  end;

  // operators
  if isOperator1(reader^) then
  begin
    fTokKind := tkSymbl;
    while isOperator1(readerNext^) do
      if (fTokStop - fTokStart = 4) or (reader^= #10) then
        break;
    if (fTokStop - fTokStart = 4) then
    begin
      if isOperator4(fLineBuf[fTokStart..fTokStart+3]) then
        exit;
      if isOperator3(fLineBuf[fTokStart..fTokStart+2]) then
      begin
        readerPrev;
        exit;
      end;
      if isOperator2(fLineBuf[fTokStart..fTokStart+1]) then
      begin
        readerPrev;
        readerPrev;
        exit;
      end;
      if isOperator1(fLineBuf[fTokStart]) then
      begin
        readerPrev;
        readerPrev;
        readerPrev;
        exit;
      end;
    end;
    if (fTokStop - fTokStart = 3) then
    begin
      if isOperator3(fLineBuf[fTokStart..fTokStart+2]) then
        exit;
      if isOperator2(fLineBuf[fTokStart..fTokStart+1]) then
      begin
        readerPrev;
        exit;
      end;
      if isOperator1(fLineBuf[fTokStart]) then
      begin
        readerPrev;
        readerPrev;
        exit;
      end;
    end;
    if (fTokStop - fTokStart = 2) then
    begin
      if isOperator2(fLineBuf[fTokStart..fTokStart+1]) then
        exit;
      if isOperator1(fLineBuf[fTokStart]) then
      begin
        readerPrev;
        exit;
      end;
    end;
    if (fTokStop - fTokStart = 1) and isOperator1(fLineBuf[fTokStart]) then
      exit;
    fTokKind := tkError;
    exit;
  end;

  // attributes
  if reader^ = '@' then
  begin
    if isAlpha(readerNext^) then
    begin
      fTokKind:=tkAttri;
      while isAlNum(reader^) or (reader^ = '_') do
      begin
        if (reader^= #10) or (reader^= '@') then
          exit;
        readerNext;
      end;
      exit;
    end else
      readerPrev;
  end;

  // Keywords & identifiers
  if isFirstIdentifier(reader^) then
  begin
    fTokKind := tkIdent;
    while(true) do
    begin
      if isWhite(readerNext^) or isSymbol(reader^) or isOperator1(reader^) or
        (reader^ = '@') then
          break;
    end;
    if keywordsMap.match(fLineBuf[FTokStart..fTokStop-1]) then
    begin
      fTokKind := tkKeywd;
      if (fLineBuf[FTokStart..fTokStop-1] = 'asm') then
        fCurrRange.rangeKinds += [rkAsm];
    end
    else if specialKeywordsMap.match(fLineBuf[FTokStart..fTokStop-1]) then
      fTokKind := tkSpecK
    else if fPhobosStyleType and ('A' <= fLineBuf[FTokStart]) and (fLineBuf[FTokStart] <= 'Z') then
      fTokKind:= tkTypes
    else if rkAsm in fCurrRange.rangeKinds then
      fTokKind:=tkAsmbl;
    exit;
  end;

  if fLineBuf[fTokStop] = #10 then exit;

  readUntilAmong(reader, fTokStop, [#9, #10, ' ']);
  fTokKind := tkError;
end;

function TSynD2Syn.GetEol: Boolean;
begin
  result := fTokStop > length(fLineBuf);
end;

function TSynD2Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  result := fAttribLut[fTokKind];
end;

procedure TSynD2Syn.SetRange(value: Pointer);
var
  stored: TSynD2SynRange;
begin
  inherited SetRange(value);
  stored := TSynD2SynRange(CodeFoldRange.RangeType);
  if not assigned(fCurrRange) or not Assigned(stored) then
    exit;
  fCurrRange.copyFrom(stored);
end;

function TSynD2Syn.GetRange: Pointer;
var
  stored: TSynD2SynRange;
begin
  stored := TSynD2SynRange(inherited GetRange);
  if (stored = nil) then
    stored := TSynD2SynRange.Create(nil);
  stored.copyFrom(fCurrRange);
  //
  CodeFoldRange.RangeType := Pointer(stored);
  Result := inherited GetRange;
end;

procedure TSynD2Syn.ResetRange;
begin
  if fCurrRange = nil then
    fCurrRange := TSynD2SynRange.Create(nil)
  else
    fCurrRange.Clear;
end;

function TSynD2Syn.GetTokenPos: Integer;
begin
  result := fTokStart - 1;
end;

function TSynD2Syn.GetToken: string;
begin
  result := copy(fLineBuf, FTokStart, fTokStop - FTokStart);
end;

procedure TSynD2Syn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart  := @fLineBuf[FTokStart];
  TokenLength := fTokStop - FTokStart;
end;

function TSynD2Syn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  result := nil;
end;

function TSynD2Syn.GetTokenKind: integer;
begin
  Result := Integer(fTokKind);
end;

end.
