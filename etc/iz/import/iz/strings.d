/**
 * iz string handling functions, mostly related to lexical scanning
 */
module iz.strings;

import
    std.range, std.traits, std.algorithm.searching;
import
    iz.sugar, iz.types;

version(unittest) import std.stdio;

// Character-related-structs --------------------------------------------------+

/**
 * CharRange is an helper struct that allows to test
 * if a char is within a full range of characters.
 */
struct CharRange
{
    import std.conv: to;
    private immutable dchar _min, _max;
    
    /**
     * Constructs the char range using a string that contains the range bounds.
     *
     * Params:
     *      s = A string. It neither has to be sorted nor to contain the full range.
     */
    this(S)(S s) pure @safe
    if (isSomeString!S)
    {
        import std.algorithm.sorting: sort;
        auto sorted = sort(to!(dchar[])(s));
        _min = sorted[0];
        _max = sorted[$-1];
    }
    
    /**
     * Constructs the char range using the two chars passed as argument.
     *
     * Params:
     *      cmin: The lower character in the range.
     *      cmax: The upper (inclusive) character in the range.
     */
    this(C)(C cmin, C cmax) pure @safe
    if (isSomeChar!C || isImplicitlyConvertible!(C, dchar))
    {
        auto const maybeMin = to!dchar(cmin);
        auto const maybeMax = to!dchar(cmax);
        if (maybeMin <= maybeMax)
        {
            _min = maybeMin;
            _max = maybeMax;
        }
        else
        {
            _min = maybeMax;
            _max = maybeMin;
        }     
    }
    
    /// Returns the lower char in the range.
    dchar min() pure nothrow @safe @nogc
    {return _min;}
    
    /// Returns the upper char in the range.
    dchar max() pure nothrow @safe @nogc
    {return _max;}

    /**
     * Returns true if a character is within the range.
     *
     * Params:
     *      c = A character or any value convertible to a dchar.
     */
    bool opBinaryRight(string op = "in", C)(C c) const pure nothrow @safe @nogc
    if (op == "in")
    {
        static if (isSomeChar!C || isImplicitlyConvertible!(C, dchar))
        {
            return ((c >= _min) & (c <= _max)); 
        }
        else static assert(0, "invalid argument type for CharRange.opIn_r(): " ~ C.stringof);
    }
    
    /**
     * Returns the range representation, as a string.
     * This function fails if the range is not within the 0x0 .. 0x80 range.
     */
    string toString() const pure @safe
    {
        auto r = iota(_min, _max+1);
        string result;
        while (!r.empty)
        {
            result ~= to!char(r.front);
            r.popFront;
        }
        return result;
    }
}
///
pure @safe unittest
{
    auto cs1 = CharRange("ajslkdfjlz");
    assert(cs1.min == 'a');
    assert(cs1.max == 'z');
    assert('b' in cs1);
    
    auto cs2 = CharRange('f', 'a');
    assert(cs2.min == 'a');
    assert(cs2.max == 'f');
    assert('b' in cs2);
    assert('g' !in cs2);
    assert(cs2.toString == "abcdef", cs2.toString);
    
    auto cs3 = CharRange(65, 70);
    assert(cs3.min == 65);
    assert(cs3.max == 70);
    assert(66 in cs3);
    assert(71 !in cs3);
}

/// a CharSwitch that verify characters for decimal numbers.
static immutable CharSwitch!("[0..9]") decimalChars;
/// a CharSwitch that verify characters for octal numbers.
static immutable CharSwitch!("[0..7]") octalChars;

/**
 * CharMap is an helper struct that allows to test
 * if a char is within a set of characters.
 */
struct CharMap
{
    private bool[] _map;
    private dchar _min, _max;
    
    private void setMinMax(dchar value) pure nothrow @safe
    {
        if (value <= _min) _min = value;
        else if (value >= _max) _max = value;
        _map.length = _max + 1 - _min; 
    }

    /**
     * Used in the construction process.
     *
     * Params:
     *      lo = The dchar that defines the range lower bound.
     *      hi = The dchar that defines the range upper bound (inclusive).
     *
     * Examples:
     * ---
     * CharMap cm = CharMap['0'..'9'];
     * ---
     */
    static CharRange opSlice(int index)(dchar lo, dchar hi) pure nothrow @safe @nogc
    {
        return CharRange(lo, hi);
    }
    
    /**
     * Used in the construction process.
     *
     * Params:
     *      a = A list made of character slices, of single characters or
     *
     * any other values whose type is implicitly convertible to dchar.
     *
     * Examples:
     * ---
     * CharMap cm = CharMap['0'..'9', '.', 'f', 'd', 38, 39];
     * ---
     */
    static CharMap opIndex(A...)(A a) pure nothrow @safe
    {   
        CharMap result;
        
        // bounds
        foreach(elem; a)
        {
            alias T = typeof(elem);
            static if (isSomeChar!T || isImplicitlyConvertible!(T, dchar))
            {
                result.setMinMax(elem);      
            }
            else static if (is(T == CharRange))
            {
                result.setMinMax(elem._min);
                result.setMinMax(elem._max);    
            }
            else static assert(0, "unsupported opIndex argument type: " ~ T.stringof);
        }
        
        result._map[] = false;   
        foreach(elem; a)
        {    
            alias T = typeof(elem);
            static if (isSomeChar!T || isImplicitlyConvertible!(T, dchar))
                result._map[elem - result._min] = true;   
            else static if (is(T == CharRange))
            {
                foreach(size_t i; elem._min - result._min .. elem._max - result._min + 1)
                    result._map[i] = true;
            }
        }
        return result;
    }
    
    /**
     * Returns true if a character is within the map.
     *
     * Params:
     *      c = A character or any value convertible to a dchar.
     */
    bool opBinaryRight(string op = "in", C)(C c) const pure nothrow @safe @nogc
    if (op == "in")
    {
        static if (isSomeChar!C || isImplicitlyConvertible!(C, dchar))
        {
            if (_min > c || c > _max) return false;
            else return _map[c - _min]; 
        }
        else static assert(0, "invalid argument type for CharMap.opIn_r(): " ~ C.stringof);
    }
}
///
pure @safe unittest
{
    CharMap cm = CharMap['a'..'f', '0'..'9' , 'A'..'F', '_', 9];
    assert('a' in cm);
    assert('b' in cm);
    assert('c' in cm);
    assert('d' in cm);
    assert('e' in cm);
    assert('f' in cm);
    assert('g' !in cm);
    assert('A' in cm);
    assert('B' in cm);
    assert('C' in cm);
    assert('D' in cm);
    assert('E' in cm);
    assert('F' in cm);
    assert('G' !in cm);
    assert('0' in cm);
    assert('4' in cm);
    assert('9' in cm);
    assert('_' in cm);
    assert('%' !in cm);
    assert('\t' in cm);
}

/// A CharSwitch that includes the hexadecimal characters.
static immutable CharSwitch!("[a..f]", "[A..F]", "[0..9]") hexChars;
/// A CharSwitch that includes the white characters.
static immutable CharSwitch!("[\t..\r]", ' ') whiteChars;


/**
 * The CharSwitch structure allows to define static sets of character.
 *
 * Unlike CharMap it's more adapted to sets made of unicode characters
 * because the characters are tested with a switch table that's generated at
 * compile-time rather than reading, with an indirection, a sparse array of booleans.
 *
 * The structure uniquely implements the $(D in) operator.
 *
 * Params:
 *      A = Variadic parameters made of literal characters, integer numbers or
 *      character ranges literals, such as $(D "[0..9]") or $(D "[a..z]").
 */
struct CharSwitch(A...)
if (A.length)
{

    import std.conv: to;
    import std.range: iota;
    import std.meta: aliasSeqOf;

    private alias case_ = (uint v) => "\tcase " ~ to!string(v) ~ ": return true;\n";

    // CTFE: generates cases for single chars: 'a', 'é', 57, ...
    private static string caseForChar(V)(V value)
    if (isSomeChar!V || isIntegral!V)
    {
        return case_(value);
    }

    // CTFE: generates cases for char ranges: "[a..g]", '[0..9]', ...
    private static string casesForRange(string value) @safe pure
    {
        enum err = "invalid character range format, the format must respect [<lo>..<hi>]";
        assert(!value.empty, err);
        assert(value.front == '[', err);
        value.popFront;

        size_t i = 1;
        dchar min = value.front;
        if (min == '\\')
        {
            while (true)
            {
                if (value.empty || value.front == '.')
                    break;

                value.popFront;
                ++i;
            }
            min = value[1..i].to!dchar;
        }

        assert(!value.empty, err);
        value.popFront;
        assert(!value.empty, err);
        assert(value.front == '.', err);
        value.popFront;
        assert(!value.empty, err);
        assert(value.front == '.', err);
        value.popFront;

        dchar max = value.front;
        if (max == '\\')
        {
            ++++i;
            const lo = i;
            while (true)
            {
                if (value.empty || value.front == '.')
                    break;

                value.popFront;
                ++i;
            }
            max = value[lo..i].to!dchar;
        }

        assert(min < max, "invalid character range: " ~ min.to!string ~ " > " ~ max.to!string);
        assert(!value.empty, err);
        value.popFront;
        assert(value.front == ']', err);
        assert(!value.empty, err);
        value.popFront;
        assert(value.empty, err);

        string result;
        foreach(c; iota(min, max + 1))
        {
            result ~= caseForChar(c);
        }
        return result;
    }

    // CTFE: makes the switch
    private static string makeOpIn(A...)()
    {
        string result = "static bool opIn_r(dchar value) @safe @nogc pure nothrow"
            ~ "\n{\n\tswitch(value)\n\t{\n\tdefault: return false;\n ";

        foreach(i; aliasSeqOf!(iota(0, A.length)))
        {
            alias T = typeof(A[i]);
            static if (isSomeChar!T)
            {
                result ~= caseForChar(A[i]);
            }
            else static if (isIntegral!T)
            {
                static if (dchar.min <= A[i] && A[i] <= dchar.max)
                    result ~= caseForChar(A[i]);
                else
                    static assert(0, "integral value exceeds dchar.max");
            }
            else static if (is(T == string))
            {
                result ~= casesForRange(A[i]);
            }
            else
                static assert(0, "invalid argument type: " ~ T.stringof);
        }

        result ~= "\t}\n};";

        return result;
    }

    mixin(makeOpIn!A);
}
///
@nogc @safe pure unittest
{
    // syntax example for a character range
    alias digits = static immutable CharSwitch!("[0..9]") ;
    assert('0' in digits);
    assert('e' !in digits);

    // many ranges are accepted
    alias hexdgs = static immutable CharSwitch!("[0..9]", "[A..F]", "[a..f]");
    assert('0' in hexdgs);
    assert('e' in hexdgs);
    assert('f' in hexdgs);
    assert('G' !in hexdgs);

    // ranges and chars can be mixed
    alias frenchchars = static immutable CharSwitch!('à','é','è','ç',"[a..z]","[A..Z]",'ù');
    assert('ß' !in frenchchars);
    assert('é' in frenchchars);
}


/**
 * Returns an input range that processes directly a null terminated C string,
 * without fully converting it to a phobos string.
 *
 * Params:
 *      decode = When set to true the front is decoded otherwise (the default)
 *          each code point is supposed to contain 1 unit.
 *      c = A pointer to a character.
 * Returns:
 *      When decoding is enabled, nullTerminated always returns a range of dchar
 *      otherwise the front type is the same as target type of the pointer passed
 *      as parameter.
 */
auto nullTerminated(bool decode = false, C)(C c)
if (isPointer!C && isSomeChar!(PointerTarget!(C)))
{
    // trusting:
    // - read only = no corruption
    // - at the end we always know what happens:
    //      - null pointer encountered if valid string ptr passed.
    //      - null pointer encountered after a while and if invalid ptr passed, but still not corruption.
    //      - otherwise segfault, when the range goes over the process memory.
    struct NullTerminated(C)
    {
        private C _front;
        enum dec = !is(C == dchar*) && decode;
        static if (dec) private size_t _cnt;

        private this(C c) @trusted @nogc
        {
            _front = c;
        }
        ///
        bool empty() @trusted @nogc
        {
            return !_front || *_front == 0;
        }
        ///
        auto front() @trusted
        {
            import std.utf: decodeFront;
            static if (dec)
            {
                auto frt = _front[0 .. 4 / (PointerTarget!C).sizeof];
                return decodeFront(frt, _cnt);
            }
            else return *_front;
        }
        ///
        void popFront() @trusted @nogc
        {
            static if (dec)
                _front += _cnt;
            else
                ++_front;
        }
        ///
        C save() @trusted @nogc
        {
            return _front;
        }
    }
    return NullTerminated!C(c);
}
///
pure @safe unittest
{
    auto text = "ab cd\0";
    auto cString = nullTerminated(&text[0]);
    assert(nextWord(cString) == "ab");
    auto saved = nullTerminated(cString.save);
    assert(nextWord(cString) == "cd");
    assert(nextWord(saved) == "cd");
    assert(cString.empty);
    auto wtext = "ab cd\0"w;
    auto cWideString = nullTerminated(&wtext[0]);
    assert(nextWord(cWideString) == "ab"w);
    assert(nextWord(cWideString) == "cd"w);
    assert(cWideString.empty);
}

pure @safe unittest
{
    auto text = "été\0";
    auto cString = nullTerminated!true(&text[0]);
    assert(cString.front == 'é');
    cString.popFront;
    assert(cString.front == 't');
    cString.popFront;
    assert(cString.front == 'é');
    cString.popFront;
    assert(cString.empty);
}

pure @nogc @safe unittest
{
    char* text;
    assert(text.nullTerminated.empty);
}

pure @safe unittest
{
    auto text = "été\0"w;
    auto cString = nullTerminated!true(&text[0]);
    assert(cString.front == 'é');
    cString.popFront;
    assert(cString.front == 't');
    cString.popFront;
    assert(cString.front == 'é');
    cString.popFront;
    assert(cString.empty);
}

pure @safe unittest
{
    auto text = "été\0"d;
    {
        auto cString = nullTerminated!true(&text[0]);
        assert(cString.front == 'é');
        cString.popFront;
        assert(cString.front == 't');
        cString.popFront;
        assert(cString.front == 'é');
        cString.popFront;
        assert(cString.empty);
    }
    {
        auto cString = nullTerminated!false(&text[0]);
        assert(cString.front == 'é');
        cString.popFront;
        assert(cString.front == 't');
        cString.popFront;
        assert(cString.front == 'é');
        cString.popFront;
        assert(cString.empty);
    }
}


// -----------------------------------------------------------------------------
// Generic Scanning functions -------------------------------------------------+
private template CharType(T)
{
    alias CharType = Unqual!(ElementEncodingType!T);
}

/**
 * Tests wether $(D T) is supported in the several scanning functions.
 *
 * T must either be a CharRange, a CharMap, supports std.algorithm.searching.canFind,
 * be a callable of type $(D bool(dchar)) or $(D bool function(dchar)) or be a
 * single dchar.
 */
template isCharTester(T)
{
    static if (isInputRange!T && isSomeChar!(ElementType!T))
        enum isCharTester = true;
    else static if (is(Unqual!T == CharRange))
        enum isCharTester = true;
    else static if (is(Unqual!T == CharMap))
        enum isCharTester = true;
    else static if (isAssociativeArray!T && isSomeChar!(KeyType!T))
        enum isCharTester = true;
    else static if (isSomeFunction!T && is(ReturnType!T == bool) &&
        Parameters!T.length == 1 && is(Parameters!T[0] == dchar))
        enum isCharTester = true;
    else static if (isSomeChar!T)
        enum isCharTester = true;
    else
    {
        alias B = TemplateOf!(Unqual!T);
        static if (is(B!'a' == CharSwitch!'a'))
            enum isCharTester = true;
        else
            enum isCharTester = false;
    }
}

unittest
{
    alias B = TemplateOf!(Unqual!(typeof(whiteChars)));
    static assert (is(B!'a' == CharSwitch!'a'));
}


/**
 * Returns the next word in the range passed as argument.
 *
 * Params: 
 *      range = A character input range. The range is consumed for each word.
 *      charTester = Defines the valid characters to make a word.
 *
 * Returns:
 *      A string containing the word. If the result length is null then the
 *      range parameter has not been consumed.
 */
auto nextWord(Range, T, bool until = false)(ref Range range, T charTester)
if (isInputRange!Range && isSomeChar!(ElementType!Range) && isCharTester!T)
{
    alias UT = Unqual!T; 
    CharType!Range[] result;
    dchar current = void;

    static if (hasInOperator!(UT, dchar))
    {
        while (true)
        {
            if (range.empty) break;
            current = range.front;
            
            static if (until)
            {
                if (current !in charTester)
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
            else
            {
                if (current in charTester)
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
        }
    }
    else static if (isInputRange!T)
    {
        while (true)
        {
            if (range.empty) break;
            current = range.front;
            
            static if (until)
            {
                if (!canFind(charTester, current))
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
            else
            {
                if (canFind(charTester, current))
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
        }
    }
    else static if (isSomeFunction!UT)
    {
        while (true)
        {
            if (range.empty) break;
            current = range.front;

            static if (until)
            {
                if (!charTester(current))
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
            else
            {
                if (charTester(current))
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
        }
    }
    else static if (isSomeChar!UT)
    {
        while (true)
        {
            if (range.empty) break;
            current = range.front;

            static if (until)
            {
                if (charTester != current)
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
            else
            {
                if (charTester == current)
                {
                    result ~= current;
                    range.popFront;
                }
                else break;
            }
        }
    }
    else static assert(0, "unsupported charTester argument type in nextWord(): " ~ T.stringof);

    return result;
}
///
@safe pure unittest
{
    auto cs1 = "azertyuiopqsdfghjklmwxcvbn";
    auto cs2 = " \r\n\t";
    auto cs3 = CharRange('a','z');
    bool[dchar] cs4 = ['\r':true, '\n': true, '\t':true, ' ':true ];
    auto src1 = "az er
    ty";
    auto src2 = "az er
    ty";

    auto w1 = nextWord(src1, cs1);
    assert(w1 == "az");
    nextWord(src1, cs2);
    auto w2 = nextWord(src1, cs1);
    assert(w2 == "er");
    nextWord(src1, cs2);
    auto w3 = nextWord(src1, cs1);
    assert(w3 == "ty");
    nextWord(src1, cs2);

    auto w11 = nextWord(src2, cs3);
    assert(w11 == "az");
    nextWord(src2, cs4);
    auto w22 = nextWord(src2, cs3);
    assert(w22 == "er");
    nextWord(src2, cs4);
    import std.ascii: isAlpha, isDigit;
    assert(nextWord(src2, &isDigit) == "");
    auto w33 = nextWord(src2, &isAlpha);
    assert(w33 == "ty");
}


/**
 * Returns the next word in the range passed as argument.
 *
 * Params: 
 *      range = A character input range. The range is consumed for each word.
 *      charTester = Defines the opposite of the valid characters to make a word.
 *
 * Returns:
 *      A string containing the word. If the result length is null then the
 *      range parameter has not been consumed.
 */
auto nextWordUntil(Range, T)(ref Range range, T charTester)
{
    return nextWord!(Range, T, true)(range, charTester);
}
///
@safe pure unittest
{
    auto src = "azertyuiop
    sdfghjk".dup;
    auto skp = CharRange("\r\n\t".dup);
    auto w = nextWordUntil(src, skp);
    assert(w == "azertyuiop");
}


/**
 * Skips the next word in the range passed as argument.
 *
 * Params:
 *      range = A character input range. The range is consumed for each word.
 *      charTester = Defines the valid characters to make a word.
 */
void skipWord(Range, T, bool until = false)(ref Range range, T charTester)
if (isInputRange!Range && isSomeChar!(ElementType!Range) && isCharTester!T)
{
    alias UT = Unqual!T;
    static if (hasInOperator!(UT, dchar))
    {
        while (true)
        {
            if (range.empty) break;
            static if (until)
            {
                if (range.front !in charTester)
                    range.popFront;
                else break;
            }
            else
            {
                if (range.front in charTester)
                    range.popFront;
                else break;
            }       
        }
    }
    else static if (isInputRange!T)
    {
        while (true)
        {
            if (range.empty) break;
            static if (until)
            {
                if (!canFind(charTester, range.front))
                    range.popFront;
                else break;            
            }
            else
            {
                if (canFind(charTester, range.front))
                    range.popFront;
                else break;
            }
        }
    }
    else static if (isSomeFunction!UT)
    {
        while (true)
        {
            if (range.empty) break;
            static if (until)
            {
                if (!charTester(range.front))
                    range.popFront;
                else break;            
            }
            else
            {                        
                if (charTester(range.front))
                    range.popFront;
                else break;
            }
        }
    }
    else static if (isSomeChar!UT)
    {
        while (true)
        {
            if (range.empty) break;
            static if (until)
            {
                if (charTester != range.front)
                    range.popFront;
                else break;
            }
            else
            {
                if (charTester == range.front)
                    range.popFront;
                else break;
            }
        }
    }
    else static assert(0, "unsupported charTester argument type in skipWord(): " ~ T.stringof);
}
///
@safe pure unittest
{
    auto src1 = "\t\t\r\ndd";
    auto skp1 = CharRange("\r\n\t");
    skipWord(src1, skp1);
    assert(src1 == "dd");
    import std.ascii: isWhite;
    auto src2 = "\t\t\r\nee";
    skipWord(src2, &isWhite);
    assert(src2 == "ee");
}


/**
 * Skips the next word in the range passed as argument.
 *
 * Params:
 *      range = A character input range. The range is consumed for each word.
 *      charTester = Defines the opposite of the valid characters to make a word.
 */
void skipWordUntil(Range, T)(ref Range range, T charTester)
{
    skipWord!(Range, T, true)(range, charTester);
}
///
@safe pure unittest
{
    auto src = "dd\r";
    auto skp = CharRange("\r\n\t");
    skipWordUntil(src, skp);
    assert(src == "\r");
}


/**
 * Tries to make a fixed length slice by consuming range.
 *
 * Params:
 *      range = A character input range. The range is consumed for each word.
 *      len = An integral value.
 *
 * Returns:
 *      At the tail a string whose length is less or equal to $(I len), otherwise
 *      always a string of length $(I len).
 */
auto nextSlice(Range, T)(ref Range range, T len)
if (isInputRange!Range && isSomeChar!(ElementType!Range) && isIntegral!T)
{
    CharType!Range[] result;
    size_t cnt;
    while (true)
    {
        if (cnt == len || range.empty)
            break;
        result ~= range.front;
        range.popFront;
        ++cnt;
    }   
    
    return result;
}
///
@safe pure unittest
{
    auto text0 = "012"; 
    assert(text0.nextSlice(2) == "01");
    auto text1 = "3";
    assert(text1.nextSlice(8) == "3");
    auto text2 = "45";
    assert(text2.nextSlice(0) == "");
    assert(text1.nextSlice(12_34_56) == "");
    auto ut = "é_é";
    assert(ut.nextSlice(3) == "é_é");
}


/**
 * Returns true if a string starts with a particular sub string.
 *
 * Params:
 *      range: A character input range. The range is not consumed.
 *      stuff: the sub string, also works with single chars.
 */
bool canRead(Range, Stuff)(ref Range range, Stuff stuff)
if (isInputRange!Range && isSomeChar!(ElementType!Range)
    && (isSomeChar!Stuff || isSomeString!Stuff))
{
    static if (isSomeString!Range)
    {
        if (range.empty)
            return false;
        else
        {
            static if (isSomeChar!Stuff)
                return range.front == stuff;
            else
            {
                import std.conv: to;
                auto dstuff = to!dstring(stuff);
                auto reader = ArrayRange!(ElementEncodingType!Range)(range);
                auto slice = reader.nextSlice(dstuff.walkLength);
                return dstuff == slice;
            }
        }
    } 
    else
    {
        import std.algorithm.searching: startsWith;
        return startsWith(range, stuff);
    } 
}
///
pure unittest
{
    auto text0 = "{0}".dup;
    assert(text0.canRead('{'));
    auto text1 = "(* bla *)".dup;
    assert(text1.canRead("(*"));
    assert(text1 == "(* bla *)");
    string text2 = "0x123456";
    assert(!text2.canRead("0b"));
}
//------------------------------------------------------------------------------
// Text scanning utilities ----------------------------------------------------+

/**
 * Returns an input range consisting of the input argument sliced by group of 
 * length len.
 */
auto bySlice(Range)(auto ref Range range, size_t len)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
{
    struct BySlice
    {
        private bool _emptyLine;
        private CharType!Range[] _front;
        ///
        void popFront()
        {
            _front = nextSlice(range, len);
        }
        ///
        auto front()
        {
            return _front;
        }
        ///
        bool empty()
        {
            return _front.length == 0;
        }
    }
    BySlice bs;
    if (!range.empty)
        bs.popFront;
    return bs;
}
///
@safe pure unittest
{
    auto text = "AABBCCDD";
    assert(text.bySlice(2).array == ["AA","BB","CC","DD"]);
    auto str = "AAE";
    assert(str.bySlice(2).array == ["AA","E"]);
}


/**
 * Tries to read immediatly an EOL in range and returns it.
 */
auto readEol(Range)(ref Range range)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
{
    CharType!Range[] result;
    if (range.canRead("\r\n")) result = range.nextSlice(2);
    else if (range.canRead('\n')) result = range.nextSlice(1);
    return result;
}
///
pure unittest
{
    auto text0 = "";
    assert(readEol(text0) == "");
    auto text1 = " ";
    assert(readEol(text1) == "");
    auto text2 = "\n";
    assert(readEol(text2) == "\n");
    auto text3 = "\r\n";
    assert(readEol(text3) == "\r\n");
}


/**
 * Tries to skip immediatly an EOL in range.
 */
void skipEol(Range)(ref Range range)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
{
    if (range.canRead("\r\n")) range.nextSlice(2);
    else if (range.canRead('\n')) range.nextSlice(1);        
}
///
pure unittest
{
    auto text0 = "";
    skipEol(text0);
    assert(text0 == "");
    auto text1 = " ";
    skipEol(text1);
    assert(text1 == " ");
    auto text2 = "\n";
    skipEol(text2);
    assert(text2 == "");
    auto text3 = "\r\na";
    skipEol(text3);
    assert(text3 == "a");
}


/**
 * Returns the next line within range.
 */
auto nextLine(bool keepTerminator = false, Range)(ref Range range)
{
    auto result = nextWordUntil(range, "\r\n");
    static if (keepTerminator) result ~= range.readEol;
    else range.skipEol;
    return result;
}
///
pure unittest
{
    auto text = "123456\r\n12345\n1234\r\n123\r\n12\r\n1";
    assert(nextLine!false(text) == "123456");
    assert(nextLine!false(text) == "12345");
    assert(nextLine!false(text) == "1234");
    assert(nextLine!false(text) == "123");
    assert(nextLine!false(text) == "12");
    assert(nextLine!false(text) == "1");
    assert(nextLine!false(text) == "");
    assert(nextLine!false(text) == "");
}


/**
 * Returns an input range consisting of each line in the input argument
 */
auto byLine(Range)(auto ref Range range)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
{ 
    struct ByLine
    {
        private bool _emptyLine;
        private CharType!Range[] _front, _strippedfront;
        ///
        void popFront()
        {
            _front = nextLine!true(range);
            import std.string: stripRight;
            _strippedfront = stripRight(_front);
        }
        ///
        auto front()
        {
            return _strippedfront;
        }
        ///
        bool empty()
        {
            return _front.length == 0;
        }
    }
    ByLine bl;
    if (!range.empty)
        bl.popFront;
    return bl;
}
///
pure unittest
{
    auto text = "aw\r\nyess";
    auto range = text.byLine;
    assert(range.front == "aw");
    range.popFront;
    assert(range.front == "yess");
    auto nums = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9";
    import std.algorithm.iteration: reduce;
    assert(nums.byLine.reduce!((a,b) => a ~ b) == "0123456789");
}


/**
 * Returns the lines count within the input range.
 * The input range is not consumed.
 */
size_t lineCount(Range)(Range range)
{
    return range.byLine.array.length;
}
///
pure unittest
{
    auto text1= "";
    assert(text1.lineCount == 0);
    auto text2 = "\n\r\n";
    assert(text2.lineCount == 2);
    auto text3 = "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n\n\n";
    assert(text3.lineCount == 12);
}


/**
 * Returns the next word within range. 
 * Words are spliited using the White characters, which are never included.
 */
auto nextWord(Range)(ref Range range) pure @safe
{
    skipWord(range, whiteChars);
    return nextWordUntil(range, whiteChars);
}
///
@safe pure unittest
{
    auto text = " lorem ipsum 123456";
    assert(text.nextWord == "lorem");
    assert(text.nextWord == "ipsum");
    assert(text.nextWord == "123456");
}


/**
 * Returns an input range consisting of each non-blank word in the input argument.
 */
auto byWord(Range)(auto ref Range range)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
{ 
    struct ByWord
    {
        private CharType!Range[] _front;
        ///
        void popFront()
        {
            _front = nextWord(range);
        }
        ///
        auto front()
        {
            return _front;
        }
        ///
        bool empty()
        {
            return _front.length == 0;
        }
    }
    ByWord bw;
    if (!range.empty)
        bw.popFront;
    return bw;
}
///
@safe pure unittest
{
    auto text = "aw yess, this is so cool";
    auto range = text.byWord;
    assert(range.front == "aw");
    range.popFront;
    assert(range.front == "yess,");
    range.popFront;
    assert(range.front == "this");
    auto nums = "0 1 2 3 4 5 6 7 8 9";
    import std.algorithm.iteration: reduce;
    assert(nums.byWord.reduce!((a,b) => a ~ b) == "0123456789");
}


/**
 * Returns the word count within the input range.
 * Words are separatedd by ascii whites. input range is not consumed.
 */
size_t wordCount(Range)(Range range)
{
    return range.byWord.array.length;
}
///
@safe pure unittest
{
    auto text = "1 2 3 4 5 6 7 8 9 \n 10";
    assert(text.wordCount == 10);
    assert(text == "1 2 3 4 5 6 7 8 9 \n 10");
}


/**
 * Returns the next separated word.
 * Separators are always removed, white characters optionally.
 */
auto nextSeparated(Range, Separators, bool strip = true)(auto ref Range range, Separators sep)
{
    auto result = nextWordUntil(range, sep);
    if (!range.empty) range.popFront;
    static if (strip)
    {
        skipWord(result, whiteChars);
        result = nextWordUntil(result, whiteChars);
    }
    return result;
}
///
@safe pure unittest
{
    auto seps = CharMap[',', '\n'];
    auto text = "name, âge \n Douglas, 27 \n Sophia 26";
    assert(text.nextSeparated(seps) == "name");
    assert(text.nextSeparated(seps) == "âge");
    assert(text.nextSeparated(seps) == "Douglas");
    assert(text.nextSeparated(seps) == "27");
}


/**
 * Returns an input range consisting of each separated word in the input argument
 */
auto bySeparated(Range, Separators, bool strip = true)(auto ref Range range, Separators sep)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
{
    struct BySep
    {
        private CharType!Range[] _front;
        ///
        void popFront()
        {
            _front = nextSeparated!(Range, Separators, strip)(range, sep);
        }
        ///
        auto front()
        {
            return _front;
        }
        ///
        bool empty()
        {
            return _front.length == 0;
        }
    }
    BySep bs;
    if (!range.empty)
        bs.popFront;
    return bs;
}
///
@safe pure unittest
{
    auto text = "name = Douglas \n age =27 \n";
    auto range = text.bySeparated(CharMap['=', '\n']);
    assert(range.front == "name");
    range.popFront;
    assert(range.front == "Douglas");
    range.popFront;
    assert(range.front == "age");
    range.popFront;
    assert(range.front == "27");
    range.popFront;
    assert(range.empty);
}

/**
 * Immediatly reads a decimal number.
 */
auto readDecNumber(Range)(auto ref Range range)
{
    return range.nextWord(decimalChars);
}
///
@safe pure unittest
{
    auto text = "0123456 789";
    assert(text.readDecNumber == "0123456");
    text.popFront;
    assert(text.readDecNumber == "789");
    
    string t = "456";
    if (auto num = readDecNumber(t))
        assert (num == "456");
}


/**
 * Immediatly reads an hexadecimal number.
 */
auto readHexNumber(Range)(auto ref Range range)
{
    return range.nextWord(hexChars);
}
///
@safe pure unittest
{
    auto text1 = "1a2B3C o";
    assert(text1.readHexNumber == "1a2B3C");
    assert(text1 == " o");
    auto text2 = "A897F2f2Ff2fF3c6C9c9Cc9cC9c123 o";
    assert(text2.readHexNumber == "A897F2f2Ff2fF3c6C9c9Cc9cC9c123");
    assert(text2 == " o");
}


/**
 * Strips leading white characters.
 */
void stripLeftWhites(Range)(auto ref Range range)
{
    range.skipWord(whiteChars);
}
///
pure unittest
{
    auto text = "  \n\r\v bla".dup;
    auto rng = ArrayRange!char(text);
    rng.stripLeftWhites;
    assert(rng.array == "bla");
}


/**
 * Escapes characters in the input text.
 *
 * Params:
 *      range = The character range to process. The source is not consumed.
 *      pairs = An array of pair. Each pair (char[2]) defines a source and a
 *      target character. The slash is automatically escaped and must not be
 *      included in the array.
 * Returns:
 *      An array of character whose type matches the range element type.
 */
auto escape(Range)(Range range, const char[2][] pairs)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
in
{
    foreach(pair; pairs)
    {
        assert(pair[0] != '\\', "the backslash should not be set as pair");
        assert(pair[1] != '\\', "the backslash should not be set as pair");
    }
}
body
{
    CharType!Range[] result;
    dchar front;
    bool done = void, wasSlash = void;
    while (!range.empty)
    {
        wasSlash = front == '\\';
        front = range.front;
        done = false;
        foreach(pair; pairs) if (front == pair[0] && !wasSlash)
        {
            done = true;
            result ~= `\` ~ pair[1];
            range.popFront;
            break;
        }
        if (front == '\\')
            result ~= front;
        if (!done)
        {
            result ~= front;
            range.popFront;
        }
    }
    return result;
}
///
@safe pure unittest
{
    assert(`1"`.escape([['"','"']]) == `1\"`);
    assert(`1"1"11"1`.escape([['"','"']]) == `1\"1\"11\"1`);
    assert("\n\"1".escape([['"','"'],['\n','n']]) == `\n\"1`);
    assert(`1\"`.escape([['"','"']]) == `1\\"`);
    assert(`\`.escape([]) == `\\`);
}


/**
 * Un-escapes characters in the input text.
 *
 * Params:
 *      range = The character range to process. The source is not consumed.
 *      pairs = An array of pair. Each pair (char[2]) defines a target and a
 *      source character. The slash is automatically unescaped and must not be
 *      included in the array.
 * Returns:
 *      An array of character whose type matches the range element type.
 *      Even if invalid, any unterminated sequence located at the end of the
 *      range is appended to the result.
 */
auto unEscape(Range)(Range range, const char[2][] pairs)
if (isInputRange!Range && isSomeChar!(ElementType!Range))
in
{
    foreach(pair; pairs)
    {
        assert(pair[0] != '\\', "the backslash should not be set as pair");
        assert(pair[1] != '\\', "the backslash should not be set as pair");
    }
}
body
{
    CharType!Range[] result;
    dchar front = void;
    bool slash;
    while(!range.empty)
    {
        front = range.front;
        if (slash && front == '\\')
        {
            result ~= '\\';
            slash = false;
            range.popFront;
            continue;
        }
        if (front == '\\')
        {
            slash = true;
            range.popFront;
            if (range.empty)
                result ~= '\\';
            continue;
        }
        if (slash)
        {
            foreach(pair; pairs) if (front == pair[1])
            {
                result ~= pair[0];
                slash = false;
                break;
            }
            if (slash) result ~= '\\';
            slash = false;
        }
        else result ~= front;
        range.popFront;
    }
    return result;
}
///
@safe pure unittest
{
    assert( `1\"`.unEscape([['"','"']]) == `1"`);
    assert(`1\"1\"11\"1`.unEscape([['"','"']]) == `1"1"11"1`);
    assert(`\n\"1`.unEscape([['"','"'],['\n','n']]) == "\n\"1");
    assert(`\\\\`.unEscape([]) == `\\`);
    assert(`\\`.unEscape([]) == `\`);
    assert(`\`.unEscape([]) == `\`);
}

//------------------------------------------------------------------------------
// Misc string-related stuff --------------------------------------------------+

/**
 * The suffix array is a data structure that can be used to test quickly
 * the presence of a value in a list. It's also adapted to get completion
 * proposals for a prefix.
 *
 * The performance gain over a canFind() is excellent (98%). The gain over the
 * built-in associative array is slight (3%) and the gain over EMSI hash map is
 * good (40%). Despite of its speed, it always wastes a lot of memory because
 * each byte in an entry is represented by an array of 256 pointers.
 * For example count 800 MB for /usr/share/dict/words (300K words) on X86_64.
 *
 * This implementation only works with arrays of character made of single byte
 * units (char[], string, const(char)[], etc) but is compatible with multi byte
 * characters.
 */
struct SuffixArray(T)
if ((ElementEncodingType!T).sizeof == 1)
{

private:

    import iz.memory: construct, destruct;
    import std.experimental.allocator: make, dispose;

    static if (isSomeString!T)
        alias TT = ElementEncodingType!T;
    else
        alias TT = typeof(T.init[0]);

    alias Nodes = Node*[256];

    Node _root;

public:

    /// A node in the array.
    struct Node
    {

    private:

        bool _terminates;
        immutable ubyte _index;
        Nodes _nodes;

        /// To be private (emplace)
        public this(const ubyte index) pure nothrow @safe @nogc
        {
            _index = index;
        }

    public:

        /**
         * Adds a suffix to the prefix represented by this node.
         */
        void addSuffix(const T suffix) nothrow @safe @nogc
        {
            //TODO-csuffixarray: value returned by find() doesnt allow addSuffix without casting const away.
            if (!suffix.length)
                _terminates = true;
            else
            {
                ubyte newIndex = suffix[0];
                if (!_nodes[newIndex])
                    _nodes[newIndex] = construct!Node(newIndex);
                _nodes[newIndex].addSuffix(suffix[1..$]);
            }
        }

        /**
         * Returns true if this node represents a full entry.
         */
        bool terminates() const pure nothrow @safe @nogc
        {
            return _terminates;
        }

        /**
         * Finds a full suffix from this node.
         */
        const(Node)* opBinaryRight(string op : "in")(const T suffix) const pure nothrow @safe @nogc
        {
            if (suffix.length == 0)
            {
                if (_terminates)
                    return &this;
                else
                    return null;

            }
            else if (_nodes[suffix[0]] == null)
                return null;
            else
                return suffix[1..$] in *(_nodes[suffix[0]]);
        }

        /// Aliases to the $(D in) operator.
        alias find = opBinaryRight!"in";

        /**
         * Finds a prefix from this node.
         */
        const(Node)* findPrefix(const T value) const pure nothrow @safe @nogc
        {
            if (!value.length)
                return null;

            const(Node)* result = &this;
            foreach (i; 0 .. value.length)
            {
                result = result._nodes[value[i]];
                if (!result)
                    break;
            }
            return result;
        }

        //TODO-csuffixarray: conditionalVisit() that allows to break

        /// see SuffixArray.visitAll.
        void visit(alias fun, bool descending = false, bool childrenFirst = true, A...)
            (ref ubyte[] path, auto ref A a) const nothrow @safe
        if (isValidVisitor!fun)
        {
            path ~= _index;
            scope (exit) path.length -= 1;

            static if (!childrenFirst)
                fun(&this, path, a);

            static if (descending)
            {
                foreach_reverse (ubyte i; 0..256)
                    if (_nodes[i])
                        _nodes[i].visit!(fun, descending, childrenFirst)(path, a);
            }
            else
            {
                foreach (ubyte i; 0..256)
                    if (_nodes[i])
                        _nodes[i].visit!(fun, descending, childrenFirst)(path, a);
            }

            static if (childrenFirst)
                fun(&this, path, a);
        }

        /**
         * Returns the terminated entries that begin from this node.
         * In the results, an empty value means that this node terminates a word.
         */
        T[] entries() const nothrow @safe
        {
            nothrow @trusted
            static void fun(const(Node)* node, ref const ubyte[] path, ref T[] results)
            {
                if (node._terminates)
                    results ~= (cast(T) path)[1..$];
            }

            T[] results;
            ubyte[] path;
            visit!(fun, false, true)(path, results);
            return results;
        }
    }

    @disable this();
    @disable this(this);

    /**
     * Constructs the array from a range of elements.
     */
    this(E)(E entries)
    if (isInputRange!E && isImplicitlyConvertible!(ElementType!E,T))
    {
        clear;
        static if (isArray!E)
        {
            foreach (ref entry; entries)
            {
                if (!entry.length)
                    continue;
                _root.addSuffix(entry);
            }
        }
        else foreach (entry; entries)
        {
            if (!entry.length)
                continue;
            _root.addSuffix(entry);
        }
    }

    ~this()
    {
        clear;
    }

    /**
     * Empties the array.
     */
    void clear() @safe @nogc
    {
        void clearNode(ref Node* node) @trusted @nogc
        {
            if (!node)
                return;
            foreach (ubyte i; 0..256)
                clearNode(node._nodes[i]);
            destruct(node);
            node = null;
        }
        foreach (ubyte i; 0..256)
            clearNode(_root._nodes[i]);
    }

    /**
     * Determines wether a full value is in the array.
     *
     * Params:
     *      value = The value to search for.
     * Returns:
     *      Null if value is not in the array otherwise a pointer to
     *      the node that terminates the path to value.
     */
    const(Node)* opBinaryRight(string op : "in")(const T value) const pure nothrow @safe @nogc
    {
        if (!value.length)
            return null;
        else
            return value in _root;
    }

    /// ditto
    alias find = opBinaryRight!"in";

    /**
     * Determines wether an entry starts with value.
     *
     * Params:
     *      value = The prefix to search for.
     * Returns:
     *      Null if value is not in the array otherwise a pointer to
     *      the node that gives the entries starting with value.
     */
    const(Node)* findPrefix(const T value) const pure nothrow @safe @nogc
    {
        if (!value.length)
            return null;
        else
            return _root.findPrefix(value);
    }

    /**
     * Prototype for the function passed in visitAll and Node.visit.
     *
     * Params:
     *      node = The node that's visited.
     *      path = The path that leads to the node. It also represents the value.
     *      a = The variadic parameters, i.e the callback "user parameters".
     */
    alias Fun(A...) = void function(const(Node)* node, ref const ubyte[] path, A a);

    /// Indicates wether a function is suitable for visitAll() or Node.visit()
    template isValidVisitor(alias fun)
    {
        static if (!is(fun))
            alias F = typeof(fun);
        else
            alias F = fun;

        enum isValidVisitor =
            (isCallable!F) &&
            (Parameters!F).length >= 2 &&
            is(Parameters!F[0] == Parameters!(Fun!())[0]) &&
            is(Parameters!F[1] == Parameters!(Fun!())[1]) &&
            ParameterStorageClassTuple!F[1] == ParameterStorageClass.ref_;
    }

    /**
     * Visits all the nodes with a function.
     *
     * Params:
     *      fun = See the Fun prototype.
     *      descending = Indicates if the visit starts from the end.
     *      childrenFirst = Indicates if the children are visited before their parent.
     *      a = The variadic parameters passed to fun.
     */
    void visitAll(alias fun, bool descending = false,
        bool childrenFirst = true, A...)(auto ref A a) const nothrow @safe
    if (isValidVisitor!fun)
    {
        ubyte[] path;
        _root.visit!(fun, descending, childrenFirst)(path, a);
    }

    /**
     * Sorts the entries.
     *
     * While sorting can be easily done with suffix trees this is much slower
     * than a classic quick sort. Also note that sorting is not unicode aware
     * which means that unless the entries are made of ASCII chars, the results
     * will be different from std.algorithm sort().
     *
     * Params:
     *      descending = Defines the sorting direction.
     * Returns:
     *      An array of entries.
     */
    T[] sort(bool descending = false) nothrow @safe
    {
        nothrow @trusted
        static void fun(const(Node)* node, ref const ubyte[] path, ref T[] results)
        {
            if (node.terminates)
                results ~= (cast(T) path)[1..$];
        }

        T[] results;
        if (descending)
            visitAll!(fun, true, true)(results);
        else
            visitAll!(fun, false, true)(results);
        return results;
    }

    /**
     * Indicates the amount of memory used by the array.
     */
    size_t memoryUsage() const nothrow @safe
    {
        nothrow @safe
        static void fun(const(Node)* node, const ref ubyte[] path, ref size_t result)
        {
            result += Node.sizeof;
        }

        size_t result;
        visitAll!fun(result);
        return result;
    }
}
///
unittest
{
    string[] source = ["Cairo", "Calcutta", "Calgary", "Cali", "Campinas",
        "Cape Town", "Caracas", "Casablanca", "Changchun", "Changde"];

    auto cities = SuffixArray!string(source);

    // test for presence
    assert("Cairo" in cities);
    assert("Calcutta" in cities);
    assert("Calgary" in cities);
    assert("Chicago" !in cities);
    assert("" !in cities);

    // get completion list
    auto pre = cities.findPrefix("Cal");
    assert(pre);
    assert(!pre.terminates);
    assert(pre.entries == ["Calcutta"[3..$], "Calgary"[3..$], "Cali"[3..$]]);

    // sorting
    auto desc = cities.sort(true);
    import std.algorithm.sorting;
    assert(desc == sort!"a > b"(source).array);
    auto asc = cities.sort(false);
    assert(asc == sort!"a < b"(source).array);

    // adds from a prefix
    auto ch = cast(cities.Node*) cities.findPrefix("Ch");
    assert(ch);
    ch.addSuffix("icago");
    assert("Chicago" in cities);

    // memory usage
    size_t count;
    foreach(s; source) foreach(i; 0..s.length)
        ++count;
    // actual usage is more close to count * 256 * size_t.sizeof
    assert(cities.memoryUsage >= count);

    // clearing is global
    cities.clear;
    assert("Cairo" !in cities);
    assert("Calcutta" !in cities);
}

unittest
{
    ubyte[][] source =
    [
        [0x11,0x22,0x33],
        [0x11,0x33],
        [0x1,0x2,0x3]
    ];
    SuffixArray!(ubyte[]) sar = SuffixArray!(ubyte[])(source);

    assert(sar.find(source[0]));
    assert(sar.find(source[1]));
    assert(sar.find(source[2]));

    ubyte[] notin = [0x11,0x22,0x34];
    assert(!sar.find(notin));
    assert(sar.findPrefix([0x76]) is null);
    assert(sar.findPrefix([]) is null);
    assert(sar.findPrefix([0x11]).findPrefix([0x22]).findPrefix([]) is null);
    assert(sar.findPrefix([0x11]).find([0x22,0x99]) is null);
    assert(sar.findPrefix([0x11]).find([]) is null);
}

//TODO-csuffixarray: choose the right version
struct SuffixArrayTemp(T)
if ((ElementEncodingType!T).sizeof == 1)
{

private:

    import iz.memory: construct, destruct, getMem, freeMem;

    static if (isSomeString!T)
        alias TT = ElementEncodingType!T;
    else
        alias TT = typeof(T.init[0]);

    alias Nodes = Node*[256];

    Node _root;


public:

    struct Node
    {

    private:

        bool _terminates;
        immutable ubyte _index;
        Nodes* _nodes;

        public this(const ubyte index) pure nothrow @safe @nogc
        {
            _index = index;
        }

    public:

        ~this() @nogc
        {
            if (_nodes)
                freeMem(cast(void*)_nodes);
            _nodes = null;
        }


        void addSuffix(const T suffix) nothrow @trusted @nogc
        {
            if (!suffix.length)
                _terminates = true;
            else
            {
                ubyte newIndex = suffix[0];
                if (!_nodes)
                {
                    _nodes = cast(Nodes*) getMem(256 * size_t.sizeof);
                    (*_nodes)[] = null;
                }
                if (!(*_nodes)[newIndex])
                    (*_nodes)[newIndex] = construct!Node(newIndex);
                (*_nodes)[newIndex].addSuffix(suffix[1..$]);
            }
        }


        bool terminates() const pure nothrow @safe @nogc
        {
            return _terminates;
        }


        const(Node)* opBinaryRight(string op = "in")(const T suffix) const pure nothrow @trusted @nogc
        if (op == "in")
        {
            if (suffix.length == 0)
            {
                if (_terminates)
                    return &this;
                else
                    return null;

            }
            else if (!_nodes)
                return null;
            else if ((*_nodes)[suffix[0]] == null)
                return null;
            else
                return (*_nodes)[suffix[0]].find(suffix[1..$]);
        }

        alias find = opBinaryRight!"in";

        const(Node)* findPrefix(const T value) const pure nothrow @trusted @nogc
        {
            if (!value.length)
                return null;

            const(Node)* result = &this;
            if (result._nodes) foreach (i; 0 .. value.length)
            {
                result = (*result._nodes)[value[i]];
                if (!result)
                    break;
            }
            return result;
        }

        void visit(alias fun, bool descending = false, bool childrenFirst = true, A...)
            (ref ubyte[] path, auto ref A a) const nothrow @trusted
        if (isValidVisitor!fun)
        {
            path ~= _index;
            scope (exit) path.length -= 1;

            static if (!childrenFirst)
                fun(&this, path, a);

            static if (descending)
            {
                if (_nodes) foreach_reverse (ubyte i; 0..256)
                    if ((*_nodes)[i])
                        (*_nodes)[i].visit!(fun, descending, childrenFirst)(path, a);
            }
            else
            {
                if (_nodes) foreach (ubyte i; 0..256)
                    if ((*_nodes)[i])
                        (*_nodes)[i].visit!(fun, descending, childrenFirst)(path, a);
            }

            static if (childrenFirst)
                fun(&this, path, a);
        }

        T[] entries() const nothrow @safe
        {
            nothrow @trusted
            static void fun(const(Node)* node, ref const ubyte[] path, ref T[] results)
            {
                if (node._terminates)
                    results ~= (cast(T) path)[1..$];
            }

            T[] results;
            ubyte[] path;
            visit!(fun, false, true)(path, results);
            return results;
        }
    }

    this(E)(E entries) nothrow @safe @nogc
    if (isInputRange!E && isImplicitlyConvertible!(ElementType!E,T))
    {
        clear;
        foreach (entry; entries)
        {
            if (!entry.length)
                continue;
            _root.addSuffix(entry);
        }
    }

    ~this()
    {
        clear;
    }

    void clear() @safe @nogc
    {
        void clearNode(ref Node* node) @trusted @nogc
        {
            if (!node)
                return;
            if (node._nodes) foreach (ubyte i; 0..256)
                clearNode((*node._nodes)[i]);
            destruct(node);
            node = null;
        }
        if (_root._nodes)
        {
            foreach (ubyte i; 0..256)
                clearNode((*_root._nodes)[i]);
            freeMem(cast(void*) _root._nodes);
            _root._nodes = null;
        }
    }

    const(Node)* opBinaryRight(string op = "in")(const T value) const pure nothrow @safe @nogc
    if (op == "in")
    {
        if (!value.length)
            return null;
        else
            return _root.find(value);
    }

    alias find = opBinaryRight!"in";

    const(Node)* findPrefix(const T value) const pure nothrow @safe @nogc
    {
        if (!value.length)
            return null;
        else
            return _root.findPrefix(value);
    }

    alias Fun(A...) = void function(const(Node)* node, ref const ubyte[] path, A a);

    template isValidVisitor(alias fun)
    {
        static if (!is(fun))
            alias F = typeof(fun);
        else
            alias F = fun;

        enum isValidVisitor =
            (isCallable!F) &&
            (Parameters!F).length >= 2 &&
            is(Parameters!F[0] == const(Node)*) &&
            is(Parameters!F[1] == const ubyte[]) &&
            ParameterStorageClassTuple!F[1] == ParameterStorageClass.ref_;
    }


    void visitAll(alias fun, bool descending = false,
        bool childrenFirst = true, A...)(auto ref A a) const nothrow @safe
    if (isValidVisitor!fun)
    {
        ubyte[] path;
        _root.visit!(fun, descending, childrenFirst)(path, a);
    }


    T[] sort(bool descending = false) nothrow @safe
    {
        nothrow @trusted
        static void fun(const(Node)* node, ref const ubyte[] path, ref T[] results)
        {
            if (node.terminates)
                results ~= (cast(T) path)[1..$];
        }

        T[] results;
        if (descending)
            visitAll!(fun, true, true)(results);
        else
            visitAll!(fun, false, true)(results);
        return results;
    }

    size_t memoryUsage() const nothrow @safe
    {
        nothrow @safe
        static void fun(const(Node)* node, const ref ubyte[] path, ref size_t result)
        {
            result += Node.sizeof;
        }

        size_t result;
        visitAll!fun(result);
        return result;
    }
}

unittest
{
    string[] source = ["Cairo", "Calcutta", "Calgary", "Cali", "Campinas",
        "Cape Town", "Caracas", "Casablanca", "Changchun", "Changde"];

    auto cities = SuffixArrayTemp!string(source);

    // test for presence
    assert("Cairo" in cities);
    assert("Calcutta" in cities);
    assert("Calgary" in cities);
    assert("Chicago" !in cities);
    assert("" !in cities);

    // get completion list
    auto pre = cities.findPrefix("Cal");
    assert(pre);
    assert(!pre.terminates);
    assert(pre.entries == ["Calcutta"[3..$], "Calgary"[3..$], "Cali"[3..$]]);

    // sorting
    auto desc = cities.sort(true);
    import std.algorithm.sorting;
    assert(desc == sort!"a > b"(source).array);
    auto asc = cities.sort(false);
    assert(asc == sort!"a < b"(source).array);

    // adds from a prefix
    auto ch = cast(cities.Node*) cities.findPrefix("Ch");
    assert(ch);
    ch.addSuffix("icago");
    assert("Chicago" in cities);

    // memory usage
    size_t count;
    foreach(s; source) foreach(i; 0..s.length)
        ++count;
    // actual usage is actually more close to count * 256 * size_t.sizeof
    assert(cities.memoryUsage >= count);

    // clearing is global
    cities.clear;
    assert("Cairo" !in cities);
    assert("Calcutta" !in cities);
}


//------------------------------------------------------------------------------

