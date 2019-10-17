module common;

import
    std.array, std.traits, std.meta, std.conv;
import
    dparse.lexer, dparse.ast, dparse.parser, dparse.rollback_allocator;
import
    iz.memory;

enum ErrorType: ubyte
{
    warning,
    error
}

/// Stores a dparse AST error
@NoInit @NoGc struct AstError
{
    ///
    ErrorType type;
    ///
    @NoGc string message;
    ///
    size_t line, column;

    @disable this();

    ///
    this(ErrorType type, string message, size_t line, size_t column) @nogc @safe
    {
        this.type = type;
        this.message = message;
        this.line = line;
        this.column = column;
    }
}

alias AstErrors = AstError*[];

@nogc @safe unittest
{
    AstError* err = construct!(AstError)(ErrorType.warning, "warning", 0, 0);
    assert(err);
    assert(err.type == ErrorType.warning);
    assert(err.message == "warning");
    assert(err.column == 0);
    assert(err.line == 0);
    destruct(err);
}

/// Write function call when compiled with version "devel"
enum logCall =
q{
    import std.experimental.logger: log;
    version(devel) log();
    else version(unittest) log();
};


/**
 * Contains all the D version identifiers that are not valid
 * for this operating system.
 */
ref const(bool[string]) badVersions()
{
    if (!_badVersions.length)
        fillBadVersions;
    return _badVersions;
}

private __gshared bool[string] _badVersions;

private static immutable predefinedVersions = [
    "AArch64",
    "AIX",
    "all",
    "Alpha",
    "Alpha_HardFloat",
    "Alpha_SoftFloat",
    "Android",
    "ARM",
    "ARM_HardFloat",
    "ARM_SoftFloat",
    "ARM_SoftFP",
    "ARM_Thumb",
    "assert",
    "BigEndian",
    "BSD",
    "CRuntime_Bionic",
    "CRuntime_DigitalMars",
    "CRuntime_Glibc",
    "CRuntime_Microsoft",
    "Cygwin",
    "DigitalMars",
    "DragonFlyBSD",
    "D_Coverage",
    "D_Ddoc",
    "D_HardFloat",
    "D_InlineAsm_X86",
    "D_InlineAsm_X86_64",
    "D_LP64",
    "D_NoBoundsChecks",
    "D_PIC",
    "D_SIMD",
    "D_SoftFloat",
    "D_Version2",
    "D_X32",
    "ELFv1",
    "ELFv2",
    "Epiphany",
    "FreeBSD",
    "FreeStanding",
    "GNU",
    "Haiku",
    "HPPA",
    "HPPA64",
    "Hurd",
    "IA64",
    "iOS",
    "LDC",
    "linux",
    "LittleEndian",
    "MinGW",
    "MIPS32",
    "MIPS64",
    "MIPS_EABI",
    "MIPS_HardFloat",
    "MIPS_N32",
    "MIPS_N64",
    "MIPS_O32",
    "MIPS_O64",
    "MIPS_SoftFloat",
    "NetBSD",
    "none",
    "NVPTX",
    "NVPTX64",
    "OpenBSD",
    "OSX",
    "PlayStation",
    "PlayStation4",
    "Posix",
    "PPC",
    "PPC64",
    "PPC_HardFloat",
    "PPC_SoftFloat",
    "S390",
    "S390X",
    "SDC",
    "SH",
    "SH64",
    "SkyOS",
    "Solaris",
    "SPARC",
    "SPARC64",
    "SPARC_HardFloat",
    "SPARC_SoftFloat",
    "SPARC_V8Plus",
    "SystemZ",
    "SysV3",
    "SysV4",
    "TVOS",
    "unittest",
    "WatchOS",
    "Win32",
    "Win64",
    "Windows",
    "X86",
    "X86_64"
];

private void fillBadVersions()
{
    // note: compiler switch -m32/64 can lead to wrong results

    alias addVerId = (ver) => `version(` ~ ver ~ `){}
        else _badVersions["` ~ ver ~ "\"] = true;\n";

    string addVerionIdentifiers()
    {
        import std.meta: aliasSeqOf;
        import std.range: iota;
        string result;
        foreach(i; aliasSeqOf!(iota(0, predefinedVersions.length)))
        {
            result ~= addVerId(predefinedVersions[i]);
        }
        return result;
    }

    mixin(addVerionIdentifiers);
}

/**
 * Make a D string compatible with an Object Pascal string literal.
 */
string patchPascalString(size_t lenLimit = 0)(string value)
{
    bool needed;
    foreach(i; 0 .. value.length)
        if (value[i] == '\'')
    {
        needed = true;
        break;
    }
    if (!needed)
        return value;

    Appender!string app;
    static if (lenLimit)
        const size_t len = value.length > 100 ? 100 : value.length;
    else
        const size_t len = value.length;
    app.reserve(len);
    bool skip;
    L: foreach (immutable i; 0..value.length)
    {
        const char c = value[i];
        switch (c)
        {
            case 0x80: .. case 0xFF:
            {
                app ~= value[i];
                skip = true;
                break;
            }
            case '\'':
            {
                if (skip)
                    app ~= value[i];
                else
                    app ~= "'#39'";
                skip = false;
                break;
            }
            case '\r': case '\n':
            {
                if (skip)
                    app ~= value[i];
                else
                    app ~= "'#10'";
                skip = false;
                break;
            }
            default:
            {
                app ~= value[i];
                skip = false;
                static if (lenLimit)
                    if (app.data.length >= len)
                        break L;
            }
        }
    }
    return app.data;
}

/// Used to annotate a public field that is written in `pascalStreaming()`.
enum Pascal;

/**
 * Streams a class or a struct using the Object Pascal streaming format, as defined
 * in FPC's FCL or Delphi's RTL.
 */
void pascalStreaming(T, string[][] enumLuts = [[""]], bool bin = false)(auto ref T t,
    ref Appender!string stream)
if (is(T == struct) || is(T == class))
{

    // TODO: find speification of the Pascal binary format
    static if (bin) {}
    else
    {
        stream ~= "object ";
        stream ~= T.stringof;
        stream ~= "\r";
    }

    foreach(member; __traits(allMembers, T))
    {
        static if (is(typeof(__traits(getMember, t, member))) &&
            hasUDA!(__traits(getMember, t, member), Pascal))
        {
            alias MT = typeof(__traits(getMember, t, member));
            alias TestBasicTypes = templateOr!(isSomeString, isIntegral,
                isFloatingPoint, isBoolean,);

            static if (is(MT == class) || is(MT == struct))
            {
                pascalStreaming!(MT, enumLuts, bin)(__traits(getMember, t, member), stream);
            }
            else static if (is(MT == enum))
            {
                import std.range: iota;
                bool done;
                static if (isIntegral!(OriginalType!MT))
                    foreach (i; aliasSeqOf!(iota(0,enumLuts.length)))
                {
                    static if (enumLuts[i].length == MT.max+1)
                    {
                        static if (bin) {}
                        else
                        {
                            stream ~= member;
                            stream ~= " = ";
                            stream ~= enumLuts[i][__traits(getMember, t, member)];
                            stream ~= "\r";
                        }
                        done = true;
                        break;
                    }
                }
                if (!done)
                {
                    static if (bin) {}
                    else
                    {
                        stream ~= member;
                        stream ~= " = ";
                        static if (isSomeString!MT)
                            stream ~= "'";
                        stream ~= to!string(__traits(getMember, t, member));
                        static if (isSomeString!MT)
                            stream ~= "'";
                        stream ~= "\r";
                    }
                }
            }
            else static if (TestBasicTypes!MT)
            {
                static if (bin) {}
                else
                {
                    stream ~= member;
                    stream ~= " = ";
                    stream ~= to!string(__traits(getMember, t, member));
                    stream ~= "\r";
                }
            }
            else static assert(0);
        }
    }

    static if (bin) {}
    else
    {
        stream ~= "end\r";
    }
}

unittest
{
    enum Bar{bar}

    static struct TRat
    {
        int notaprop1 = 0;
        @Pascal ubyte subProperty1 = 1;
        @Pascal string subProperty2 = "pascal";
    }

    static class TFoo
    {
        int notaprop1 = 0;
        @Pascal ubyte property1 = 1;
        @Pascal string property2 = "pascal";
        @Pascal Bar property3 = Bar.bar;
        @Pascal TRat rat;
    }

    Appender!string stream;
    pascalStreaming!(TFoo, [["bar"]])(new TFoo, stream);
    assert(stream.data != "");
}

/**
 * Produces and visits the AST for a source code.
 *
 * This function is used to handle the content of a MixinExpression in an
 * ASTVisitor.
 */
T parseAndVisit(T : ASTVisitor)(const(char)[] source)
{
    import std.functional;

    RollbackAllocator allocator;
    LexerConfig config = LexerConfig("", StringBehavior.source, WhitespaceBehavior.skip);
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    const(Token)[] tokens = getTokensForParser(cast(ubyte[]) source, config, &cache);
    Module mod = parseModule(tokens, "", &allocator, toDelegate(&ignoreErrors));
    T result = construct!(T);
    result.visit(mod);
    return result;
}

/**
 * By default libdparse outputs errors and warnings to the standard streams.
 * This function prevents that.
 */
void ignoreErrors(string, size_t, size_t, string, bool) @system
{}

