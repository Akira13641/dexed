/**
 * Command line argument handling based on UDAs.
 */
module iz.options;

import
    std.traits, std.meta, std.typecons, std.conv, std.stdio, std.exception;
import
    iz.enumset;

/// Possible flags associated to an argument.
enum ArgFlag: ubyte
{
    /// Argument must have a matching command line element.
    mandatory,
    /// Processing stops successfully if the matching command line element is found.
    stopper,
    /// If argument is a "long option", handles it's "short" equivalent automatically.
    allowShort
}

/// AFmandatory, AFstopper, etc
mixin AliasedEnumMembers!(ArgFlag, "AF");

//TODO: workaround for the fact that unittest dont work with static struct nested in unittest{}
//TODO: more solid grammar for argument syntax.

/// Set of $(D ArgFlag).
alias ArgFlags = EnumSet!(ArgFlag, Set8);

/**
 * Used to describe a program argument.
 *
 * Handled types are $(D bool), everything that impliclty converts to
 * $(D ulong), $(D real), $(D string) or $(D dchar), $(D void function()) and
 * finally the function setters for the types listed above.
 */
struct Argument
{
    /// The expected string, including the hyphens
    immutable string name;
    /// Help string associated to the argument.
    immutable string help;
    /// The flags.
    immutable ArgFlags flags;
    /// Helpers for the flags.
    bool isMandatory() const {return ArgFlag.mandatory in flags;}
    /// ditto
    bool isStopper() const {return ArgFlag.stopper in flags;}
    /// ditto
    bool isShortAllowed() const {return ArgFlag.allowShort in flags;}
}

private bool isShortOptionAllowed(string s)
{
    bool result;
    if (s.length > 2 && s[0..2] == "--")
        result = true;
    return result;
}

unittest
{
    assert(isShortOptionAllowed("--help"));
    assert(!isShortOptionAllowed("-h"));
    assert(!isShortOptionAllowed("h"));
    assert(Argument("--help", "", ArgFlags(ArgFlag.allowShort)).isShortAllowed);
}

/**
 * Collects the description of the arguments in a tuple of $(D Argument).
 * Can be used to build custom help messages.
 *
 * Params:
 *      Locations = The modules, $(D struc)s or $(D class)es containing the
 *          $(D @Argument) variables. When declared in an aggregate,
 *          the variables must be static.
 */
template ArgDescriptions(Locations...)
{
    static if (Locations.length == 0)
    {
        alias ArgDescriptions = AliasSeq!();
    }
    else static if (Locations.length == 1)
    {
        alias getArgument(alias S) = getUDAs!(S, Argument);
        alias Symbols = getSymbolsByUDA!(Locations, Argument);
        alias ArgDescriptions = staticMap!(getArgument,  Symbols);

        // check dups
        static foreach(i, a1; ArgDescriptions)
            static foreach(j, a2; ArgDescriptions)
        {
            static if (i != j)
                static assert(a1.name != a2.name,
                    "duplicated Argument description: `" ~ a2.name ~ "`");
            static if (a1.isShortAllowed)
                static assert(a1.name[1..3] != a2.name,
                    "conflicting Argument description due to allowShort: `"
                        ~ a1.name ~ "` and `" ~ a2.name ~ "`");
        }

        // check whites
        import std.ascii: isWhite;
        import std.algorithm.searching: canFind;
        import std.range.primitives: front, popFront, empty;
        static foreach(a; ArgDescriptions)
        {
            static assert(!a.name.canFind!(a => a.isWhite),
                "Argument contains white chars: `" ~ a.name ~ "`");
        }

        // check if valid for short option
        static foreach(a; ArgDescriptions)
        {
            static if (a.isShortAllowed)
                static assert(a.name.isShortOptionAllowed(),
                    "Argument is not suitable for a short equivalent: `" ~ a.name ~ "`");
        }

    }
    else static if (Locations.length > 1)
    {
        alias ArgDescriptions = AliasSeq!(
            ArgDescriptions!(Locations[0..$/2]),
            ArgDescriptions!(Locations[$/2..$])
        );
        enum check(alias T) = T.name != ArgDescriptions[0].name;
        static assert(allSatisfy!(check, ArgDescriptions[1..$]),
            "duplicated Argument description: " ~ ArgDescriptions[0].name);
    }
}

version(unittest)
{
    static struct ContainerWithDups
    {
        static @Argument("-a") bool a;
        static @Argument("-a") bool ab;
    }
    static struct Cwd1
    {
        static @Argument("-a") bool a;
    }
    static struct Cwd2
    {
        static @Argument("-a") bool b;
    }
    static struct CWhites1
    {
        static @Argument("-a ") bool b;
    }
    static struct CWhites2
    {
        static @Argument("  -a") bool b;
        static @Argument("--a c") bool c;
    }
    static struct ShortLongDup
    {
        static @Argument("-long", "", ArgFlags(AFallowShort)) bool b;
        static @Argument("-l") bool c;
    }
}

unittest
{
    static assert(!__traits(compiles, ArgDescriptions!ContainerWithDups));
    static assert(!__traits(compiles, ArgDescriptions!(Cwd1,Cwd2)));
    static assert(!__traits(compiles, ArgDescriptions!(CWhites1)));
    static assert(!__traits(compiles, ArgDescriptions!(CWhites2)));
    static assert(!__traits(compiles, ArgDescriptions!(ShortLongDup)));
}

// Collects the informations allowing to set the arg targets using their ident.
private template ArgumentTargets(Locations...)
{
    static if (Locations.length == 0)
    {
        alias ArgumentTargets = AliasSeq!();
    }
    else static if (Locations.length == 1)
    {
        template getTarget(alias S)
        {
            enum tmp = __traits(identifier, S);
            static if (tmp[0] == '(')
                enum sym = tmp[1..$-1];
            else
                enum sym = tmp;

            enum loc = Locations.stringof[0] == '(' ? Locations.stringof[1..$-1] : Locations.stringof;
            enum fqn = fullyQualifiedName!S;
            enum mod = moduleName!(S);
            enum getTarget = tuple(typeof(S).stringof, fqn, mod);
        }
        alias Symbols = getSymbolsByUDA!(Locations, Argument);
        alias ArgumentTargets = staticMap!(getTarget,  Symbols);
    }
    else static if (Locations.length > 1)
    {
        alias ArgumentTargets = AliasSeq!(
            ArgumentTargets!(Locations[0..$/2]),
            ArgumentTargets!(Locations[$/2..$])
        );
    }
}

unittest
{
    static struct Container1
    {
        static @Argument("-a", "help for a", ArgFlags(AFmandatory)) bool a;
        static @Argument("--ab", "help for ab", ArgFlags(AFmandatory, AFstopper)) bool ab;
    }
    static struct Container2
    {
        static @Argument("-c", "help for c") bool c;
        static @Argument("--cd", "help for cd") bool cd;
    }

    alias Descr = ArgDescriptions!(Container1, Container2);
    static assert(Descr.length == 4);
    assert(Descr[0].isMandatory /*&& Descr[0].isFatal*/);
    assert(Descr[1].isMandatory && Descr[1].isStopper);
}

/**
 * Splits the argument and its optional value. This function is public in order
 * to document the possible syntaxes of an argument.
 *
 * Grammar:
 *        Identifier
 *      | '-' Character IdentifierOrValue
 *      | '-' Character ('=' IdentifierOrValue)?
 *      | '--' Identifier ('=' IdentifierOrValue)?
 *      ;
 *
 * Params:
 *      s = An element of the $(D string[] args).
 *
 * Returns:
 *      An array of two strings. The first is always set, it represents the
 *      argument name, including the hyphens. The second, optional is its
 *      associated value.
 */
string[2] argNameAndValue()(string s)
in
{
    assert(s.length);
    if (s[0] == '-')
        assert(s.length >= 2, "minimal length of an hyphenized argument is 2 (-<letter>)");
}
do
{
    import std.string : indexOf;
    string[2] result;

    if (s[0] == '"' && s[$-1] == '"')
    {
        s = s[1..$-1];
    }
    // simple command
    if (s[0] != '-')
    {
        result[0] = s;
    }
    else
    {
        // arg cant begin with '='
        if (const ptrdiff_t p = s.indexOf('=') + 1)
        {
            result[0] = s[0..p-1];
            result[1] = s[p..$];
        }
        else
        {
            // no equal, single "-"
            if (s.length > 2 && s[0..2] != "--")
            {
                result[0] = s[0..2];
                result[1] = s[2..$];
            }
            else
            {
                result[0] = s;
            }
        }
    }
    if (result[1].length &&
        result[1][0] == '"' && result[1][$-1] == '"')
    {
        result[1] = result[1][1..$-1];
    }
    return result;
}
/// Accepted syntax
pure nothrow @safe @nogc unittest
{
    assert("-a".argNameAndValue == ["-a", ""]);
    assert("cmd".argNameAndValue == ["cmd", ""]);
    assert("-tTRUE".argNameAndValue == ["-t", "TRUE"]);
    assert("-a=true".argNameAndValue == ["-a", "true"]);
    assert("--a=42".argNameAndValue == ["--a", "42"]);
    assert("command".argNameAndValue == ["command", ""]);
    assert(`--s="tic tac toe"`.argNameAndValue == ["--s", "tic tac toe"]);
    assert(`"--s=tic tac toe"`.argNameAndValue == ["--s", "tic tac toe"]);
}
/// Invalid syntax
@system unittest
{
    import std.exception: assertThrown;
    import core.exception: AssertError;
    assertThrown!AssertError("-".argNameAndValue);
    assertThrown!AssertError("".argNameAndValue);
    //assertThrown!AssertError("--".argNameAndValue);
}

enum : bool
{
    /**
     * When passed as first CT arg of $(D handleArgument), indicates that,
     * when possible, the code for throwing exceptions will not be generated.
     */
    CantThrow  = false,
    /**
     * When passed as first CT arg of $(D handleArgument), indicates that
     * the code for throwing exceptions will always be generated.
     */
    CanThrow  = true
}

/**
 * Thrown on $(D handleArgument) errors.
 */
class IzOptionException : Exception
{
    import std.exception : basicExceptionCtors;
    mixin basicExceptionCtors;
}

/**
 * Handles program arguments.
 *
 * This is a new generation (as for Spring 2018...), faster but simpler,
 * $(D getopt)-like function, based on UDA and that contains only the code needed
 * to handle the possible arguments, since it is heavily based on the D template
 * meta-programming techniques. Argument targets are directly accessed (vs using
 * pointers in the stdandard library) which has the advantage, in addition to the
 * speed, to be usable in safe D code).
 *
 * Params:
 *      mustThrow = A boolean value that indicates wether exceptions are gagged
 *          (or if the code to throw them is generated). When equal to $(D false)
 *          and even if it doesn't have to be, the function is callable by a
 *          $(D @nothrow) caller. Success is always indicated in the result.
 *      Locations = Indicates the modules, $(D struct)s or $(D class)es containing the
 *          $(D @Argument) declarations. When declared in an aggregate,
 *          the declarations must be static.
 *      args = The arguments passed to $(D main(string[] args)), less the program name.
 *
 * Returns:
 *      $(D true) if $(D Locations) requirements were encountered, $(D false) otherwise.
 *
 * Throws:
 *      Only if $(D mustThrow), a $(D ConvException) if one of the value passed
 *      in an argument does not convert to the matching target type or
 *      a $(D IzOptionException) if one of the $(D Argument) flag is not verified
 *      (such as $(D ArgFlag.mandatory)) or if more $(D args) than $(D Argument)
 *      are passed.
 */
bool handleArguments(alias mustThrow, Locations...)(string[] args)
if (is(typeof(mustThrow) : bool))
{
    static assert(Locations.length > 0,
        "need at least 1 location containing @Argument variable(s)");
    static assert((Locations.length == 1 && __VERSION__ < 2080L) ||
                  (Locations.length >= 1 && __VERSION__ > 2079L),
        "multiple locations are only handled from D version 2.080");

    bool             result = true;
    alias            argTrgts = ArgumentTargets!Locations;
    static immutable argDescr = ArgDescriptions!Locations;
    string[2]        argParts;
    bool[argTrgts.length] done;

    enum gotoL0withFalse = "{result = false; goto L0;}";

    static if (mustThrow)
    {
        if (args.length > done.length )
            throw new IzOptionException("too many arguments passed");
    }

    static foreach(i, t; argTrgts)
    {
        foreach(ref arg; args)
        {
            mixin("import " ~ t[2] ~ ";");
            argParts = argNameAndValue(arg);

            bool shortOption;
            static if (argDescr[i].isShortAllowed)
            {
                shortOption = argParts[0] == argDescr[i].name[1..3];
            }

            if (shortOption || argParts[0] == argDescr[i].name)
            {
                // functions
                alias FunT = typeof(mixin(t[1]));
                static if (isFunction!FunT)
                {
                    alias P = Parameters!FunT;
                    static assert(P.length <= 1, "invalid number of parameters for " ~ t[1]);

                    // bool
                    static if (P.length == 0)
                    {
                        if (argParts[1].length == 0)
                        {
                            done[i] = true;
                            mixin(t[1] ~ "();");
                            static if (argDescr[i].isStopper)
                                goto L0;
                        }
                        else static if (argDescr[i].isMandatory)
                            mixin(gotoL0withFalse);
                    }
                    else static if (is(Unqual!(P[0]) == bool))
                    {
                        if (argParts[1] == "false")
                        {
                            mixin(t[1] ~ "(false);");
                            done[i] = true;
                            static if (argDescr[i].isStopper)
                                goto L0;
                        }
                        else if (argParts[1] == "true")
                        {
                            mixin(t[1] ~ "(true);");
                            done[i] = true;
                            static if (argDescr[i].isStopper)
                                goto L0;
                        }
                        else if (argParts[1].length)
                        {
                            static if (mustThrow)
                                throw new ConvException("cannot convert string to bool: "
                                    ~ argParts[1]);
                            else
                                mixin(gotoL0withFalse);
                        }
                        else static if (argDescr[i].isMandatory)
                            mixin(gotoL0withFalse);
                    }
                    // implictly convertible types
                    else static foreach(IC; AliasSeq!(ulong, real, string, dchar))
                    {{
                        alias TT = Unqual!(P[0]);
                        static if (is(TT : IC))
                        {
                            if (argParts[1].length != 0)
                            {
                                static if (mustThrow)
                                {
                                    const TT value = to!TT(argParts[1]);
                                }
                                else
                                {
                                    TT value; // = void  + @trusted
                                    try value = to!TT(argParts[1]);
                                    catch (Exception)
                                        mixin(gotoL0withFalse);
                                }
                                done[i] = true;
                                mixin(t[1] ~ "(value);");
                                static if (argDescr[i].isStopper)
                                    goto L0;
                            }
                            else static if (argDescr[i].isMandatory)
                                mixin(gotoL0withFalse);
                        }
                    }}
                }
                else
                {
                    mixin("alias TT = " ~ t[0] ~ ";");
                    // bool
                    static if (is(TT == bool))
                    {
                        if (argParts[1].length == 0 || argParts[1] == "true")
                        {
                            done[i] = true;
                            mixin(t[1]) = true;
                            static if (argDescr[i].isStopper)
                                goto L0;
                        }
                        else if (argParts[1] == "false")
                        {
                            done[i] = true;
                            mixin(t[1]) = false;
                            static if (argDescr[i].isStopper)
                                goto L0;
                        }
                        else if (argParts[1].length)
                        {
                            static if (mustThrow)
                                throw new ConvException("cannot convert string to bool: "
                                    ~ argParts[1]);
                            else
                                mixin(gotoL0withFalse);
                        }
                        else static if (argDescr[i].isMandatory)
                            mixin(gotoL0withFalse);
                    }
                    // implictly convertible types
                    else static foreach(IC; AliasSeq!(ulong, real, string, dchar))
                    {
                        static if (is(TT : IC))
                        {
                            if (argParts[1].length != 0)
                            {
                                static if (mustThrow)
                                {
                                    const TT value = to!TT(argParts[1]);
                                }
                                else
                                {
                                    TT value; // = void  + @trusted
                                    try value = to!TT(argParts[1]);
                                    catch (Exception)
                                        mixin(gotoL0withFalse);
                                }
                                done[i] = true;
                                mixin(t[1]) = value;
                                static if (argDescr[i].isStopper)
                                    goto L0;
                            }
                            else static if (argDescr[i].isMandatory)
                                mixin(gotoL0withFalse);
                        }
                    }
                }
            }
        }
        static if (argDescr[i].isMandatory)
        {
            if (!done[i])
            {
                result = false;
                static if (mustThrow)
                    throw new IzOptionException("mandatory argument not set: " ~
                        argDescr[i].name);
                else goto L0;
            }
        }
    }
    L0:

    //static if (mustThrow)
    //{
    //    if (!result)
    //        throw new IzOptionException("Invalid arguments: " ~ to!string(args));
    //}

    return result;
}
///
version(D_Ddoc) @safe unittest
{
    // the definition container does not have to be dedicated to the sole purpose
    // of hosting the options and several containers are allowed.
    struct ArgumentDefinition
    {
        // simple bool argument.
        static @Argument("-verbose", "as much blabla as possible...") bool v;
        // an int, requiring to use "=".
        static @Argument("--numThreads", "compute in parallel") int n;
        // a stopper flag: it shortcuts arguments processing if found.
        static @Argument("--update", "", ArgFlags(ArgFlag.stopper)) bool u;
        // bool function, works without pointer. "-h" triggers it too, as indicated in the flags.
        static @Argument("--help", "", ArgFlags(ArgFlag.allowShort)) void help() @safe {writeln("RTFM");}
    }
    // a @safe argument checking...
    assert(handleArguments!(CantThrow, ArgumentDefinition)(["-verbose", "--numThreads=8"]));
    // error reporting is then a simple bool,
    // which is however enough to display the help.
    if (!handleArguments!(CantThrow, ArgumentDefinition)(["-verbose", "--numThreads=whoops"]))
        writeln(help!ArgumentDefinition);
}

/**
 * Returns: The default help string for the $(D Argument)s found in the $(D Locations).
 */
string help(Locations...)()
{
    import std.format: format;

    string result;
    size_t width;
    static foreach(a; ArgDescriptions!Locations)
    {
        static if (!a.isShortAllowed)
        {
            if (a.name.length > width)
                width = a.name.length;
        }
        else
        {
            if (a.name.length + 5 > width)
                width = a.name.length + 5;
        }
    }
    immutable string specifier = "%-" ~ to!string(width) ~ "s : %s\n";
    static foreach(a; ArgDescriptions!Locations)
    {
        static if (!a.isShortAllowed)
            result ~= specifier.format(a.name, a.help);
        else
            result ~= specifier.format(a.name ~ " | " ~ a.name[1..3] , a.help);
    }
    return result;
}

version(unittest) private static struct Container
{
    static @Argument("-a", "help for a") bool a;
    static @Argument("--seed", "help for seed") int seed;
    static @Argument("--letter", "help for letter", ArgFlags(AFallowShort)) char letter;
    static @Argument("--fname", "help for fname") string fname;
    static @Argument("--scaleX", "help for scaleX") double scaleX;

    static bool c;
    static @Argument("-c", "help for c") void cFun() @safe nothrow {c = true;}
}

unittest
{
    string a = help!Container;
}

@safe nothrow unittest
{
    assert(handleArguments!(CantThrow, Container)(
        ["-a", "-c", "--seed=42", "--fname=/home/fantomas.txt", "--scaleX=1.0", "--letter=K"]
    ));
    assert(Container.a);
    assert(Container.c);
    assert(Container.fname == "/home/fantomas.txt");
    assert(Container.scaleX == 1.0);
    assert(Container.seed == 42);
    assert(Container.letter == 'K');
    assert(handleArguments!(CantThrow, Container)(["-a=false"]));
    assert(!Container.a);
    assert(handleArguments!(CantThrow, Container)(["-a=true"]));
    assert(Container.a);
}

@safe nothrow unittest
{
    assert(!handleArguments!(CantThrow, Container)(
        ["-a", "-c", "--seed=reallyNAN", "--fname=/home/fantomas.txt", "--scaleX=1.0"]
    ));
}

@safe nothrow unittest
{
    assert(handleArguments!(CantThrow, Container)(["-c"]));
}

@safe unittest
{
    assert(handleArguments!(CanThrow, Container)(["-c"]));
}

@safe unittest
{
    import std.exception : assertThrown;
    assertThrown(handleArguments!(CanThrow, Container)(["--seed=reallyNAN"]));
}

version(unittest) private static struct ContainerWithMandatories
{
    static @Argument("--s0", "", ArgFlags(ArgFlag.mandatory) ) int s0;
    static @Argument("--s1", "", ArgFlags(ArgFlag.mandatory) ) int s1;
}

@safe unittest
{
    import std.exception : assertThrown;
    assertThrown(handleArguments!(CanThrow, ContainerWithMandatories)(["--s0=1"]));
    assertThrown(handleArguments!(CanThrow, ContainerWithMandatories)(["--s1=1"]));
    assert(handleArguments!(CanThrow, ContainerWithMandatories)(["--s1=1", "--s0=2"]));
}

@safe nothrow unittest
{
    assert(!handleArguments!(CantThrow, ContainerWithMandatories)(["--s0=1"]));
    assert(!handleArguments!(CantThrow, ContainerWithMandatories)(["--s1=1"]));
    assert(handleArguments!(CantThrow, ContainerWithMandatories)(["--s1=1", "--s0=2"]));
}

@safe nothrow unittest
{
    assert(!handleArguments!(CantThrow, ContainerWithMandatories)(["--s1=1", "--s0"]));
    assert(!handleArguments!(CantThrow, ContainerWithMandatories)(["--s1=1", "--s0="]));
    assert(!handleArguments!(CantThrow, ContainerWithMandatories)(["--s1=1", "--s0=$$"]));
}

version(unittest) private static struct ContainerWithStop
{
    static @Argument("--s0", "", ArgFlags(ArgFlag.mandatory, ArgFlag.stopper) ) int s0;
    static @Argument("--s1", "", ArgFlags(ArgFlag.mandatory) ) int s1;
}

@safe nothrow unittest
{
    // dont care about s1 being mandatory.
    assert(handleArguments!(CantThrow, ContainerWithStop)(["--s0=1"]));
    assert(!handleArguments!(CantThrow, ContainerWithStop)(["--s1=1"]));
    // stopped before s1 error
    assert(handleArguments!(CantThrow, ContainerWithStop)(["--s0=1", "--s1"]));
}

version(unittest) private static struct ContainerWithFuncs
{
    static int s0;
    static float s1 = 123;
    static @Argument("--s0") void s0Fun(const int v) @safe {s0 = v;}
    static @Argument("--s1") void s1Fun(float v) @safe {s1 = v;}
}

@safe unittest
{
    assert(handleArguments!(CantThrow, ContainerWithFuncs)(["--s0=1", "--s1=1.0"]));
    assert(ContainerWithFuncs.s0 == 1);
    static if (__VERSION__ >= 2080L)
        assert(ContainerWithFuncs.s1 == 1.0f, to!string(ContainerWithFuncs.s1));
}

version(unittest) private static struct ContainerForInvalidBools
{
    static @Argument("-a") bool b;
    static @Argument("-b") void s1Fun(bool) @safe {}
}

version(unittest) private static struct ContainerForMandatoryAndThrow
{
    static @Argument("-a", "", ArgFlags(ArgFlag.mandatory)) bool a;
}

@safe unittest
{
    import std.exception : assertThrown, assertNotThrown;
    assertThrown(handleArguments!(CanThrow, ContainerForInvalidBools)(["-bYES"]));
    assertThrown(handleArguments!(CanThrow, ContainerForInvalidBools)(["-aYES"]));
    assertNotThrown(handleArguments!(CanThrow, ContainerForInvalidBools)(["-atrue", "-bfalse"]));
    assertNotThrown(handleArguments!(CanThrow, ContainerForInvalidBools)(["-afalse", "-btrue"]));
    assertThrown(handleArguments!(CanThrow, ContainerForMandatoryAndThrow)(["-k"]));
}

version(unittest)
{
    private static struct ContainerWithRole1
    {
        static @Argument("-a") bool a;
        static @Argument("-b") void bFun(bool) @safe {}
    }
    private static struct ContainerWithRole2
    {
        static @Argument("-c") bool c;
        static @Argument("-d") void dFun(bool) @safe {}
    }
}

static if (__VERSION__ >= 2080L) unittest
{
    assert(handleArguments!(CantThrow, ContainerWithRole1, ContainerWithRole2)(["-a","-c"]));
    assert(ContainerWithRole1.a);
    assert(ContainerWithRole2.c);
}

version(unittest) private static struct ContainerForAString
{
    static @Argument("-s") string s;
}

@safe unittest
{
    assert(handleArguments!(CanThrow, ContainerForAString)([`-s"tic tac toe"`]));
    assert(ContainerForAString.s == "tic tac toe");
}

version(unittest) private static struct ContainerForShared
{
    static @Argument("-s") shared(size_t) s;
}

@safe unittest
{
    assert(handleArguments!(CanThrow, ContainerForShared)([`-s123`]));
    assert(ContainerForShared.s == 123);
}

version(unittest) private static struct ParentContainer
{
    static struct Nested
    {
        static @Argument("-s") string s;
    }
}

@safe unittest
{
    assert(handleArguments!(CanThrow, ParentContainer.Nested)([`-sK`]));
    assert(ParentContainer.Nested.s == "K");
}

@safe unittest
{
    assertThrown!IzOptionException(
        handleArguments!(CanThrow, ParentContainer.Nested)([`-sK`, "other"]));
}

version(unittest) private static struct ContainerForLongShortOption
{
    static @Argument("--help", "", ArgFlags(AFallowShort)) bool h;
    static @Argument("--value", "", ArgFlags(AFallowShort)) int v;
}

@safe unittest
{
    assert(handleArguments!(CanThrow, ContainerForLongShortOption)([`-h`, `-v8`]));
    assert(ContainerForLongShortOption.h);
    assert(ContainerForLongShortOption.v == 8);
    assert(handleArguments!(CanThrow, ContainerForLongShortOption)([`--help=false`, `--value=9`]));
    assert(!ContainerForLongShortOption.h);
    assert(ContainerForLongShortOption.v == 9);
}

version(none)
{

    struct ContainerForBenchmark
    {
        static @Argument("--num") int num;
        static @Argument("-v") bool v;
        static @Argument("-f") bool f;
        static @Argument("-t") bool t;
    }

    import std.datetime.stopwatch;
    static string[] args = ["program", "--num=8", "-v", "-f", "-t"];

    static void stdO() @safe
    {
        import std.getopt: getopt;
        getopt(args,    "num", &ContainerForBenchmark.num,
                        "v", &ContainerForBenchmark.v,
                        "f", &ContainerForBenchmark.f,
                        "t", &ContainerForBenchmark.t);
    }

    static void izO() @safe nothrow
    {
        handleArguments!(CantThrow, ContainerForBenchmark)(args[1..$]);
    }

    void main()
    {

        benchmark!(stdO)(1_000_000)[0].writeln(" (std)");
        benchmark!(izO)(1_000_000)[0].writeln(" (IZ)");
        benchmark!(stdO)(1000)[0].writeln(" (std)");
        benchmark!(izO)(1000)[0].writeln(" (IZ)");
        benchmark!(stdO)(10)[0].writeln(" (std)");
        benchmark!(izO)(10)[0].writeln(" (IZ)");
        benchmark!(stdO)(1)[0].writeln(" (std)");
        benchmark!(izO)(1)[0].writeln(" (IZ)");
    }
}
