/**
 * Utilities related to D named enumerations.
 */
module iz.enumset;

import
    std.traits, std.range;
import
    iz.types;

private enum hasInt128 = is(ucent);

/// Container for a EnumSet based on an enum which has up to 8 members.
alias Set8 = ubyte;
/// Container for a EnumSet based on an enum which has up to 16 members.
alias Set16 = ushort;
/// Container for a EnumSet based on an enum which has up to 32 members.
alias Set32 = uint;
/// Container for a EnumSet based on an enum which has up to 64 members.
alias Set64 = ulong;
/// Container for a EnumSet based on an enum which has up to 128 members (not working, relies on ucent).
static if (hasInt128) alias Set128 = ucent;


static if (hasInt128)
    private alias BigestSet = Set128;
else
    private alias BigestSet = Set64;


/**
 * Returns true if the parameter is suitable for being used as a EnumSet container.
 * Params:
 * S = a enumset container type.
 */
template isSetSuitable(S)
{
    static if (isSigned!S)
        enum isSetSuitable = false;
    else static if (is(S==Set8))
        enum  isSetSuitable = true;
    else static if (is(S==Set16))
        enum  isSetSuitable = true;
    else static if (is(S==Set32))
        enum  isSetSuitable = true;
    else static if (is(S==Set64))
        enum  isSetSuitable = true;
    else static if (hasInt128 && is(S==Set128))
        enum  isSetSuitable = true;
    else
        enum isSetSuitable = false;
}

/**
 * Returns the member count of an enum.
 * Params:
 * E = an enum.
 */
size_t enumMemberCount(E)()
if (is(E==enum))
{
    static if (isOrderedEnum!E)
        return 1 + E.max - E.min;
    else
    {
        size_t result;
        foreach(member; EnumMembers!E) result++;
        return result;
    }
}

/**
 * Provides the information about the rank of the members of an enum.
 * The properties are all static and can be retrieved from a template alias.
 * Params:
 * E = an enum.
 */
struct EnumRankInfo(E) if (is(E==enum))
{

private:

    enum isOrdered = isOrderedEnum!E;
    static if (!isOrdered)
    {
        static immutable size_t[E] _rankLUT;
        static immutable E[size_t] _membLUT;
        static immutable size_t _count;
    }

public:

    static if (!isOrdered)
    static this() pure nothrow @safe
    {
        if (__ctfe){}
        else foreach(member; EnumMembers!E)
        {
            _rankLUT[member] = _count;
            _membLUT[_count] = member;
            _count++;
        }
    }

    /// Returns the rank of the last member.
    static size_t max() pure nothrow @safe @nogc
    {
        static if (isOrdered)
            return E.max - E.min;
        else
            return _count-1;
    }

    /// Returns the member count. It's always equal to max + 1.
    static size_t count() pure nothrow @safe @nogc
    {
        static if (isOrdered)
            return 1 + E.max - E.min;
        else
            return _count;
    }

    /// Always returns 0.
    static nothrow @safe @nogc  enum min = 0;

    /// Returns the rank of aMember.
    static size_t opIndex()(E aMember) pure nothrow @safe @nogc
    if (isOrdered)
    {
        return aMember - E.min;
    }

    /// ditto
    static size_t opIndex()(E aMember) pure nothrow @safe
    if (!isOrdered)
    {
        if (__ctfe)
        {
            size_t result;
            foreach(member; EnumMembers!E)
            {
                if (member == aMember)
                    return result;
                ++result;
            }
            assert(0);
        }
        else return _rankLUT[aMember];
    }

    /// Returns the member at aRank.
    static E opIndex()(size_t aRank) pure nothrow @safe @nogc
    if (isOrdered)
    {
        return cast(E) (E.min + aRank);
    }

    static E opIndex()(size_t aRank) pure nothrow @safe
    if (!isOrdered)
    {
        if (__ctfe)
        {
            size_t rank;
            foreach(member; EnumMembers!E)
            {
                if (rank == aRank)
                    return member;
                ++rank;
            }
            assert(0);
        }
        else return _membLUT[aRank];
    }
}

/**
 * Indicates if the members of an enum fit in a container.
 * Params:
 * E = an enum.
 * S = a container, either a Set8, a Set16, a Set32 or Set64.
 */
bool enumFitsInSet(E, S)()
if (is(E==enum) && isSetSuitable!S)
{
    S top = S.max;
    ulong max;
    foreach(i, member; EnumMembers!E)
    {
        max +=  cast(S) 1 << i;
    }
    return (max <= top) & (max > 0);
}

/**
 * An EnumSet allows to create a bit field using the members of an enum.
 *
 * It's designed similarly to the Pascal built-in sets (the "Set Of" construct).
 * It's also related to the phobos type EnumFlag, except that it has no constraint
 * related to the enum member values since it's based on the enum members rank.
 *
 * It's efficient as function parameter since the size of an EnumSet is
 * equal to the size of its container (so from 1 to 8 bytes). Since manipulating
 * an EnumSet set is mostly about making bitwise operations an EnumSet is completly
 * safe. Another notable characteristic is that an EnumSet is ordered, so if a
 * range is implemented to allow the usage of std.algorithm.searching functions
 * it's always more simple and efficient to use the "in" operator.
 *
 * There are two ways to use an EnumSet:
 * * using the C-like operators, "+" and "-" to add or remove members, "^" and "&" to get the difference and the intersection.
 * * using the Pascal-like intrinsics (here some functions): include(), exclude() and the 'in' operator.
 *
 * Params:
 * S = A Set8, Set16, Set32 or Set64. It must be wide enough to contain all the enum members.
 * E = An enum, which must have at least one members.
 *
 * Example:
 * ---
 * enum Employee {jhon, steve, sophia, douglas, clarice, mitch}
 * alias Team = EnumSet!(Employee, Set8);
 * auto team1 = Team(Employee.jhon, Employee.sophia, Employee.douglas);
 * auto team2 = Team(Employee.jhon, Employee.clarice, Employee.mitch);
 * if (Employee.sophia in team1) writeln("Sophia works in team1");
 * Team overzealous = team1.intersection(team2);
 * if (overzealous != 0)
 * {
 *     writeln(overzealous, " work(s) too much !");
 *     team1 -= overzealous;
 *     assert(team1 == [Employee.sophia, Employee.douglas]);
 * }
 * if (team1.memberCount != team2.memberCount)
 *     writeln("teams are not well balanced !");
 * ---
 */
struct EnumSet(E, S)
if (enumFitsInSet!(E, S))
{

    alias SetType = S;
    alias EnumSetType = typeof(this);

    pure: @safe:

private:

    SetType _container;
    static immutable SetType _max;
    static immutable EnumRankInfo!E _infs;
    static immutable SetType _1 = cast(SetType) 1;

    struct Range
    {
        private SetType frontIndex;
        private SetType backIndex;
        private EnumSet!(E,S) set;
        private alias infs = EnumRankInfo!E;

        private void toNextMember() nothrow @nogc
        {
            while (!set[infs[cast(size_t) frontIndex]])
            {
                ++frontIndex;
                if (frontIndex == infs.count)
                    break;
            }
        }

        private void toPrevMember() nothrow @nogc
        {
            while (!set[infs[cast(size_t)backIndex]])
            {
                --backIndex;
                if (backIndex == 0)
                    break;
            }
        }

        this(T)(T t)
        {
            set = SetType(t);
            backIndex = cast(set.SetType) infs.count;
            toNextMember;
            toPrevMember;
        }

        bool empty() nothrow @nogc
        {
            return set.none;
        }

        E front() nothrow  @nogc
        in
        {
            assert(frontIndex >= 0);
            assert(frontIndex <= infs.max);
        }
        body
        {
            return infs[cast(size_t) frontIndex];
        }

        void popFront() nothrow @nogc
        in
        {
            assert(frontIndex >= 0);
            assert(frontIndex <= infs.max);
        }
        body
        {
            set.exclude(infs[cast(size_t) frontIndex]);
            toNextMember;
        }

        E back() nothrow @nogc
        in
        {
            assert(backIndex >= 0 && backIndex <= infs.max);
        }
        body
        {
            return infs[cast(size_t) backIndex];
        }

        void popBack() nothrow @nogc
        in
        {
            assert(backIndex >= 0 && backIndex <= infs.max);
        }
        body
        {
            set.exclude(infs[cast(size_t) backIndex]);
            toPrevMember;
        }

        auto save() nothrow @nogc
        {
            return Range(set._container);
        }
    }

public:

// constructors ---------------------------------------------------------------+

    ///
    static this() nothrow @nogc
    {
        foreach(i, member; EnumMembers!E)
            _max +=  _1 << i;
    }

    /**
     * Initializes the set with some stuff.
     * Params:
     * stuff = either some E member(s), an E input range, an E array
     * or a string representation.
     */
    this(Stuff...)(Stuff stuff)
    {
        _container = 0;
        static if (stuff.length == 1)
        {
            alias T = typeof(stuff[0]);
            static if (is(T == SetType)) _container = stuff[0];
            else static if (is(T == E)) include(stuff[0]);
            else static if (is(T == E[])) foreach(s; stuff) include(s);
            else static if (is(T == string)) fromString(stuff[0]);
            else static if (isInputRange!T && is(ElementType!T == E))
                foreach(E e;stuff[0]) include(e);
            else static assert(0, "unsupported _ctor argument");
        }
        else include(stuff);
    }
// -----------------------------------------------------------------------------
// string representation ------------------------------------------------------+

    /**
     * Returns the string representation of the set as a binary representation.
     * Note that the result is preffixed with "0b", as a binary litteral.
     */
    string asBitString() const nothrow
    {
        static immutable char[2] bitsCh = ['0', '1'];
        string result = "";
        foreach_reverse(member; EnumMembers!E)
            result ~= bitsCh[isIncluded(member)];

        return "0b" ~ result;
    }

    /**
     * Returns the string representation of the set.
     * The format is the same as the one used in this() and fromString(),
     * similar to an array litteral.
     */
    string toString() const
    {
        import std.conv: to;
        scope(failure){}
        string result = "[";
        bool first;
        foreach(i, member; EnumMembers!E)
        {
            if ((!first) & (isIncluded(member)))
            {
                result ~= to!string(member);
                first = true;
            }
            else if (isIncluded(member))
                result ~= ", " ~ to!string(member);
        }
        return result ~ "]";
    }

    /**
     * Defines the set with a string representation.
     * Params:
     * str = a string representing one or several E members. It must have the
     * form that's similar to an array litteral. Binary litterals are not handled
     * by this function.
     */
    void fromString(const(char)[] str) nothrow @trusted
    {
        import std.conv: to;
        if (str.length < 2)
            return;

        _container = 0;
        if (str == "[]")
            return;

        char[] identifier;
        const(char)* reader = str.ptr;
        while(true)
        {
            if (*reader != ',' && *reader != '[' && *reader != ']' &&
                *reader != ' ') identifier ~= *reader;

            if (*reader == ',' || *reader == ']')
            {
                try
                {
                    auto member = to!E(identifier);
                    include(member);
                }
                catch (Exception e) break;
                identifier = identifier.init;
            }

            if (reader == str.ptr + str.length)
                break;

            ++reader;
        }
    }
// -----------------------------------------------------------------------------
// operators ------------------------------------------------------------------+

    /**
     * Support for the assignment operator.
     * Params:
     * rhs = a setXX, an array of E members, an InputRange of E
     * or an EnumSet with the same type.
     */
    void opAssign(const S rhs) nothrow @nogc
    {
        _container = (rhs <= _max) ? rhs : _max;
    }

    /// ditto
    void opAssign(const E[] rhs) nothrow
    {
        _container = 0;
        foreach(elem; rhs)
            include(elem);
    }

    /// ditto
    void opAssign()(auto ref const EnumSetType rhs)
    {
        _container = rhs._container;
    }

    /// ditto
    void opAssign(R)(auto ref R rhs)
    if (!(isArray!R) && isInputRange!R && is(ElementType!R == E))
    {
        _container = 0;
        foreach(E e; rhs) include(e);
    }

    /**
     * Support for the array syntax.
     * Params:
     * index = either an unsigned integer or an E member.
     */
    bool opIndex(I)(I index) const nothrow
    {
        static if (isSigned!I || is(I == SetType))
            return (_container == (_container | _1 << index));
        else static if (is(I == E))
            return isIncluded(index);
        else static assert(0, "opIndex not implemented when indexer is " ~ I.stringof);
    }

    /// Support for the array assignment syntax.
    void opIndexAssign(bool value, E index) nothrow @nogc
    {
        set(index, value);
    }

    /**
     * Support for "+" and "-" operators.
     * Params:
     * rhs = either an E member, an E array or another EnumSet (or its container)
     * with the same type.
     */
    EnumSetType opBinary(string op)(E rhs) nothrow
    {
        static if (op == "+")
        {
            EnumSetType s = EnumSetType(_container);
            s.include(rhs);
            return s;
        }
        else static if (op == "-")
        {
            EnumSetType s = EnumSetType(_container);
            s.exclude(rhs);
            return s;
        }
        else static assert(0, "opBinary not implemented for " ~ op);
    }

    /// ditto
    EnumSetType opBinary(string op)(E[] rhs) nothrow
    {
        static if (op == "+")
        {
            EnumSetType s = EnumSetType(_container);
            s.include(rhs);
            return s;
        }
        else static if (op == "-")
        {
            EnumSetType s = EnumSetType(_container);
            s.exclude(rhs);
            return s;
        }
        else static assert(0, "opBinary not implemented for " ~ op);
    }

    /// ditto
    EnumSetType opBinary(string op)(EnumSetType rhs) nothrow @nogc
    {
        static if (op == "+")
        {
            SetType s = _container | rhs._container;
            return EnumSetType(s);
        }
        else static if (op == "-")
        {
            SetType s = _container;
            s &= s ^ rhs._container;
            return EnumSetType(s);
        }
        else static if (op == "^")
        {
            return difference(rhs);
        }
        else static if (op == "&")
        {
            return intersection(rhs);
        }
        else static assert(0, "opBinary not implemented for " ~ op);
    }

    /// ditto
    EnumSetType opBinary(string op)(SetType rhs) nothrow @nogc
    {
        static if (op == "+")
        {
            SetType s = _container | rhs;
            return EnumSetType(s);
        }
        else static if (op == "-")
        {
            SetType s = _container;
            s &= s ^ rhs;
            return EnumSetType(s);
        }
        else static if (op == "^")
        {
            return difference(EnumSetType(rhs));
        }
        else static if (op == "&")
        {
            return intersection(EnumSetType(rhs));
        }
        else static assert(0, "opBinary not implemented for " ~ op);
    }

    /**
     * Support for "+=" and "-=" operators.
     * Params:
     * rhs = either some E member(s), an E array or an EnumSet with the same type.
     */
    void opOpAssign(string op)(E[] rhs) nothrow
    {
        static if (op == "+") include(rhs);
        else static if (op == "-") exclude(rhs);
        else static assert(0, "opOpAssign not implemented for " ~ op);
    }

    /// ditto
    void opOpAssign(string op, E...)(E rhs) nothrow
    {
        static if (op == "+") include(rhs);
        else static if (op == "-") exclude(rhs);
        else static assert(0, "opOpAssign not implemented for " ~ op);
    }

    /// ditto
    void opOpAssign(string op)(EnumSetType rhs) nothrow @nogc
    {
        static if (op == "+") _container |= rhs._container;
        else static if (op == "-") _container &= _container ^ rhs._container;
        else static assert(0, "opOpAssign not implemented for " ~ op);
    }

    // selected for Set8, Set16 and Set32
    private size_t toHashImpl()(uint x) const nothrow @nogc
    {
        x = ((x >> 16) ^ x) * 0x119de1f3U;
        x = ((x >> 16) ^ x) * 0x119de1f3U;
        x = (x >> 16) ^ x;
        return cast(size_t) x;
    }

    // selected for Set64
    private size_t toHashImpl()(ulong x) const nothrow @nogc
    {
        x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9UL;
        x = (x ^ (x >> 27)) * 0x94d049bb133111ebUL;
        x = x ^ (x >> 31);
        return cast(size_t) x;
    }

    /// Support for built-in AA.
    size_t toHash() const nothrow @nogc
    {
        return toHashImpl(_container);
    }

    /// Support for comparison "=" and "!=" operators.
    bool opEquals(T)(T rhs) const nothrow
    {
        static if (is(T == SetType))
            return (_container == rhs);
        else static if (isIntegral!T && T.max >= SetType.max)
            return (_container == rhs);
        else static if (is(T == EnumSetType))
            return (_container == rhs._container);
        else static if (is(T == E[])){
            auto rhsset = EnumSetType(rhs);
            return (rhsset._container == _container);
        }
        else
            static assert(0, "opEquals not implemented when rhs is " ~ T.stringof);
    }

    /// Support for built-in AA.
    bool opEquals(ref const EnumSetType rhs) const nothrow @nogc
    {
        return (rhs._container == _container);
    }

    /// see range()
    Range opSlice() const nothrow
    {
        return Range(_container);
    }

    /**
     * Support for the in operator.
     *
     * Indicates if the right hand side is included in the set.
     * Params:
     * rhs = either an E member or a set (or its container) with the same type,
     * in the last case, calling opIn_r is equivalent to test for greater or equal.
     */
    bool opIn_r(T)(T rhs) const nothrow
    {
        static if (is(T == E))
            return isIncluded(rhs);
        else static if (is(T == EnumSetType))
            return (_container & rhs._container) >= rhs._container;
        else static if (is(T == SetType))
            return (_container & rhs) >= rhs;
        else
            static assert(0, "opIn_r not implemented when rhs is " ~ T.stringof);
    }
// -----------------------------------------------------------------------------
// set operations -------------------------------------------------------------+

    /**
     * Returns a set representing the difference between this set and the argument.
     * Params:
     * rhs = either a set with the same type or a set container with the same size.
     */
    EnumSetType difference(R)(R rhs) const nothrow
    if (is(R == EnumSetType) || is(R == SetType))
    {
        SetType s;
        static if (is(R == EnumSetType))
            s = _container ^ rhs._container;
        else
            s = _container ^ rhs;
        return EnumSetType(s);
    }

    /**
     * Returns a set representing the intersection between this set and the argument.
     * Params:
     * rhs = either a set with the same type or a set container with the same size.
     */
    EnumSetType intersection(R)(R rhs) const nothrow
    if (is(R == EnumSetType) || is(R == SetType))
    {
        SetType s;
        static if (is(R == EnumSetType))
            s = _container & rhs._container;
        else
            s = _container & rhs;
        return EnumSetType(s);
    }
// -----------------------------------------------------------------------------
// Pascal-ish primitives ------------------------------------------------------+


    /**
     * Sets the bit of a particular member
     */
    void set(E member, bool value) @nogc nothrow
    {
        _container ^= (ubyte(value * 255) ^ _container) & (1 << _infs[member]);
    }

    /**
     * Includes someMembers in the set.
     * This is the primitive used for to the operator "+".
     * Params:
     * someMembers = a list of E members or an array of E members
     */
    void include(E...)(E someMembers) nothrow
    {
        static if (someMembers.length == 1)
            _container |= _1 << _infs[someMembers];
        else foreach(member; someMembers)
            _container |= _1 << _infs[member];
    }

    /// ditto
    void include(E[] someMembers) nothrow
    {
        foreach(member; someMembers)
            _container |= _1 << _infs[member];
    }

    /**
     * Excludes someMembers from the set.
     * This is the primitive used for to the operator "-".
     * Params:
     * someMembers = a list of E members or an array of E members.
     */
    void exclude(E...)(E someMembers) nothrow
    {
        static if (someMembers.length == 1)
            _container &= ~(_1 << _infs[someMembers]);
        else foreach(member; someMembers)
            _container &= ~(_1 << _infs[member]);
    }

    /// ditto
    void exclude(E[] someMembers) nothrow
    {
        foreach(member; someMembers)
            _container &= ~(_1 << _infs[member]);
    }

    /**
     * Returns true if aMember is in the set.
     * This is the primitive used for to the operator "in".
     * Params:
     * aMember = an E member.
     */
    bool isIncluded(E aMember) const nothrow
    {
        return (_container >> _infs[aMember]) & 1;
    }
//------------------------------------------------------------------------------
// misc helpers ---------------------------------------------------------------+

    /// Returns a range allowing to iterate for each member included in the set.
    Range range() const nothrow
    {
        return Range(_container);
    }

    /// Returns true if the set is empty.
    bool none() const nothrow @nogc
    {
        return _container == 0;
    }

    /// Returns true if at least one member is included.
    bool any() const nothrow @nogc
    {
        return _container != 0;
    }

    /// Returns true if all the members are included.
    bool all() const nothrow @nogc
    {
        return _container == _max;
    }

    /// Returns the maximal value the set can have.
    static const(S) max() nothrow @nogc
    {
        return _max;
    }

    /// Returns a lookup table that can be used to retrieve the rank of a member.
    static ref const(EnumRankInfo!E) rankInfo() nothrow @nogc
    {
        return _infs;
    }

    /// Returns the enum count
    static const(S) memberCount() nothrow @nogc
    {
        return cast(S) rankInfo.count;
    }

    /// Returns the enum count
    ref const(SetType) container() const nothrow @nogc
    {
        return _container;
    }

    /// Implements the iz.rtti "text struct" traits to allow the deserialization.
    void loadFromText(const(char)[] value)
    {
        fromString(value);
    }

    /// Implements the iz.rtti "text struct" traits to allow the serialization.
    const(char)[] saveToText()
    {
        return toString;
    }

//------------------------------------------------------------------------------
}

/**
 * Aliases the smallest set in which E fits.
 */
template SmallestSet(E)
if (enumFitsInSet!(E, BigestSet))
{
    static if (enumFitsInSet!(E, Set8))
        alias SmallestSet = Set8;
    else static if (enumFitsInSet!(E, Set16))
        alias SmallestSet = Set16;
    else static if (enumFitsInSet!(E, Set32))
        alias SmallestSet = Set32;
    else static if (enumFitsInSet!(E, Set64))
        alias SmallestSet = Set64;
    else static if (hasInt128 && enumFitsInSet!(E, Set128))
        alias SmallestSet = Set128;
}

/**
 * Returns an EnumSet using the smallest container possible.
 * Params:
 * E = an enum
 * a = the parameters passed to the EnumSet constructor.
 */
auto enumSet(E, A...)(A a) @property
if (enumFitsInSet!(E, BigestSet))
{
    return EnumSet!(E, SmallestSet!E)(a);
}

/**
 * Alias the members of an enum, allowing to use them without the parent name,
 * and making the use of EnumSet more friendly.
 *
 * Params:
 *      E = The named enum to alias.
 *      prefix = The prefix of the aliases.
 */
mixin template AliasedEnumMembers(E, string prefix = "")
if (is(E == enum))
{
    string genAliasedEnumMembers()
    {
        string result;
        foreach(em; EnumMembers!E)
        {
            import std.conv : to;
            result ~= "alias " ~ prefix ~ to!string(em) ~ " = " ~
                E.stringof ~ "." ~ to!string(em) ~ ";";
        }
        return result;
    }
    mixin(genAliasedEnumMembers());
}
///
unittest
{
    enum Option {Option1, Option2}
    mixin AliasedEnumMembers!(Option, "o");
    static assert(oOption1 is Option.Option1);

    alias Options = EnumSet!(Option, Set8);
    Options opts = Options(oOption1);
}

/// returns true if T and E are suitable for constructing an EnumProcs
bool isCallableFromEnum(T, E)()
{
    return ((is(E==enum)) & (isCallable!T));
}

/**
 * CallTable based on an enum. It can be compared to an associative array of type E[T].
 * Additionally an EnumSet can be used to fire a burst of call.
 * Params:
 * E = an enum.
 * T = a callable type.
 */
struct EnumProcs(E,T)
if (isCallableFromEnum!(T,E))
{

@safe nothrow pure:

private:

    static immutable EnumRankInfo!E _infs;
    alias retT = ReturnType!T;
    enum procLen = enumMemberCount!E;
    T[procLen] _procs;

public:

// constructors ---------------------------------------------------------------+

    /**
     * Constructs an EnumProcs with a set of T.
     * Params:
     * a = a list of T.
     */
    this(A...)(A a)
    in
    {
        static assert(a.length == enumMemberCount!E);
        foreach(callable; a)
            static assert(is(A[0] == T));
    }
    body
    {
        foreach(immutable i, item; a)
            _procs[i] = item;
    }

    /**
     * Constructs an EnumProcs with an array of T.
     * Params:
     * someItems = an array of T.
     */
    this(T[] someItems) @nogc
    in
    {
        assert(someItems.length == enumMemberCount!E);
        assert(is(typeof(someItems[0]) == T));
    }
    body
    {
        foreach(i, item; someItems)
        {
            _procs[i] = someItems[i];
        }
    }
//------------------------------------------------------------------------------
// operators ------------------------------------------------------------------+

    /**
     * opIndex allows a more explicit call syntax than opCall.myStuffs[E.member](params).
     */
    const(T) opIndex(E aMember) @nogc
    {
        return _procs[_infs[aMember]];
    }

//------------------------------------------------------------------------------
// call -----------------------------------------------------------------------+

    /**
     * Calls the function matching to selector rank.
     * Params:
     * selector = an E member.
     * prms = arguments for calling the function.
     * Returns: a value of type ReturnType!T.
     */
    retT opCall(CallParams...)(E selector, CallParams prms) @nogc
    {
        return _procs[_infs[selector]](prms);
    }

    /**
     * Calls the functions matching to a set of selectors.
     * Params:
     * selectors = a set of E.
     * prms = common or selector-sepcific arguments for calling the functions.
     * Returns: an array representing the result of each selector, by rank.
     */
    retT[] opCall(BS,CallParams...)(BS selectors, CallParams prms)
    if  (   (is(BS == EnumSet!(E, Set8)))
        ||  (is(BS == EnumSet!(E, Set16)))
        ||  (is(BS == EnumSet!(E, Set32)))
        ||  (is(BS == EnumSet!(E, Set64)))
        ||  (is(BS == EnumSet!(E, BigestSet)))
        )
    {
        retT[] result;
        result.length = cast(size_t) enumMemberCount!E;

        static if (!CallParams.length)
        {
            foreach(immutable i; 0 .. selectors.memberCount)
            {
                if (selectors[i])
                    result[i] = _procs[i]();
            }
            return result;
        }
        else static if (!isArray!(CallParams[0]))
        {
            foreach(immutable i; 0 .. selectors.memberCount)
            {
                if (selectors[i])
                    result[i] = _procs[i](prms);
            }
            return result;
        }
        else
        {
            foreach(immutable i; 0 .. selectors.memberCount)
            {
            // Hard to believe it works ! A unittest HAS to show it can fail.
                if (selectors[i])
                    result[i] = _procs[i](prms[0][i]);
            }
            return result;
        }
    }
//------------------------------------------------------------------------------
// misc. ----------------------------------------------------------------------+

    /// Returns the array of callable for additional containers operations.
    ref T[procLen] procs() @nogc
    {
        return _procs;
    }
//------------------------------------------------------------------------------

}

/**
 * Encapsulates an array of T and uses the rank of the enum members
 * E to perform the actions usually done with integer indexes.
 */
struct EnumIndexedArray(E,T,bool staticArray = false)
if (is(E==enum))
{

pure nothrow @safe:

private:

    static if (staticArray)
    {
        enum arrayLen = enumMemberCount!E;
        alias arrayT = T[arrayLen];
    }
    else
    {
        alias arrayT = T[];
    }
    arrayT _array;
    static immutable EnumRankInfo!E _infs;

public:

    /// Returns the length of the internal container.
    size_t length()
    {
        return _array.length;
    }

    /// Returns the length of the internal container.
    size_t opDollar()
    {
        return length;
    }

    /**
     * Sets the array length using a standard integer value.
     * Unless bounds checking is turned off, the parameter is dynamically
     * checked according to E highest rank.
     */
    static if (!staticArray)
    void length(size_t aValue)
    in
    {
        assert(aValue <= _infs.count);
    }
    body
    {
        _array.length = aValue;
    }

    /**
     * Sets the array length according to the value following aMember rank.
     */
    static if (!staticArray)
    void length(E aMember)
    {
        _array.length = _infs[aMember] + 1;
    }

    /**
     * Returns the value of the slot indexed by aMember rank.
     */
    T opIndex(E aMember)
    {
        return _array[_infs[aMember]];
    }

    /**
     * Sets the slot indexed by aMember rank to aValue.
     */
    void opIndexAssign(T aValue,E aMember)
    {
        _array[_infs[aMember]] = aValue;
    }

    /**
     * Returns a T slice using loMember and hiMember ranks to define the range.
     */
    T[] opSlice(E loMember, E hiMember)
    in
    {
        assert(_infs[loMember] <= _infs[hiMember]);
    }
    body
    {
        return _array[_infs[loMember].._infs[hiMember]];
    }

    /**
     * Returns a reference to the the internal container.
     */
    ref const(arrayT) array()
    {
        return _array;
    }
}

version(unittest)
{
    enum a0;
    enum a4     {a0,a1,a2,a3}
    enum a8     {a0,a1,a2,a3,a4,a5,a6,a7}
    enum a9     {a0,a1,a2,a3,a4,a5,a6,a7,a8}
    enum a16    {a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15}
    enum a17    {a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16}
    enum b0     {b0 = 8, b1 = 1, b2 = 17}

    /// Constraints
    pure unittest
    {
        static assert( isSetSuitable!ubyte );
        static assert( isSetSuitable!ushort );
        static assert( isSetSuitable!uint );
        static assert( isSetSuitable!ulong );
        static assert( !isSetSuitable!byte );
    }

    pure unittest
    {
        static assert( enumFitsInSet!(a8, Set8));
        static assert( !enumFitsInSet!(a9, Set8));
        static assert( enumFitsInSet!(a16, Set16));
        static assert( !enumFitsInSet!(a17, Set16));
        static assert( !enumFitsInSet!(a0, Set64));
    }

    /// CTFE
    pure @safe unittest
    {
        static assert(EnumSet!(a8, Set8)(a8.a0,a8.a1) == 0b00000011);
        enum set = EnumSet!(a8, Set8)(a8.a0,a8.a1);
        static assert(set == 0b011, set);
        static assert(EnumRankInfo!a8[0] == a8.a0);
        static assert(EnumRankInfo!a8[a8.a1] == 1);
        static assert(EnumRankInfo!b0[1] == b0.b1);
        static assert(EnumRankInfo!b0[b0.b2] == 2);
    }

    /// EnumSet
    pure nothrow @safe unittest
    {
        alias bs8 = EnumSet!(a8, Set8);
        bs8 set = bs8(a8.a0,a8.a1,a8.a2,a8.a3,a8.a4,a8.a5,a8.a6,a8.a7);
        assert(set == 0b1111_1111);
        assert(set.all);
        set = set - a8.a0;
        assert(set == 0b1111_1110);
        set = set - a8.a1;
        assert(set == 0b1111_1100);
        set = set - a8.a7;
        assert(set == 0b0111_1100);
        set = 0;
        assert(set.none);
        assert(set == 0b0000_0000);
        set = set + a8.a4;
        assert(set == 0b0001_0000);
        set = set + a8.a5;
        assert(set != 0b0001_0000);
        assert(set.any);
        set = 0;
        assert(set == 0b0000_0000);
        set = set + [a8.a0, a8.a1, a8.a3];
        assert(set == 0b0000_1011);
        set = set - [a8.a1, a8.a3];
        assert(set == 0b0000_001);
        set = set.max;
        set = set - [a8.a5,a8.a6,a8.a7];
        assert(set == [a8.a0,a8.a1,a8.a2,a8.a3,a8.a4]);
        set = [a8.a0,a8.a2,a8.a4];
        assert(set == 0b0001_0101);
        set = [a8.a0,a8.a1];
        assert(set == 0b0000_0011);
        set += [a8.a2,a8.a3];
        assert(set == 0b0000_1111);
        set -= [a8.a0,a8.a1];
        assert(set == 0b0000_1100);
        set = 0;
        set.exclude([a8.a0,a8.a1,a8.a2,a8.a3,a8.a4]);
        assert( set == 0);
        set -= a8.a0;
        assert( set == 0);

        bs8 set1 = bs8(a8.a0,a8.a1);
        bs8 set2 = bs8(a8.a1,a8.a2);
        set1 += set2;
        assert( set1 == 0b0000_0111);
        set1 = bs8(a8.a0,a8.a1,a8.a2);
        set2 = bs8(a8.a1,a8.a2,a8.a3);
        set1 -= set2;
        assert( set1 == 0b0000_0001);

        set1 = bs8(a8.a0);
        set2 = bs8(a8.a1);
        auto set3 = set1 + set2;
        assert(set3 == 0b0000_0011);
        assert(set1 == 0b0000_0001);
        assert(set2 == 0b0000_0010);
        set2.set(a8.a1, false),
        assert(set2 == 0b0000_0000);
        set2.set(a8.a0, true);
        set2.set(a8.a1, true);
        set2.set(a8.a3, true);
        assert(set2 == 0b0000_1011);
        set2.set(a8.a3, false);
        assert(set2 == 0b0000_0011);
        set2[a8.a4] = true;
        assert(set2 == 0b0001_0011);
    }

    pure nothrow @safe unittest
    {
        EnumSet!(a17, Set32) set;
        set.include(a17.a8,a17.a9);
        assert(!set.isIncluded(a17.a7));
        assert(set.isIncluded(a17.a8));
        assert(set.isIncluded(a17.a9));
        assert(!set.isIncluded(a17.a10));
        assert(!(a17.a7 in set));
        assert(a17.a8 in set);
        assert(a17.a9 in set);
        assert(!(a17.a10 in set));
        set = 0;
        set += [a17.a5, a17.a6, a17.a7];
        EnumSet!(a17, Set32) set2;
        set2 += [a17.a5,a17.a6];
        assert(set2 in set);
        set -= [a17.a5];
        assert(!(set2 in set));
        set2 -= [a17.a5];
        assert(set2 in set);
    }

    pure nothrow @safe @nogc unittest
    {
        auto bs = EnumSet!(a17, Set32)(a17.a0, a17.a1, a17.a16);
        assert(bs[0]);
        assert(bs[1]);
        assert(bs[16]);
        assert(bs[a17.a0]);
        assert(bs[a17.a1]);
        assert(bs[a17.a16]);
        assert(!bs[8]);
        assert(!bs[a17.a8]);
    }

    pure @safe unittest
    {
        EnumSet!(a8, Set8) set = EnumSet!(a8, Set8)(a8.a3, a8.a5);
        assert(set == 0b0010_1000);
        auto rep = set.toString;
        set = 0;
        assert(set == 0);
        set = EnumSet!(a8, Set8)(rep);
        assert(set == 0b0010_1000);

        // test asBitString
        auto brep = set.asBitString;
        assert( brep == "0b00101000", brep );
        set = 0b1111_0000;
        brep = set.asBitString;
        assert( brep == "0b11110000", brep );

        // err
        assert(set != 0);
        set.fromString("[56346");
        assert(set == 0);

        //set = 0;
        //set = to!Set8(brep);
        //assert(set == 0b1111_0000);
    }

    pure nothrow @safe @nogc unittest
    {
        auto set = EnumSet!(a17, Set32)(a17.a0);
        assert( set.rankInfo[a17.a16] == 16);
        assert( set.rankInfo[a17.a15] == 15);
    }

    pure nothrow @safe unittest
    {
        import std.range;

        static assert(isInputRange!((EnumSet!(a17, Set32)).Range));
        static assert(isForwardRange!((EnumSet!(a17, Set32)).Range));
        static assert(isBidirectionalRange!((EnumSet!(a17, Set32)).Range));

        auto set = EnumSet!(a17, Set32)(a17.a0, a17.a8 , a17.a16);
        auto rng = set.range;
        assert(rng.front == a17.a0);
        rng.popFront;
        assert(rng.front == a17.a8);
        rng.popFront;
        assert(rng.front == a17.a16);
        rng.popFront;
        assert(rng.empty);

        with (a17) set = [a0, a8, a16, a13];
        size_t i;
        foreach(a17 a; set.range) {++i;}
        assert(i == 4);

        // bidir & forward ranges are not that usefull since an EnumSet is ordered
        import std.algorithm;
        with (a17) set = [a8, a16, a13];
        assert(startsWith(set.range, a17.a8));
        with (a17) set += [a2, a4];
        assert(startsWith(set[], a17.a2));

        auto set1 = set;
        set1 = 0;
        assert(set1.none);
        set1 = set.range;
        assert(set1 == set);

        auto tes = EnumSet!(a17, Set32)(a17.a0, a17.a1, a17.a8 , a17.a16);
        auto gnr = tes.range;
        assert(gnr.back == a17.a16);
        gnr.popBack;
        assert(gnr.back == a17.a8);
        auto sav = gnr.save;
        gnr.popBack;
        sav.popBack;
        assert(gnr.back == sav.back);
        sav.popBack;
        assert(gnr.back != sav.back);
    }

    pure nothrow @safe @nogc unittest
    {
        enum E {e1, e2}
        alias ESet = EnumSet!(E, Set8);
        ESet eSet1 = ESet(E.e1);
        ESet eSet2 = ESet(E.e1, E.e2);
        assert(eSet1 != eSet2);
        eSet2 -= E.e2;
        assert(eSet1 == eSet2);
    }

    pure nothrow @safe unittest
    {
        alias Set = EnumSet!(a8, Set8);
        Set set1 = Set([a8.a0, a8.a1]);
        Set set2 = Set([a8.a1, a8.a3]);
        assert(set1.intersection(set2) == Set(a8.a1));
        assert(set1.difference(set2) == Set([a8.a0,a8.a3]));
        set1 = 0b1010_1010;
        assert(set1.intersection(cast(ubyte)0b0000_1010) == 0b0000_1010);
        assert(set1.difference(cast(ubyte)0b0000_1010) == 0b1010_0000);

        set1 = Set([a8.a0, a8.a1]);
        assert((set1 & cast(Set8)0b1110) == Set(a8.a1));
        assert((set1 ^ cast(Set8)0b1010) == Set([a8.a0,a8.a3]));
    }

    pure nothrow @safe unittest
    {
        enum E {e1, e2}
        alias ESet = EnumSet!(E, Set8);

        ESet eSet1;
        ESet eSet2 = [E.e1];
        ESet eSet3 = [E.e2];
        ESet eSet4 = [E.e1, E.e2];

        string[ESet] setDescription;

        setDescription[eSet1] = "empty";
        setDescription[eSet2] = "e1";
        setDescription[eSet3] = "e2";
        setDescription[eSet4] = "e1 and e2";

        // AA with EnumSet as key is about the set value, not the instance.
        assert( setDescription[* new ESet] == "empty");
        ESet eSet5 = [E.e1, E.e2];
        assert( setDescription[eSet5] == "e1 and e2");
        eSet5 -= E.e1;
        assert( setDescription[eSet5] == "e2");
        eSet5 -= E.e2;
        assert( setDescription[eSet5] == "empty");
        eSet5 -= E.e2; eSet5 -= E.e1;
        assert( setDescription[eSet5] == "empty");
    }

    /// enumSet
    pure nothrow @safe unittest
    {
        assert( is(typeof(enumSet!a4) == EnumSet!(a4,Set8)) );
        assert( is(typeof(enumSet!a8) == EnumSet!(a8,Set8)) );
        assert( is(typeof(enumSet!a9) == EnumSet!(a9,Set16))) ;
        assert( is(typeof(enumSet!a16) == EnumSet!(a16,Set16)) );
        assert( is(typeof(enumSet!a17) == EnumSet!(a17,Set32)) );
    }

    /// EnumProcs
    pure nothrow @safe unittest
    {
        enum A {t1=8,t2,t3}
        void At1(){}
        void At2(){}
        void At3(){}

        auto ACaller = EnumProcs!(A, typeof(&At1))(&At1,&At2,&At3);

        int Bt1(int p){return 10 + p;}
        int Bt2(int p){return 20 + p;}
        int Bt3(int p){return 30 + p;}
        auto BCaller = EnumProcs!(A, typeof(&Bt1))([&Bt1,&Bt2,&Bt3]);
        assert( BCaller.procs[0]== &Bt1);
        assert( BCaller.procs[1]== &Bt2);
        assert( BCaller.procs[2]== &Bt3);
        assert( BCaller(A.t1, 1) == 11);
        assert( BCaller(A.t2, 2) == 22);
        assert( BCaller(A.t3, 3) == 33);
        assert( BCaller[A.t1](2) == 12);
        assert( BCaller[A.t2](3) == 23);
        assert( BCaller[A.t3](4) == 34);

        auto bs = EnumSet!(A, Set8)();
        bs.include(A.t1,A.t3);

        auto arr0 = BCaller(bs,8);
        assert(arr0[0] == 18);
        assert(arr0[1] == 0);
        assert(arr0[2] == 38);

        bs.include(A.t2);
        auto arr1 = BCaller(bs,[4,5,6]);
        assert(arr1[0] == 14);
        assert(arr1[1] == 25);
        assert(arr1[2] == 36);

        int Ct1(int[2] p){return p[0] + p[1];}
        int Ct2(int[2] p){return p[0] * p[1];}
        int Ct3(int[2] p){return p[0] - p[1];}
        auto CCaller = EnumProcs!(A, typeof(&Ct1))(&Ct1,&Ct2,&Ct3);
        assert(bs.all);
        auto arr2 = CCaller(bs,[cast(int[2])[2,2],cast(int[2])[3,3],cast(int[2])[9,8]]);
        assert(arr2[0] == 4);
        assert(arr2[1] == 9);
        assert(arr2[2] == 1);

        int Dt1(int p, int c, int m){return 1 + p + c + m;}
        int Dt2(int p, int c, int m){return 2 + p + c + m;}
        int Dt3(int p, int c, int m){return 3 + p + c + m;}
        auto DCaller = EnumProcs!(A, typeof(&Dt1))(&Dt1,&Dt2,&Dt3);
        assert(bs.all);
        auto arr3 = DCaller(bs,1,2,3);
        assert(arr3[0] == 7);
        assert(arr3[1] == 8);
        assert(arr3[2] == 9);
    }

    /// EnumRankInfo
    pure @safe nothrow unittest
    {
        enum E
        {
            e1 = 0.15468,
            e2 = 1256UL,
            e3 = 'A'
        }

        alias infs = EnumRankInfo!E;
        assert(infs.min == 0);
        assert(infs.max == 2);
        assert(infs.count == 3);
        assert(infs[2] == 'A');
        assert(infs[E.e3] == 2);
    }

    /// EnumIndexedArray
    pure @safe nothrow unittest
    {
        enum E {e0 = 1.8,e1,e2,e3 = 888.459,e4,e5,e6,e7}
        alias E_Fp_Indexed = EnumIndexedArray!(E,float);
        E_Fp_Indexed arr;
        assert(arr.length == 0);
        arr.length = EnumRankInfo!E.count;

        foreach(i,memb; EnumMembers!E)
            arr[memb] = 1.0 + 0.1 * i;

        assert(arr[E.e1] == 1.1f);
        assert(arr[E.e0] == 1.0f);

        auto slice = arr[E.e2..E.e4];
        assert(slice == [1.2f,1.3f]);
    }

    pure @safe nothrow @nogc unittest
    {
        enum E {e0, e1}
        alias Arr = EnumIndexedArray!(E, int, true);
        Arr a;
        a[E.e0] = 4;
        a[E.e1] = 5;
        assert(a[E.e0] == 4);
        assert(a[E.e1] == 5);
    }
}

