/**
 * The iz Runtime type informations.
 */
module iz.rtti;

import
    std.format, std.traits, std.meta;
import
    iz.memory, iz.types, iz.properties, iz.enumset, iz.streams, iz.containers;

private __gshared HashMap_AB!(string, const(Rtti)*) _name2rtti;
private __gshared HashMap_AB!(const(Rtti)*, string) _rtti2name;

shared static this()
{
    _name2rtti.reserve(128);
    _rtti2name.reserve(128);
}

/**
 * Enumerates the type constructors
 */
enum TypeCtor
{
    _const,
    _immutable,
    _inout,
    _shared
}

/// Set of TypeCtor.
alias TypeCtors = EnumSet!(TypeCtor, Set8);

/**
 * Enumerates the types supported by the Rtti
 */
enum RtType: ubyte
{
    _invalid,
    _bool, _byte, _ubyte, _short, _ushort, _int, _uint, _long, _ulong,
    _float, _double, _real,
    _char, _wchar, _dchar,
    _object, _struct, _union,
    _enum,
    _funptr,
    _stream,
    _aa,
    _pointer,
}

/**
 * Enumerates the special struct type recognized by the Rtti
 */
enum StructType: ubyte
{
    _none,      /// no special traits.
    _publisher, /// the struct has the traits of a PropertyPublisher.
    _binary,    /// the struct can write and read itself to/from ubyte[].
    _text,      /// the struct can write and read itself to/from char[].
}

private static immutable RtTypeArr =
[
    RtType._invalid,
    RtType._bool, RtType._byte, RtType._ubyte, RtType._short, RtType._ushort,
    RtType._int, RtType._uint, RtType._long, RtType._ulong,
    RtType._float, RtType._double, RtType._real,
    RtType._char, RtType._wchar, RtType._dchar,
    RtType._object, RtType._struct, RtType._union,
    RtType._enum,
    RtType._funptr,
    RtType._stream,
    RtType._aa,
    RtType._pointer
];

/// used as a wildcard to represent any struct.
package struct GenericStruct {}
/// used as a wildcard to represent any union.
package struct GenericUnion {}
/// used as a wildcard to represent any enum.
package struct GenericEnum {int value; alias value this;}
/// not reall used...
package struct GenericFunPtr{}

package alias GenericRtTypes = AliasSeq!(
    void,
    bool, byte, ubyte, short, ushort, int, uint, long, ulong,
    float, double, real,
    char, wchar, dchar,
    Object, GenericStruct, GenericUnion,
    GenericEnum,
    GenericFunPtr,
    Stream,
);

package alias BasicRtTypes = AliasSeq!(
    void,
    bool, byte, ubyte, short, ushort, int, uint, long, ulong,
    float, double, real,
    char, wchar, dchar
);

/**
 * Indicates if $(D T) is a basic runtime type (fixed length, not array, no type identifier)
 */
template isBasicRtType(T)
{
    enum i = staticIndexOf!(T, GenericRtTypes);
    enum isBasicRtType = i > 0 && i <= staticIndexOf!(dchar, GenericRtTypes);
}

/**
 * Indicates the size of a variable according to its RtType
 *
 * Returns:
 *      $(D 0) is returned if the size is variable otherwise the equivalent
 *      of the $(D .sizeof) property.
 */
ubyte size(RtType type)
{
    with(RtType) final switch (type)
    {
        case _invalid:
        case _object:
        case _struct:
        case _funptr:
        case _stream:
        case _enum:
        case _aa:
        case _pointer:
        case _union:
            return 0;
        case _bool, _byte, _ubyte, _char:
            return 1;
        case _short, _ushort, _wchar:
            return 2;
        case _int, _uint, _dchar, _float:
            return 4;
        case _long, _ulong, _double:
            return 8;
        case _real:
            return real.sizeof;
    }
}
///
unittest
{
    struct St {} St st;
    assert(st.getRtti.type.size == 0);
    bool bo;
    assert(bo.getRtti.type.size == 1);
    ushort us;
    assert(us.getRtti.type.size == 2);
    uint ui;
    assert(ui.getRtti.type.size == 4);
    long lo;
    assert(lo.getRtti.type.size == 8);
    real re;
    assert(re.getRtti.type.size == real.sizeof);
}

/**
 * Returns the identifier of a type.
 *
 * Params:
 *      t = Either a RtType or a pointer to a Rtti.
 * Returns:
 *      For the basic types, always the equivalent of the keyword $(D .stringof) property.
 *      For the other types, if $(D t) is a RtType then "struct", "enum", "Object", etc
 *      are returned, otherwise if $(D t) is a pointer to a $(D Rtti) then the type
 *      identifier is returned, e.g "E" for $(D enum E {e}).
 */
string typeString(T)(T t)
{
    static if (is(Unqual!T == RtType))
    {
        with(RtType) final switch (t)
        {
            case _pointer:  return "pointer";
            case _aa:       return "aa";
            case _invalid:  return "invalid";
            case _bool:     return "bool";
            case _byte:     return "byte";
            case _ubyte:    return "ubyte";
            case _short:    return "short";
            case _ushort:   return "ushort";
            case _int:      return "int";
            case _uint:     return "uint";
            case _long:     return "long";
            case _ulong:    return "ulong";
            case _float:    return "float";
            case _double:   return "double";
            case _real:     return "real";
            case _char:     return "char";
            case _wchar:    return "wchar";
            case _dchar:    return "dchar";
            case _object:   return "Object";
            case _struct:   return "struct";
            case _enum:     return "enum";
            case _funptr:   return "funptr";
            case _stream:   return "Stream";
            case _union:    return "union";
        }
    }
    else static if (is(T == const(Rtti)*))
    {
        if (auto s = t in _rtti2name)
            return *s;
        else
            return "";
    }
    else static assert(0);
}

private mixin template setContext()
{
    private auto internalSetContext(bool restore)(void* context) const
    {
        static if (!restore) void* old;
        foreach(member; __traits(allMembers, typeof(this)))
            static if (is(typeof(__traits(getMember, typeof(this), member)) == delegate))
            {
                static if (!restore) old = __traits(getMember, typeof(this), member).ptr;
                __traits(getMember, typeof(this), member).ptr = context;
            }
        static if (!restore) return old;
    }

    /// Sets the context before usage.
    public alias setContext = internalSetContext!false;

    /// Restores the context after usage.
    public alias restoreContext = internalSetContext!true;
}

/**
 * Runtime information for the function pointers.
 */
struct FunPtrInfo
{
    /// Indicates the function type identifier.
    string identifier;
    /// If hasContext is true then it's a delegate, otherwise a pointer to a function.
    bool hasContext;
    /// Indicates the return type.
    const Rtti* returnType;
    /// Contains the Rtti of each parameter.
    const Rtti*[] parameters;
    /// Indicates the size of the pointer.
    ubyte size() const
    {
        if (hasContext) return size_t.sizeof * 2;
        else return size_t.sizeof;
    }
}

/**
 * Runtime information for the classes.
 */
struct ClassInfo
{
    /// Indicates the class type identifier.
    string identifier;
    /// Stores the static address of the default __ctor.
    Object function() constructor;
    /// Stores the static initializer.
    const void[] initialLayout;
    /// Indicates wether the class is a iz.Stream
    bool isStream;
}

/**
 * Runtime information for the enums.
 */
struct EnumInfo
{
    /// Indicates the enum type identifier.
    string identifier;
    /// Contains the identifier of each member.
    string[] members;
    /// Contains the value of each member.
    uint[] values;
    /// Indicates the type of the values.
    const(Rtti)* valueType;
}

/**
 * Runtime information for a struct that can be saved and reloaded from an array
 * of bytes.
 */
struct BinTraits
{
    /// Returns a delegate to the struct's restoreFromBytes() method.
    void delegate(ubyte[]) loadFromBytes;

    /// Returns a delegate to the struct's saveToBytes() method.
    ubyte[] delegate() saveToBytes;

    /// Sets the delegates context.
    mixin setContext;
}

/**
 * Runtime information for a struct that can be saved and reloaded from a string
 */
struct TextTraits
{
    /// Returns a delegate to the struct's restoreFromText() method.
    void delegate(const(char)[]) loadFromText;

    /// Returns a delegate to the struct's saveToText() method.
    const(char)[] delegate() saveToText;

    /// Sets the delegates context.
    mixin setContext;
}

/**
 * Runtime information for a struct that has the traits of a PropertyPublisher.
 *
 * Publishing structs have the ability to be used as any class
 * that implements the PropertyPublisher interface.
 */
struct PubTraits
{
    /// Returns a delegate to the struct's publicationFromName() method.
    GenericDescriptor* delegate(const(char)[]) publicationFromName;

    /// Returns a delegate to the struct's publicationFromIndex() method.
    GenericDescriptor* delegate(size_t) publicationFromIndex;

    /// Returns a delegate to the struct's publicationCount() method.
    size_t delegate() publicationCount;

    /// Sets the delegates context.
    mixin setContext;
}

private union StructTraits
{
    BinTraits binTraits;
    TextTraits textTraits;
    PubTraits pubTraits;
}

/**
 * Runtime information for the structs.
 */
struct StructInfo
{
    /// Constructs the info for a "noname" struct
    this(string identifier, StructType type)
    {
        this.identifier = identifier;
        this.type = type;
    }

    /// Constructs the info for a special struct.
    this(T)(string identifier, StructType type, T t)
    {
        this.identifier = identifier;
        this.type = type;
        static if (is(T == PubTraits))
            structInfo.publisherInfo = t;
        else static if (is(T == TextTraits))
            structInfo.textInfo = t;
        else static if (is(T == BinTraits))
            structInfo.binaryInfo = t;
        else static assert(0, "third argument of Rtti ctor must be an Info");
    }

    /// Indicates the struct type identifier.
    string identifier;

    /// Indicates the special struct type.
    StructType type;

    /// The information for the structure.
    StructTraits structSpecialInfo;

    /// Returns: The struct information when structType is equal to StructType._binary
    const(BinTraits)* binTraits() const
    {
        return &structSpecialInfo.binTraits;
    }

    /// Returns: The struct information when structType is equal to StructType._text
    const(TextTraits)* textTraits() const
    {
        return &structSpecialInfo.textTraits;
    }

    /// Returns the struct information when structType is equal to StructType._publisher
    const(PubTraits)* pubTraits() const
    {
        return &structSpecialInfo.pubTraits;
    }
}

/**
 * Runtime information for the unions.
 */
struct UnionInfo
{
    this(A...)(string identifier, const(Rtti)*[] members)
    {
        membersType = members;
        this.identifier = identifier;
    }

    /// Indicates the union type identifier.
    string identifier;

    /// An array with the Rtti of each member
    const(Rtti)*[] membersType;
}

/**
 * Runtime information for the associative arrays
 */
struct AAInfo
{
    /// A pointer to the Rtti of the keys.
    const(Rtti)* keyType;
    /// A pointer to the Rtti of the values.
    const(Rtti)* valueType;
}

/**
 * Runtime information for the pointers
 */
struct PointerInfo
{
    /// A pointer to the Rtti of the target.
    const(Rtti)* type;
}

private union Infos
{
    FunPtrInfo funptrInfo;
    ClassInfo classInfo;
    EnumInfo enumInfo;
    StructInfo structInfo;
    AAInfo aaInfo;
    PointerInfo pointerInfo;
    UnionInfo unionInfo;
}

/**
 * Runtime type information
 */
struct Rtti
{
    /// Constructs a Rtti with a type and the info for this type.
    this(T)(RtType type, ubyte dim, TypeCtors typeCtors, auto ref T t)
    {
        this.type = type;
        this.dimension = dim;
        this.typeCtors = typeCtors;
        static if (is(T == FunPtrInfo))
            infos.funptrInfo = t;
        else static if (is(T == ClassInfo))
            infos.classInfo = t;
        else static if (is(T == EnumInfo))
            infos.enumInfo = t;
        else static if (is(T == StructInfo))
            infos.structInfo = t;
        else static if (is(T == AAInfo))
            infos.aaInfo = t;
        else static if (is(T == PointerInfo))
            infos.pointerInfo = t;
        else static if (is(T == UnionInfo))
            infos.unionInfo = t;
        else static assert(0, "last argument of Rtti ctor must be an Info");
    }

    /// Constructs a Rtti with a (basic) type.
    this(RtType rtType, ubyte dim, TypeCtors typeCtors)
    in
    {
        import std.conv: to;
        assert(rtType >= RtType._invalid &&rtType <= RtType._dchar,
            "this ctor must only be used with basic types");
    }
    body
    {
        this.type = rtType;
        this.dimension = dim;
        this.typeCtors = typeCtors;
    }

    /// The runtime type
    RtType type;

    /// Type is an array
    ubyte dimension;

    /// Attributes for this type
    TypeCtors typeCtors;

    /// The information for this type.
    Infos infos;

    /// Returns: A valid information when type is equal to $(D RtT._callable).
    const(FunPtrInfo)* funptrInfo() const
    {
        return &infos.funptrInfo;
    }

    /// Returns: A valid information when type is equal to $(D RtT._object).
    const(ClassInfo)* classInfo() const
    {
        return &infos.classInfo;
    }

    /// Returns: A valid information when type is equal to $(D RtT._enum).
    const(EnumInfo)* enumInfo() const
    {
        return &infos.enumInfo;
    }

    /// Returns: A valid information when type is equal to $(D RtT._struct).
    const(StructInfo)* structInfo() const
    {
        return &infos.structInfo;
    }

    /// Returns: A valid information when type is equal to $(D RtT._aa).
    const(AAInfo)* aaInfo() const
    {
        return &infos.aaInfo;
    }

    /// Returns: A valid information when type is equal to $(D RtT._pointer).
    const(PointerInfo)* pointerInfo() const
    {
        return &infos.pointerInfo;
    }

    /// Returns: A valid information when type is equal to $(D RtT._union).
    const(UnionInfo)* unionInfo() const
    {
        return &infos.unionInfo;
    }
}

/**
 * Returns the Rtti for the type that has its .stringof property
 * equal to $(D typeString).
 */
const(Rtti)* getRtti(const(char)[] typeString)
{
    return *(typeString in _name2rtti);
}

/**
 * Registers and returns the Rtti for the type (or the variable)
 * passed as argument.
 */
const(Rtti)* getRtti(A = void, B...)(auto ref B b)
if (B.length < 2)
{
    static if (!B.length)
        alias TT = A;
    else
        alias TT = B[0];

    if (const(Rtti)** result = TT.stringof in _name2rtti)
        return *result;

    enum err = "unsupported type \"" ~ TT.stringof ~ "\": ";

    alias UnAttr(T) = SetFunctionAttributes!(T, "D", 0);

    enum ubyte dim = dimensionCount!TT;
    static assert(dim <= ubyte.max);
    static if (dim > 0)
        alias T = Unqual!(ArrayElementType!TT);
    else
        alias T = TT;

    TypeCtors typeCtors;
    static if (is(T==const)) typeCtors += TypeCtor._const;
    static if (is(T==immutable)) typeCtors += TypeCtor._immutable;
    static if (is(T==inout)) typeCtors += TypeCtor._inout;
    static if (is(T==shared)) typeCtors += TypeCtor._shared;

    static if (staticIndexOf!(Unqual!T, BasicRtTypes) != -1)
    {
        RtType i = RtTypeArr[staticIndexOf!(Unqual!T, BasicRtTypes)];
        const(Rtti)* result = construct!Rtti(i, dim, typeCtors);
    }
    else static if (is(T == enum))
    {
        static if (isImplicitlyConvertible!(OriginalType!T, uint))
        {
            string[] members;
            uint[] values;
            foreach(e; __traits(allMembers, T))
            {
                members ~= e;
                values  ~= __traits(getMember, T, e);
                static assert(__traits(getMember, T, e) > -1, err ~
                    "negative enum values are not supported");
                static assert(!is(e == enum), err ~ "nested enums are not supported");
            }
            const(Rtti)* result = construct!Rtti(RtType._enum, dim, typeCtors,
                EnumInfo(T.stringof, members, values, getRtti!(OriginalType!T)));
        }
        else static assert(0, err ~ "only enums whose type is convertible to int are supported");
    }
    else static if (is(PointerTarget!T == function) || is(T == delegate))
    {
        alias R = ReturnType!T;
        alias P = Parameters!T;

        const(Rtti*)[] pr;
        foreach(Prm; P)
            pr ~= getRtti!Prm;
        const(Rtti)* result = construct!Rtti(RtType._funptr, dim, typeCtors,
            FunPtrInfo(T.stringof, is(T == delegate), cast(Rtti*)getRtti!R, pr));
    }
    else static if (is(T == class) || is(T == Stream))
    {
        static if(!is(T == Stream))
        {
            static if (hasDefaultConstructor!T)
                enum ctor = cast(Object function()) defaultConstructor!T;
            else
                enum ctor = cast(Object function()) null;
        }
        else
            enum ctor = cast(Object function()) null;
        auto init = typeid(T).initializer[];
        const(RtType) tp = (is(T:Stream) | is(T==Stream)) ? RtType._stream : RtType._object;
        const(Rtti)* result = construct!Rtti(tp, dim, typeCtors, ClassInfo(T.stringof, ctor, init));
    }
    else static if (is(T == struct) && __traits(hasMember, T, "publicationFromName"))
    {
        static if (!__traits(hasMember, T, "publicationFromName") ||
            !is(typeof(__traits(getMember, T, "publicationFromName")) ==
                typeof(__traits(getMember, PropertyPublisher, "publicationFromName"))))
            static assert(0, "no valid publicationFromName member");

        static if (!__traits(hasMember, T, "publicationFromIndex") ||
            !is(typeof(__traits(getMember, T, "publicationFromIndex")) ==
                typeof(__traits(getMember, PropertyPublisher, "publicationFromIndex"))))
            static assert(0, "no valid publicationFromIndex member");

        static if (!__traits(hasMember, T, "publicationCount") ||
            !is(typeof(__traits(getMember, T, "publicationCount")) ==
                typeof(__traits(getMember, PropertyPublisher, "publicationCount"))))
            static assert(0, "no valid publicationCount member");

        const(Rtti)* result = construct!Rtti(RtType._struct, dim, typeCtors,
            StructInfo(T.stringof, StructType._publisher));

        const(StructInfo)* si = result.structInfo;
        si.pubTraits.publicationFromName.funcptr = &__traits(getMember, T, "publicationFromName");
        si.pubTraits.publicationFromIndex.funcptr = &__traits(getMember, T, "publicationFromIndex");
        si.pubTraits.publicationCount.funcptr = &__traits(getMember, T, "publicationCount");
    }
    else static if (is(T == struct) && __traits(hasMember, T, "saveToBytes"))
    {
        static if (!__traits(hasMember, T, "saveToBytes") ||
            !is(UnAttr!(typeof(&__traits(getMember, T, "saveToBytes"))) == ubyte[] function()))
            static assert(0, "no valid saveToBytes member");

        static if (!__traits(hasMember, T, "loadFromBytes") ||
            !is(UnAttr!(typeof(&__traits(getMember, T, "loadFromBytes"))) == void function(ubyte[])))
            static assert(0, "no valid loadFromBytes member");

        const(Rtti)* result = construct!Rtti(RtType._struct, dim, typeCtors,
            StructInfo(T.stringof, StructType._binary));
        const(StructInfo)* si = result.structInfo;
        si.binTraits.saveToBytes.funcptr = &__traits(getMember, T, "saveToBytes");
        si.binTraits.loadFromBytes.funcptr = &__traits(getMember, T, "loadFromBytes");
    }
    else static if (is(T == struct) && __traits(hasMember, T, "saveToText"))
    {
        static if (!__traits(hasMember, T, "saveToText") ||
            !is(UnAttr!(typeof(&__traits(getMember, T, "saveToText"))) == const(char)[] function()))
            static assert(0, "no valid saveToText member");

        static if (!__traits(hasMember, T, "loadFromText") ||
            !is(UnAttr!(typeof(&__traits(getMember, T, "loadFromText"))) == void function(const(char)[])))
            static assert(0, "no valid loadFromText member");

        const(Rtti)* result = construct!Rtti(RtType._struct, dim, typeCtors,
            StructInfo(T.stringof, StructType._text));
        const(StructInfo)* si = result.structInfo;
        si.textTraits.saveToText.funcptr = &__traits(getMember, T, "saveToText");
        si.textTraits.loadFromText.funcptr = &__traits(getMember, T, "loadFromText");
    }
    else static if (is(T == struct))
    {
        const(Rtti)* result = construct!Rtti(RtType._struct, dim, typeCtors,
            StructInfo(T.stringof, StructType._none));
    }
    else static if (is(T == union))
    {
        const(Rtti)*[] members;
        foreach(member; __traits(allMembers, T))
        {
            members ~= getRtti!(typeof(__traits(getMember, T, member)));
        }
        const(Rtti)* result = construct!Rtti(RtType._union, dim, typeCtors, UnionInfo(T.stringof, members));
    }
    else static if (isAssociativeArray!T)
    {
        const(Rtti)* result = construct!Rtti(RtType._aa, dim, typeCtors,
            AAInfo(getRtti!(KeyType!T), getRtti!(ValueType!T)));
    }
    else static if (isPointer!T)
    {
        const(Rtti)* result = construct!Rtti(RtType._pointer, dim, typeCtors,
            PointerInfo(getRtti!(PointerTarget!T)));
    }
    else
    {
        typeCtors = 0;
        version(none) static assert(0, err ~ "not handled at all");
        else const(Rtti)* result = construct!Rtti(RtType._invalid, 0, typeCtors);
    }

    _name2rtti[TT.stringof] = result;
    const(Rtti)** res = TT.stringof in _name2rtti;
    _rtti2name[*res] = TT.stringof;
    return *res;
}
///
unittest
{
    enum Option: ubyte {o1 = 2, o2, o3}
    Option option1, option2;
    // first call will register
    const(Rtti)* rtti1 = getRtti(option1);
    const(Rtti)* rtti2 = getRtti(option2);
    // variables of same type point to the same info.
    assert(rtti1 is rtti2);
    // get the Rtti without the static type.
    const(Rtti)* rtti3 = getRtti("Option");
    assert(rtti3 is rtti1);
    assert(rtti3.enumInfo.identifier == "Option");
    assert(rtti3.enumInfo.members == ["o1", "o2", "o3"]);
    assert(rtti3.enumInfo.values == [2, 3, 4]);

    assert(rtti3.enumInfo.valueType is rtti1.enumInfo.valueType);
    assert(rtti3.enumInfo.valueType is rtti2.enumInfo.valueType);
    assert(rtti3.enumInfo.valueType is getRtti!ubyte);
}

/**
 * Returns true if the Rtti passed as argument are for a "publising" struct.
 */
bool isPublisingStruct(const(Rtti)* ti)
{
    bool result;
    if (ti && ti.type == RtType._struct && ti.structInfo.type == StructType._publisher)
        result = true;
    return result;
}

unittest
{
    static struct PubStr
    {
        mixin PropertyPublisherImpl;
    }
    assert(isPublisingStruct(getRtti!PubStr));
}

unittest
{
    enum Option {o1 = 2, o2, o3}
    Option[][] opts = [[Option.o1, Option.o2],[Option.o1, Option.o2]];
    assert(getRtti(opts[0]) is getRtti(opts[1]));
}

unittest
{
    import std.algorithm.searching: countUntil;
    enum Enumeration: ubyte {a1, a2, a3}
    const(Rtti)* ati = getRtti!Enumeration;
    assert(ati.enumInfo.values[0] == 0);
    assert(ati.enumInfo.values[1] == 1);
    assert(ati.enumInfo.values[2] == 2);
    assert(ati.enumInfo.members[0] == "a1");
    assert(ati.enumInfo.members[1] == "a2");
    assert(ati.enumInfo.members[2] == "a3");
    assert(0 == countUntil(ati.enumInfo.values, 0));
    assert(1 == countUntil(ati.enumInfo.values, 1));
    assert(2 == countUntil(ati.enumInfo.values, 2));
}

unittest
{
    void basicTest(T)()
    {
        const(Rtti)* inf = getRtti!T;
        assert(inf.type == RtTypeArr[staticIndexOf!(T, BasicRtTypes)]);
    }
    foreach(T; BasicRtTypes[1..$])
        basicTest!(T);
}

unittest
{
    void arrayTest(T)()
    {
        const(Rtti)* inf = getRtti!T;
        assert(inf.type == RtTypeArr[staticIndexOf!(Unqual!(ArrayElementType!T), BasicRtTypes)]);
        assert(inf.dimension == dimensionCount!T);
    }
    foreach(T; BasicRtTypes[1..$])
    {
        arrayTest!(T[]);
        arrayTest!(T[][]);
        arrayTest!(T[][][]);
    }
}

unittest
{
    static struct Foo
    {
        uint delegate(uint) a;
        string function(ulong,char) b;
    }
    Foo foo;
    const(Rtti)* dgi = getRtti(foo.a);
    assert(dgi.type == RtType._funptr);
    assert(dgi.funptrInfo.hasContext);
    assert(dgi.funptrInfo.returnType.type == RtType._uint);
    assert(dgi.funptrInfo.parameters.length == 1);
    assert(dgi.funptrInfo.parameters[0].type == RtType._uint);
    assert(dgi.funptrInfo.size == size_t.sizeof * 2);

    const(Rtti)* fgi = getRtti(foo.b);
    assert(fgi.type == RtType._funptr);
    assert(!fgi.funptrInfo.hasContext);
    assert(fgi.funptrInfo.returnType.type == RtType._char); // _string
    assert(fgi.funptrInfo.parameters.length == 2);
    assert(fgi.funptrInfo.parameters[0].type == RtType._ulong);
    assert(fgi.funptrInfo.parameters[1].type == RtType._char);
    assert(fgi.funptrInfo.size == size_t.sizeof);
}

unittest
{
    static struct Bar
    {
        size_t publicationCount(){return 0;}
        GenericDescriptor* publicationFromName(const(char)[]){return null;}
        GenericDescriptor* publicationFromIndex(size_t){return null;}
    }
    Bar bar;
    const(Rtti)* rtti = getRtti(bar);
    assert(rtti.type == RtType._struct);
    assert(rtti.structInfo.type == StructType._publisher);
    assert(rtti.structInfo.identifier == "Bar");
    rtti.structInfo.pubTraits.setContext(cast(void*) &bar);
    assert(rtti.structInfo.pubTraits.publicationCount == &bar.publicationCount);
    assert(rtti.structInfo.pubTraits.publicationFromIndex == &bar.publicationFromIndex);
    assert(rtti.structInfo.pubTraits.publicationFromName == &bar.publicationFromName);
    // coverage
    assert(rtti.structInfo.pubTraits.publicationCount() == bar.publicationCount);
    assert(rtti.structInfo.pubTraits.publicationFromIndex(0) == bar.publicationFromIndex(0));
    assert(rtti.structInfo.pubTraits.publicationFromName("") == bar.publicationFromName(""));
}

unittest
{
    class Gaz
    {
        this(){}
    }
    Gaz gaz = new Gaz;
    import std.stdio;
    const(Rtti)* rtti = getRtti(gaz);
    assert(rtti.type == RtType._object);
    assert(rtti.classInfo.identifier == "Gaz");
    assert(rtti.classInfo.constructor == &Gaz.__ctor);
    assert(rtti.classInfo.initialLayout == typeid(Gaz).initializer);
}

unittest
{
    struct Hop
    {
        const(char)[] saveToText(){return "hop";}
        void loadFromText(const(char)[] value){}
    }
    Hop hop;
    const(Rtti)* rtti = getRtti(hop);
    assert(rtti.type == RtType._struct);
    assert(rtti.structInfo.type == StructType._text);
    assert(rtti.structInfo.identifier == "Hop");
    rtti.structInfo.textTraits.setContext(cast(void*) &hop);
    assert(rtti.structInfo.textTraits.saveToText == &hop.saveToText);
    assert(rtti.structInfo.textTraits.loadFromText == &hop.loadFromText);
    // coverage
    assert(rtti.structInfo.textTraits.saveToText() == "hop");
    rtti.structInfo.textTraits.loadFromText("hop");
}

unittest
{
    struct Boo
    {
        ubyte[] saveToBytes(){return [0x0];}
        void loadFromBytes(ubyte[] value){}
    }
    Boo boo;
    const(Rtti)* rtti = getRtti(boo);
    assert(rtti.type == RtType._struct);
    assert(rtti.structInfo.type == StructType._binary);
    assert(rtti.structInfo.identifier == "Boo");
    rtti.structInfo.binTraits.setContext(cast(void*) &boo);
    assert(rtti.structInfo.binTraits.saveToBytes == &boo.saveToBytes);
    assert(rtti.structInfo.binTraits.loadFromBytes == &boo.loadFromBytes);
    // coverage
    assert(rtti.structInfo.binTraits.saveToBytes() == [0x0]);
    rtti.structInfo.binTraits.loadFromBytes([0x0]);
}

unittest
{
    shared(int) si;
    assert(TypeCtor._shared in si.getRtti.typeCtors);
    const(int) ci;
    assert(TypeCtor._const in ci.getRtti.typeCtors);
    immutable(int) ii;
    assert(TypeCtor._immutable in ii.getRtti.typeCtors);
    const(shared(int)) sii;
    assert(TypeCtor._const in sii.getRtti.typeCtors);
    assert(TypeCtor._shared in sii.getRtti.typeCtors);
}

unittest
{
    struct Gap
    {
        static void foo(ref const(int)){}
        static void baz(ref const(int)){}
        static void bar(out shared(int)){}
    }
    auto fti0 = getRtti(&Gap.foo);
    assert(TypeCtor._const in fti0.funptrInfo.parameters[0].typeCtors);
    auto fti1 = getRtti(&Gap.bar);
    assert(TypeCtor._shared in fti1.funptrInfo.parameters[0].typeCtors);
    auto fti2 = getRtti(&Gap.baz);
    assert(fti0 is fti2);
}

unittest
{
    assert(getRtti!(const(int)) !is getRtti!(int));
}

unittest
{
    MemoryStream str;
    assert(getRtti(str).type == RtType._stream);
}

unittest
{
    struct Cup
    {
        ubyte[] saveToBytes(){return [0x0];}
        void loadFromBytes(ubyte[] value){}
    }
    Cup cup0;
    Cup cup1;
    const(Rtti)* rtti = getRtti(cup0);

    // to bind, two simultaneous xxxTraits must exist
    const(BinTraits) bt0 = *rtti.structInfo.binTraits;
    const(BinTraits) bt1 = *rtti.structInfo.binTraits;
    bt0.setContext(&cup0);
    bt1.setContext(&cup1);

    assert(bt0.saveToBytes.funcptr == bt1.saveToBytes.funcptr);
    assert(bt0.saveToBytes.ptr != bt1.saveToBytes.ptr);

    bt0.loadFromBytes(bt0.saveToBytes());
}

unittest
{
    int[byte] k;
    const(Rtti)* kti = getRtti(k);
    assert(kti.type == RtType._aa);
    assert(kti.aaInfo.keyType is getRtti!byte);
    assert(kti.aaInfo.valueType is getRtti!int);

    long[string] h;
    const(Rtti)* hti = getRtti(h);
    assert(hti.type == RtType._aa);
    assert(hti.aaInfo.keyType is getRtti!string);
    assert(hti.aaInfo.valueType is getRtti!long);
}

unittest
{
    int* i;
    const(Rtti)* iti = getRtti(i);
    assert(iti.type == RtType._pointer);
    assert(iti.pointerInfo.type is getRtti!int);

    int** pi;
    const(Rtti)* piti = getRtti(pi);
    assert(piti.type == RtType._pointer);
    assert(piti.pointerInfo.type is getRtti!(int*));
    assert(piti.pointerInfo.type.pointerInfo.type is getRtti!(int));
}

unittest
{
    union U {int i; void* a;} U u;
    const(Rtti)* uti = getRtti(u);
    assert(uti.type == RtType._union);
    assert(uti.unionInfo.identifier == "U");
    assert(uti.unionInfo.membersType.length == 2);
    assert(uti.unionInfo.membersType[0] is getRtti!int);
    assert(uti.unionInfo.membersType[1] is getRtti!(void*));
}

