/**
 * Several templates, alias, traits or functions related to types.
 */
module iz.types;

import
    std.traits, std.meta;

version(unittest) import std.stdio;

/// pointer.
alias Ptr = void*;


/** 
 * BasicTypes elements verify isBasicType().
 */
alias BasicTypes = AliasSeq!( 
    bool, byte, ubyte, short, ushort, int, uint, long, ulong, 
    float, double, real, 
    char, wchar, dchar
);
///
unittest
{
    foreach(T; BasicTypes)
        assert( isBasicType!T, T.stringof);
}

    
/**
 * Returns true if T is a fixed-length data.
 */
bool isFixedSize(T)()
{
    return (
        staticIndexOf!(T,BasicTypes) != -1) ||
        (is(T==struct) & (__traits(isPOD, T))
    );
}

unittest
{
    class Foo{}
    struct Bar{byte a,b,c,d,e,f;}
    alias myInt = int;
    assert(isFixedSize!myInt);
    assert(!isFixedSize!Foo);
    assert(isFixedSize!Bar);
}

/// Common type for all the delagate kinds, when seen as a struct (ptr & funcptr).
alias GenericDelegate = void delegate();

/// Common type for all the function kinds.
alias GenericFunction = void function();


/**
 * Returns the dynamic class name of an Object or an interface.
 * Params:
 *      assumeDemangled = Must only be set to false if the class is declared in a unittest.
 *      t = Either an interface or a class instance.
 */
string className(bool assumeDemangled = true, T)(T t)
if (is(T == class) || is(T == interface))
{
    static if (is(T == class)) Object o = t;
    else Object o = cast(Object) t;
    import std.array: split;
    static if (assumeDemangled)
        return (cast(TypeInfo_Class)typeid(o)).name.split('.')[$-1];
    else
    {
        import std.demangle: demangle;
        return (cast(TypeInfo_Class)typeid(o)).name.demangle.split('.')[$-1];
    }
}
///
unittest
{
    static interface I {}
    static class A{}
    static class B: I{}
    static class C{}
    assert(className(new A) == "A");
    assert(className(new B) == "B");
    assert(className(cast(Object)new A) == "A");
    assert(className(cast(Object)new B) == "B");
    assert(className(cast(I) new B) == "B");
    assert(className!(false)(new C) == "C");
}

/**
 * Indicates if the member of a struct or class is accessible for compile time
 * introspection.
 *
 * The template has to be mixed in the scope where the other __traits()
 * operations are performed.
 * A simple function template that uses __traits(getProtection) does not faithfully
 * represent the member accessibility if the function is declared in another module.
 * Another problem is that __traits(getProtection) does not well represent the
 * accessibility of the private members (own members or friend classes /structs).
 */
mixin template ScopedReachability()
{
    bool isMemberReachable(T, string member)()
    if (is(T==class) || is(T==struct) || is(T==union))
    {
        return is(typeof(__traits(getMember, T, member)));
    }
}

/**
 * Detects whether type $(D T) is a multi dimensional array.
 *
 * Params:
 *      T = type to be tested
 *
 * Returns:
 *      true if T is a multi dimensional array
 */
template isMultiDimensionalArray(T)
{
    static if (!isArray!T)
        enum isMultiDimensionalArray = false;
    else
    {
        import std.range.primitives: hasLength;
        alias DT = typeof(T.init[0]);
        enum isMultiDimensionalArray = hasLength!DT;
    }
}
///
unittest
{
    static assert(!isMultiDimensionalArray!(string[]) );
    static assert(isMultiDimensionalArray!(int[][]) );
    static assert(!isMultiDimensionalArray!(int[]) );
    static assert(!isMultiDimensionalArray!(int) );
    static assert(!isMultiDimensionalArray!(string) );
    static assert(!isMultiDimensionalArray!(int[][] function()) );
    static assert(!isMultiDimensionalArray!void);
}

/**
 * Indicates the dimension count of an built-in array.
 *
 * Params:
 *      T = type to be tested
 *
 * Returns:
 *      0 if $(D T) is an not a build-in array, otherwise a number
 *      at least equal to 1, according to the array dimension count.
 */
template dimensionCount(T)
{
    static if (isArray!T)
    {
        static if (isMultiDimensionalArray!T)
        {
            alias DT = typeof(T.init[0]);
            enum dimensionCount = dimensionCount!DT + 1;
        }
        else enum dimensionCount = 1;
    }
    else enum dimensionCount = 0;
}
///
unittest
{
    static assert(dimensionCount!char == 0);
    static assert(dimensionCount!(string[]) == 1);
    static assert(dimensionCount!(int[]) == 1);
    static assert(dimensionCount!(int[][]) == 2);
    static assert(dimensionCount!(int[][][]) == 3);
}

/**
 * Indicates the array element type of an array.
 *
 * Contrary to $(D ElementType), dchar is not returned for narrow strings.
 * The template strips the type lookup goes until the last dimenssion of a
 * multi-dim array.
 *
 * Params:
 *      T = type to be tested.
 *
 * Returns:
 *      T element type.
 */
template ArrayElementType(T)
if (isArray!T)
{
    static if (isArray!(typeof(T.init[0])))
        alias ArrayElementType = ArrayElementType!(typeof(T.init[0]));
    else
        alias ArrayElementType = typeof(T.init[0]);
}
///
unittest
{
    static assert(is(ArrayElementType!(int[][]) == int));
    static assert(is(ArrayElementType!(char[][][][][]) == char));
    static assert(is(ArrayElementType!(wchar[]) == wchar));
}

/**
 * Indicates wether an enum is ordered.
 *
 * An enum is considered as ordered if its members can index an array,
 * optionally with a start offset.
 *
 * Params:
 *      T = enum to be tested.
 *
 * Returns:
 *      true if T members type is integral and if the delta between each member
 *      is one, false otherwise.
 */
template isOrderedEnum(T)
if (is(T == enum))
{
    static if (!isIntegral!(OriginalType!T))
    {
        enum isOrderedEnum = false;
    }
    else
    {
        bool checker()
        {
            OriginalType!T previous = T.min;
            foreach(member; EnumMembers!T[1..$])
            {
                if (member != previous + 1)
                    return false;
                previous = member;
            }
            return true;
        }
        enum isOrderedEnum = checker;
    }
}
///
unittest
{
    enum A {a,z,e,r}
    static assert(isOrderedEnum!A);
    enum B: ubyte {a = 2,z,e,r}
    static assert(isOrderedEnum!B);

    enum C: float {a,z,e,r}
    static assert(!isOrderedEnum!C);
    enum D: uint {a,z = 8,e,r}
    static assert(!isOrderedEnum!D);
}

/**
 * Returns: If the class type passed as template parameter has a user-defined
 * default constructor then a pointer to this constructor, otherwise the function
 * is $(D void).
 */
auto defaultConstructor(C)()
if (is(C == class))
{
    static if (__traits(hasMember, C, "__ctor"))
        static foreach (overload; __traits(getOverloads, C, "__ctor"))
    {
        static if (is(Unqual!(ReturnType!overload) == C) && !Parameters!overload.length)
            return &overload;
    }
}
///
unittest
{
    static class Foo{this(){} this(int){} this(string){}}
    assert(defaultConstructor!Foo is cast(void*) Foo.classinfo.defaultConstructor);
    static class Bar{this(int){} this(string){}}
    assert(!(is(defaultConstructor!Bar)));
}

/**
 * Indicates wether a class has a default constructor.
 */
template hasDefaultConstructor(C)
if(is(C==class))
{
    enum hasDefaultConstructor = !is(typeof(defaultConstructor!C()) == void);
}
///
unittest
{
    class A{}
    class B{this() const {}}
    class C{this(int a){}}
    class D{this(){} this(int a){}}

    static assert(!hasDefaultConstructor!A);
    static assert( hasDefaultConstructor!B);
    static assert(!hasDefaultConstructor!C);
    static assert( hasDefaultConstructor!D);
}

/**
 * Indicates wether an aggregate can be used with the $(D in) operator.
 *
 * Params:
 *      T = The aggregate or an associative array type.
 *      A = The type of the $(D in) left hand side argument.
 */
template hasInOperator(T, A)
{
    static if (isAssociativeArray!T && is(KeyType!T == A))
        enum hasInOperator = true;
    else static if (is(T == class) || is(T == struct))
    {
        static if (is(typeof({A a; auto b = a in T;})))
            enum hasInOperator = true;
        else
        {
            static if (is(typeof({A a; auto b = a in new T;})))
                enum hasInOperator = true;
            else
                enum hasInOperator = false;
        }
    }
    else
        enum hasInOperator = false;
}
///
unittest
{
    alias AA = int[string];
    static assert(hasInOperator!(AA, string));
    static assert(!hasInOperator!(AA, int));

    struct Tester1 {static bool opIn_r(int){return true;}}
    static assert(hasInOperator!(Tester1, int));

    struct Tester2 {bool opIn_r(int){return true;}}
    static assert(hasInOperator!(Tester2, int));
    static assert(!hasInOperator!(Tester2, string));

    struct Nothing {}
    static assert(!hasInOperator!(Nothing, int));
}

/**
 * Detects wether T is an instantiated template.
 */
template isTemplateInstance(alias T : Base!Args, alias Base, Args...)
{
    enum isTemplateInstance = is(typeof(T));
}
///
unittest
{
    enum a(T) = false;
    void b(T)(int){}
    void c()(){}
    template d(T){}
    class e(T){T t;}
    interface I{}

    static assert(isTemplateInstance!(a!int));
    static assert(isTemplateInstance!(b!int));
    static assert(isTemplateInstance!(d!int));
    static assert(isTemplateInstance!(e!int));

    static assert(!isTemplateInstance!(I));
    static assert(!isTemplateInstance!(int));

    static assert(!isTemplateInstance!(a));
    static assert(!isTemplateInstance!(b));
    static assert(!isTemplateInstance!(c));
}

/// ditto
template isTemplateInstance(T : Base!Args, alias Base, Args...)
{
    enum isTemplateInstance = is(T);
}

/// ditto
template isTemplateInstance(T)
{
    enum isTemplateInstance = false;
}

/// ditto
template isTemplateInstance(alias T)
{
    enum isTemplateInstance = isTemplateInstance!(typeof(T));
}

template TemplateBase(T : Base!Args, alias Base, Args...)
if (is(T == class) || is(T == interface) || is(T == struct) || is(T == union))
{
    alias TemplateBase = Base;
}

unittest
{
    class A(T){}
    template B(T){class R{}}
    alias F = B!int.R;
}

/**
 * Indicates wether a type or a variable type is an eponymous template.
 */
template isEponymousTemplate(T)
{
    static if (is(T == class) || is(T == interface) || is(T == struct) || is(T == union))
    {
        enum p = __traits(parent, T).stringof == T.stringof;
        enum isEponymousTemplate = p && isTemplateInstance!T;
    }
    else
        enum isEponymousTemplate = false;
}
///
unittest
{
    static class A(T){}
    static struct B(T){}
    static assert(isEponymousTemplate!(A!int));
    static assert(isEponymousTemplate!(B!int));
    static assert(!isEponymousTemplate!int);
    template C(T)
    {
        static class C{}
    }
    static assert(isEponymousTemplate!(C!int));
}

unittest
{
    template A(T)
    {
        static class A{}
    }
    static assert(isEponymousTemplate!(A!int));

    template B(T)
    {
        static class A{}
    }
    static assert(!isEponymousTemplate!(B!int.A));

    class C(T){}
    static assert(!isEponymousTemplate!int);

    A!int a;
    static assert(isEponymousTemplate!a);
}


/// ditto
template isEponymousTemplate(alias T)
{
    static if (is(typeof(T)))
        enum isEponymousTemplate = isEponymousTemplate!(typeof(T));
    else
        enum isEponymousTemplate = false;
}

/**
 * Decomposes the chain of the templates and arguments used to create the alias
 * passed as argument.
 *
 * Params:
 *      T = An alias.
 *      TemplateAndArgsOf = used internally only.
 *
 * Returns:
 *      An AliasSeq formed by a chain of templates and arguments.
 */
template NestedTemplateAndArgsOf(alias T, TemplateAndArgsOf...)
{
    static if (isTemplateInstance!(AliasSeq!(__traits(parent, T))[0]))
        alias PT = AliasSeq!(__traits(parent, T));
    else
        alias PT = void;

    static if (!is(PT == void))
    {
        alias NestedTemplateAndArgsOf = AliasSeq!(NestedTemplateAndArgsOf!
            (__traits(parent, T)), TemplateOf!T, TemplateArgsOf!T
        );
    }
    else static if (isTemplateInstance!T)
    {
        alias NestedTemplateAndArgsOf = AliasSeq!(TemplateOf!T, TemplateArgsOf!T,
            TemplateAndArgsOf);
    }
    else alias NestedTemplateAndArgsOf = void;
}
///
unittest
{
    template A(As...)
    {
        template B(Bs...){int a;}
    }
    alias AB0 = A!(1,2).B!(3,4,5);
    alias SQ0 = NestedTemplateAndArgsOf!AB0;
    static assert(__traits(isSame, SQ0[0], A));
    static assert(__traits(isSame, SQ0[1], 1));
    static assert(__traits(isSame, SQ0[2], 2));
    static assert(__traits(isSame, SQ0[3], A!(1,2).B));
    static assert(__traits(isSame, SQ0[4], 3));
    static assert(__traits(isSame, SQ0[5], 4));
    static assert(__traits(isSame, SQ0[6], 5));

    template C(T)
    {
        template D(T)
        {
            template E(T) {}
        }
    }
    alias CDE0 = C!int.D!int.E!int;
    alias SQ1 = NestedTemplateAndArgsOf!CDE0;
    static assert(__traits(isSame, SQ1[0], C));
    static assert(is(SQ1[1] == int));
    static assert(__traits(isSame, SQ1[2], C!int.D));
    static assert(is(SQ1[3] == int));
    static assert(__traits(isSame, SQ1[4], C!int.D!int.E));
    static assert(is(SQ1[5] == int));

    alias B = NestedTemplateAndArgsOf!int;
    static assert(is(B == void));

    template G(Gs...)
    {
        struct G {}
    }

    alias GI = G!int;

    //pragma(msg, NestedTemplateAndArgsOf!(G!int).stringof);
}

/// ditto
template NestedTemplateAndArgsOf(T)
{
    static if (__traits(isTemplate, T))
    {
        alias TT = TemplateOf!T;
        alias NestedTemplateAndArgsOf = NestedTemplateAndArgsOf!TT;
    }
    else static if (isEponymousTemplate!T)
    {
        alias TT = TemplateBase!T;
        alias NestedTemplateAndArgsOf = NestedTemplateAndArgsOf!TT;
    }
    else alias NestedTemplateAndArgsOf = void;
}

/**
 * Indicates wether something is a specialized type.
 *
 * Returns:
 * If the input argument is a type or a variable declaration for a type that's
 * an complete specialization of a template then true is returned. In all the
 * other cases (build-in types, non templatized aggregates or aliases to
 * partially specialized templates, false.
 */
template isSpecializedType(T)
{
    enum isSpecializedType = isTemplateInstance!T && is(T);
}
///
unittest
{
    class A(T){}
    static assert(isSpecializedType!(A!int));
    class B(T,TT){}
    alias PartialB(T) = B!int;
    static assert(!isSpecializedType!PartialB);
    static assert(isSpecializedType!(B!(int,void)));
    static assert(!isSpecializedType!int);
}

/// ditto
template isSpecializedType(alias T)
{
    static if (is(typeof(T)))
        enum isSpecializedType = isSpecializedType!(typeof(T));
    else
        enum isSpecializedType = false;
}

/**
 * Indicates wether something is a value known at compile time.
 *
 * Params:
 *      V = The value to test.
 *      T = Optional, the expected value type.
 */
template isCompileTimeValue(alias V, T...)
if (T.length == 0 || (T.length == 1 && is(T[0])))
{
    enum isKnown = is(typeof((){enum v = V;}));
    static if (!T.length)
        enum isCompileTimeValue = isKnown;
    else
        enum isCompileTimeValue = isKnown && is(typeof(V) == T[0]);
}
///
unittest
{
    string a;
    enum b = "0";
    enum c = 0;
    static assert(!isCompileTimeValue!a);
    static assert(isCompileTimeValue!b);
    static assert(isCompileTimeValue!c);
    static assert(isCompileTimeValue!(b,string));
    static assert(isCompileTimeValue!(c,int));
    static assert(!isCompileTimeValue!(c,char));
    static assert(!isCompileTimeValue!(char));
}

/// ditto
template isCompileTimeValue(V, T...)
if (T.length == 0 || (T.length == 1 && is(T[0])))
{
    enum isCompileTimeValue = false;
}

/**
 * Indicates wether something is a string literal.
 */
enum isStringLiteral(alias V) = isCompileTimeValue!(V, string);

/// ditto
template isStringLiteral(V){enum isStringLiteral = false;}

///
unittest
{
    string a;
    enum b = "0";
    enum c = 0;
    static assert(!isStringLiteral!a);
    static assert(isStringLiteral!b);
    static assert(!isStringLiteral!c);
    static assert(!isStringLiteral!int);
}

/**
 * Indicates wether the class passed as template argument has the extern(C++)
 * linkage attribute.
 */
template isCppClass(T)
if (is(T == class))
{
    bool allCpp()
    {
        bool result = true;
        foreach (member; __traits(allMembers, T))
        {
            static if (member != "this") // ?
            static if (__traits(getOverloads, T, member).length)
            foreach (ov; __traits(getOverloads, T, member))
                static if (functionLinkage!ov != "C++")
            {
                result = false;
                break;
            }
            return result;
        }
    }
    enum isCppClass = allCpp();
}
///
unittest
{
    class Foo{void bar(){} int a;}
    static assert(!isCppClass!Foo);

    extern(C++) class Bar{void bar(){} int a;}
    static assert(isCppClass!Bar);
}

/**
 * Indicates wether the type passed as template parameter has a custom
 * $(D opEquals) function that allows self comparison.
 *
 * This triat mostly allows compare objects, with a known derived type, in
 * $(D @nogc) code.
 */
template hasElaborateSelfEquals(T)
{
    static if (is(T == class) || is(T == struct))
    {
        static if (is(T == class))
            alias B = Object;
        else
            alias B = Unqual!T;
        static if (__traits(hasMember, T, "opEquals")
            && Parameters!(T.opEquals).length == 1
            && is(Unqual!(Parameters!(T.opEquals)[0]) : B))
            enum bool hasElaborateSelfEquals = true;
        else
            enum bool hasElaborateSelfEquals = false;
    }
    else enum bool hasElaborateSelfEquals = false;
}
///
unittest
{
    struct Foo {}
    struct Bar {bool opEquals(const(Bar)){return true;}}
    struct Baz {bool opEquals(Foo){return true;}}
    static assert(!hasElaborateSelfEquals!Foo);
    static assert(hasElaborateSelfEquals!Bar);
    static assert(!hasElaborateSelfEquals!Baz);
}

