/**
 * Memory managment utilities.
 */
module iz.memory;

import
    core.stdc.string, core.stdc.stdlib, core.exception;
import
    std.traits, std.meta;
import
    iz.types;

/**
 * Like malloc() but for @safe context.
 */
Ptr getMem(size_t size) nothrow @trusted @nogc
in
{
    //assert(size);
}
body
{
    auto result = malloc(size);
    if (!result)
        throw construct!OutOfMemoryError;
    return result;
}

/**
 * Like realloc() but for @safe context.
 */
Ptr reallocMem(ref Ptr src, size_t newSize) nothrow @trusted @nogc
in
{
    assert(newSize);
}
body
{
    src = realloc(src, newSize);
    if (!src)
        throw construct!OutOfMemoryError;
    return src;
}

/**
 * Like memmove() but for @safe context.
 * dst and src can overlap.
 *
 * Params:
 *      dst = The data source.
 *      src = The data destination.
 *      count = The count of byte to meove from src to dst.
 * Returns:
 *      the pointer to the destination, (same as dst).
 */
Ptr moveMem(Ptr dst, const Ptr src, size_t count) nothrow @trusted @nogc
in
{
    if (count)
    {
        assert(dst);
        assert(src);
    }
}
body
{
    return memmove(dst, src, count);
}

/**
 * Like memmove() but for @safe context.
 * dst and src can't overlap.
 *
 * Params:
 *      dst = The data source.
 *      src = The data destination.
 *      count = The count of byte to meove from src to dst.
 * Returns:
 *      the pointer to the destination, (same as dst).
 */
Ptr copyMem(Ptr dst, Ptr src, size_t count) nothrow @trusted @nogc
in
{
    if (count)
    {
        assert(dst);
        assert(src);
        assert(dst + count <= src || dst >= src + count);
    }
}
body
{
    auto result = memcpy(dst, src, count);
    if (!result)
        throw construct!OutOfMemoryError;
    return result;
}

/**
 * Frees a manually allocated pointer to a basic type.
 * Like free() but for @safe context.
 *
 * Params:
 *      src = The pointer to free.
 */
void freeMem(T)(auto ref T src) nothrow @trusted @nogc
if (isPointer!T && isBasicType!(PointerTarget!T))
{
    if (src)
    {
        free(cast(void*)src);
        src = null;
    }
}

/**
 * This enum must be used as an UDA to mark a variable of a type that looks
 * like GC-managed but that is actually not GC-managed.
 */
enum NoGc;

/**
 * When this enum is used as UDA on aggregate types whose instances are
 * created with construct() a compile time message indicates if a GC range
 * will be added for the members.
 */
enum TellRangeAdded;

/**
 * When this enum is used as UDA on aggregate types whose instances are
 * created with construct() they won't be initialized, i.e the
 * static layout representing the initial value of the members is not copied.
 *
 * For example it can be used on a struct that has a $(D @disable this()) and
 * when the others constructor are suposed to do the initialization job.
 */
enum NoInit;
///
unittest
{
    @NoInit static struct Foo{int a = 42;}
    Foo* foo = construct!Foo;
    // initializer well skipped
    assert(foo.a != 42);
    destruct(foo);
}

/**
 * Indicates if an aggregate contains members that might be
 * collected by the garbage collector. This is used in $(D construct)
 * to determine if the content of a manually allocated aggregate must
 * be declared to the GC.
 */
template MustAddGcRange(T)
if (is(T==struct) || is(T==union) || is(T==class))
{
    string check()
    {
        import std.meta: aliasSeqOf;
        import std.range: iota;

        string managedMembers;

        enum addManaged = q{managedMembers ~= " " ~ T.tupleof[i].stringof;};

        // TODO: allow CPP classes detection when protection compliance removed.
        static if (is(T == class) /*&& (!isCppClass!T)*/)
        {
            foreach(BT; BaseClassesTuple!T)
            {
                string m = MustAddGcRange!BT;
                if (m.length)
                    managedMembers ~= " " ~ m;
            }
        }
        // TODO: use __trait(allMembers) when protection compliance removed.
        // ".tupleof" doesn't include the static fields.
        foreach(i; aliasSeqOf!(iota(0, T.tupleof.length)))
        {
            static if (!is(typeof(T.tupleof[i])== void))
            {
                alias MT = typeof(T.tupleof[i]);
                static if (isArray!MT && !hasUDA!(T.tupleof[i], NoGc) && hasManagedDimension!MT)
                    mixin(addManaged);
                else static if (isPointer!MT && !hasUDA!(T.tupleof[i], NoGc))
                    mixin(addManaged);
                else static if (is(MT == class) && (!is(MT : T)) && !hasUDA!(T.tupleof[i], NoGc)
                    && !(isTemplateInstance!T /*&& staticIndexOf!(MT,TemplateArgsOf!T) > 0*/))
                {
                    // failure here when the class is a template and when one of the member
                    // type is one of the template argument.
                    //pragma(msg, T.stringof, " ", MT.stringof);
                    static if (MustAddGcRange!MT)
                        mixin(addManaged);
                }
                else static if (is(MT == struct) && !is(MT == T) && !hasUDA!(T.tupleof[i], NoGc))
                {
                    static if (MustAddGcRange!MT)
                        mixin(addManaged);
                }
                else static if (is(MT == union) && !is(MT == T) && !hasUDA!(T.tupleof[i], NoGc))
                {
                    static if (MustAddGcRange!MT)
                        mixin(addManaged);
                }
            }
        }
        return managedMembers;
    }

    static if (hasUDA!(T, NoGc))
        static immutable MustAddGcRange = [];
    else
        static immutable MustAddGcRange = check();

    static if (hasUDA!(T, TellRangeAdded))
    {
        static if (MustAddGcRange.length)
            pragma(msg, "a GC range will be added for any new " ~ T.stringof ~
                ", because of: " ~ MustAddGcRange);
        else
            pragma(msg, "a GC range wont be added for any new " ~ T.stringof);
    }

}
///
unittest
{
    // 'a' will be managed with expand/Shrink
    class Foo{@NoGc int[] a; @NoGc void* b;}
    static assert(!MustAddGcRange!Foo);
    // 'a' will be managed with '.length' so druntime.
    class Bar{int[] a; @NoGc void* b;}
    // b's annotation is canceled by a type.
    static assert(MustAddGcRange!Bar);
    // Baz base is not @NoGc
    class Baz: Bar{@NoGc void* c;}
    static assert(MustAddGcRange!Baz);
}

package template hasManagedDimension(T)
{
    import std.range: ElementType;
    static if (isDynamicArray!T)
        enum hasManagedDimension = true;
    else static if (isStaticArray!T)
        enum hasManagedDimension = hasManagedDimension!(ElementType!T);
    else
        enum hasManagedDimension = false;
}

unittest
{
    alias A0 = int[];
    static assert(hasManagedDimension!A0);
    alias A1 = int[2][];
    static assert(hasManagedDimension!A1);
    alias A2 = int[][2];
    static assert(hasManagedDimension!A2);
    alias A3 = int[3][2];
    static assert(!hasManagedDimension!A3);
    alias A4 = int[][3][2];
    static assert(hasManagedDimension!A4);
}

unittest
{
    class Foo{int[][2][4] a;}
    static assert(MustAddGcRange!Foo);
    class Bar{@NoGc int[][2][4] a;}
    static assert(!MustAddGcRange!Bar);
}

/**
 * When mixed in a aggregate this template has for effect to disable the usage
 * the $(D new) operator.
 */
mixin template disableNew()
{
    @disable new (size_t){return null;}
}
///
unittest
{
    // class requiring users to use allocators.
    class NotUsableWithNew
    {
        mixin disableNew;
    }

    // statically verify that `new` cannot be used.
    static assert(!__traits(compiles, new NotUsableWithNew));

    // Ok with a custom allocator
    auto a = construct!NotUsableWithNew();
    destruct(a);
}

/**
 * When mixed in class this template has for effect to automatically call the
 * nearest inherited destructor if no destructor is present, otherwise a call
 * to $(D callInheritedDtor()) should be made in the last LOC of the destructor.
 */
mixin template inheritedDtor()
{

    static assert(is(typeof(this) == class));

    private
    {
        import std.traits: BaseClassesTuple;

        alias __iz_B = BaseClassesTuple!(typeof(this));
        enum hasDtor = __traits(hasMember, typeof(this), "__dtor");
        static if (hasDtor && !__traits(isSame, __traits(parent, typeof(this).__dtor), typeof(this)))
            enum inDtor = true;
        else
            enum inDtor = false;
    }

    public void callInheritedDtor(classT = typeof(this))()
    {
        import std.meta: aliasSeqOf;
        import std.range: iota;

        foreach(i; aliasSeqOf!(iota(0, __iz_B.length)))
            static if (__traits(hasMember, __iz_B[i], "__xdtor"))
            {
                mixin("this." ~ __iz_B[i].stringof ~ ".__xdtor;");
                break;
            }
    }

    static if (!hasDtor || inDtor)
    public ~this() {callInheritedDtor();}
}

/**
 * Returns a new, GC-free, class instance.
 *
 * Params:
 *      CT = A class type.
 *      a = Variadic parameters passed to the constructor.
 */
CT construct(CT, A...)(A a) @trusted
if (is(CT == class) && !isAbstractClass!CT)
{
    auto size = typeid(CT).initializer.length;
    auto memory = getMem(size);
    static if (!hasUDA!(CT, NoInit))
        memory[0 .. size] = typeid(CT).initializer[];
    static if (__traits(hasMember, CT, "__ctor"))
        (cast(CT) (memory)).__ctor(a);
    static if (MustAddGcRange!CT)
    {
        import core.memory: GC;
        GC.addRange(memory, size, typeid(CT));
    }
    return cast(CT) memory;
}

/**
 * Returns a new, GC-free, class instance.
 *
 * This overload is designed to create factories and like the default
 * Object.factory method, it only calls, if possible, the default constructor.
 * The factory() function implemented in this iz.memory is based on this
 * construct() overload.
 *
 * Params:
 *      tic = The TypeInfo_Class of the Object to create.
 */
Object construct(TypeInfo_Class tic) @trusted
{
    if (tic.m_flags & 64)
        return null;
    auto size = tic.initializer.length;
    auto memory = getMem(size);
    memory[0 .. size] = tic.initializer[];
    Object result = cast(Object) memory;
    if (tic.defaultConstructor)
        tic.defaultConstructor(result);
    import core.memory: GC;
    GC.addRange(memory, size, tic);
    return result;
}

/**
 * Returns a new, GC-free, pointer to a struct or to an union.
 *
 * Params:
 *      ST = A struct or an union type.
 *      a = Variadic parameters passed to the constructor.
 */
ST* construct(ST, A...)(A a) @trusted
if(is(ST==struct) || is(ST==union))
{
    auto size = ST.sizeof;
    auto memory = getMem(size);
    static if (!hasUDA!(ST, NoInit))
    {
        __gshared static ST init = ST.init;
        void* atInit = &init;
        memory[0..size] = atInit[0..size];
    }
    static if (A.length)
    {
        static if (__traits(hasMember, ST, "__ctor"))
        {
            (cast(ST*) (memory)).__ctor(a);
        }
        else static if (A.length <= (__traits(allMembers, ST)).length)
        {
            import std.range: iota;
            foreach(i; aliasSeqOf!(iota(0, A.length)))
            {
                __traits(getMember, cast(ST*) memory, __traits(allMembers, ST)[i]) = a[i];
            }
        }
        else static assert(0, "too much argument to generate an automatic constructor");
    }
    static if (MustAddGcRange!ST)
    {
        import core.memory: GC;
        GC.addRange(memory, size, typeid(ST));
    }
    return cast(ST*) memory;
}

/**
 * Destructs a struct or a union that's been previously constructed
 * with $(D construct()).
 *
 * The function calls the destructor and, when passed as reference,
 * set the the instance to null.
 *
 * Params:
 *      T = A union or a struct type, likely to be infered.
 *      instance = A $(D T) instance.
 */
void destruct(T)(auto ref T* instance)
if (is(T == struct) || is(T==union))
{
    if (!instance)
        return;
    static if (__traits(hasMember, T, "__xdtor"))
        instance.__xdtor;
    freeMem(cast(void*) instance);
    instance = null;
}

/**
 * Destructs a struct or a union that's allocated within another
 * aggregate that's not GC-managed.
 *
 * Params:
 *      T = A union or a struct type, likely to be infered.
 *      instance = A $(D T) instance.
 */
void destruct(T)(ref T instance)
if (is(T == struct))
{
    static if (__traits(hasMember, T, "__xdtor"))
        instance.__xdtor;
}

/**
 * Destructs a class that's been previously constructed with $(D construct())
 * and when the static type is known.
 *
 * The function calls the destructor and, when passed as reference,
 * set the the instance to null.
 * When the static type is not known, destruct must be called after a cast to
 * Object.
 *
 * Params:
 *      assumeNoDtor = When no __ctor is found this avoids to search one
 *      in the base classes.
 *      T = A class type (most derived), likely to be infered.
 *      instance = A $(D T) instance.
 */
void destruct(bool assumeNoDtor = false, T)(auto ref T instance)
if (is(T == class) && T.stringof != Object.stringof)
{
    if (instance)
    {
        static if (__traits(hasMember, T, "__xdtor") || assumeNoDtor)
        {
            static if (__traits(hasMember, T, "__xdtor"))
                instance.__xdtor;
            freeMem(cast(void*)instance);
            instance = null;
        }
        else // dtor might be in an ancestor
        {
            destruct(cast(Object) instance);
        }
    }
}

/**
 * Destructs a class that's been previously constructed with $(D construct()) and
 * when the static type is not known.
 *
 * This overload is only selected when the instance is casted as Object.
 * It should be used when there no guarantee that the instance type is the most
 * derived. This overload is never @nogc.
 *
 * Params:
 *      instance = A class instance casted to Object.
 */
void destruct(T)(auto ref T instance)
if (is(T == class) && T.stringof == Object.stringof)
{
    if (instance)
    {
        TypeInfo_Class tic = cast(TypeInfo_Class) typeid(instance);
        if (void* dtorPtr = tic.destructor)
        {
            void delegate() dtor;
            dtor.funcptr = cast(void function()) dtorPtr;
            dtor.ptr = cast(void*) instance;
            dtor();
        }
        freeMem(cast(void*)instance);
        instance = null;
    }
}

/**
 * Destructs an interface implemented in an Object that's been previously
 * constructed with $(D construct()).
 *
 * This overload is never @nogc.
 *
 * Params:
 *      T = A class type, likely to be infered.
 *      instance = A $(D T) instance.
 */
void destruct(T)(auto ref T instance)
if (is(T == interface))
{
    if (instance)
    {
        version(Windows)
        {
            import core.sys.windows.unknwn: IUnknown;
            static assert(!is(T: IUnknown), "COM interfaces can't be destroyed in "
                ~ __PRETTY_FUNCTION__);
        }
        static if (__traits(allMembers, T).length)
        {
            bool allCpp()
            {
                bool result = true;
                foreach (member; __traits(allMembers, T))
                    foreach (ov; __traits(getOverloads, T, member))
                        static if (functionLinkage!ov != "C++")
                {
                    result = false;
                    break;
                }
                return result;
            }
            static assert(!allCpp, "C++ interfaces can't be destroyed in "
                ~ __PRETTY_FUNCTION__);
        }
        destruct(cast(Object) instance);
        instance = null;
    }
}

/**
 * Destructs on pointers simply forwards $(D freeMem).
 *
 * Params:
 *      instance = A pointer, typed or not.
 */
void destruct(T)(auto ref T* instance)
if (isBasicType!T)
{
    if (instance)
        freeMem(cast(void*) instance);
    instance = null;
}

/**
 * Returns a pointer to a new, GC-free, basic variable.
 * Any variable allocated using this function must be manually freed with freeMem.
 *
 * Params:
 *      T = The type of the pointer to return.
 *      preFill = Optional, boolean indicating if the result has to be initialized.
 *      a = Optional, the value.
 */
T* newPtr(T, bool preFill = false, A...)(A a)
if (isBasicType!T && A.length <= 1)
{
    T* result = cast(T*) getMem(T.sizeof);
    static if (A.length == 1)
        *result = a[0];
    else static if (preFill)
        *result = T.init;
    return result;
}

/**
 * Frees and invalidates a list of classes instances or struct pointers.
 * $(D destruct()) is called for each item.
 *
 * Params:
 *      objs = Variadic list of Object instances.
 */
void destructEach(Objs...)(auto ref Objs objs)
if (Objs.length > 1)
{
    foreach(ref obj; objs)
        destruct(obj);
}

/**
 * Register a class type that can be created dynamically, using its name.
 *
 * Class registration should only be done in module constructors. This allow
 * the registration to be thread safe since module constructors are executed
 * in the main thread.
 *
 * Params:
 *      T = A class.
 *      name = The name used to register the class.
 *      By default the $(D T.stringof) is used.
 *      f = The class repository, a hashmap of TypeInfo_Class by string.
 */
void registerFactoryClass(T, F)(ref F f, string name = "")
if (is(T == class) && !isAbstractClass!T)
{
    if (!name.length)
        name = T.stringof;
    f[name] = typeid(T);
}

/**
 * Calls registerClass() for each template argument.
 *
 * Params:
 *      A = A list of classes, from one up to nine.
 *      f = The class repository, a hashmap of TypeInfo_Class by string.
 */
void registerFactoryClasses(A1, A2 = void, A3 = void, A4 = void, A5 = void,
    A6 = void, A7 = void, A8 = void, A9 = void, F)(ref F f)
{
    void tryRegister(A)()
    {
        static if (is(A == class))
            registerFactoryClass!(A, F)(f);
        else static if (!is(A == void))
            static assert(0, A.stringof ~ " is not a class");
    }
    tryRegister!A1;tryRegister!A2;tryRegister!A3;
    tryRegister!A4;tryRegister!A5;tryRegister!A6;
    tryRegister!A7;tryRegister!A8;tryRegister!A9;
}

/**
 * Constructs a class instance using a gc-free factory.
 * The usage is similar to Object.factory() except that by default
 * registered classes don't take the module in account.
 *
 * Params:
 *      className = the class name, as registered in registerFactoryClass().
 *      factory = The class repository, a hashmap of TypeInfo_Class by string.
 * Throws:
 *      An Exception if the class is not registered.
 */
Object factory(F)(ref F f, string className)
{
    TypeInfo_Class* tic = className in f;
    if (!tic)
        throw construct!Exception("factory exception, unregistered class");
    return
        construct(*tic);
}

unittest
{
    import core.memory: GC;

    interface I{}
    class AI: I{}

    auto a = construct!Object;
    a.destruct;
    assert(!a);
    a.destruct;
    assert(!a);
    a.destruct;

    auto b = construct!Object;
    auto c = construct!Object;
    destructEach(a,b,c);
    assert(!a);
    assert(!b);
    assert(!c);
    destructEach(a,b,c);
    assert(!a);
    assert(!b);
    assert(!c);

    Object foo = construct!Object;
    Object bar = new Object;
    assert( GC.addrOf(cast(void*)foo) == null );
    assert( GC.addrOf(cast(void*)bar) != null );
    foo.destruct;
    bar.destroy;

    struct Foo
    {
        this(size_t a,size_t b,size_t c)
        {
            this.a = a;
            this.b = b;
            this.c = c;
        }
        size_t a,b,c;
    }

    Foo * foos = construct!Foo(1,2,3);
    Foo * bars = new Foo(4,5,6);
    assert(foos.a == 1);
    assert(foos.b == 2);
    assert(foos.c == 3);
    assert( GC.addrOf(cast(void*)foos) == null );
    assert( GC.addrOf(cast(void*)bars) != null );
    foos.destruct;
    bars.destroy;
    assert(!foos);
    foos.destruct;
    assert(!foos);

    union Uni{bool b; ulong ul;}
    Uni* uni0 = construct!Uni();
    Uni* uni1 = new Uni();
    assert( GC.addrOf(cast(void*)uni0) == null );
    assert( GC.addrOf(cast(void*)uni1) != null );
    uni0.destruct;
    assert(!uni0);
    uni0.destruct;
    assert(!uni0);

    auto ai = construct!AI;
    auto i = cast(I) ai;
    destruct(i);
    assert(i is null);

    abstract class Abst {}
    Object ab = construct(typeid(Abst));
    assert(ab is null);
}

@nogc unittest
{
    class Foo {@nogc this(int a){} @nogc~this(){}}
    Foo foo = construct!Foo(0);
    destruct(foo);
    assert(foo is null);

    static struct Bar {}
    Bar* bar = construct!Bar;
    destruct(bar);
    assert(bar is null);

    static struct Baz {int i; this(int,int) @nogc {}}
    Baz* baz = construct!Baz(0,0);
    destruct(baz);
    assert(baz is null);
}

unittest
{
    struct Foo {int a; ulong b;}
    Foo* f = construct!Foo(1,2);
    assert(f.a == 1);
    assert(f.b == 2);
    destruct(f);
}

unittest
{
    import core.memory: GC;
    import std.math: isNaN;

    auto f = newPtr!(float,true);
    assert(isNaN(*f));
    auto ui = newPtr!int;
    auto i = newPtr!uint;
    auto l = new ulong;
    auto u = newPtr!uint(8);
    assert(*u == 8u);

    assert(ui);
    assert(i);
    assert(f);

    assert(GC.addrOf(f) == null);
    assert(GC.addrOf(i) == null);
    assert(GC.addrOf(ui) == null);
    assert(GC.addrOf(l) != null);
    assert(GC.addrOf(u) == null);

    *i = 8u;
    assert(*i == 8u);

    freeMem(ui);
    freeMem(i);
    freeMem(f);
    freeMem(u);

    assert(ui == null);
    assert(i == null);
    assert(f == null);

    auto ptr = getMem(16);
    assert(ptr);
    assert(GC.addrOf(ptr) == null);
    ptr.freeMem;
    assert(!ptr);
}

unittest
{
    class A{string text; this(){text = "A";}}
    class B{string text; this(){text = "B";}}
    class C{int[] array; this(){array = [1,2,3];}}
    enum TypeInfo_Class[3] tics = [typeid(A),typeid(B),typeid(C)];

    A a = cast(A) construct(tics[0]);
    assert(a.text == "A");
    B b = cast(B) construct(tics[1]);
    assert(b.text == "B");
    C c = cast(C) construct(tics[2]);
    assert(c.array == [1,2,3]);

    destructEach(a, b, c);
}

unittest
{
    alias Factory = __gshared TypeInfo_Class[string];
    Factory classRegistry;

    class A{string text; this(){text = "A";}}
    class B{string text; this(){text = "B";}}
    class C{int[] array; this(){array = [1,2,3];}}
    registerFactoryClass!A(classRegistry);
    classRegistry.registerFactoryClass!B;
    classRegistry.registerFactoryClass!C;

    registerFactoryClasses!(A,B)(classRegistry);

    A a = cast(A) classRegistry.factory("A");
    assert(a.text == "A");
    B b = cast(B) classRegistry.factory("B");
    assert(b.text == "B");
    C c = cast(C) classRegistry.factory("C");
    assert(c.array == [1,2,3]);

    version(checkleaks) {} else
    {
        import std.exception: assertThrown;
        assertThrown(classRegistry.factory("D"));
    }

    destructEach(a,b,c);
}

@nogc unittest
{
    void* src = getMem(32);
    void* dst = getMem(32);
    (cast (ubyte*)src)[0..32] = 8;
    copyMem(dst, src, 32);
    foreach(immutable i; 0 .. 32)
        assert((cast (ubyte*)src)[i] == (cast (ubyte*)dst)[i]);
    (cast (ubyte*)src)[0..32] = 7;
    src = reallocMem(src, 32 + 16);
    ubyte* ovl = (cast (ubyte*)src) + 16;
    moveMem(cast(void*)ovl, cast(void*)src, 32);
    assert((cast (ubyte*)ovl)[31] == 7);
    freeMem(src);
    freeMem(dst);
}

@nogc unittest
{
    @NoInit static struct Foo {uint a = 1; ~this() @nogc{}}
    Foo* foo = construct!Foo;
    assert(foo.a != 1);
    @NoInit static class Bar {uint a = 1; ~this() @nogc{}}
    Bar bar = construct!Bar;
    assert(bar.a != 1);
    destructEach(foo, bar);
}

unittest
{
    static int i;

    template impl()
    {
        ~this(){i += 1;}
    }

    static class Base
    {
        mixin impl;
        mixin inheritedDtor;
        ~this(){i += 2;}
    }

    static class Derived: Base
    {
        mixin inheritedDtor;
        ~this(){i += 3; callInheritedDtor;}
    }

    Base b = construct!Derived;
    // without static type
    destruct(cast(Object) b);
    assert(i == 6);
    i = 0;

    Derived d = construct!Derived;
    // with static type
    destruct(d);
    assert(i == 6);
}

