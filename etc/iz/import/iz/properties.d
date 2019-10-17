/**
 * The iz property descriptor system.
 */
module iz.properties;

import
    std.traits;
import
    iz.memory, iz.types, iz.containers, iz.rtti, iz.enumset;

version(unittest) import std.stdio;

/**
 * Used as a generic property descriptor when Rtti are used to detect
 * the target type.
 */
alias GenericDescriptor = PropDescriptor!int;

/**
 * Describes a property declared in an aggregate.
 *
 * A property is described by a name, a setter and a getter. Several constructors
 * allow to define the descriptor using a setter, a getter but also a pointer to
 * the targeted field. Another useful member is the pointer to the Rtti structure
 * matching to the descriptor specialization.
 */
struct PropDescriptor(T)
{
    @disable this(this);

public:

    static if (!is(T == struct))
    {
        /// setter proptotype
        alias PropSetter = void delegate(T value);
        /// getter prototype
        alias PropGetter = T delegate();
        /// alternative setter prototype.
        alias PropSetterConst = void delegate(const T value);
    }
    else
    {
        /// setter proptotype
        alias PropSetter = void delegate(T value);
        /// getter prototype
        alias PropGetter = ref T delegate();
        /// alternative setter prototype.
        alias PropSetterConst = void delegate(const T value);
    }

package:

    PropHints _hints;

private:

    PropSetter _setter;
    PropGetter _getter;
    Object _declarator;
    const(Rtti)* _rtti;

    T* _directPtr;

    Array!(char) _name;

    void cleanup()
    {
        _directPtr = null;
        _setter = null;
        _getter = null;
        _declarator = null;
        _name = _name.init;
    }

    // pseudo setter internally used when a T is directly written.
    void internalSetter(T value)
    {
        alias TT = Unqual!T;
        T current = getter()();
        if (value != current)
            *cast(TT*) _directPtr = value;
    }

    // pseudo getter internally used when a T is directly read
    static if (is(T==struct))
        ref T internalGetter()
    {
        return *_directPtr;
    }
    else
        T internalGetter()
    {
        return *_directPtr;
    }

public:


// constructors ---------------------------------------------------------------+

    /**
     * Constructs a property descriptor from a PropSetter and a PropGetter.
     */
    this(PropSetter aSetter, PropGetter aGetter, string aName = "")
    in
    {
        assert(aSetter);
        assert(aGetter);
    }
    body
    {
        define(aSetter, aGetter, aName);
    }

    /**
     * Constructs a property descriptor from a PropSetterConst and a PropGetter.
     */
    static if (!is(T == const))
    this(PropSetterConst aSetter, PropGetter aGetter, string aName = "")
    in
    {
        assert(aSetter);
        assert(aGetter);
    }
    body
    {
        define(cast(PropSetter)aSetter, aGetter,aName);
    }

    /**
     * Constructs a property descriptor from a PropSetter and as getter
     * a pointer to a variable.
     */
    this(PropSetter aSetter, T* aSourceData, string aName = "")
    in
    {
        assert(aSetter);
        assert(aSourceData);
    }
    body
    {
        define(aSetter, aSourceData, aName);
    }

    /**
     * Constructs a property descriptor from a pointer to a variable used as
     * a setter and getter.
     */
    this(T* aData, string aName = "")
    in
    {
        assert(aData);
    }
    body
    {
        define(aData, aName);
    }

    ~this()
    {
        destruct(_name);
    }
// ----
// define all the members -----------------------------------------------------+

    /**
     * Defines a property descriptor from a PropSetter and a PropGetter.
     */
    void define(PropSetter aSetter, PropGetter aGetter, string aName = "")
    {
        cleanup;
        _rtti = getRtti!T;
        setter(aSetter);
        getter(aGetter);
        if (aName != "")
            name(aName);
        _declarator = cast(Object) aSetter.ptr;
    }

    /**
     * Defines a property descriptor from a PropSetter and as getter
     * a pointer to a variable.
     */
    void define(PropSetter aSetter, T* aSourceData, string aName = "")
    {
        cleanup;
        _rtti = getRtti!T;
        setter(aSetter);
        setDirectSource(aSourceData);
        if (aName != "")
            name(aName);
        _declarator = cast(Object) aSetter.ptr;
    }

    /**
     * Defines a property descriptor from a pointer to a variable used as
     * a setter and getter.
     */
    void define(T* aData, string aName = "", Object aDeclarator = null)
    {
        cleanup;
        _rtti = getRtti!T;
        setDirectSource(aData);
        setDirectTarget(aData);
        if (aName != "")
            name(aName);
        _declarator = aDeclarator;
    }
// ----
// setter ---------------------------------------------------------------------+

    /**
     * Sets the property setter using a standard method.
     */
    void setter(PropSetter value)
    {
        _setter = value;
        _declarator = cast(Object) value.ptr;
    }

    /// ditto
    PropSetter setter(){return _setter;}

    /**
     * Sets the property setter using a pointer to a variable
     */
    void setDirectTarget(T* location)
    {
        _directPtr = location;
        _setter = &internalSetter;
    }
    /**
     * Sets the property value
     */
    void set(T value) {_setter(value);}
// ----
// getter ---------------------------------------------------------------------+

    /**
     * Sets the property getter using a standard method.
     */
    void getter(PropGetter value)
    {
        _getter = value;
        _declarator = cast(Object) value.ptr;
    }

    /// ditto
    PropGetter getter(){return _getter;}

    /**
     * Sets the property getter using a pointer to a variable
     */
    void setDirectSource(T* value)
    {
        _directPtr = value;
        _getter = &internalGetter;
    }

    /**
     * Gets the property value
     */
    T get(){return _getter();}
// ----
// misc -----------------------------------------------------------------------+

    /**
     * Returns this descriptor casted as pointer to a GenericDescriptor.
     */
    PropDescriptor!int* genericDescriptor()
    {
        return cast(PropDescriptor!int*) &this;
    }

    /**
     * Returns this descriptor casted as pointer to a descriptor of the type
     * given by template parameter.
     */
    PropDescriptor!A* as(A)()
    {
        return cast(PropDescriptor!A*) &this;
    }

    /**
     * Sets of gets the string used to identify the property
     */
    void name(const(char)[] value)
    {
        _name = value;
    }

    /// ditto
    ref const(Array!char) name()
    {
        return _name;
    }

    /**
     * The object that declares this property.
     * When really needed, this value is set automatically.
     */
    void declarator(Object value)
    {
        _declarator = value;
    }

    /// ditto
    Object declarator(){return _declarator;}

    /**
     * Returns the RuntimeTypeInfo struct for the property type.
     */
    const(Rtti*) rtti(){return _rtti;}

    /**
     * Returns the hints for this property.
     */
    ref PropHints hints(){return _hints;}

// ----

}
///
unittest
{
    static struct Foo
    {
        private int _value = 1;
        PropDescriptor!int valueDescriptor;

        void value(int v){_value = v;}
        int value(){return _value;}
    }

    Foo foo;
    // defines the property using the setter and the getter.
    foo.valueDescriptor.define(&foo.value, &foo.value, "foo.value");
    // defines the property using the setter and a pointer to the field.
    foo.valueDescriptor.define(&foo.value, &foo._value, "foo.value");
    // .get and .set allow to access the property value
    foo.valueDescriptor.set(2);
    assert(foo.valueDescriptor.get() == 2);
    assert(foo.valueDescriptor.get() == foo.value());
    // getter() and setter() too but they are used to set/get the delegates.
    foo.valueDescriptor.setter()(1);
    assert(foo.valueDescriptor.getter()() == 1);
    // a descriptor has a fixed size, whatever is it's specialization,
    // that allows to cast safely to any other descriptor type.
    PropDescriptor!float* fpDescr = cast(PropDescriptor!float*) &foo.valueDescriptor;
    // that's why the halper "as" is provided to cast
    PropDescriptor!byte* byteDescr = fpDescr.as!byte;
    // and the actual type can be retrieved with the rtti
    assert(fpDescr.rtti is getRtti!int);
    assert(byteDescr.rtti is getRtti!int);
}

unittest
{
    class A
    {
        private int fi;
        int i(){return fi;}
        void i(in int aValue){fi = aValue;}
    }
    struct Si{uint f,r,e;}
    class B
    {
        private Si fi;
        ref Si i(){return fi;}
        void i(Si aValue){fi = aValue;}
    }

    auto a = construct!A;
    auto descrAi = PropDescriptor!int(&a.i,&a.i,"I");
    descrAi.setter()(5);
    assert(a.i == 5);
    assert(a.i == descrAi.getter()());
    assert(descrAi.declarator is a);

    auto refval = Si(1,2,333);
    auto b = construct!B;
    auto descrBi = PropDescriptor!(Si)(&b.i,&b.i,"I");
    descrBi.setter()(refval);
    assert(b.i.e == 333);
    assert(b.i.e == descrBi.getter()().e);

    auto descrSi0 = PropDescriptor!(Si)(&b.i, &b.fi);
    auto descrSi1 = PropDescriptor!(Si)(&b.fi);
    assert(descrSi0.get() == descrSi1.get());
    assert(descrSi0.genericDescriptor.rtti is descrSi0.rtti);

    auto refval0 = Si(1,2,3);
    descrSi1.set(refval0);

    destructEach(a,b);
}

unittest
{
    // the key that allows "safe" cast using iz.types.RunTimeTypeInfo
    static assert((PropDescriptor!(int)).sizeof == (PropDescriptor!(ubyte[])).sizeof);
    static assert((PropDescriptor!(string)).sizeof == (PropDescriptor!(ubyte[][][][])).sizeof);
}

/**
 * Allows to get the original $(D this) of a struct, for example when
 * passed from a $(D ref) getter function.
 */
void* getThis(T)(ref T t)
if (is(T == struct))
{
    return (cast(void*) &t);
}
///
unittest
{
    static struct Bar {uint[64] ui;}
    class Foo
    {
        Bar bar;
        ref Bar barRef(){return bar;}
    }

    Foo foo = construct!Foo;
    assert(&foo.bar == foo.barRef.getThis);
    Bar b = foo.barRef;
    assert(&foo.bar != &b);
    destruct(foo);
}

/// designed to annotate a property setter, e.g @Set
enum Set;
/// designed to annotate a property getter, e.g @Get
enum Get;
/// designed to annotate a field used as a property without accessors.
enum SetGet;
/// ditto
alias GetSet = SetGet;
/// designed to cancel the effect of @Set and @Get in the ancestor classes.
enum HideSet;
/// ditto
enum HideGet;

/**
 * Enumerates the property hints.
 *
 * Hints can be attributed to a property. They may be used or not,
 * depending on the context.
 */
enum PropHint
{
    /**
     * Indicates that a special behavior should be adopted
     * depending on if the value is equal or not the initializer.
     */
    initCare,
    /**
     * Indicates that a property should be considered read-only,
     * even if a setter is detected.
     */
    dontSet,
    /**
     * Indicates that a property should be considered write-only,
     * even if a getter is detected.
     */
    dontGet,
    /**
     * Indicates that a property shouldn't be used anymore.
     */
    obsolete
}

/**
 * PropHints can be used to set the hints attributed to a property,
 * using the UDA syntax: $(D @PropHints(...)).
 */
alias PropHints = EnumSet!(PropHint, Set8);

private string genStandardPropDescriptors()
{
    string result;
    foreach(T; BasicRtTypes[1..$])
    {
        result ~= ("public alias " ~ T.stringof ~ "Prop = PropDescriptor!(" ~
            T.stringof ~ ")" ~ "; ");
    }
    return result;
}

/// Property descriptors for the types defined in the iz.types.BasicTypes aliases.
mixin(genStandardPropDescriptors);

unittest
{
    // for cov
    assert(genStandardPropDescriptors.length);
}

/**
 * The PropertyPublisher interface allows a class to publish a collection
 * of properties described using the PropDescriptor format.
 *
 * The methods don't have to be implemented by hand as it's automatically done
 * when the PropertyPusblisherImpl template is mixed in a class.
 *
 * Inspiration:
 * The semantic used for this interface is inspired by the Object-Pascal
 * "published" protection attribute. In pascal, "published" causes the
 * member (called a property) to have the matching RTTI emitted. They
 * are used to stream objects, to build IDE inspectors, bindings list, etc.
 *
 * This interface (as well as its default implementation) reproduces a similar
 * system. Instead of "published", there are anotations, instead of the RTTI
 * tree structure there is an array of PropDescriptor.
 */
interface PropertyPublisher
{
    /**
     * Returns the count of descriptor this class publishes.
     */
    size_t publicationCount();
    /**
     * Returns a pointer to a descriptor according to its name.
     * Similar to the publication() function template excepted that the
     * result type has not to be specified.
     */
    GenericDescriptor* publicationFromName(const(char)[] name);
    /**
     * Returns a pointer the index-th descriptor.
     * Index must be within the [0 .. publicationCount] range.
     */
    GenericDescriptor* publicationFromIndex(size_t index);
    /**
     * Pointer to the object that has created the descriptor leading to this
     * PropertyPublisher instance.
     */
    Object declarator(); //acquirer
    /// ditto
    void declarator(Object value);
}
///
unittest
{
    import iz.streams;
    static class StuffPublisher: PropertyPublisher
    {
        // implements the interface as well as other usefull functions.
        mixin PropertyPublisherImpl;

        protected:
            @SetGet char[] _name = "Fantomas".dup;
            @SetGet ubyte _age = 33;
            MemoryStream _opaque;

        public:
            this()
            {
                // scans the stuff anotated with @GetSet, @Set, @Get
                collectPublications!StuffPublisher;
                _opaque = construct!MemoryStream;
            }
            ~this() {destruct(_opaque);}

            @Set void opaque(Stream stream)
            {/*class @Set are noop*/}
            @Get Stream opaque()
            {
                return _opaque;
            }
    }

    StuffPublisher stuffPublisher = construct!StuffPublisher;
    // 3 publications are available: name, age and opaque.
    // they will be handled automatically when binding or serializing.
    assert(stuffPublisher.publicationCount == 3);
    // One way to access the publications
    assert(stuffPublisher.publication!(char[])("name").get == "Fantomas");
    assert(stuffPublisher.publication!(Stream)("opaque").get is stuffPublisher.opaque());
    destruct(stuffPublisher);
}

/**
 * Returns true if the argument is a property publisher.
 */
bool isPropertyPublisher(T)()
{
    bool result = true;
    static if (is(T : PropertyPublisher))
        return result;
    else
    {
        foreach(interfaceFun;__traits(allMembers, PropertyPublisher))
        static if (!__traits(hasMember, T, interfaceFun))
        {
            result = false;
            break;
        }
        return result;
    }
}

///ditto
bool isPropertyPublisher(Object o)
{
    return (cast(PropertyPublisher) o) !is null;
}

unittest
{
    struct Foo{mixin PropertyPublisherImpl;}
    class Bar{mixin PropertyPublisherImpl;}
    class Baz: PropertyPublisher {mixin PropertyPublisherImpl;}
    static assert(isPropertyPublisher!Foo);
    static assert(isPropertyPublisher!Bar);
    static assert(isPropertyPublisher!Baz);
    auto baz = new Baz;
    assert( baz.isPropertyPublisher);
}

/**
 * Default implementation of a PropertyPublisher.
 *
 * When mixed in an aggregate type, two analyzers can be used to create
 * automatically the PropDescriptors that match the setter and getter pairs
 * anotated with @Set and @Get or that match the fields annotated with @SetGet.
 *
 * The analyzers are usually called in this(). The template has to be mixed in
 * each class generation that introduces new annotated properties.
 *
 * The analyzers, propCollectorGetPairs() and propCollectorGetFields(), are
 * function templates that must be instantiated with the type they have
 * to scan (typeof(this)). The two analyzers can be called with a
 * third function template: collectPublications().
 */
mixin template PropertyPublisherImpl()
{
    /**
     * Contains the list of PropDesrcriptors created by the analyzers.
     * Even if it's possible to access directly the array, it's safer to use
     * ($D publication()), ($D publicationFromName()) and ($D publicationFromIndex()).
     */
    static if (!__traits(hasMember, typeof(this), "_publishedDescriptors"))
    {
        import iz.containers: Array;
        protected Array!(void*) _publishedDescriptors;
    }

    static if (!__traits(hasMember, typeof(this), "clearDescriptors"))
    {
        /**
         * Destructs each property descriptor and empties their container.
         * After the call new descriptors can still be added.
         */
        pragma(inline, false)
        public final void clearDescriptors()
        {
            if (_publishedDescriptors.length)
            {
                import iz.memory: destruct;
                foreach(ptr; _publishedDescriptors)
                    if (ptr) destruct(cast(GenericDescriptor*) ptr);
            }
            _publishedDescriptors.length = 0;
        }

        ~this()
        {
            clearDescriptors;
            import iz.memory: destruct;
            destruct(_publishedDescriptors);
        }
    }

    static if (!__traits(hasMember, typeof(this), "_declarator"))
    protected Object _declarator;

// virtual methods or PropDescriptorCollection methods
//
// static if: the template injects some virtual methods that don't need
// to be overriden
// oror Base: because it looks like the interface makes the members
// detectable even if not yet implemented.

    import std.traits: BaseClassesTuple;
    alias ToT = typeof(this);
    // descendant already implements the interface
    enum BaseHas = is(BaseClassesTuple!ToT[0] : PropertyPublisher);
    enum HasItf = is(ToT : PropertyPublisher);
    // interface must be implemented from this generation, even if methods detected
    enum Base = HasItf & (!BaseHas);

    /// see PropertyPublisher
    static if (!__traits(hasMember, ToT, "declarator") || Base)
    public Object declarator() {return _declarator;}

    /// ditto
    static if (!__traits(hasMember, ToT, "declarator") || Base)
    pragma(inline, false)
    public void declarator(Object value) {_declarator = value;}

    /// see PropertyPublisher
    static if (!__traits(hasMember, ToT, "publicationCount") || Base)
    pragma(inline, false)
    public size_t publicationCount() {return _publishedDescriptors.length;}

    /// see PropertyPublisher
    static if (!__traits(hasMember, ToT, "publicationFromName") || Base)
    pragma(inline, false)
    public GenericDescriptor* publicationFromName(const(char)[] name)
    {return publication!int(name);}

    /// see PropertyPublisher
    static if (!__traits(hasMember, ToT, "publicationFromIndex") || Base)
    pragma(inline, false)
    public GenericDescriptor* publicationFromIndex(size_t index)
    {return cast(GenericDescriptor*) _publishedDescriptors[index];}

// templates: no problem with overrides, instantiated according to class This or That

    /**
     * Returns a pointer to a descriptor according to its name.
     * Params:
     *      T = The type of the property.
     *      name = The identifier used for the setter and the getter.
     *      createIfMissing = When set to true, the result is never null.
     * Returns:
     *      Null if the operation fails otherwise a pointer to a PropDescriptor!T.
     */
    pragma(inline, false)
    public PropDescriptor!T* publication(T)(const(char)[] name, bool createIfMissing = false)
    {
        PropDescriptor!T* descr;
        foreach(immutable i; 0 .. _publishedDescriptors.length)
        {
            auto maybe = cast(PropDescriptor!T*) _publishedDescriptors[i];
            if (maybe.name == name)
            {
                descr = maybe;
                break;
            }
        }
        if (createIfMissing && !descr)
        {
            import iz.memory: construct;
            _publishedDescriptors ~= construct!(PropDescriptor!T);
            (cast(typeof(descr))_publishedDescriptors[$-1]).name = name;
            descr = cast(typeof(descr)) _publishedDescriptors[$-1];
        }
        return descr;
    }

    /**
     * Performs all the possible analysis.
     */
    pragma(inline, false)
    public void collectPublications(T)()
    {
        collectPublicationsFromPairs!T;
        collectPublicationsFromFields!T;
    }

    /**
     * Creates the properties descriptors for each field annotated with @SetGet.
     *
     * If the field identifier starts with '_', 'f' or 'F' then the descriptor
     * .name member excludes this prefix, otherwise the descriptor .name is
     * identical.
     */
    pragma(inline, false)
    public void collectPublicationsFromFields(T)()
    {
        import iz.types: ScopedReachability;
        import std.traits: isCallable, isDelegate, isFunctionPointer, getUDAs;
        import iz.rtti: Rtti, getRtti;

        bool isFieldPrefix(char c) {return c == '_' || c == 'f' || c == 'F';}
        enum getStuff = q{__traits(getMember, T, member)};

        mixin ScopedReachability;
        foreach (member; __traits(derivedMembers, T))
        static if (member != "__dtor" && member != "__ctor")
        static if (isMemberReachable!(T, member))
        static if (is(typeof(mixin(getStuff))))
        static if (!isCallable!(mixin(getStuff)) || isDelegate!(mixin(getStuff))
            || isFunctionPointer!(mixin(getStuff)))
        {
            foreach (attribute; __traits(getAttributes, __traits(getMember, this, member)))
            static if (is(attribute == SetGet))
            {
                alias Type = typeof(__traits(getMember, this, member));
                auto propPtr = &__traits(getMember, this, member);
                static if (isFieldPrefix(member[0]))
                auto propName = member[1..$];
                else auto propName = member;
                auto descriptor = publication!Type(propName, true);
                descriptor.define(propPtr, propName);
                //
                static if (is(T : Object)) descriptor.declarator = cast(Object)this;
                static if (is(Type : Object))
                {
                    auto o = *cast(Object*) propPtr;
                    PropertyPublisher pub = cast(PropertyPublisher) o;
                    // RAII: if it's initialized then it's mine
                    if (pub) pub.declarator = this;
                }
                //
                enum h = getUDAs!(__traits(getMember, T, member), PropHints);
                static if (h.length) descriptor.hints = h[0];
                //
                version(none) writeln(attribute.stringof, " : ", member);
                break;
            }
        }
    }

    /**
     * Creates the property descriptors for the setter/getter pairs
     * annotated with @Set/@Get.
     *
     * In a class hierarchy, an overriden accessor replaces the ancestor's one.
     * If a setter is annoted with @HideSet or a getter with @HideGet then
     * the descriptor created when the ancestor was scanned is removed from the
     * publications.
     */
    pragma(inline, false)
    public void collectPublicationsFromPairs(T)()
    {
        import iz.types: ScopedReachability;
        import iz.rtti: Rtti, getRtti;
        import std.traits: Parameters, ReturnType, getSymbolsByUDA, getUDAs;
        import std.meta: AliasSeq, staticIndexOf;
        import std.algorithm.searching: countUntil;
        import std.format: format;

        // collect
        mixin ScopedReachability;
        foreach (member; __traits(derivedMembers, T))
        static if (isMemberReachable!(T, member))
        foreach (overload; __traits(getOverloads, T, member))
        {
            alias Attributes = AliasSeq!(__traits(getAttributes, overload));
            enum bool getterAttrib = staticIndexOf!(Get, Attributes) != -1;
            enum bool setterAttrib = staticIndexOf!(Set, Attributes) != -1;
            enum bool ungetAttrib  = staticIndexOf!(HideGet, Attributes) != -1;
            enum bool unsetAttrib  = staticIndexOf!(HideSet, Attributes) != -1;
            // define the getter
            static if (getterAttrib && !ungetAttrib)
            {
                alias Type = ReturnType!overload;
                const(Rtti)* ti = getRtti!Type;
                alias DescriptorType = PropDescriptor!Type;
                auto descriptor = publication!(Type)(member, true);
                auto dg = &overload;
                version(assert) if (descriptor.setter) assert (
                    // note: rtti unqalifies the type
                    ti is descriptor.rtti,
                    "setter and getter type mismatch for " ~ descriptor.name[]);
                descriptor.define(descriptor.setter, dg, member);
                //
                static if (is(T : Object)) descriptor.declarator = cast(Object)this;
                static if (is(Type : Object))
                {
                    auto o = cast(Object) dg();
                    PropertyPublisher pub = cast(PropertyPublisher) o;
                    // RAII: if it's initialized then it's mine
                    if (pub) pub.declarator = this;
                }
                //
                enum h = getUDAs!(overload, PropHints);
                static if (h.length) descriptor.hints = h[0];
                //
                version(none) writeln(attribute.stringof, " < ", member);
            }
            // define the setter
            else static if (setterAttrib && !unsetAttrib)
            {
                alias Type = Parameters!overload;
                const(Rtti)* ti = getRtti!Type;
                version(assert) static assert(Type.length == 1,
                    "setter must only have one parameter");
                alias DescriptorType = PropDescriptor!Type;
                auto descriptor = publication!(Parameters!overload)(member, true);
                auto dg = &overload;
                version(assert) if (descriptor.getter) assert (
                    ti == descriptor.rtti,
                    "setter and getter type mismatch for " ~ descriptor.name[]);
                descriptor.define(dg, descriptor.getter, member);
                //
                enum h = getUDAs!(overload, PropHints);
                static if (h.length) descriptor.hints = h[0];
                //
                version(none) writeln(attribute.stringof, " > ", member);
            }
            // hide from this descendant
            else static if (ungetAttrib || unsetAttrib)
            {
                if (auto descr = publication!size_t(member, false))
                {
                    const ptrdiff_t index = _publishedDescriptors[].countUntil(descr);
                    assert(index != -1);
                    destruct(cast(GenericDescriptor*)_publishedDescriptors[index]);
                    _publishedDescriptors = _publishedDescriptors[0..index] ~ _publishedDescriptors[index+1 .. $];
                }
            }
        }
        // check
        bool flag;
        string[] names;
        foreach (immutable i; 0 .. _publishedDescriptors.length)
        {
            GenericDescriptor* descr = publicationFromIndex(i);
            if (descr.setter is null || descr.getter is null)
            {
                flag = true;
                names ~= descr.name[];
            }
        }
        if (flag)
        {
            enum messg =
                "Several invalid property descriptors detected.\n" ~
                "The setter or the getter is missing for %s.\n" ~
                "Tip: check for typos in the function names.";
            throw new Error(format(messg, names));
        }
    }
}

unittest
{
    // test basic PropertyPublisher features: get descriptors, use them.
    class Foo: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        this(A...)(A a)
        {
            collectPublicationsFromFields!Foo;
            collectPublicationsFromPairs!Foo;
        }

        @SetGet private uint _anUint;
        @SetGet private static char[] _manyChars;
        private uint _a, _b;
        private char[] _c;

        @PropHints(PropHint.initCare)
        @Get uint propA(){return _a;}

        @Set void propA(uint aValue){_a = aValue;}

        @PropHints(PropHint.initCare,PropHint.dontSet)
        @Get uint propB(){return _b;}
        @Set void propB(uint aValue){_b = aValue;}

        @Get char[] propC(){return _c;}
        @Set void propC(char[] aValue){_c = aValue;}

        void use()
        {
            assert(propA == 0);
            auto aDescriptor = publication!uint("propA");
            aDescriptor.setter()(1234_5678);
            assert(propA == 1234_5678);

            assert(propB == 0);
            auto bDescriptor = publication!uint("propB");
            bDescriptor.setter()(8765_4321);
            assert(propB == 8765_4321);

            assert(!propC.length);
            auto cDescriptor = publication!(char[])("propC");
            cDescriptor.setter()("Too Strange To Be Good".dup);
            assert(propC == "Too Strange To Be Good");
            propC = "Too Good To Be Strange".dup;
            assert( publication!(char[])("propC").getter()() == "Too Good To Be Strange");

            assert(_anUint == 0);
            auto anUintDescriptor = publication!uint("anUint");
            anUintDescriptor.setter()(1234_5678);
            assert(_anUint == 1234_5678);

            assert(_manyChars == null);
            auto manyCharsDescriptor = publication!(char[])("manyChars");
            manyCharsDescriptor.setter()("BimBamBom".dup);
            assert(_manyChars == "BimBamBom");
            _manyChars = "BomBamBim".dup;
            assert(manyCharsDescriptor.getter()() == "BomBamBim");
        }
    }
    Foo foo = construct!Foo;
    foo.use;
    foo.destruct;
}

unittest
{
    class Bar
    {
        size_t _field;
        string info;
        mixin PropertyPublisherImpl;
        this()
        {
            collectPublicationsFromPairs!Bar;
        }
        @Set void field(size_t aValue)
        {
            info ~= "Bar";
        }
        @Get size_t field()
        {
            info = "less derived";
            return _field;
        }
    }
    class Baz : Bar
    {
        @Set override void field(size_t aValue)
        {
            super.field(aValue);
            info ~= "Baz";
        }
        @Get override size_t field()
        {
            info = "most derived";
            return _field;
        }
    }

    // test that the most derived override is used as setter or getter
    Baz baz = construct!Baz;
    assert(baz.publicationCount == 1);
    auto prop = baz.publication!size_t("field");
    prop.set(0);
    assert(baz.info == "BarBaz");
    assert(baz.publicationCount == 1);
    auto a = prop.get;
    assert(baz.info == "most derived");
    baz.destruct;
}

unittest
{
    alias Delegate = void delegate(uint a);
    class Cat
    {
        mixin PropertyPublisherImpl;
        @SetGet Delegate _meaow;
        this(){collectPublications!Cat;}
    }

    class Fly
    {
        mixin PropertyPublisherImpl;
        @GetSet string _bzzz(){return "bzzz";}
        this(){collectPublications!Fly;}
    }

    // test that a delegate is detected as a field
    Cat cat = construct!Cat;
    assert(cat.publicationCount == 1);
    auto descr = cast(PropDescriptor!uint*) cat.publicationFromIndex(0);
    assert(descr);
    assert(descr.rtti.type == RtType._funptr);
    // test that a plain function is not detected as field
    Fly fly = construct!Fly;
    assert(fly.publicationCount == 0);
    destructEach(cat, fly);
}

unittest
{
    class Bee
    {
        mixin PropertyPublisherImpl;
        this(){collectPublicationsFromPairs!Bee;}
        @Set void delegate(uint) setter;
        @Get int delegate() getter;
    }
    // test that delegates as fields are not detected as set/get pairs
    Bee bee = construct!Bee;
    assert(bee.publicationCount == 0);
    destruct(bee);
}

unittest
{
    class B0
    {
        mixin PropertyPublisherImpl;
        this(){collectPublications!B0;}
        @SetGet int _a;
    }
    class B1: B0
    {
        mixin PropertyPublisherImpl;
        this(){collectPublications!B1;}
        @Set void b(int value){}
        @Get int b(){return 0;}
        @SetGet int _c;
    }
    // test that all props are detected in the inheritence list
    auto b1 = construct!B1;
    assert(b1.publicationCount == 3);
    destruct(b1);
}

unittest
{
    class B0
    {
        mixin PropertyPublisherImpl;
        this(){collectPublications!B0;}
        @Set void b(int value){}
        @Get int b(){return 0;}
    }
    class B1: B0
    {
        mixin PropertyPublisherImpl;
        this(){collectPublications!B1;}
        @HideGet override int b(){return super.b();}
    }
    // test that a prop marked with @HideSet/Get is not published anymore
    auto b0 = construct!B0;
    assert(b0.publicationCount == 1);
    auto b1 = construct!B1;
    assert(b1.publicationCount == 0);
    destructEach(b0,b1);
}

unittest
{
    struct Bug
    {
        mixin PropertyPublisherImpl;
        this(uint value){collectPublications!Bug;}
        @SetGet uint _a;
    }
    // test that the 'static if things' related to 'interface inheritance'
    // dont interfere when mixed in struct
    Bug bug = Bug(0);
    assert(bug.publicationCount == 1);
    assert(bug.publicationFromName("a") != null);
    assert(bug.publicationFromName("a") == bug.publicationFromIndex(0));
}

unittest
{
    // test safety, multiple setter types
    enum decl = q{
        class Bug
        {
            mixin PropertyPublisherImpl;
            this(){collectPublications!Bug;}
            @Set void b(int value, uint ouch){}
            @Get int b(){return 0;}
        }
    };
    static assert( !__traits(compiles, mixin(decl)));
}

version(checkleaks){} else unittest
{
    // test safety, setter & getter types mismatch
    version(assert)
    {
        bool test;
        class Bug
        {
            mixin PropertyPublisherImpl;
            this(){collectPublications!Bug;}
            @Set void b(string value){}
            @Get int b(){return 0;}
        }
        Bug b;
        try b = construct!Bug;
        catch(Error e) test = true;
        assert(test);
        destruct(b);
    }
}

unittest
{
    // test initial collector/declarator/ownership
    class B: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        this(){collectPublications!B;}
    }
    class A: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        B _owned2;
        this()
        {
            _owned1 = construct!B;
            _owned2 = construct!B;
            collectPublications!A;
        }
        ~this()
        {
            destructEach(_owned1, _owned2);
        }
        @SetGet uint _a;
        // ownership is set in getFields
        @SetGet B _owned1;
        // ownership is set in getPairs
        @Get B owned2(){return _owned2;}
        @Set void owned2(B value){}
        // ownership is not set because value initially is null
        @SetGet B _notowned;
    }
    auto a1 = construct!A;
    auto a2 = construct!A;
    a1._notowned = a2._owned1;
    //
    assert(a1._owned1.declarator is a1);
    assert(a1._owned2.declarator is a1);
    assert(a1._notowned.declarator is a2);
    destructEach(a1,a2);
}

version(checkleaks){} else unittest
{
    // test that invalid pairs throw
    class Test: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        this(){collectPublications!Test;}
        // missing getter.
        @Set void error1(Object o){}
        // typo in funct name.
        @Get Object errror2(){return null;}
        @Set void error2(Object o){}
    }
    bool ouch;
    Test test;
    try test = construct!Test;
    catch(Error e)ouch = true;
    assert(ouch);
    destruct(test);
}

unittest
{
    struct A{}
    class B
    {
        mixin PropertyPublisherImpl;
        A _a;
        @Get ref A a(){return _a;}
        @Set void a(A){}
        this()
        {
            collectPublications!B;
        }
    }
    B b = construct!B;
    destruct(b);
}

/**
 * Returns true if the target of a PropDescriptor!Object is owned by another
 * object.
 *
 * The serializer and the binders use this to determine if the members of a
 * nested object have to be copied / serialized or rather just the reference.
 *
 * Params:
 *      descriptor = A pointer to the target accessor.
 *      t = The potential owner.
 */
bool targetObjectOwnedBy(T)(PropDescriptor!Object* descriptor, T t)
if (isPropertyPublisher!T)
{
    auto o = cast(PropertyPublisher) descriptor.get();
    if (o)
        return o.declarator !is t.declarator;
    else
        return false;
}

/**
 * Constrains the target of a PropDescriptor!Object to be owned
 * by a particular object.
 *
 * Params:
 *      descriptor = A pointer to the target accessor.
 *      t = The future owner.
 */
void setTargetObjectOwner(T)(PropDescriptor!Object* descriptor, T t)
if (isPropertyPublisher!T)
{
    if (auto o = cast(PropertyPublisher) descriptor.get())
    {
        o.declarator = t;
        descriptor.declarator = t;
    }
}

unittest
{
    class Foo(bool Nested) : PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        this()
        {
            static if (Nested)
                _full = construct!(Foo!false);
            collectPublications!Foo;
        }

        ~this()
        {
            static if (Nested)
                destruct(_full);
        }

        @SetGet Foo!false _asref;
        static if (Nested)
        @SetGet Foo!false _full;
    }
    auto foo = construct!(Foo!true);
    assert(targetObjectOwnedBy(foo.publication!Object("full"), foo));
    assert(!targetObjectOwnedBy(foo.publication!Object("asref"), foo));
    destruct(foo);
}

/**
 * Destructs the PropertyPublishers that are published and owned by an object.
 *
 * Note that after the call, the references to the objects that are destructed
 * are dangling.
 *
 * Params:
 *      recursive = If set to true, the default, the function is called on the
 *      sub-publishers before being destructed.
 *      pub = The PropertyPublisher whose sub-publishers have to be destructed.
 */
void destructOwnedPublishers(bool recursive = true)(PropertyPublisher pub)
{
    foreach(immutable i; 0 .. pub.publicationCount)
    {
        PropDescriptor!Object* d = cast(PropDescriptor!Object*)
            pub.publicationFromIndex(i);

        if (!d || d.rtti.type != RtType._object || !d.declarator)
            continue;

        Object obj = d.get();
        if (!obj)
            continue;

        PropertyPublisher sub =  cast(PropertyPublisher) obj;
        if (!sub || !sub.declarator)
            continue;

        if (sub.declarator !is d.declarator)
            continue;

        static if (recursive)
            destructOwnedPublishers(sub);

        destruct(obj);
    }
}
///
unittest
{
    class A19: PropertyPublisher
    {mixin PropertyPublisherImpl; string c;}

    class B19: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        @SetGet A19 _a0;
        @SetGet A19 _a1;
        this()
        {
            _a0 = construct!A19;
            collectPublications!B19;
            // _a1 not seen by the scanner so not owned
            _a1 = construct!A19;
        }
        ~this()
        {
            destruct(_a1);
        }
    }
    B19 b = construct!B19;
    // implicitly destructs b._a0.
    destructOwnedPublishers(b);
    destruct(b);
}

unittest
{
    class A: PropertyPublisher {mixin PropertyPublisherImpl;}
    A a = construct!A;
    destruct(a);
}

//TODO-cproperties: findPublisher overload that can also take a pub struct and return a pointer to a publisher

/**
 * Finds a sub-publisher in an object's publications tree.
 *
 * Params:
 *      pub = The root publisher, either a class or a struct.
 *      accessChain = A string made of property names separated by dots.
 *      It must not starts with the name of the root.
 * Returns:
 *      null if the sub-publisher could not be found.
 *      null if the sub-publisher could be found but doesn't exist yet.
 *      The matching publisher otherwise.
 */
PropertyPublisher findPublisher(PropertyPublisher pub, string accessChain)
{
    import iz.strings: bySeparated;
    auto names = accessChain.bySeparated('.');

    while(true)
    {
        if (!pub || names.empty)
            return pub;
        if (auto descr = cast(PropDescriptor!Object*) pub.publicationFromName(names.front))
        {
            pub = null;
            if (descr.rtti.type == RtType._object)
                if (Object o = descr.get())
                    pub = cast(PropertyPublisher) o;
        }
        else pub = null;
        names.popFront;
    }
}
///
unittest
{
    class AU: PropertyPublisher
    {mixin PropertyPublisherImpl;}

    class B: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        mixin inheritedDtor;

        @SetGet AU _a;
        this()
        {
            _a = construct!AU;
            collectPublications!B;
        }
        ~this()
        {
            destruct(_a);
        }
    }

    class C: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        mixin inheritedDtor;

        @SetGet B _b;
        this()
        {
            _b = construct!B;
            collectPublications!C;
        }
        ~this()
        {
            destruct(_b);
        }
    }

    C c = construct!C;

    assert(c.findPublisher("b") == c._b);
    assert(c.findPublisher("b.a") == c._b._a);
    assert(c.findPublisher("b.a.g") is null);
    destruct(c);
}

/**
 * Returns true if two property descriptors are bindable.
 * To be bindable, the property name must be identical but also their types.
 * Type checking is based on RTTI and neither source nor target
 * needs to be passed with the correct type.
 */
bool areBindable(S,T)(PropDescriptor!S* source, PropDescriptor!T* target)
in
{
    assert(source);
    assert(target);
    assert(cast(void*)target != cast(void*)source);
}
body
{
    return source.name == target.name && source.rtti is target.rtti;
}
///
unittest
{
    int a,b;

    // exact match
    PropDescriptor!int aDescriptor = PropDescriptor!int(&a, "a");
    PropDescriptor!int bDescriptor = PropDescriptor!int(&b, "a");
    assert(areBindable(&aDescriptor, &bDescriptor));

    // it also works with different static types thanks to the RTTI
    auto cDescriptor = cast(PropDescriptor!float*) &aDescriptor;
    assert(areBindable(cDescriptor, &bDescriptor));
}

/**
 * Binds two property publishers.
 *
 * After the call, each property published in source and target that have the
 * same runtime type and the same name share the same value.
 *
 * Params:
 *      recursive = Indicates if the process is recursive.
 *      src = The aggregate from where the properties values are copied.
 *          Either a class or a struct that's mixed with PropertyPublisherImpl
 *          or a PropertyPublisher.
 *      trg = The aggregate to where the properties values are copied.
 *          The Target type has the same requirement as the source type.
 */
void bindPublications(bool recursive = false, S, T)(auto ref S src, auto ref T trg)
{
    // try to get a publisher from src
    static if (is(S == class))
    {
        PropertyPublisher source = cast(PropertyPublisher) src;
        if (!src)
            return;
    }
    else static if (is(S == PropertyPublisher))
    {
        alias source = src;
        if (!src)
            return;
    }
    else static if (is(S == const(PubTraits)))
    {
        alias source = src;
    }
    else static if (is(S == struct))
    {
        if (!isPublisingStruct(getRtti!S))
            return;
        alias source = src;
    }
    else static assert(0, S.stringof ~ " is not a property publisher");

    // try to get a publisher from trg
    static if (is(T == class))
    {
        PropertyPublisher target = cast(PropertyPublisher) trg;
        if (!trg)
            return;
    }
    else static if (is(T == PropertyPublisher))
    {
        alias target = trg;
        if (!trg)
            return;
    }
    else static if (is(T == const(PubTraits)))
    {
        alias target = trg;
    }
    else static if (is(T == struct))
    {
        if (!isPublisingStruct(getRtti!T))
            return;
        alias target = trg;
    }
    else static assert(0, T.stringof ~ " cannot be a PropertyPublisher");

    enum PubIsStruct = is(S == struct);

    // bind publications
    GenericDescriptor* srcP, trgP;
    foreach(immutable i; 0 .. source.publicationCount())
    {
        srcP = cast(GenericDescriptor*) source.publicationFromIndex(i);
        trgP = cast(GenericDescriptor*) target.publicationFromName(srcP.name[]);

        if (!trgP) continue;
        if (srcP.rtti !is trgP.rtti) continue;

        void setBasicType(T)()
        {
            final switch (srcP.rtti.dimension != 0)
            {
                // value ABI
                case false:
                    alias DT0 = PropDescriptor!T*;
                    (cast(DT0) trgP).set((cast(DT0) srcP).get());
                    break;
                // array ABI
                case true:
                    alias DT1 = PropDescriptor!(T[])*;
                    (cast(DT1) trgP).set((cast(DT1) srcP).get());
                    break;
            }
        }

        void setAA()
        {
            alias DT = PropDescriptor!(int[int])*;
            (cast(DT) trgP).set((cast(DT) srcP).get());
        }

        void setObject()
        {
            static if (!PubIsStruct) // PubTraits should also have declarator, even if not used
            {
                const Object srcObj = srcP.as!Object.get();
                const Object trgObj = trgP.as!Object.get();
                if (!srcObj || !trgObj)
                    return;

                PropertyPublisher sPb = cast(PropertyPublisher) srcObj;
                PropertyPublisher tPb = cast(PropertyPublisher) trgObj;
                if (!sPb || !tPb)
                    return;

                // reference
                if (sPb.declarator !is cast(Object) source
                    && tPb.declarator !is cast(Object) target)
                        setBasicType!Object;
                // sub object
                else static if (recursive)
                {
                    bindPublications!true(sPb, tPb);
                }
            }
        }

        void setStruct()
        {
            auto srcPd = cast(PropDescriptor!GenericStruct*) srcP;
            auto trgPd = cast(PropDescriptor!GenericStruct*) trgP;
            void* srcStruct = getThis(srcPd.getter()());
            void* trgStruct = getThis(trgPd.getter()());
            const(StructInfo)* srcStructRtti(){return srcP.rtti.structInfo;}
            const(StructInfo)* trgStructRtti(){return trgP.rtti.structInfo;}
            with(StructType) final switch(srcStructRtti.type)
            {
                case _none:
                    break;
                case _text:
                    void* oldPtr = srcStructRtti.textTraits.setContext(srcStruct);
                    const(char)[] value = srcStructRtti.textTraits.saveToText();
                    srcStructRtti.textTraits.restoreContext(oldPtr);
                    oldPtr = trgStructRtti.textTraits.setContext(trgStruct);
                    trgStructRtti.textTraits.loadFromText(value);
                    trgStructRtti.textTraits.restoreContext(oldPtr);
                    break;
                case _binary:
                    void* oldPtr = srcStructRtti.binTraits.setContext(srcStruct);
                    ubyte[] value = srcStructRtti.binTraits.saveToBytes();
                    srcStructRtti.binTraits.restoreContext(oldPtr);
                    oldPtr = trgStructRtti.binTraits.setContext(trgStruct);
                    trgStructRtti.binTraits.loadFromBytes(value);
                    trgStructRtti.binTraits.restoreContext(oldPtr);
                    break;
                case _publisher:
                    static if (recursive)
                    {
                        const(PubTraits) srcPub = *srcStructRtti.pubTraits;
                        const(PubTraits) trgPub = *trgStructRtti.pubTraits;
                        srcPub.setContext(srcStruct);
                        trgPub.setContext(trgStruct);
                        bindPublications!true(srcPub, trgPub);
                    }
            }
        }

        import iz.streams: Stream;

        with(RtType) final switch (srcP.rtti.type)
        {
            case _invalid: case _union: break;
            case _aa:       setAA; break;
            case _bool:     setBasicType!bool; break;
            case _ubyte:    setBasicType!ubyte; break;
            case _byte:     setBasicType!byte; break;
            case _ushort:   setBasicType!ushort; break;
            case _short:    setBasicType!short; break;
            case _uint:     setBasicType!uint; break;
            case _int:      setBasicType!int; break;
            case _ulong:    setBasicType!ulong; break;
            case _long:     setBasicType!long; break;
            case _float:    setBasicType!float; break;
            case _double:   setBasicType!double; break;
            case _real:     setBasicType!real; break;
            case _char:     setBasicType!char; break;
            case _wchar:    setBasicType!wchar; break;
            case _dchar:    setBasicType!dchar; break;
            case _stream:   setBasicType!Stream; break;
            case _funptr:   setBasicType!GenericFunction; break;
            case _enum:     setBasicType!int; break;
            case _struct:   setStruct; break;
            case _object:   setObject; break;
            case _pointer:  setBasicType!(int*);
        }
    }
}

unittest
{
    import iz.streams: Stream, MemoryStream;

    static struct T23Bytes
    {
        enum ubyte[] _value = [0,1,2,3];
        ubyte[] saveToBytes() {return _value;}
        void loadFromBytes(ubyte[] value){assert(value == _value);}
    }

    static struct T23Text
    {
        enum const(char)[] _value = "value";
        const(char)[] saveToText() {return _value;}
        void loadFromText(const(char)[] value){assert(value == _value);}
    }

    class T23Foo(bool Nested) : PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        mixin inheritedDtor;

        this()
        {
            static if (Nested)
                _sub = construct!(T23Foo!false);
            str = construct!MemoryStream;
            collectPublications!(T23Foo!Nested);

            static if (Nested)
                assert(_sub.declarator is this);

        }
        ~this()
        {
            // TODO-cbugfix: nested prop publisher bug after binding
            static if (Nested) if (_sub)
                destruct(_sub);
            destruct(str);
        }
        @SetGet uint _a;
        @SetGet ulong _b;
        @SetGet string _c;
        @SetGet int[][] _d;
        @SetGet int[][][] _e;
        @SetGet T23Bytes _bytes;
        @SetGet T23Text _text;
        @SetGet int[string] _f;
        MemoryStream str;

        enum Ea{a0, a1}
        @SetGet Ea _ea;

        @Set void stream(Stream s)
        {str.loadFromStream(s);}
        @Get Stream stream(){return str;}

        static if (Nested)
        {
            @NoGc @SetGet T23Foo!false _sub;
        }
    }

    T23Foo!true source = construct!(T23Foo!true);
    T23Foo!true target = construct!(T23Foo!true);
    source._a = 8; source._b = ulong.max; source._c = "123";
    source._d = [[0,1,2,3],[4,5,6,7]];
    source._e = [[[0,1],[2,3]],[[4,5],[6,7]],[[8,9],[10,11]]];
    source._f = ["a":0, "b":1];
    source._ea = source.Ea.a1;
    source._sub._a = 8; source._sub._b = ulong.max; source._sub._c = "123";
    source.str.writeInt(1);source.str.writeInt(2);source.str.writeInt(3);
    source._sub.str.writeInt(1);source._sub.str.writeInt(2);source._sub.str.writeInt(3);

    bindPublications!true(source, target);

    assert(target._a == source._a);
    assert(target._b == source._b);
    assert(target._c == source._c);
    assert(target._d == source._d);
    assert(target._e == source._e);
    assert(target._f == source._f);
    assert(target._ea == source.Ea.a1);
    assert(target._sub._a == source._sub._a);
    assert(target._sub._b == source._sub._b);
    assert(target._sub._c == source._sub._c);
    target.str.position = 0;
    assert(target.str.readInt == 1);
    assert(target.str.readInt == 2);
    assert(target.str.readInt == 3);
    target._sub.str.position = 0;
    assert(target._sub.str.readInt == 1);
    assert(target._sub.str.readInt == 2);
    assert(target._sub.str.readInt == 3);

    destructEach(source, target);
}

unittest
{
    static struct Child
    {
        mixin PropertyPublisherImpl;
        @SetGet int _a = 8;
        @SetGet int _b = 9;
        ~this() {this.clearDescriptors; }
    }

    static struct Parent
    {
        mixin PropertyPublisherImpl;
        @SetGet Child _child1;
        @SetGet Child _child2;
        ~this() {this.clearDescriptors; }
    }

    Parent p0, p1;
    p0.collectPublications!Parent;
    p0._child1.collectPublications!Child;
    p0._child2.collectPublications!Child;
    p1.collectPublications!Parent;
    p1._child1.collectPublications!Child;
    p1._child2.collectPublications!Child;

    assert(p0.publicationCount == 2);
    assert(p0._child1.publicationCount == 2);
    //
    p0._child1._a = 0;
    p0._child1._b = 0;
    p0._child2._a = 0;
    p0._child2._b = 0;
    //
    bindPublications!true(p0, p1);
    assert(p1._child1._a == 0);
    assert(p1._child1._b == 0);
    assert(p1._child2._a == 0);
    assert(p1._child2._b == 0);
}

/**
 * A PropertyBinder synchronizes the value of several variables between themselves.
 *
 * The access to the variables is done via the PropDescriptor format, hence
 * a PropertyBinder stores a list of PropDescriptor with the same types.
 *
 * Params:
 *      T = The type of the properties.
 *      RttiCheck = When set to true, an additional run-time check is performed.
 */
class PropertyBinder(T, bool RttiCheck = false)
{

    mixin inheritedDtor;

private:

    ContiguousList!(PropDescriptor!T*) _itemsToDestruct;
    ContiguousList!(PropDescriptor!T*) _items;
    PropDescriptor!T *_source;

public:

    ///
    this()
    {
        _items = construct!(ContiguousList!(PropDescriptor!T*));
        _itemsToDestruct = construct!(ContiguousList!(PropDescriptor!T*));
    }

    ~this()
    {
        foreach(immutable i; 0 .. _itemsToDestruct.count)
        {
            auto descr = _itemsToDestruct[i];
            if (descr) destruct(descr);
        }
        _items.destruct;
        _itemsToDestruct.destruct;
        callInheritedDtor;
    }

    /**
     * Adds a property to the list.
     * If the binder is not local then aProp should neither be a local descriptor,
     * otherwise the descritpor reference will become invalid.
     *
     * Params:
     *      aProp = A PropDescriptor of type T.
     *      isSource = Optional boolean indicating if the descriptor is used as
     *          master property.
     *
     * Returns:
     *      The index of the descriptor in the binding list.
     */
    ptrdiff_t addBinding(ref PropDescriptor!T prop, bool isSource = false)
    {
        static if (RttiCheck)
        {
            if (getRtti!T !is aProp.rtti)
                return -1;
        }
        if (isSource) _source = &prop;
        return _items.add(&prop);
    }

    /**
     * Adds a new property to the list.
     * The life-time of the new descriptor is handled internally.
     *
     * Returns:
     *      A new PropDescriptor of type T.
     */
    PropDescriptor!T* newBinding()
    {
        auto result = construct!(PropDescriptor!T);
        _items.add(result);
        _itemsToDestruct.add(result);
        return result;
    }

    /**
     * Removes the aIndex-nth property from the list.
     * The item is freed if it has been allocated by newBinding.
     * source might be invalidated if it matches the item.
     *
     * Params:
     *      index = The index of the descriptor to remove.
     */
    void removeBinding(size_t index)
    {
        auto itm = _items.extract(index);
        if (_source && itm == _source)
            _source = null;
        if (_itemsToDestruct.remove(itm))
            destruct(itm);
    }

    /**
     * Triggers the setter of each property.
     * This method is usually called at the end of a setter method
     * (in the master/source setter).
     *
     * Params:
     *      value = the new value to send to binding.
     */
    void change(T value)
    {
        foreach(item; _items)
            if (item.setter)
                item.set(value);
    }

    /**
     * Calls change() using the value of source.
     */
    void updateFromSource()
    {
        if (_source)
            change(_source.get());
    }

    /**
     * Sets the property used as source in updateFromSource().
     * Params:
     *      src = The property to be used as source.
     */
    void source(ref PropDescriptor!T src) {_source = &src;}

    /**
     * Returns the property used as source in _updateFromSource().
     */
    PropDescriptor!T* source() {return _source;}

    /**
     * Provides an access to the property descriptors for additional List operations.
     * Note that the items whose life-time is managed should not be modified.
     */
    ContiguousList!(PropDescriptor!T*) items() {return _items;}
}

unittest
{
    alias intprops = PropertyBinder!int;
    alias floatprops = PropertyBinder!float;

    class Foo
    {
        private
        {
            int fA;
            float fB;
            intprops fASlaves;
            floatprops fBSlaves;
        }
        public
        {
            this()
            {
                fASlaves = construct!intprops;
                fBSlaves = construct!floatprops;
            }
            ~this()
            {
                fASlaves.destruct;
                fBSlaves.destruct;
            }
            void A(int value)
            {
                if (fA == value) return;
                fA = value;
                fASlaves.change(fA);
            }
            int A(){return fA;}

            void B(float value)
            {
                if (fB == value) return;
                fB = value;
                fBSlaves.change(fB);
            }
            float B(){return fB;}

            void addABinding(ref intProp aProp)
            {
                fASlaves.addBinding(aProp);
            }

            void addBBinding(ref floatProp aProp)
            {
                fBSlaves.addBinding(aProp);
            }
        }
    }

    class FooSync
    {
        private
        {
            int fA;
            float fB;
        }
        public
        {
            void A(int value){fA = value;}
            int A(){return fA;}
            void B(float value){fB = value;}
            float B(){return fB;}
        }
    }

    class Bar
    {
        public int A;
        public float B;
    }

    // 1 master, 2 slaves
    auto a0 = construct!Foo;
    auto a1 = construct!FooSync;
    auto a2 = construct!FooSync;
    auto a3 = construct!Bar;

    auto prp1 = intProp(&a1.A,&a1.A);
    a0.addABinding(prp1);

    auto prp2 = intProp(&a2.A,&a2.A);
    a0.addABinding(prp2);

    intProp prp3 = intProp(&a3.A);
    a0.addABinding(prp3);

    auto prpf1 = floatProp(&a1.B,&a1.B);
    auto prpf2 = floatProp(&a2.B,&a2.B);
    auto prpf3 = floatProp(&a3.B);
    a0.addBBinding(prpf1);
    a0.addBBinding(prpf2);
    a0.addBBinding(prpf3);

    a0.A = 2;
    assert( a1.A == a0.A);
    a1.A = 3;
    assert( a1.A != a0.A);
    a0.A = 4;
    assert( a2.A == a0.A);
    a0.A = 5;
    assert( a3.A == a0.A);

    a0.B = 2.5;
    assert( a1.B == a0.B);
    a1.B = 3.5;
    assert( a1.B != a0.B);
    a0.B = 4.5;
    assert( a2.B == a0.B);
    a0.B = 5.5;
    assert( a3.B == a0.B);

    // interdependent bindings
    auto m0 = construct!Foo;
    auto m1 = construct!Foo;
    auto m2 = construct!Foo;

    intProp mprp0 = intProp(&m0.A, &m0.A);
    intProp mprp1 = intProp(&m1.A, &m1.A);
    intProp mprp2 = intProp(&m2.A, &m2.A);

    m0.addABinding(mprp1);
    m0.addABinding(mprp2);

    m1.addABinding(mprp0);
    m1.addABinding(mprp2);

    m2.addABinding(mprp0);
    m2.addABinding(mprp1);

    m0.A = 2;
    assert( m1.A == m0.A);
    assert( m2.A == m0.A);
    m1.A = 3;
    assert( m0.A == m1.A);
    assert( m2.A == m1.A);
    m2.A = 4;
    assert( m1.A == m2.A);
    assert( m0.A == m2.A);

    a0.destruct;
    a1.destruct;
    a2.destruct;
    a3.destruct;
    m0.destruct;
    m1.destruct;
    m2.destruct;
}

unittest
{
    auto strSync = construct!(PropertyBinder!int);

    class A
    {
        private int fStr;
        public void str(int aValue){fStr = aValue;}
        public int str(){return fStr;}
    }

    auto a0 = construct!A;
    auto a1 = construct!A;
    auto a2 = construct!A;

    auto propa0str = strSync.newBinding;
    propa0str.define(&a0.str,&a0.str);
    auto propa1str = strSync.newBinding;
    propa1str.define(&a1.str,&a1.str);
    auto propa2str = strSync.newBinding;
    propa2str.define(&a2.str,&a2.str);

    strSync.change(8);

    assert(a0.str == 8);
    assert(a1.str == 8);
    assert(a2.str == 8);

    a0.destruct;
    a1.destruct;
    a2.destruct;
    strSync.destruct;
}

