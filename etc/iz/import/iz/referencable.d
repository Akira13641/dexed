/**
 * Look up table system for storing and retrieving references
 * from an unique identifier.
 */
module iz.referencable;

import iz.containers;

/**
 * interface for a class reference.
 */
interface Referenced
{
    /// the ID, as set when added as reference.
    string refID();
    /// the type, as registered in the ReferenceMan ( typeString!typeof(this) )
    string refType();
}

private template isReferenceType(T)
{
    enum isReferenceType = is(T == class) || is(T == interface);
}

/**
 * Associates an pointer (a reference) to an unique ID (ulong).
 */
version(none) private alias ItemsById = void*[char[]];
else alias ItemsById = HashMap_AB!(const(char)[], void*);

/**
 * itemsById for a type (identified by a string).
 */
version(none) private alias ItemsByIdByType = ItemsById[string];
else private alias ItemsByIdByType = HashMap_AB!(const(char)[], ItemsById);

/**
 * The Referencable manager associates variables of a particular type to
 * an unique identifier.
 *
 * This manager is mostly used by iz.classes and iz.serializer.
 * For example, in a setting file, it allows to store the unique identifier
 * associated to a class instance, rather than storing all its properties, as
 * the instance settings may be saved elsewhere.
 * It also allow to serialize fat pointers, such as delegates.
 */
static struct ReferenceMan
{

private:

    __gshared ItemsByIdByType _store;

    shared static this()
    {
        _store.reserve(128);
    }

public:

// Helpers --------------------------------------------------------------------+

    /**
     * Indicates if a type is referenced.
     *
     * Params:
     *      RT = The type to test.
     *
     * Returns:
     *      True if the type is referenced otherwise false.
     */
    static bool isTypeStored(RT)()
    {
        return ((RT.stringof in _store) !is null);
    }

    /**
     * Indicates if a variable is referenced.
     *
     * Params:
     *      RT = a referencable type. Optional, likely to be infered.
     *      aReference = a pointer to a RT.
     *
     * Returns:
     *      True if the variable is referenced otherwise false.
     */
    static bool isReferenced(RT)(RT* aReference)
    if (!isReferenceType!RT)
    {
        return (referenceID!RT(aReference) != "");
    }

    static bool isReferenced(RT)(RT aReference)
    if (isReferenceType!RT)
    {
        return (referenceID!RT(aReference) != "");
    }

    /**
     * Support for the in operator.
     * Evaluates to true if the variable is referenced otherwise false.
     */
    static bool opBinaryRight(string op : "in", RT)(RT* aReference)
    if (!isReferenceType!RT)
    {
        return (referenceID!RT(aReference) != "");
    }

    static bool opBinaryRight(string op : "in", RT)(RT aReference)
    if (isReferenceType!RT)
    {
        return (referenceID!RT(aReference) != "");
    }

    /**
     * Empties the references and the types.
     */
    static void reset()
    {
        _store.clear;
    }
// -----------------------------------------------------------------------------
// Add stuff ------------------------------------------------------------------+

    /**
     * Stores a type. This is a convenience function since
     * storeReference() automatically stores a type when needed.
     *
     * Params:
     *      RT = A type to reference.
     */
    static void storeType(RT)()
    {
        _store[RT.stringof, ""] = null;
    }

    /**
     * Proposes an unique ID for a reference. This is a convenience function
     * that will not return the same values for each software session.
     *
     * Params:
     *      RT = A referencable type. Optional, likely to be infered.
     *      aReference = A pointer to a RT.
     *
     * Returns:
     *      The unique string used to identify the reference.
     */
    static const(char)[] getIDProposal(RT)(RT* aReference)
    {
        // already stored ? returns current ID
        const string ID = referenceID(aReference);
        if (ID != "") return ID;

        // not stored ? returns 1
        if (!isTypeStored)
        {
            storeType!RT;
            return "entry_1";
        }

        // try to get an available ID in the existing range
        foreach(immutable i; 0 .. _store[RT.stringof].length)
        {
            import std.string: format;
            if (_store[RT.stringof, i] == null)
                return format("entry_%d", i);
        }

        // otherwise returns the next ID after the current range.
        foreach(immutable i; 0 .. ulong.max)
        {
            import std.string: format;
            if (i > _store[RT.stringof].length)
                return format("entry_%d", i);
        }

        assert(0, "ReferenceMan is full for this type");
    }

    /**
     * Tries to store a reference.
     *
     * Params:
     *      RT = the type of the reference.
     *      aReference = a pointer to a RT. Optional, likely to be infered.
     *      anID = the unique identifier for this reference.
     *
     * Returns:
     *      true if the reference is added otherwise false.
     */
    static bool storeReference(RT)(RT* aReference, const(char)[] anID)
    if (!isReferenceType!RT)
    {
        if (anID == "") return false;
        // what's already there ?
        const RT* curr = reference!RT(anID);
        if (curr == aReference) return true;
        if (curr != null) return false;
        //
        _store[RT.stringof, anID] = aReference;
        return true;
    }

    /// ditto
    static bool storeReference(RT)(RT aReference, const(char)[] anID)
    if (isReferenceType!RT)
    {
        if (anID == "") return false;
        // what's already there ?
        auto curr = reference!RT(anID);
        if (curr == aReference) return true;
        if (curr !is null) return false;
        //
        _store[RT.stringof, anID] = cast(RT*)aReference;
        return true;
    }

// -----------------------------------------------------------------------------
// Remove stuff ---------------------------------------------------------------+


    /**
     * Removes all the references for a type.
     *
     * Params:
     *      RT = The type of the references to remove.
     */
    static void removeReferences(RT)()
    {
        if (auto t = RT.stringof in _store)
            t.clear;
    }

    /**
     * Empties the storage.
     */
    static void clear()
    {
        foreach(k; _store.byKey)
            _store[k].clear;
        _store.clear;
    }

    /**
     * Tries to remove the reference matching to an ID.
     *
     * Params:
     *      RT = The type of the reference to remove.
     *      anID = The string that identifies the reference to remove.
     *
     * Returns:
     *      The reference if it's found otherwise null.
     */
    static auto removeReference(RT)(const(char)[] anID)
    {
        auto result = reference!RT(anID);
        if (result) _store[RT.stringof, anID] = null;
        return result;
    }

    /**
     * Removes a reference.
     *
     * Params:
     *      RT = The type of the reference to remove. Optional, likely to be infered.
     *      aReference = The pointer to the RT to be removed.
     */
    static void removeReference(RT)(RT* aReference)
    if (!isReferenceType!RT)
    {
        if (auto id = referenceID!RT(aReference))
            _store[RT.stringof, id] = null;
    }

    /// ditto
    static void removeReference(RT)(RT aReference)
    if (isReferenceType!RT)
    {
        if (auto id = referenceID!RT(aReference))
            _store[RT.stringof, id] = null;
    }

// -----------------------------------------------------------------------------
// Query stuff ----------------------------------------------------------------+

    /**
     * Indicates the reference ID of a variable.
     *
     * Params:
     *      RT = The type of the reference. Optional, likely to be infered.
     *      aReference = A pointer to a RT or a RT.
     *
     * Returns:
     *      A non empty string if the variable is referenced.
     */
    static const(char)[] referenceID(RT)(RT* aReference)
    if (!isReferenceType!RT)
    {
        if (!isTypeStored!RT) return "";
        foreach (k; _store[RT.stringof].byKey)
        {
            static if (!is(RT == delegate))
            {
                if (_store[RT.stringof][k] == aReference)
                    return k;
            }
            else
            {
                struct Dg {void* a,b;}
                auto stored = *cast(Dg*) _store[RT.stringof][k];
                auto passed = *cast(Dg*) aReference;
                if (stored.a == passed.a && stored.b == passed.b)
                    return k;
            }
        }
        return "";
    }

    /// ditto
    static const(char)[] referenceID(RT)(RT aReference)
    if (isReferenceType!RT)
    {
        if (!isTypeStored!RT) return "";
        foreach (k; _store[RT.stringof].byKey)
        {
            if (_store[RT.stringof][k] == cast(void*)aReference)
                return k;
        }
        return "";
    }

    /**
     * Indicates the reference ID of a variable, without using its static type.
     *
     * Params:
     *      dg = indicates if the reference to find points to a delegate.
     *      type = The $(D .stringof) of the type of the reference.
     *      aReference = A pointer to a variable
     *
     * Returns:
     *      A non empty string if the variable is referenced.
     */
    static const(char)[] referenceID(bool dg = false)(const(char)[] type, void* aReference)
    {
        if (type == "") return "";
        if (type !in _store) return "";
        foreach (k; _store[type].byKey)
        {
            static if (!dg)
            {
                if (_store[type][k] == aReference)
                    return k;
            }
            else
            {
                struct Dg {void* a,b;}
                auto stored = *cast(Dg*) _store[type][k];
                auto passed = *cast(Dg*) aReference;
                if (stored.a == passed.a && stored.b == passed.b)
                    return k;
            }
        }
        return "";
    }

    /**
     * Retrieves a reference.
     *
     * Params:
     *      RT = The type of the reference to retrieve.
     *      anID = The unique identifier of the reference to retrieve.
     *
     * Returns:
     *      Null if the operation fails otherwise a pointer to a RT, or
     *      a RT if RT is a reference type.
     */
    static RT* reference(RT)(const(char)[] anID)
    if (!isReferenceType!RT)
    {
        if (anID == "") return null;
        if (!isTypeStored!RT) return null;
        if (void** result = anID in _store[RT.stringof])
            return *cast(RT**) result;
        else
            return null;
    }

    static RT reference(RT)(const(char)[] anID)
    if (isReferenceType!RT)
    {
        if (anID == "") return null;
        if (!isTypeStored!RT) return null;
        if (void* result = anID in _store[RT.stringof])
            return *cast(RT*) result;
        else
            return null;
    }

    /**
     * Retrieves a reference without the static type
     *
     * Params:
     *      type = A string that represents the type of the reference.
     *      anID = The unique identifier of the reference to retrieve.
     *
     * Returns:
     *      Null if the operation fails otherwise a raw pointer.
     */
    static void* reference(const(char)[] type, const(char)[] anID)
    {
        import std.stdio;
        if (anID == "") return null;
        if (type !in _store) return null;
        if (void** result = anID in _store[type])
            return *result;
        else
            return null;
    }
// -----------------------------------------------------------------------------        

}

unittest
{
    import iz.memory: construct, destructEach;
    
    alias delegate1 = ubyte delegate(long param);
    alias delegate2 = short delegate(uint param);
    class Foo{int aMember;}

    assert( !ReferenceMan.isTypeStored!delegate1 );
    assert( !ReferenceMan.isTypeStored!delegate2 );
    assert( !ReferenceMan.isTypeStored!Foo );

    ReferenceMan.storeType!delegate1;
    ReferenceMan.storeType!delegate2;
    ReferenceMan.storeType!Foo;

    assert( ReferenceMan.isTypeStored!delegate1 );
    assert( ReferenceMan.isTypeStored!delegate2 );
    assert( ReferenceMan.isTypeStored!Foo );

    auto f1 = construct!Foo;
    auto f2 = construct!Foo;
    auto f3 = construct!Foo;
    scope(exit) destructEach(f1,f2,f3);

    assert( !ReferenceMan.isReferenced(f1) );
    assert( !ReferenceMan.isReferenced(f2) );
    assert( !ReferenceMan.isReferenced(f3) );

    assert( ReferenceMan.referenceID(f1) == "");
    assert( ReferenceMan.referenceID(f2) == "");
    assert( ReferenceMan.referenceID(f3) == "");

    ReferenceMan.storeReference( f1, "a.f1" );
    ReferenceMan.storeReference( f2, "a.f2" );
    ReferenceMan.storeReference( f3, "a.f3" );

    assert( ReferenceMan.reference!Foo("a.f1") == f1);
    assert( ReferenceMan.reference!Foo("a.f2") == f2);
    assert( ReferenceMan.reference!Foo("a.f3") == f3);

    assert( ReferenceMan.referenceID(f1) == "a.f1");
    assert( ReferenceMan.referenceID(f2) == "a.f2");
    assert( ReferenceMan.referenceID(f3) == "a.f3");

    assert( ReferenceMan.isReferenced(f1) );
    assert( ReferenceMan.isReferenced(f2) );
    assert( ReferenceMan.isReferenced(f3) );
    assert( f3 in ReferenceMan );

    auto f11 = f1;
    assert( ReferenceMan.referenceID!Foo(f11) == "a.f1");
    assert( ReferenceMan.reference("Foo","a.f1") != null);

    ReferenceMan.removeReference(f1);
    ReferenceMan.removeReference(f2);
    ReferenceMan.removeReference!Foo("a.f3");

    assert( !ReferenceMan.isReferenced(f1) );
    assert( !ReferenceMan.isReferenced(f2) );
    assert( !ReferenceMan.isReferenced(f3) );

    ReferenceMan.removeReference!Foo("a.f1");
    ReferenceMan.removeReference(f2);
    ReferenceMan.removeReference!Foo("a.f3");

    ReferenceMan.reset;
    assert( !ReferenceMan.isTypeStored!Foo );

    ReferenceMan.storeReference( f1, "a.f1" );
    assert( ReferenceMan.isTypeStored!Foo );

    ReferenceMan.clear;
    assert( !ReferenceMan.isTypeStored!Foo );

    alias Dg3 = void delegate(int function());
    ReferenceMan.storeType!Dg3;
    assert(ReferenceMan.reference!Dg3("notThere") == null);
    assert(ReferenceMan.reference(Dg3.stringof, "notThere") == null);
}

unittest
{
    struct Foo
    {
        this(bool){adg = &a;}
        void a(){}
        void delegate() adg;
    }
    Foo foo = Foo(false);
    Foo bar = Foo(false);
    auto dg1 = foo.adg;
    ReferenceMan.storeReference(&foo.adg, "foo.adg");
    assert(ReferenceMan.isReferenced(&foo.adg));
    assert(ReferenceMan.referenceID(&foo.adg) == "foo.adg");
    assert(ReferenceMan.reference!(typeof(dg1))("foo.adg") );
    assert(*ReferenceMan.reference!(typeof(dg1))("foo.adg") == foo.adg);
    assert(!ReferenceMan.isReferenced(&bar.adg));
    assert(ReferenceMan.referenceID(&bar.adg) == "");
    assert( ReferenceMan.referenceID(&dg1) == "foo.adg");
}

