/**
 * Several implementations of standard containers.
 */
module iz.containers;

import
    core.exception, core.stdc.string;
import
    std.exception, std.string, std.traits, std.conv, std.range.primitives,
    std.typecons;
import
    iz.memory, iz.types, iz.streams;
version(unittest) import
    std.stdio: writeln, write, stdout;

version(X86_64)
    version(linux) version = Nux64;

/**
 * Generic, manually managed, array.
 *
 * Array(T) implements a single-dimension array of uncollected memory.
 * It internally allocates memory blocks to minimize the reallocation fingerprints,
 * allowing insertions to be 2 times faster than built-in arrays.
 *
 * Its layout differs from built-in D's dynamic arrays and they cannot be cast as T[]
 * however, most of the slicing operations are possible.
 *
 * Manual management implies that $(D destruct()) must be called on the array when
 * it goes out of scope. $(D destruct()) is only called on the content when the
 * specialization is a $(D struct()) or a $(D union)). Classes and pointers must
 * be freed by hand.
 */
struct Array(T)
{

private:

    size_t _length;
    @NoGc Ptr _elems;
    uint _granularity = 4096;
    size_t _blockCount;

    pragma(inline, true)
    void setLength(size_t value) @nogc
    {
        assert(_granularity != 0);

        const size_t newBlockCount = ((value * T.sizeof) / _granularity) + 1;
        if (_blockCount != newBlockCount)
        {
            _blockCount = newBlockCount;
            _elems = reallocMem(_elems, _granularity * _blockCount);
        }
        _length = value;
    }

    pragma(inline, true)
    void grow() @nogc
    {
        length(_length + 1);
    }

    pragma(inline, true)
    void shrink() @nogc
    {
        length(_length - 1);
    }

    pragma(inline, true)
    Unqual!T* rwPtr(size_t index) pure const nothrow @nogc
    {
        return cast(Unqual!T*) (_elems + index * T.sizeof);
    }

    struct Range
    {
        private size_t _i;
        private Array!T* _a;
        ///
        this(ref Array!T array, size_t index = 0) @nogc @trusted
        {
            _a = &array;
            _i = index;
        }
        ///
        ref T front() @nogc
        {
            return _a.opIndex(_i);
        }
        ///
        void popFront() @nogc @safe
        {
            ++_i;
        }
        ///
        bool empty() @nogc @safe
        {
            return _i >= _a._length;
        }
    }

    pragma(inline, true)
    void postblitElements()()
    {
        static if (is(T == struct) && hasMember!(T, "__postblit") && isCopyable!T )
        {
            foreach(i; 0.._length)
                (*rwPtr(i)).__postblit();
        }
    }

public:

    /// Constructs the array with a list of T.
    this(E...)(E elements) @nogc
    if (is(Unqual!E == T) || is(T == E))
    {
        opAssign(elements);
    }

    /// Constructs the array with a D array of T.
    this(E)(E[] elements...) @nogc
    if (is(Unqual!E == T) || is(T == E))
    {
        opAssign(elements);
    }

    /// Constructs by dispatching to the existing opAssign overloads.
    this(E)(auto ref E value) @nogc
    {
        opAssign(value);
    }

    this(this)
    {
        Ptr old = _elems;
        const size_t sz = _granularity * _blockCount;
        _elems = getMem(sz);
        moveMem(_elems, old, sz);
        postblitElements;
    }

    ~this()
    {
        length(0);
        if (_elems)
            freeMem(_elems);
        _elems = null;
    }

    /**
     * Indicates the memory allocation block-size.
     */
    uint granularity() const pure nothrow @safe @nogc
    {
        return _granularity;
    }

    /**
     * Sets the memory allocation block-size.
     * value should be set to 16 or 4096 (the default).
     */
    void granularity(uint value) @nogc
    {
        if (_granularity == value)
            return;

        if (value < T.sizeof)
        {
            value = 16 * T.sizeof;
        }
        else if (value < 16)
        {
            value = 16;
        }
        else while (_granularity % 16 != 0)
        {
            value--;
        }
        _granularity = value;
        setLength(_length);
    }

    /**
     * Indicates how many block the array is made of.
     */
    pragma(inline, true)
    size_t blockCount() const pure nothrow @safe @nogc
    {
        return _blockCount;
    }

    /// Sets or gets the element count.
    pragma(inline, true)
    size_t length() const pure nothrow @safe @nogc
    {
        return _length;
    }

    /// ditto
    void length(V)(V value)
    if (isIntegral!V)
    {
        if (value == _length)
            return;
        size_t oldLen = _length;
        static if (is(T == struct) && !isTuple!T)
        {
            if (value < _length)
            {
                foreach (i; value.._length)
                    destruct(opIndex(i));
            }
        }
        setLength(value);
        if (value > oldLen)
        {
            static if (is(T == struct) && !hasUDA!(T, NoInit))
            {
                foreach (i; oldLen.._length)
                    emplace(rwPtr(i));
            }
            else static if (std.traits.isIntegral!T)
            {
                memset(rwPtr(oldLen), 0, (_length - oldLen) * T.sizeof);
            }
            else static if (is(T == class))
            {
                memset(rwPtr(oldLen), ubyte(), (_length - oldLen) * size_t.sizeof);
            }
            else static if (isBasicType!T)
            {
                (rwPtr(0))[oldLen.._length] = T.init;
            }
            else
            {
                foreach (i; oldLen.._length)
                    *rwPtr(i) = T.init;
            }
        }
    }

    /**
     * Pointer to the first element.
     * As it's always assigned It cannot be used to determine if the array is empty.
     */
    pragma(inline, true)
    Ptr ptr() pure nothrow @nogc
    {
        return _elems;
    }

    /**
     * Typed pointer to the first element.
     */
    pragma(inline, true)
    T* typedPtr() pure nothrow @nogc
    {
        return cast(T*) _elems;
    }

    /**
     * Returns the string representation of the array.
     *
     * Throw:
     *      A ConvException if T is not converitble to string.
     */
    static if (__traits(compiles, to!string(opSlice())))
    string toString()
    {
        if (_length == 0)
            return "[]";
        else
        {
            auto r = opSlice();
            return to!string(r);
        }
    }

    /// Returns a mutable (deep) copy of the array.
    Array!T dup()()
    {
        Array!T result = this;
        return result;
    }

    /// Support for associative arrays.
    size_t toHash() nothrow @trusted
    {
        return opSlice().hashOf;
    }

    /// Support for equality tests.
    bool opEquals(A)(auto ref A array) const pure @nogc @trusted
    if ((is(Unqual!A == Unqual!(Array!T)) || is(Unqual!(ElementEncodingType!A) == T)))
    {
        if (_length != array.length)
            return false;
        else if (_length == 0 && array.length == 0)
            return true;
        static if (is(Unqual!A == Unqual!(Array!T)))
            return _elems[0.._length * T.sizeof] ==
              array._elems[0..array._length * T.sizeof];
        else
            return _elems[0.._length * T.sizeof] ==
              array.ptr[0..array.length];
    }

    /// Support for the array syntax.
    pragma(inline, true)
    ref T opIndex(size_t i) const pure @nogc
    {
        return *rwPtr(i);
    }

    /// Support for the array syntax.
    void opIndexAssign()(ref T item, size_t i) @nogc
    {
        *rwPtr(i) = item;
    }

    /// Ditto
    void opIndexAssign()(T item, size_t i) @nogc
    {
        *rwPtr(i) = item;
    }

    /// ditto
    static if (isTemplateInstance!(T) && __traits(isSame, TemplateOf!T, TemplateOf!(typeof(this))))
    void opIndexAssign()(TemplateArgsOf!(T)[0][] item, size_t i) @nogc
    {
        *rwPtr(i) = item;
    }

    /// Support for the foreach operator.
    int opApply(scope int delegate(ref T) dg)
    {
        int result;
        foreach (immutable i; 0 .. _length)
        {
            result = dg(*rwPtr(i));
            if (result) break;
        }
        return result;
    }

    /// Support for the foreach_reverse operator.
    int opApplyReverse(scope int delegate(ref T) dg)
    {
        int result;
        foreach_reverse (immutable i; 0 .. _length)
        {
            result = dg(*rwPtr(i));
            if (result) break;
        }
        return result;
    }

    /// Support for the dollar operator.
    size_t opDollar() const pure nothrow @safe @nogc
    {
        return _length;
    }

    /// Assign another Array!T.
    void opAssign(E)(auto ref Array!E elements) @nogc
    if (is(Unqual!E == T) || is(E == T))
    {
        setLength(elements._length);
        moveMem(_elems, elements._elems, T.sizeof * _length);
        /*setLength(0);
        _granularity = elements._granularity;
        _length = elements.length;
        _elems = elements._elems;
        _blockCount = elements._blockCount;
        __postblit();*/
    }

    /// Assigns a D array.
    void opAssign(E)(auto ref E[] elements) @nogc
    if (is(Unqual!E == T) || is(E == T))
    {
        setLength(elements.length);
        foreach (i, element; elements)
            *rwPtr(i) = cast(T) element;
    }

    /// Assigns an inpunt range.
    void opAssign(E)(auto ref E elements) @nogc
    if (isInputRange!E && is(Unqual!(ElementType!E) == T) && !isRandomAccessRange!E)
    {
        const len = walkLength(elements);
        setLength(len);
        foreach (immutable i; 0..len)
        {
            opAssign(elements.front);
            elements.popFront;
        }
    }

    /// Support for the cat operator
    auto ref typeof(this) opBinary(string op : "~", R)(auto ref R rhs) return @nogc
    if (__traits(hasMember, R, "length") && __traits(hasMember, R, "ptr"))
    {
        typeof(this) result;
        result.length = _length + rhs.length;
        moveMem(result.rwPtr(0), _elems , T.sizeof * _length);
        moveMem(result.rwPtr(_length), rhs._elems , T.sizeof * rhs.length);
        return result;
    }

    /// Support for the cat operator.
    ref typeof(this) opOpAssign(string op, E = T)(auto ref E[] elements) @nogc
    if (is(Unqual!E == T) || is(E == T))
    {
        static if (op == "~")
        {
            const old = _length;
            setLength(_length + elements.length);
            moveMem( rwPtr(old), elements.ptr , T.sizeof * elements.length);
            return this;
        }
        else static assert(0, "operator not implemented");
    }

    /// Support for the cat operator.
    ref typeof(this)  opOpAssign(string op)(T aElement) @nogc
    {
        static if (op == "~")
        {
            grow;
            opIndexAssign(aElement, _length-1);
            return this;
        }
        else static assert(0, "operator not implemented");
    }

    /// Support for output ranges.
    alias put = opOpAssign!"~";

    /// ditto
    void put(E)(auto ref E[] elements) @nogc
    if (is(Unqual!E == T) || is(E == T))
    {
        const old = _length;
        setLength(_length + elements.length);
        moveMem( rwPtr(old), elements.ptr , T.sizeof * elements.length);
    }

    /// Returns a slice of the array. The memory is not duplicated.
    auto ref opSlice(bool dSlice = false)(size_t lo, size_t hi) return @nogc
    {
        static if (dSlice)
        {
            return (cast(T*) _elems)[lo..hi];
        }
        else
        {
            Array!T result;
            result.length = hi - lo;
            moveMem(result.rwPtr(0), rwPtr(lo), (hi - lo) * T.sizeof);
            return result;
        }
    }

    /// Returns the array as a D slice. The memory is not duplicated.
    T[] opSlice() const
    {
        return (cast(T*) _elems)[0.._length];
    }

    /// Support for filling the array with a single element.
    void opSliceAssign()(T value) @nogc
    {
        rwPtr(0)[0.._length] = value;
    }

    /// ditto
    void opSliceAssign()(T value, size_t lo, size_t hi) @nogc
    {
        foreach (immutable i; lo .. hi)
            *rwPtr(i) = value;
    }

    /// Returns an input range with an assignable front.
    auto range()
    {
        return Range(this, 0);
    }

    /// Allows to use the array as a D built-in array.
    alias opSlice this;
}
///
unittest
{
    Array!int a;
    a.length = 2;
    a[] = 1;
    assert(a[0] == 1);
    assert(a[1] == 1);
}

unittest
{
    Array!int a;
    a.length = 1;
    auto r = a.range();
    assert(r.front() == 0);
    r.popFront();
    assert(r.empty);
    assert(a.toString() == "[0]");
}

unittest
{
    import std.range : iota;
    import std.array : array;
    Array!int a = iota(1000).array;
    assert(a.granularity == 4096);
    a.granularity = 64;
    a.granularity = 64;
    assert(a.granularity == 64);
    a.granularity = 2;
    assert(a.granularity == 64);
    Array!int b = iota(1000).array;
    assert(a == b);
}

unittest
{
    class Foo{}
    Array!Foo a;
    a.length = 1;
    assert(a[0] is null);
}

unittest
{
    Array!char c;
    c.length = 1;
    assert(c[0] == char.init);
    Array!float f;
    f.length = 1;
    import std.math : isNaN;
    assert(f[0].isNaN);
}

unittest
{
    import std.range.primitives;
    static assert(isOutputRange!(Array!int, int));
    Array!(char) a;
    a.put("someString");
}

unittest
{
    import std.range : iota;
    import std.array : array;
    Array!int a = iota(1000).array;
    Array!int b;
    b = a;
    assert(b == iota(1000).array);
}

unittest
{
    alias Key = string;
    alias A = Array!int;
    A[Key] x;
    //x["a"] = [0];
}

unittest
{
    // init-index
    Array!size_t a;
    a.length = 2;
    a[0] = 8;
    a[1] = 9;
    assert( a[0] == 8);
    assert( a[1] == 9);
    assert( a[$-1] == 9);

    Array!int b = Array!int(0,1,2,3,4,5,6);
    assert( b.length == 7);
    assert( b[$-1] == 6);

    Array!float floatarr = Array!float ([0.0f, 0.1f, 0.2f, 0.3f, 0.4f]);
    assert( floatarr.length == 5);
    assert( floatarr[0] == 0.0f);
    assert( floatarr[1] == 0.1f);
    assert( floatarr[2] == 0.2f);
    assert( floatarr[3] == 0.3f);
    assert( floatarr[4] == 0.4f);

    // loops
    int i;
    foreach (float aflt; floatarr)
    {
        float v = i * 0.1f;
        assert( aflt == v);
        i++;
    }
    foreach_reverse(float aflt; floatarr)
    {
        i--;
        float v = i * 0.1f;
        assert( aflt == v);
    }

    // opEquals
    auto nativeArr = [111u, 222u, 333u, 444u, 555u];
    auto arrcpy1 = Array!uint(111u, 222u, 333u, 444u, 555u);
    auto arrcpy2 = Array!uint(111u, 222u, 333u, 444u, 555u);
    assert( arrcpy1 == nativeArr );
    assert( arrcpy2 == nativeArr );
    assert( arrcpy1 == arrcpy2 );
    arrcpy2[0] = 0;
    assert( arrcpy1 != arrcpy2 );
    arrcpy1.length = 3;
    assert( nativeArr != arrcpy1 );
    arrcpy1.length = 0;
    arrcpy2.length = 0;
    assert( arrcpy1 == arrcpy2 );

    // opSlice
    Array!float g0 = floatarr[1..4];
    assert( g0[0] ==  floatarr[1]);
    assert( g0[2] ==  floatarr[3]);
    Array!float g1 = floatarr;
    assert( g1[0] ==  floatarr[0]);
    assert( g1[4] ==  floatarr[4]);

    // opSliceAssign
    g1[] = 0.123456f;
    assert( g1[0] == 0.123456f);
    assert( g1[3] == 0.123456f);
    g1[0..1] = 0.654321f;
    assert( g1[0] == 0.654321f);
    assert( g1[1] == 0.123456f);
    assert( g1[2] == 0.123456f);

    // huge
    a.length = 10_000_000;
    a[$-1] = a.length-1;
    assert(a[a.length-1] == a.length-1);
    a.length = 10;
    a.length = 10_000_000;
    a[$-1] = a.length-1;
    assert(a[$-1] == a.length-1);
}

unittest
{
    Array!char a = "123456";
    Array!char b = a;
    assert(b.length == 6);
    assert(b == "123456");
    destructEach(a,b);
}

unittest
{
    static int c;
    static struct S{int i = 1; ~this() @nogc {c = 1;}}
    Array!S array;
    assert(array.toString == "[]");
    array.length = 1;
    assert(array[0].i == 1);
    array.length = 0;
    assert(c);
}

unittest
{
    int[] source = [0,1,2];
    Array!int a = source;
    assert(a == source);
    assert(a.dup == source);
    destruct(a);
}

@nogc unittest
{
    static int[] source = [0,1,2,3];
    static int[] r = [0,2,3];
    Array!int a = source;
    assert(a == source);
    a = a[0..1] ~ a[2..$];
    assert(a == r);
    destruct(a);
}

@nogc unittest
{
    static int[] aa = [0,1];
    static int[] bb = [2,3];
    static int[] r0 = [0,1,2,3];
    static int[] r1 = [0,1,2,3,0,1,2,3];
    Array!int a = aa;
    Array!int b = bb;
    Array!int c = a ~ b;
    assert(c  == r0);
    assert(c ~ c == r1);
    destructEach(a, b, c);
}

unittest
{
    Array!(Array!(const(char))) a;
    a.length = 2;
    a[0] = "first";
    a[1] = "second".dup;
    destruct(a);
}

private mixin template ListHelpers(T)
{
    static if (is (T == class))
    {
        final public T addNewItem(A...)(A a)
        {
            T result = construct!T(a);
            add(result);
            return result;
        }
    }
    else static if (is (T == struct))
    {
        final public T * addNewItem(A...)(A a)
        {
            T * result = construct!T(a);
            add(result);
            return result;
        }
    }
    else static if (isPointer!T)
    {
        final public T addNewItem(A...)(A a)
        {
            T result = newPtr!(PointerTarget!T)(a);
            add(result);
            return result;
        }
    }

    alias opDollar = count;
}


/**
 * A list, fast to be iterated, slow to be reorganized.
 * Encapsulates an Array!T and interfaces it as a list.
 */
class ContiguousList(T)
{
    mixin ListHelpers!T;
    mixin inheritedDtor;

private:

        @NoGc Array!T _items;


public:

    ///
    this(A...)(A elements)
    {
        _items = Array!T(elements);
    }

    ~this()
    {
        _items.length = 0;
    }

    /// Support for the array syntax.
    ref T opIndex(ptrdiff_t i)
    {
        return _items[i];
    }

    /// Support for the array syntax.
    void opIndexAssign(T item, size_t i)
    {
        _items[i] = item;
    }

    /// Support for the foreach operator.
    int opApply(scope int delegate(ref T) dg)
    {
        int result;
        foreach (immutable i; 0 .. _items.length)
        {
            result = dg(_items[i]);
            if (result) break;
        }
        return result;
    }

    /// Support for the foreach_reverse operator.
    int opApplyReverse(scope int delegate(ref T) dg)
    {
        int result;
        foreach_reverse(immutable i; 0 .. _items.length)
        {
            result = dg(_items[i]);
            if (result) break;
        }
        return result;
    }

    /// Replaces the items with the content of a D T[].
    void opAssign(T[] items)
    {
        _items.opAssign(items);
    }

    /// Replaces the items with the content of an Array!T.
    void opAssign(Array!T items)
    {
        _items.opAssign(items);
    }

    T last()()
    {
        return _items[$-1];
    }

    T first()()
    {
        return _items[0];
    }

    /**
     * Tries to find an item in the list.
     *
     * Params:
     *      item = the item to find.
     * Returns:
     *      -1 if item is not present otherwise its index.
     */
    ptrdiff_t find(T item)
    {
        ptrdiff_t result = -1;
        foreach (immutable i; 0 .. _items.length)
        {
            if (_items[i] == item)
            {
                result = i;
                break;
            }
        }
        return result;
    }

    /**
     * Adds an item at the end of the list and returns its index.
     *
     * Returns:
     *      The last item index.
     */
    ptrdiff_t add(T item)
    {
        _items.grow;
        _items[$-1] = item;
        return _items.length - 1;
    }

    /**
     * Adds items at the end of the list.
     *
     * Returns:
     *      The last item index.
     */
    ptrdiff_t add(I)(I items)
    {
        _items ~= items;
        return _items.length - 1;
    }

    /**
     * Inserts an item at the beginning of the list.
     *
     * In a Contiguous list, add() should be preferred over insert().
     *
     * Returns:
     *      Always 0.
     */
    ptrdiff_t insert(T item)
    {
        _items.grow;
        memmove(_items.ptr + T.sizeof, _items.ptr, (_items.length - 1) * T.sizeof);
        _items[0] = item;
        return 0;
    }

    /**
     * Inserts an item at a given position.
     *
     * In a Contiguous list, add() should ne preferred over insert().
     *
     * Params:
     *      position = The position where to insert.
     *      item = The item to insert.
     * Returns:
     *      The item index, position if it was valid.
     */
    ptrdiff_t insert(size_t position, T item)
    {
        if (position == 0)
            return insert(item);
        else if (position >= _items.length)
            return add(item);
        else
        {
            _items.grow;
            memmove(_items.ptr + T.sizeof * position + 1,
                    _items.ptr + T.sizeof * position,
                    (_items.length - 1 - position) * T.sizeof);
            _items[position] = item;
            return position;
        }
    }

    /**
     * Exchanges the positions of two items.
     *
     * Params:
     *      item1 = The first item.
     *      item2 = The second item.
     */
    void swapItems(T item1, T item2)
    in
    {
        assert(item1 != item2);
    }
    body
    {
        ptrdiff_t i1 = find(item1);
        ptrdiff_t i2 = find(item2);
        if (i1 != -1 && i2 != -1)
        {
            _items[i1] = _items[i2];
            _items[i2] = item1;
        }
    }

    /**
     * Exchanges the positions of two items.
     *
     * Params:
     *      index1 = The first item index.
     *      index2 = The second item index.
     */
    void swapIndexes(size_t index1, size_t index2)
    {
        if (index1 == index2) return;
        if ((index1 >= _items.length) | (index2 >= _items.length)) return;

        auto old = _items[index1];
        _items[index1] = _items[index2];
        _items[index2] = old;
    }

    /**
     * Removes an item from the list.
     *
     * Params:
     *      item = The item to remove.
     * Returns:
     *      true if the item wasin the list, false otherwise.
     */
    bool remove(T item)
    {
        auto i = find(item);
        auto result = (i != -1);
        if (result)
            extract(i);
        return result;
    }

    /**
     * Removes an item from the list.
     *
     * Params:
     *      index = The index of the item to remove.
     * Returns:
     *      The item that's been removed.
     */
    T extract(size_t index)
    {
        T result = _items[index];
        if (index == _items.length-1)
        {
            _items.shrink;
        }
        else if (index == 0)
        {
            memmove(_items.ptr, _items.ptr + T.sizeof, (_items.length - 1) * T.sizeof);
            _items.shrink;
        }
        else
        {
            Ptr fromPtr = _items.ptr + T.sizeof * index;
            memmove(fromPtr, fromPtr + T.sizeof, (_items.length - index) * T.sizeof);
            _items.shrink;
        }
        return result;
    }

    /**
     * Empties the list.
     */
    void clear()
    {
        _items.length = 0;
    }

    /**
     * Returns the items count.
     */
    final size_t count() const pure nothrow @safe
    {
        return _items.opDollar();
    }

    /**
     * Returns the internal array.
     *
     * ContiguousList doesn't contain any other hidden field
     * and the container can be modified without altering the
     * state of the list.
     */
    final ref Array!T array() nothrow @safe
    {
        return _items;
    }
}


/**
 * Payload for the dynamic list.
 */
private template dlistPayload(T)
{
    private static const prevOffs = 0;
    private static const nextOffs = size_t.sizeof;
    private static const dataOffs = size_t.sizeof + size_t.sizeof;

    @nogc nothrow private:

    void* newPld(void* aPrevious, void* aNext, ref T aData) @trusted
    {
        auto result = getMem( 2 * size_t.sizeof + T.sizeof);
        if (result)
        {
            *cast(size_t*)  (result + prevOffs) = cast(size_t) aPrevious;
            *cast(size_t*)  (result + nextOffs) = cast(size_t) aNext;
            static if (is(T == struct))
            {
                static T init;
                copyMem(result + dataOffs, &init, T.sizeof);
            }
            *cast(T*) (result + dataOffs) = aData;
        }
        return result;
    }
    void freePld(void* aPayload) @trusted
    in
    {
        assert(aPayload);
    }
    body
    {
        static if (is(T==struct))
        {
            destruct(getData(aPayload));
        }
        freeMem(aPayload);
    }

    void setPrev(void* aPayload, void* aPrevious) @trusted
    in
    {
        assert(aPayload);
    }
    body
    {
        *cast(void**) (aPayload + prevOffs) = aPrevious;
    }

    void setNext(void* aPayload, void* aNext) @trusted
    in
    {
        assert(aPayload);
    }
    body
    {
        *cast(void**) (aPayload + nextOffs) = aNext;
    }

    void setData(void* aPayload, ref T aData) @trusted
    in
    {
        assert(aPayload);
    }
    body
    {
        *cast(T*) (aPayload + dataOffs) = aData;
        static if (is(T == struct) && hasMember!(T, "__postblit") && isCopyable!T )
        {
            (cast(T*) (aPayload + dataOffs)).__postblit();
        }
    }

    void* getPrev(void* aPayload) @trusted
    {
        version(X86) asm @nogc nothrow
        {
            naked;
            mov     EAX, [EAX + prevOffs];
            ret;
        }
        else version(Win64) asm @nogc nothrow
        {
            naked;
            mov     RAX, [RCX + prevOffs];
            ret;
        }
        else version(Nux64) asm @nogc nothrow
        {
            naked;
            mov     RAX, [RDI + prevOffs];
            ret;
        }
        else return *cast(void**) (aPayload + prevOffs);
    }

    void* getNext(void* aPayload) @trusted
    {
        version(X86) asm @nogc nothrow
        {
            naked;
            mov     EAX, [EAX + nextOffs];
            ret;
        }
        else version(Win64) asm @nogc nothrow
        {
            naked;
            mov     RAX, [RCX + nextOffs];
            ret;
        }
        else version(Nux64) asm @nogc nothrow
        {
            naked;
            mov     RAX, [RDI + nextOffs];
            ret;
        }
        else return *cast(void**) (aPayload + nextOffs);
    }

    ref T getData(void* aPayload) @trusted
    {
        /*version(X86) asm @nogc nothrow
        {
            naked;
            mov     EAX, [EAX + dataOffs];
            ret;
        }
        else version(Win64) asm @nogc nothrow
        {
            naked;
            mov     RAX, [RCX + dataOffs];
            ret;
        }
        else version(Nux64) asm @nogc nothrow
        {
            naked;
            mov     RAX, [RDI + dataOffs];
            ret;
        }
        else*/ return *cast(T*) (aPayload + dataOffs);
    }
}


/**
 * A List implementation, slow to be iterated, fast to be reorganized.
 * This is a standard doubly linked list, with GC-free heap allocations.
 */
class DynamicList(T)
{
    mixin ListHelpers!T;
    mixin inheritedDtor;

private:

    size_t _count;
    void* _last;
    void* _first;
    alias payload = dlistPayload!T;

    void* getPayloadFromIx(size_t index) @safe @nogc nothrow
    {
        void* current = _first;
        foreach (immutable i; 0 .. index)
            current = payload.getNext(current);
        return current;
    }

    void* getPayloadFromDt(ref T item) @trusted
    {
        void* current = _first;
        while (current)
        {
            if (payload.getData(current) == item)
                break;
            current = payload.getNext(current);
        }
        return current;
    }

public:

    ///
    this(A...)(A elements)
    {
        foreach (elem; elements)
            add(elem);
    }

    ~this()
    {
        clear;
    }

    /// Support for the array syntax.
    ref T opIndex(ptrdiff_t i) @safe @nogc nothrow
    {
        void* _pld = getPayloadFromIx(i);
        return payload.getData(_pld);
    }

    /// Support for the array syntax.
    void opIndexAssign(ref T item, size_t i) @safe @nogc nothrow
    {
        void* _pld = getPayloadFromIx(i);
        payload.setData(_pld, item);
    }

    /// Support for the foreach operator.
    int opApply(int delegate(ref T) dg) @trusted
    {
        int result;
        void* current = _first;
        while (current)
        {
            result = dg(payload.getData(current));
            if (result) break;
            current = payload.getNext(current);
        }
        return result;
    }

    /// Support for the foreach_reverse operator.
    int opApplyReverse(int delegate(ref T) dg) @trusted
    {
        int result;
        void* current = _last;
        while (current)
        {
            result = dg(payload.getData(current));
            if (result) break;
            current = payload.getPrev(current);
        }
        return result;
    }

    /// Replaces the items with the content of a D T[].
    void opAssign(T[] elems) @trusted @nogc
    {
        clear;
        foreach (elem; elems)
            add(elem);
    }

    /// Returns the first element.
    T last() @safe @nogc nothrow
    {
        return payload.getData(_last);
    }

    /// Returns the last element.
    T first() @safe @nogc nothrow
    {
        return payload.getData(_first);
    }

    /**
     * Tries to find an item in the list.
     *
     * Params:
     *      item = the item to find.
     * Returns:
     *      -1 if item is not present otherwise its index.
     */
    ptrdiff_t find(T item) @trusted
    {
        void* current = _first;
        ptrdiff_t result = -1;
        while (current)
        {
            result++;
            if (payload.getData(current) == item)
                return result++;
            current = payload.getNext(current);
        }
        return -1;
    }

    /**
     * Adds an item at the end of the list and returns its index.
     */
    ptrdiff_t add(ref T item) @trusted @nogc
    {
        if (!_first)
            return insert(item);
        else
        {
            void* _pld = payload.newPld(_last, null, item);
            payload.setNext(_last, _pld);
            _last = _pld;
            return _count++;
        }
    }

    /// ditto
    ptrdiff_t add(T item) @trusted @nogc
    {
        return add(item);
    }

    /**
     * Adds items at the end of the list and returns the last item index.
     */
    ptrdiff_t add(T[] items) @trusted @nogc
    {
        foreach (item; items)
            add(item);
        return _count - 1;
    }

    /**
     * Inserts an item at the beginning of the list.
     */
    ptrdiff_t insert(ref T item) @trusted @nogc
    {
        void* _pld = payload.newPld(null, _first, item);
        if (_first) payload.setPrev(_first, _pld);
        else _last = _pld;
        _first = _pld;
        ++_count;
        return 0;
    }

    /// ditto
    ptrdiff_t insert(T item) @trusted @nogc
    {
        return insert(item);
    }

    /**
     * Inserts an item at a given position.
     *
     * Params:
     *      position = The position where to insert.
     *      item = The item to insert.
     * Returns:
     *      The item index, position if it was valid.
     */
    ptrdiff_t insert(size_t position, ref T item) @trusted @nogc
    {
        if (!_first || position == 0)
        {
            return insert(item);
        }
        else if (position >= _count)
        {
            return add(item);
        }
        else
        {
            void* old = getPayloadFromIx(position);
            void* prev = payload.getPrev(old);
            void* _pld = payload.newPld(prev, old, item);
            payload.setPrev(old, _pld);
            payload.setNext(prev, _pld);
            _count++;
            return position;
        }
    }

    /// ditto
    ptrdiff_t insert(size_t position, T item) @trusted @nogc
    {
        return insert(position, item);
    }

    /**
     * Exchanges the positions of two items.
     *
     * Params:
     *      item1 = The first item.
     *      item2 = The second item.
     */
    void swapItems(T item1, T item2) @trusted
    {
        void* _pld1 = getPayloadFromDt(item1);
        if (_pld1 == null) return;
        void* _pld2 = getPayloadFromDt(item2);
        if (_pld2 == null) return;

        T _data1 = payload.getData(_pld1);
        T _data2 = payload.getData(_pld2);

        payload.setData(_pld1, _data2);
        payload.setData(_pld2, _data1);
    }

    /**
     * Exchanges the positions of two items.
     *
     * Params:
     *      index1 = The first item index.
     *      index2 = The second item index.
     */
    void swapIndexes(size_t index1, size_t index2) @trusted
    {
        void* _pld1 = getPayloadFromIx(index1);
        if (_pld1 == null) return;
        void* _pld2 = getPayloadFromIx(index2);
        if (_pld2 == null) return;

        T _data1 = payload.getData(_pld1);
        T _data2 = payload.getData(_pld2);

        payload.setData(_pld1, _data2);
        payload.setData(_pld2, _data1);
    }

    /**
     * Removes an item from the list.
     *
     * Params:
     *      item = The item to remove.
     * Returns:
     *      true if the item wasin the list, false otherwise.
     */
    bool remove(T item) @trusted
    {
        void* _pld = getPayloadFromDt(item);
        if (!_pld) return false;

        void* _prev = payload.getPrev(_pld);
        void* _next = payload.getNext(_pld);
        if (_last == _pld && _prev)
        {
            _last = _prev;
            payload.setNext(_last, null);
            _next = null;
        }
        else if (_first == _pld && _next)
        {
            _first = _next;
            payload.setPrev(_first, null);
            _prev = null;
        }
        else if (_prev && _next)
        {
            if (_prev) payload.setNext(_prev, _next);
            if (_next) payload.setPrev(_next, _prev);
        }
        payload.freePld(_pld);
        if (_last == _first && _last == _pld)
        {
            _last = null;
            _first = null;
        }
        --_count;
        return true;
    }

    /**
     * Removes an item from the list.
     *
     * Params:
     *      index = The index of the item to remove.
     * Returns:
     *      The item that's been removed.
     */
    T extract(size_t index) @trusted
    {
        T result;
        void* _pld = getPayloadFromIx(index);
        if (!_pld) return result;
        result = payload.getData(_pld);

        void* _prev = payload.getPrev(_pld);
        void* _next = payload.getNext(_pld);
        if (_last == _pld && _prev)
        {
            _last = _prev;
            payload.setNext(_prev, null);
            _next = null;
        }
        else if (_first == _pld && _next)
        {
            _first = _next;
            payload.setPrev(_next, null);
            _prev = null;
        }
        else if (_prev && _next)
        {
            payload.setNext(_prev, _next);
            payload.setPrev(_next, _prev);
        }
        payload.freePld(_pld);
        if (_last == _first && _last == _pld)
        {
            _last = null;
            _first = null;
        }
        --_count;
        return result;
    }

    /**
     * Empties the list.
     */
    void clear() @trusted @nogc
    {
        void* current = _first;
        while (current)
        {
            void* _old = current;
            current = payload.getNext(current);
            payload.freePld(_old);
        }
        _count = 0;
        _first = null;
        _last = null;
    }

    /**
     * Returns the items count.
     */
    size_t count() @trusted @nogc
    {
        return _count;
    }

    Range opSlice()
    {
        return Range(_first, _last);
    }

    Range opSlice(size_t lo, size_t hi) @trusted
    {
        return Range(getPayloadFromIx(lo), getPayloadFromIx(hi));
    }

    alias length = count;

    alias put = add;

    private struct Range
    {
        private void* _begin;
        private void* _end;

        private this(void* b, void* e)
        {
            _begin = b;
            _end = e;
        }

        /**
         * Returns $(D true) if the range is _empty
         */
        bool empty() const @property
        {
            return _begin is null;
        }

        /**
         * Returns the first element in the range
         */
        T front() @safe @nogc
        {
            return payload.getData(_begin);
        }

        /**
         * Returns the last element in the range
         */
        T back() @safe @nogc
        {
            return payload.getData(_end);
        }

        /**
         * pop the front element from the range
         *
         * complexity: amortized $(BIGOH 1)
         */
        void popFront() @safe @nogc
        {
            _begin = payload.getNext(_begin);
        }

        /**
         * pop the back element from the range
         *
         * complexity: amortized $(BIGOH 1)
         */
        void popBack() @safe @nogc
        {
            _end = payload.getPrev(_end);
        }

        /**
         * Trivial _save implementation, needed for $(D isForwardRange).
         */
        Range save() @safe @nogc
        {
            return this;
        }

        size_t length() @safe @nogc
        {
            size_t result;
            auto cur = _begin;
            while (cur)
            {
                cur = payload.getNext(cur);
                ++result;
            }
            return result;
        }
    }
}

unittest
{
    struct  S{int a,b; int notPod(){return a;}}
    class   C{int a,b; int notPod(){return a;}}

    void test(alias T )()
    {
        // struct as ptr
        alias SList = T!(S*);
        S[200] arrayOfS;
        SList sList = construct!SList;
        scope(exit) sList.destruct;

        for (auto i = 0; i < arrayOfS.length; i++)
        {
            arrayOfS[i].a = i;
            sList.add( &arrayOfS[i] );
            assert( sList[i] == &arrayOfS[i]);
            assert( sList.count == i + 1);
            assert( sList.find( &arrayOfS[i] ) == i);
        }

        sList.swapIndexes(0,1);
        assert( sList.find(&arrayOfS[0]) == 1 );
        assert( sList.find(&arrayOfS[1]) == 0 );
        sList.swapIndexes(0,1);
        assert( sList.find(&arrayOfS[0]) == 0 );
        assert( sList.find(&arrayOfS[1]) == 1 );
        sList.remove(sList.last);
        assert( sList.count == arrayOfS.length -1 );
        sList.clear;
        assert( sList.count == 0 );
        for (auto i = 0; i < arrayOfS.length; i++)
        {
            sList.add( &arrayOfS[i] );
        }
        sList.extract(50);
        assert( sList.find(&arrayOfS[50]) == -1 );
        sList.insert(50,&arrayOfS[50]);
        assert( sList.find(&arrayOfS[50]) == 50 );
        sList.extract(50);
        sList.insert(&arrayOfS[50]);
        assert( sList.find(&arrayOfS[50]) == 0 );
        sList.clear;
        assert( sList.count == 0 );
        for (auto i = 0; i < arrayOfS.length; i++)
        {
            sList.add( &arrayOfS[i] );
        }

        // class as ref
        alias CList = T!C;
        C[200] arrayOfC;
        CList cList = construct!CList;
        scope(exit) cList.destruct;

        for (auto i = 0; i < arrayOfC.length; i++)
        {
            arrayOfC[i] = construct!C;
            arrayOfC[i].a = i;
            cList.add( arrayOfC[i] );
            assert( cList[i] is arrayOfC[i]);
            assert( cList.count == i + 1);
            assert( cList.find( arrayOfC[i] ) == i);
        }
        cList.swapIndexes(0,1);
        assert( cList.find(arrayOfC[0]) == 1 );
        assert( cList.find(arrayOfC[1]) == 0 );
        cList.swapIndexes(0,1);
        assert( cList.find(arrayOfC[0]) == 0 );
        assert( cList.find(arrayOfC[1]) == 1 );
        cList.remove(cList.last);
        assert( cList.count == arrayOfC.length -1 );
        cList.clear;
        assert( cList.count == 0 );
        for (auto i = 0; i < arrayOfC.length; i++)
        {
            cList.add( arrayOfC[i] );
        }
        cList.extract(50);
        assert( cList.find(arrayOfC[50]) == -1 );
        cList.insert(50,arrayOfC[50]);
        assert( cList.find(arrayOfC[50]) == 50 );
        cList.extract(50);
        cList.insert(arrayOfC[50]);
        assert( cList.find(arrayOfC[50]) == 0 );
        cList.clear;
        assert( cList.count == 0 );
        for (auto i = 0; i < arrayOfC.length; i++)
        {
            cList.add( arrayOfC[i] );
        }

        // cleanup of internally allocated items.
        C itm;
        cList.clear;
        assert(cList.count == 0);
        cList.addNewItem;
        cList.addNewItem;
        assert(cList.count == 2);

        itm = cList.extract(0);
        assert(itm);
        itm.destruct;
        assert(cList.count == 1);
        itm = cList.extract(0);
        assert(itm);
        itm.destruct;
        assert(cList.count == 0);

        cList.add(arrayOfC[0]);
        cList[0] = arrayOfC[1];
        assert(cList.find(arrayOfC[0]) == -1);
        assert(cList.find(arrayOfC[1]) == 0);
        cList.clear;

        cList = arrayOfC[10..20];
        assert(cList.count == 10);
        assert(cList[0] == arrayOfC[10]);
        assert(cList[9] == arrayOfC[19]);

        cList.clear;
        cList.insert(0, arrayOfC[24]);
        assert(cList[0] == arrayOfC[24]);
        cList.insert(123456, arrayOfC[25]);
        assert(cList.count == 2);
        assert(cList[1] == arrayOfC[25]);
        cList.swapItems(arrayOfC[24],arrayOfC[25]);
        assert(cList[0] == arrayOfC[25]);
        assert(cList[1] == arrayOfC[24]);
        cList.swapIndexes(0,1);
        assert(cList[0] == arrayOfC[24]);
        assert(cList[1] == arrayOfC[25]);
        C elem = cList.extract(1);
        assert(elem is arrayOfC[25]);
        assert(cList.count == 1);

        size_t i;
        cList = arrayOfC[0..10];
        foreach (C c; cList)
        {
            assert(c is arrayOfC[i]);
            ++i;
        }
        foreach_reverse(C c; cList)
        {
            --i;
            assert(c is arrayOfC[i]);
        }
        assert(cList.first == arrayOfC[0]);
        assert(cList.last == arrayOfC[9]);

        cList.clear;
        cList.add(arrayOfC[0..10]);
        assert(cList.count == 10);
        assert(cList.first == arrayOfC[0]);
        assert(cList.last == arrayOfC[9]);

        foreach(o; arrayOfC)
            destruct(o);
    }

    test!(ContiguousList);
    test!(DynamicList);
}

unittest
{
    alias List = DynamicList!(Array!int);
    Array!int a;
    Array!int b = [2,3];
    List lst = construct!List();
    lst.add(a);
    lst.add(b);
    assert(lst[0].ptr !is a.ptr);
    destructEach(a,b, lst);
}

/**
 * TreeItemSiblings is an input range that allows to
 * iterate the children of a TreeItem.
 */
struct TreeItemChildren(T)
{

private:

    T _front;

public:

    /// See $(D initialize()).
    this(TT)(auto ref TT t)
    if (is(TT == T))
    {
        initialize(t);
    }

    /// Initializes the range from a parent.
    void initialize(TT)(auto ref TT t)
    if (is(TT == T))
    {
        _front = t.firstChild;
    }

    ///
    ref T front()
    {
        return _front;
    }

    ///
    void popFront()
    {
        _front = _front.nextSibling;
    }

    ///
    bool empty()
    {
        return _front is null;
    }

    /**
     * Support for the array syntax.
     * Should be avoided in for() loops.
     */
    T opIndex(ptrdiff_t index)
    in
    {
        assert(_front);
    }
    body
    {
        T result = _front;
        ptrdiff_t cnt;
        while (true)
        {
            if (cnt++ == index || !result)
                return result;
            result = result.nextSibling;
        }
    }

    /// Support the array syntax.
    void opIndexAssign(T item, size_t i)
    in
    {
        assert(_front);
        assert(item);
    }
    body
    {
        auto old = opIndex(i);
        if (!old)
            _front.addSibling(item);
        else
        {
            if (_front.findSibling(item) != -1)
                _front.exchangeSibling(item,old);
            else
            {
                _front.removeSibling(old);
                _front.insertSibling(i,item);
            }
        }
    }
}

/**
 * TreeItemSiblings is an input range that allows to
 * iterate over the siblings of a TreeItem.
 */
struct TreeItemSiblings(T)
{

private:

    T _front;

public:

    /// See $(D initialize()).
    this(TT)(auto ref TT t)
    if (is(TT == T))
    {
        initialize(t);
    }

    /// Initializes the range from one of the siblings.
    void initialize(TT)(auto ref TT t)
    if (is(TT == T))
    {
        TT tt =t;
        if (tt.parent)
            _front = tt.parent.firstChild;
        else
        {
            while (tt.prevSibling !is null)
            {
                tt = tt.prevSibling;
            }
            _front = tt;
        }
    }

    ///
    T front() @safe @nogc
    {
        return _front;
    }

    ///
    void popFront()
    {
        _front = _front.nextSibling;
    }

    ///
    bool empty() @safe @nogc
    {
        return _front is null;
    }

    /**
     * Support for the array syntax.
     * Should be avoided in for loops.
     */
    T opIndex(ptrdiff_t index)
    in
    {
        assert(_front);
    }
    body
    {
        T result = _front;
        ptrdiff_t cnt;
        while (true)
        {
            if (cnt++ == index || !result)
                return result;
            result = result.nextSibling;
        }
    }

    /// Support for the array syntax.
    void opIndexAssign(T item, size_t i)
    in
    {
        assert(_front);
    }
    body
    {
        if (!item)
            _front.removeSibling(i);
        else
        {
            auto old = opIndex(i);
            if (!old)
                _front.addSibling(item);
            else
            {
                if (_front.findSibling(item) != -1)
                    _front.exchangeSibling(item,old);
                else
                {
                    _front.removeSibling(old);
                    _front.insertSibling(i,item);
                }
            }
        }
    }
}

/**
 * The TreeItem mixin turns its target into a tree item.
 */
mixin template TreeItem()
{

protected:

    import iz.memory: construct, destruct;

    enum isStruct = is(typeof(this) == struct);
    static if (isStruct)
        alias TreeItemType = typeof(this)*;
    else
        alias TreeItemType = typeof(this);

    TreeItemType _prevSibling, _nextSibling, _firstChild, _parent;
    TreeItemSiblings!TreeItemType _siblings;
    TreeItemChildren!TreeItemType _children;

    import iz.streams: Stream, writeArray;

public:

    /// Returns $(D this) when mixed in a class or $(D &this) in a struct.
    TreeItemType self()
    {
        static if (isStruct)
            return &this;
        else
            return this;
    }

    /**
     * Returns the previous TreeItem.
     */
    TreeItemType prevSibling()
    {
        return _prevSibling;
    }

    /**
     * Returns the next TreeItem.
     */
    TreeItemType nextSibling()
    {
        return _nextSibling;
    }

    /**
     * Returns the parent.
     */
    TreeItemType parent()
    {
        return _parent;
    }

    /**
     * Returns the first child.
     */
    TreeItemType firstChild()
    {
        return _firstChild;
    }

    /**
     * Returns an input range that allows to iterate the siblings.
     * The array syntax is also supported.
     */
    TreeItemSiblings!TreeItemType siblings()
    {
        _siblings.initialize(self);
        return _siblings;
    }

    /**
     * Returns an input range that allows to iterate the children.
     * The array syntax is also supported.
     */
    TreeItemChildren!TreeItemType children()
    {
        _children.initialize(self);
        return _children;
    }

// siblings -------------------------------------------------------------------+

    /**
     * Constructs, adds to the back then returns a new sibling.
     * This method should be prefered over addChild and insertChild
     * if $(D deleteChildren()) is used.
     *
     * When TreeItem is mixed in a class a template parameter that specifies
     * the class type to return must be present, otherwise in both cases the
     * function passes the optional run-time parameters to the constructor.
     */
    static if (is(TreeItemType == class))
    {
        IT addNewSibling(IT,A...)(A a)
        if (is(IT : TreeItemType))
        {
            auto result = construct!IT(a);
            addSibling(result);
            return result;
        }
    }
    else
    {
        TreeItemType addNewSibling(A...)(A a)
        {
            TreeItemType result = construct!(typeof(this))(a);
            addSibling(result);
            return result;
        }
    }

    /**
     * Returns the last item.
     * The value returned is never null.
     */
    TreeItemType lastSibling()
    {
        TreeItemType result;
        result = self;
        while (result.nextSibling)
        {
            result = result.nextSibling;
        }
        return result;
    }

    /**
     * Returns the first item.
     * The value returned is never null.
     */
    TreeItemType firstSibling()
    {
        if (_parent)
            return _parent._firstChild;
        else
        {
            TreeItemType result;
            result = self;
            while (result.prevSibling)
            {
                result = result.prevSibling;
            }
            return result;
        }
    }

    /**
     * Returns the index of sibling if it's found otherwise -1.
     */
    ptrdiff_t findSibling(TreeItemType sibling)
    in
    {
        assert(sibling);
    }
    body
    {
        auto current = self;
        while (current)
        {
            if (current is sibling) break;
            current = current.prevSibling;
        }
        if (!current)
        {
            current = self;
            while (current)
            {
                if (current is sibling) break;
                current = current.nextSibling;
            }
        }
        if (!current) return -1;
        return current.siblingIndex;
    }

    /**
     * Adds an item at the end of list.
     */
    void addSibling(TreeItemType sibling)
    in
    {
        assert(sibling);
    }
    body
    {
        if (sibling.hasSibling)
        {
            if (sibling.prevSibling !is null)
                sibling.prevSibling.removeSibling(sibling);
            else
                sibling.nextSibling.removeSibling(sibling);
        }

        auto oldlast = lastSibling;
        assert(oldlast);
        oldlast._nextSibling = sibling;
        sibling._prevSibling = oldlast;
        sibling._nextSibling = null;
        sibling._parent = parent;
    }

    /**
     * Inserts an item at the beginning of the list.
     */
    void insertSibling(TreeItemType sibling)
    in
    {
        assert(sibling);
    }
    body
    {
        if (sibling.hasSibling)
        {
            if (sibling.prevSibling !is null)
                sibling.prevSibling.removeSibling(sibling);
            else
                sibling.nextSibling.removeSibling(sibling);
        }

        auto oldfirst = firstSibling;
        assert(oldfirst);
        oldfirst._prevSibling = sibling;
        sibling._nextSibling = oldfirst;
        sibling._parent = parent;

        if (parent)
        {
            parent._firstChild = sibling;
        }
    }

    /**
     * Inserts a sibling.
     *
     * Params:
     *      index = The position where to insert.
     *      sibling = the item to insert.
     */
    void insertSibling(size_t index, TreeItemType sibling)
    in
    {
        assert(sibling);
    }
    body
    {
        if (sibling.hasSibling)
        {
            if (sibling.prevSibling !is null)
                sibling.prevSibling.removeSibling(sibling);
            else
                sibling.nextSibling.removeSibling(sibling);
        }

        const size_t cnt = siblingCount;
        if (index == 0) insertSibling(sibling);
        else if (index >= cnt) addSibling(sibling);
        else
        {
            size_t result = 1;
            auto old = firstSibling;
            while (old)
            {
                if (result == index)
                {
                    auto item1oldprev = old.prevSibling;
                    auto item1oldnext = old.nextSibling;
                    sibling._prevSibling = old;
                    sibling._nextSibling = item1oldnext;
                    old._nextSibling = sibling;
                    item1oldnext._prevSibling = sibling;
                    sibling._parent = _parent;
                    assert(sibling.siblingIndex == index);

                    return;
                }
                old = old.nextSibling;
                result++;
            }
        }
    }

    /**
     * Exchanges the position of two siblings.
     */
    void exchangeSibling(TreeItemType sibling1, TreeItemType sibling2)
    in
    {
        assert(sibling1);
        assert(sibling2);
        assert(sibling1._parent is sibling2._parent);
    }
    body
    {
        auto item1oldprev = sibling1._prevSibling;
        auto item1oldnext = sibling1._nextSibling;
        auto item2oldprev = sibling2._prevSibling;
        auto item2oldnext = sibling2._nextSibling;
        const bool contiguous = item2oldprev == sibling1 || item1oldprev == sibling2;

        if (!contiguous)
        {
            sibling1._prevSibling = item2oldprev;
            sibling1._nextSibling = item2oldnext;
            sibling2._prevSibling = item1oldprev;
            sibling2._nextSibling = item1oldnext;
            if (item1oldprev) item1oldprev._nextSibling = sibling2;
            if (item1oldnext) item1oldnext._prevSibling = sibling2;
            if (item2oldprev) item2oldprev._nextSibling = sibling1;
            if (item2oldnext) item2oldnext._prevSibling = sibling1;
        }
        else
        {
            if (item1oldnext is sibling2)
            {
                sibling1._prevSibling = sibling2;
                sibling1._nextSibling = item2oldnext;
                sibling2._nextSibling = sibling1;
                sibling2._prevSibling = item1oldprev;
            }
            else
            {
                sibling2._prevSibling = sibling1;
                sibling2._nextSibling = item1oldnext;
                sibling1._nextSibling = sibling2;
                sibling1._prevSibling = item2oldprev;
            }
        }

        if (sibling1._parent && sibling1._parent.firstChild is sibling1)
        {
            sibling1._parent._firstChild = sibling2;
        }
        else if (sibling2._parent && sibling2._parent.firstChild is sibling2)
        {
            sibling2._parent._firstChild = sibling1;
        }
    }

    /**
     * Removes an item.
     *
     * Params:
     *      sibling = The item to remove.
     * Returns:
     *      true if the item is a sibling otherwise false.
     */
    bool removeSibling(TreeItemType sibling)
    {
        if (!sibling || (sibling.parent && sibling.parent != parent))
            return false;

        TreeItemType oldprev = sibling._prevSibling;
        TreeItemType oldnext = sibling._nextSibling;
        if (oldprev) oldprev._nextSibling = oldnext;
        if (oldnext) oldnext._prevSibling = oldprev;

        if (parent && parent.firstChild is sibling)
        {
            parent._firstChild = oldnext;
        }

        sibling._prevSibling = null;
        sibling._nextSibling = null;
        sibling._parent = null;

        return true;
    }

    /**
     * Removes the nth sibling.
     *
     * Params:
     *      index = the index of the sibling to remove.
     *  Returns:
     *      The item if the index is valid, otherwise null.
     */
    TreeItemType removeSibling(size_t index)
    {
        TreeItemType result = siblings[index];
        if (result)
        {
            TreeItemType oldprev = result._prevSibling;
            TreeItemType oldnext = result._nextSibling;
            if (oldprev) oldprev._nextSibling = oldnext;
            if (oldnext) oldnext._prevSibling = oldprev;

            if (parent && parent.firstChild is result)
            {
                parent._firstChild = result._nextSibling;
            }

            result._prevSibling = null;
            result._nextSibling = null;
            result._parent = null;
        }
        return result;
    }

    /**
     * Returns the count of sibling in the branch.
     * The value returned is always greater than 0.
     */
    size_t siblingCount()
    {
        size_t toFront, toBack;
        auto current = self;
        while (current)
        {
            current = current._prevSibling;
            toFront++;
        }
        current = self;
        while (current)
        {
            current = current._nextSibling;
            toBack++;
        }
        return toFront + toBack -1;
    }

    /**
     * Returns the item position in the list.
     */
    ptrdiff_t siblingIndex()
    {
        ptrdiff_t result = -1;
        TreeItemType current = self;
        while (current)
        {
            current = current._prevSibling;
            result++;
        }
        return result;
    }

    /**
     * Sets the item position in the list.
     * The new position of the previous item is undetermined.
     */
    void siblingIndex(size_t position)
    {
        TreeItemType old = siblings[position];
        if (old !is self)
        {
            TreeItemType prt = _parent;
            removeSibling(self);
            old.insertSibling(position, self);
            _parent = prt;
        }
    }

    /**
     * Indicates if the item has neighboors.
     */
    bool hasSibling()
    {
        return prevSibling !is null || nextSibling !is null;
    }

// -----------------------------------------------------------------------------
// children -------------------------------------------------------------------+

    /**
     * Constructs, adds to the back then returns a new child.
     * This method should be prefered over addChild and insertChild
     * if $(D deleteChildren()) is used.
     *
     * When TreeItem is mixed in a class a template parameter that specifies
     * the class type to return must be present, otherwise in both cases the
     * function passes the optional run-time parameters to the constructor.
     */
    static if (is(TreeItemType == class))
    {
        IT addNewChild(IT,A...)(A a)
        if (is(IT : TreeItemType))
        {
            auto result = construct!IT(a);
            addChild(result);
            return result;
        }
    }
    else
    {
        TreeItemType addNewChild(A...)(A a)
        {
            TreeItemType result = construct!(typeof(this))(a);
            addChild(result);
            return result;
        }
    }

    /**
     * Returns the distance to the root.
     */
    size_t level()
    {
        size_t result;
        auto current = self;
        while (current._parent)
        {
            current = current._parent;
            result++;
        }
        return result;
    }

    /**
     * Returns the root.
     */
    TreeItemType root()
    {
        auto current = self;
        while (current._parent)
            current = current._parent;
        return current;
    }

    /**
     * Returns the children count.
     */
    size_t childrenCount()
    {
        if ( _firstChild is null)
            return 0;
        else
            return _firstChild.siblingCount;
    }

    /**
     * Adds child to the back.
     */
    void addChild(TreeItemType child)
    {
        if (child.parent)
        {
            if (child.parent !is self)
                child.parent.removeChild(child);
            else
                return;
        }
        if (!_firstChild)
        {
            _firstChild = child;
            child._parent = self;
            return;
        }
        else _firstChild.addSibling(child);
    }

    /**
     * Inserts the first child.
     */
    void insertChild(TreeItemType child)
    {
        if (!_firstChild)
        {
            _firstChild = child;
            child._parent = self;
            return;
        }
        else _firstChild.insertSibling(child);
    }

    /**
     * Inserts a child.
     *
     * Params:
     *      index = The position in the children list.
     *      child = The child to insert.
     */
    void insertChild(size_t index, TreeItemType child)
    in
    {
        assert(child);
    }
    body
    {
        if (!_firstChild)
        {
            _firstChild = child;
            child._parent = self;
            return;
        }
        else _firstChild.insertSibling(index, child);
    }

    /**
     * Removes a child from the list.
     *
     * Params:
     *      child = The child to remove.
     * Returns:
     *      true if child is removed.
     */
    bool removeChild(TreeItemType child)
    in
    {
        assert(child);
    }
    body
    {
        ptrdiff_t i = -1;
        if (_firstChild)
        {
            i = firstChild.findSibling(child);
            if (i != -1) removeChild(i);
        }
        return i != -1;
    }

    /**
     * Removes the nth child.
     *
     * Params:
     *      index = The child index.
     * Returns:
     *      The child if index was valid, otherwise null.
     */
    TreeItemType removeChild(size_t index)
    in
    {
        assert(index >= 0);
    }
    body
    {
        TreeItemType result = children[index];
        if (result)
        {
            if (index > 0)
                result.prevSibling.removeSibling(index);
            else
            {
                if (result.siblingCount == 1)
                {
                    result._parent = null;
                    _firstChild = null;
                }
                else
                    result._nextSibling.removeSibling(index);
            }
        }
        return result;
    }

    /**
     * Removes the children, without destructing them.
     * After the call, the links to the items siblings are also reset to null.
     */
    void removeChildren()
    {
        auto current = _firstChild;
        while (current)
        {
            current.removeChildren;

            auto _next = current.nextSibling;
            current._parent = null;
            current._prevSibling = null;
            current._nextSibling = null;
            current = _next;
        }
        _firstChild = null;
    }

    /**
     * Removes and deletes (destroy) the children.
     *
     * This method should be used in pair with $(D addNewChild()) and
     * $(D addNewSiblings()). If $(D add()) or $(D insert()) have been used to
     * build the tree then initial references will be dangling.
     */
    void deleteChildren()
    {
        while (_firstChild)
        {
            auto current = _firstChild;
            _firstChild = current.nextSibling;

            current.deleteChildren;
            current._parent = null;
            current._nextSibling = null;
            current._prevSibling = null;
            static if (is(TreeItemType == interface) || is(TreeItemType == class))
                destruct(cast(Object) current);
            else
                destruct(current);
        }
    }
// -----------------------------------------------------------------------------
// other ----------------------------------------------------------------------+

    /**
     * Converts the node to a string.
     * This is used to represent the whole tree in $(D saveToStream()).
     */
    char[] itemToTextNative()
    {
        import std.format: format;
        char[] result;
        foreach (immutable i; 0 .. level) result ~= '\t';
        result ~= format( "Index: %.4d - NodeType: %s", siblingIndex, typeof(this).stringof);
        return result;
    }

    /**
     * Saves the textual representation of the tree to a Stream.
     *
     * Params:
     *      stream = The Stream where items are written.
     *      itemToText = A custom function to render the items. When not specified,
     *      $(D itemToTextNative()) is used.
     */
    void saveToStream(Stream stream, string function(TreeItemType) itemToText = null)
    {
        char[] txt;
        if (itemToText)
            txt = itemToText(self).dup;
        else
            txt = itemToTextNative ~ "\r\n";
        writeArray!false(stream, txt);
        foreach (c; children)
            c.saveToStream(stream, itemToText);
    }
// -----------------------------------------------------------------------------
}

/**
 * Helper class template that implements TreeItem in a C descendant.
 */
class TreeItemClass(C): C
if (is(C==class))
{
    mixin inheritedDtor;
    mixin TreeItem;
}

/// Alias to the most simple TreeItem class type.
alias ObjectTreeItem = TreeItemClass!Object;

/**
 * Helper struct template that implements TreeItem in a struct.
 *
 * Params:
 *      fieldsAndFuncs = The struct declarations, as a string to mix.
 */
struct TreeItemStruct(string fieldsAndFuncs)
{
    mixin(fieldsAndFuncs);
    mixin TreeItem;
}

/// Alias to the most simple TreeItem struct type.
alias StructTreeItem = TreeItemStruct!q{public void* data;};

unittest
{
    ObjectTreeItem[20] items;
    ObjectTreeItem root;

    items[0] = construct!ObjectTreeItem;
    root = items[0];
    for (auto i =1; i < items.length; i++)
    {
        items[i] = construct!ObjectTreeItem;
        if (i>0) root.addSibling( items[i] );
        assert( items[i].siblingIndex == i );
        assert( root.siblings[i].siblingIndex == i );
        assert( root.siblings[i] == items[i] );
        if (i>0) assert( items[i].prevSibling.siblingIndex == i-1 );
        assert(root.lastSibling.siblingIndex == i);
    }
    assert(root.siblingCount == items.length);

    assert(items[1].nextSibling.siblingIndex == 2);
    assert(items[1].prevSibling.siblingIndex == 0);

    root.exchangeSibling(items[10],items[16]);
    assert(root.siblingCount == items.length);
    assert( items[10].siblingIndex == 16);
    assert( items[16].siblingIndex == 10);

    root.exchangeSibling(items[10],items[16]);
    assert(root.siblingCount == items.length);
    assert( items[10].siblingIndex == 10);
    assert( items[16].siblingIndex == 16);


    items[8].siblingIndex = 4;
    assert( items[8].siblingIndex == 4);
    //assert( ObjectTreeItems[4].siblingIndex == 5); // when siblingIndex() calls remove/insert
    //assert( ObjectTreeItems[4].siblingIndex == 8); // when siblingIndex() calls exchangeSibling.

    assert( root.siblings[16] == items[16]);
    assert( root.siblings[10] == items[10]);
    root.siblings[16] = items[10]; // exchg
    assert(root.siblingCount == items.length);
    root.siblings[16] = items[16]; // exchg
    assert(root.siblingCount == items.length);
    assert( items[16].siblingIndex == 16);
    assert( items[10].siblingIndex == 10);

    ObjectTreeItem c = construct!ObjectTreeItem;
    root.siblings[10] = c;
    root.siblings[16] = items[10];
    assert( items[16].siblingIndex == 0);
    assert( items[10].siblingIndex == 16);
    assert( c.siblingIndex == 10);

    assert(root.findSibling(items[18]) > -1);
    assert(root.findSibling(items[0]) > -1);

    foreach (item; root.siblings)
    {
        assert(root.findSibling(item) == item.siblingIndex);
    }

    root.removeSibling(19);
    assert(root.siblingCount == items.length -1);
    root.removeSibling(18);
    assert(root.siblingCount == items.length -2);
    root.removeSibling(items[13]);
    assert(root.siblingCount == items.length -3);
    //root1[0] = null; // exception because root1[0] = root1
    assert(root.siblingCount == items.length -3);
    root.siblings[1] = null;
    assert(root.siblingCount == items.length -4);

    foreach(o; items)
        destruct(o);
    destruct(c);
}

unittest
{
    ObjectTreeItem root;
    ObjectTreeItem[20] items1;
    ObjectTreeItem[4][20] items2;

    root = construct!ObjectTreeItem;
    assert(root.level == 0);
    for (auto i=0; i < items1.length; i++)
    {
        items1[i] = construct!ObjectTreeItem;
        root.addChild(items1[i]);
        assert(root.childrenCount == 1 + i);
        assert(items1[i].parent is root);
        assert(items1[i].siblingCount == 1 + i);
        assert(items1[i].level == 1);
        assert(items1[i].siblingIndex == i);
    }
    root.removeChildren;
    assert(root.childrenCount == 0);
    for (auto i=0; i < items1.length; i++)
    {
        root.addChild(items1[i]);
        assert(items1[i].siblingIndex == i);
    }

    for (auto i = 0; i < items2.length; i++)
        for (auto j = 0; j < items2[i].length; j++)
        {
            items2[i][j] = construct!ObjectTreeItem;
            items1[i].addChild(items2[i][j]);
            assert(items2[i][j].level == 2);
            assert(items1[i].childrenCount == 1 + j);
            assert(items2[i][j].siblingCount == 1 + j);
        }

    root.deleteChildren;
/*
    // this is an expected behavior:

    // original refs are dangling
    assert( items1[12] is null);
    assert( items2[12][0] is null);
    assert( items2[18][3] is null);
    // A.V: 'cause the items are destroyed
    writeln( items1[12].level );
*/

    root.addNewChild!ObjectTreeItem();
        root.children[0].addNewChild!ObjectTreeItem();
        root.children[0].addNewChild!ObjectTreeItem();
        root.children[0].addNewChild!ObjectTreeItem();
    root.addNewChild!ObjectTreeItem();
        root.children[1].addNewChild!ObjectTreeItem();
        root.children[1].addNewChild!ObjectTreeItem();
        root.children[1].addNewChild!ObjectTreeItem();
        root.children[1].addNewChild!ObjectTreeItem();
            root.children[1].children[3].addNewChild!ObjectTreeItem();
            root.children[1].children[3].addNewChild!ObjectTreeItem();
            root.children[1].children[3].addNewChild!ObjectTreeItem();

    assert(root.childrenCount == 2);
    assert(root.children[0].childrenCount == 3);
    assert(root.children[1].childrenCount == 4);
    assert(root.children[1].children[3].childrenCount == 3);
    assert(root.children[1].children[3].children[0].level == 3);

    assert(root.children[1].children[3].children[0].root is root);
    assert(root.children[1].children[3].root is root);

    auto str = construct!MemoryStream;
    root.saveToStream(str);
    //str.saveToFile("treenodes.txt");
    str.destruct;
    root.deleteChildren;
    destruct(root);
}

unittest
{
    ObjectTreeItem root = construct!ObjectTreeItem;
    ObjectTreeItem c0 = root.addNewChild!ObjectTreeItem;
    ObjectTreeItem c1 = root.addNewChild!ObjectTreeItem;
    ObjectTreeItem c2 = root.addNewChild!ObjectTreeItem;

    scope(exit) destructEach(root, c0, c1, c2);

    assert(root.childrenCount == 3);
    root.removeChild(c0);
    assert(root.childrenCount == 2);
    root.removeChild(c1);
    assert(root.childrenCount == 1);
    root.removeChild(c2);
    assert(root.childrenCount == 0);
    root.removeChild(c2);
    assert(root.childrenCount == 0);
}

unittest
{
    alias ItemStruct = TreeItemStruct!"uint a;";

    ItemStruct* root = construct!ItemStruct;

    auto c0 = root.addNewChild;
    auto c1 = root.addNewChild;
    auto c2 = root.addNewChild;

    scope(exit) destructEach(root, c0, c1, c2);

    assert(root.childrenCount == 3);
    root.removeChild(0);
    assert(root.childrenCount == 2);
    root.removeChild(0);
    assert(root.childrenCount == 1);
    root.removeChild(0);
    assert(root.childrenCount == 0);

    root.addChild(c0);
    root.addChild(c1);
    root.addChild(c2);
    assert(root.childrenCount == 3);
    root.removeChild(0);
    assert(root.childrenCount == 2);
    root.insertChild(c0);
    assert(root.childrenCount == 3);
    assert(root.children[0] == c0);
    root.removeChild(1);
    assert(c1.parent == null);
    assert(root.childrenCount == 2);
    root.insertChild(1, c1);
    assert(root.childrenCount == 3);
    root.removeChild(1);
    assert(root.childrenCount == 2);
    assert(c1.parent == null);
    root.insertChild(1, c1);
    assert(root.childrenCount == 3);
    assert(root.children[1] == c1);

    assert(root.children[0] == c0);
    assert(root.children[1] == c1);
    assert(root.children[2] == c2);

    c2.exchangeSibling(c0,c1);
    assert(root.firstChild == c1);
    assert(root.children[0] == c1);
    assert(root.children[1] == c0);
    assert(root.children[2] == c2);
    c2.exchangeSibling(c0,c1);
    assert(root.firstChild == c0);
    root.removeChild(c2);
    c0.insertSibling(c2);
    assert(root.firstChild == c2);

    root.removeChildren;
    root.addChild(c0);
    root.addChild(c1);
    root.addChild(c2);
    auto r = root.children;
    assert(r.front == c0);
    r.popFront;
    assert(r.front == c1);
    r.popFront;
    assert(r.front == c2);
    r.popFront;
    assert(r.empty);
}

unittest
{
    ObjectTreeItem root = construct!ObjectTreeItem;
    ObjectTreeItem c0 = root.addNewChild!ObjectTreeItem;
    ObjectTreeItem c1 = root.addNewChild!ObjectTreeItem;
    ObjectTreeItem c2 = root.addNewChild!ObjectTreeItem;
    scope(exit) destructEach(root, c0, c1, c2);

    size_t it;
    foreach (ObjectTreeItem child; root.children)
    {
        assert(c0.parent);
        assert(c1.parent);
        assert(c2.parent);
        if (it == 0)
        {
            c1.siblingIndex = c1.siblingCount-1;
            c0.siblingIndex = c0.siblingCount-1;
            c0.siblingIndex = 0;
        }
        else if (it == 1)
        {
            c1.siblingIndex = c1.siblingCount-1;
            c0.siblingIndex = c0.siblingCount-1;
            c0.siblingIndex = 0;
        }
        else if (it == 2)
        {
            c1.siblingIndex = c1.siblingCount-1;
            c0.siblingIndex = c0.siblingCount-1;
            c0.siblingIndex = 0;
        }
        it++;
    }
}


private struct LinearProbeSlot(K, V = void)
{

    enum isMap = !is(V == void);

private:

    K _key;
    static if (isMap) V _value;

public:

    @disable this();

    this(this) @nogc
    {
        static if (is(K == struct) && hasMember!(K, "__postblit") && isCopyable!K)
            _key.__postblit;
        static if (isMap && is(V == struct) && hasMember!(V, "__postblit") && isCopyable!V)
            _value.__postblit;
    }

    static if (!isMap)
    {
        this(K)(auto ref K key)
        {
            _key = key;
        }
    }
    else
    {
        this()(auto ref K key, auto ref V value)
        {
            _key = key;
            _value = value;
        }
    }

    ~this()
    {
        static if (is(K == struct))
        {
            destruct(_key);
        }
        static if (isMap && is(V == struct))
        {
            destruct(_value);
        }
    }

    auto opEquals(KK)(auto ref KK key) const
    {
        static if (!isMap)
        {
            static if (hasElaborateSelfEquals!K)
                return _key.opEquals(key);
            else
                return _key == key;
        }
        else
        {
            static if (hasElaborateSelfEquals!K)
            {
                if (_key.opEquals(key))
                    return cast(const(V)*) &_value;
                else
                    return cast(const(V)*) null;
            }
            else
            {
                if (key == _key)
                    return cast(const(V)*) &_value;
                else
                    return cast(const(V)*) null;
            }
        }
    }

    auto ptr()
    {
        static if (!isMap)
            return &this;
        else
            return cast(Tuple!(K,V)*) &this;
    }

    alias Slot = typeof(this);
    struct FindResult
    {
        @disable this(this);
        /// the hash after probing
        size_t endHash;
        /// the bucket after probing
        Slot* slot;
    }
}

unittest
{
    LinearProbeSlot!string lpbs = LinearProbeSlot!string("cat");
    assert(lpbs == "cat");
    assert(lpbs != "dog");

    LinearProbeSlot!(string,int) lpbsi = LinearProbeSlot!(string,int)("cat", 1);
    assert(*(lpbsi == "cat") == 1);
    assert((lpbsi == "rat") is null);
}


/**
 * Default hash function used in the HashSet and the HashMap
 */
pragma(inline, true)
size_t fnv1(V, bool fnv1a = false)(auto ref V value)
{
    static if (isBasicType!V || is(V==struct) || is(V == union))
        return fnv1Impl!fnv1a(cast(ubyte*) &value, V.sizeof);
    else static if (isArray!V)
        return fnv1Impl!fnv1a(cast(ubyte*) value.ptr, value.length * (ElementEncodingType!V).sizeof);
    else static if (isPointer!V || is(V == class) || is(V == interface))
        return fnv1Impl!fnv1a(cast(ubyte*) value, V.sizeof);
    else static assert(0);
}

private size_t fnv1Impl(bool fnv1a = false)(ubyte* data, size_t length)
{
    static if (size_t.sizeof == 8)
        size_t h = 0xCBF29CE484222325UL;
    else static if (size_t.sizeof == 4)
        size_t h = 0x811C9DC5UL;
    else static assert(0);

    static if (size_t.sizeof == 8)
    foreach (immutable i; 0..length)
    {
        static if (fnv1a)
        {
            h ^= *data++;
            h *= 0x100000001B3UL;
        }
        else
        {
            h *= 0x100000001B3UL;
            h ^= *data++;
        }
    }
    else static if (size_t.sizeof == 4)
    foreach (immutable i; 0..length)
    {
        static if (fnv1a)
        {
            h ^= *data++;
            h *= 0x1000193U;
        }
        else
        {
            h *= 0x1000193U;
            h ^= *data++;
        }
    }
    else static assert(0);
    return h;
}


/**
 * Enumerates the hashset and hashmap insertion mode
 */
enum
{
    /// Buckets are already reserved.
    imReserved = false,
    /// Always reserves a bucket.
    imExpand = true
}


/**
 * Enumerates the collision handling modes that
 * can be specified to instantiate a HashSet or a Map.
 */
enum CollisionHandling
{
    /// Hanlde collision using linear probing.
    linearProbe,
    /// Handle the collision by inserting in a bucket.
    bucketArray,
    /// Assumes the hash function not to produce collisions.
    none,
}

private alias CH = CollisionHandling;

/// Aliases an hashset implementation, by default HashSet_AB.
template HashSet(CollisionHandling ch = CH.bucketArray)
{
    static if (ch == CH.linearProbe)
        alias HashSet = HashSet_LP;
    else static if (ch == CH.bucketArray)
        alias HashSet = HashSet_AB;
    else
        static assert(0);
}

/// Aliases an hashset implementation, by default HashMap_LP.
template HashMap(CollisionHandling ch = CH.linearProbe)
{
    static if (ch == CH.linearProbe)
        alias HashSet = HashMap_LP;
    else
        static assert(0);
}

private enum
{
    rngNoMap,
    rngMapByKey,
    rngMapByValue,
    rngMapByKeyValue,
}

private struct RangeForLpSet(T, alias rngKind = rngNoMap)
{

private:

    alias PF = ReturnType!(T.slot);
    alias F = PointerTarget!PF;

    size_t index;
    T* _hashOrMap;

    //pragma(inline, true)
    void next()
    {
        while (index < _hashOrMap.slotCount)
        {
            if ((*_hashOrMap).slot(index))
                break;
            else
                ++index;
        }
    }

public:

    this(T* hashOrMap) @nogc
    {
        assert(hashOrMap);
        _hashOrMap = hashOrMap;
        next();
    }

    void popFront() @nogc
    {
        ++index;
        next();
    }

    bool empty() @nogc
    {return index >= _hashOrMap.slotCount;}

    ref front() @nogc
    {
        static if (rngKind == rngNoMap || rngKind == rngMapByKeyValue)
            return *(*_hashOrMap).slot(index);
        else static if (rngKind == rngMapByKey)
            return (*(*_hashOrMap).slot(index))[0];
        else static if (rngKind == rngMapByValue)
            return (*(*_hashOrMap).slot(index))[1];
    }
}

/**
 * A manually managed hashset that uses linear probing to solve the collisions.
 *
 * Params:
 *      K = the key type.
 *      hasherFun = The hashing function, a $(D size_t(K value);) function,
 *          literal delegate or lambda.
 */
struct HashSet_LP(K, alias hasherFun = fnv1)
{
    //static assert (is(typeof( (){size_t r = hasherFun(K.init);}  )),
      //  "invalid hash function");
    static assert (!is(K == void),
        "invalid Key type");

private:

    alias HashSetT = typeof(this);
    alias Slot = LinearProbeSlot!K;
    @NoGc Array!(Slot*) _slots;
    size_t _count;

    pragma(inline, true)
    size_t hasher(KK)(auto ref KK key)
    {
        return hasherFun(key) & (_slots.length - 1);
    }

    void reHash()() @nogc
    {
        _count = 0;
        Array!(Slot*) old = _slots;
        assert(old == _slots);
        _slots[] = null;
        foreach (immutable i; 0..old.length)
        {
            if (auto b = old[i])
            {
                insert(b._key);
                destruct(b);
            }
        }
        destruct(old);
    }

    pragma(inline, true)
    size_t nextHash(size_t value) @nogc @safe
    {
        return (value + 1) & (_slots.length - 1);
    }

    //pragma(inline, true)
    Slot.FindResult find(KK)(auto ref KK key)
    {
        Slot.FindResult fr;
        const size_t hb = hasher(key);
        fr.endHash = hb;

        if (_slots.length)
        {
            fr.slot = _slots[hb];
            while (true)
            {
                if (fr.slot && *fr.slot != key)
                {
                    fr.endHash = nextHash(fr.endHash);
                    if (fr.endHash == hb)
                        break;
                    fr.slot = _slots[fr.endHash];
                }
                else break;
            }
        }
        return fr;
    }

public:

    /**
     * Constructs using either a list of keys, arrays of keys, or both.
     */
    this(A...)(A a)
    if (A.length)
    {
        import std.meta: aliasSeqOf;
        import std.range: iota;

        foreach (i; aliasSeqOf!(iota(0, A.length)))
        {
            static if (is(A[i] == K))
                continue;
            else static if (isArray!(A[i]))
                continue;
            else static if (is(A[i] == Array!K))
                continue;
            else static assert(0, A[i].stringof ~ " not supported");
        }
        foreach (i; aliasSeqOf!(iota(0, A.length)))
                insert(a[i]);
    }

    ~this() @nogc
    {
        foreach (immutable i; 0.._slots.length)
            if (Slot* slt = _slots[i])
                destruct(slt);
        destruct(_slots);
    }

    this(this) @nogc
    {
        foreach (immutable i; 0.._slots.length)
            if (Slot* slt = _slots[i])
        {
            Slot* n = construct!Slot(slt._key);
            n.__postblit();
            _slots[i] = n;
        }
    }

    /**
     * Tries to insert key(s) in the set.
     *
     * Params:
     *      mode = If true (imExpand) then reserves a slot else (imReserved)
     *          assumes a previous call to $(D reserve()).
     *      key = either single keys, array of keys, or both.
     * Throws:
     *      An $(D OutOfMemoryError) if an internal call to $(D reserve()) fails.
     * Returns:
     *      If the key is added or if it's already included then returns $(D true),
     *      otherwise $(D false).
     */
    bool insert(alias mode)(ref K key) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        bool result;
        static if (mode)
            reserve(1);
        Slot.FindResult fr = find(key);
        if (fr.slot is null || *fr.slot != key)
        {
            assert(key !in this);
            Slot* n = construct!Slot(key);
            if (fr.slot is null)
            {
                assert(key !in this);
                _slots[fr.endHash] = n;
                result = true;
                ++_count;
            }
            else destruct(n);
        }
        else if (*fr.slot == key)
            result = true;
        return result;
    }

    /// ditto
    bool insert(alias mode = true)(K key) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        return insert!mode(key);
    }

    /// ditto
    bool insert(KK)(auto ref KK keys)
    if ((isArray!KK && is(Unqual!(ElementEncodingType!KK) == K)) || is(KK == Array!K))
    {
        bool result = true;
        reserve(keys.length);
        foreach(k; keys)
            result &= insert!false(k);
        return result;
    }

    /// ditto
    bool insert(KK...)(auto ref KK keys)
    {
        bool result = true;
        reserve(KK.length);
        foreach(k; keys)
            result &= insert!false(k);
        return result;
    }

    /**
     * Tries to remove a key from the set.
     *
     * Returns:
     *      $(D true) if the key was included otherwise $(D false).
     */
    bool remove()(auto ref K key)
    {
        bool result;
        Slot.FindResult fr = find(key);
        if (fr.slot)
        {
            result = true;
            destruct(fr.slot);
            _slots[fr.endHash] = null;
            reHash;
        }
        return result;
    }

    /**
     * Clears and empties the set.
     */
    void clear() @nogc
    {
        foreach (immutable i; 0.._slots.length)
            if (auto b = _slots[i])
                destruct(b);
        _slots[] = null;
        _slots.length = 0;
        _count = 0;
    }

    /**
     * Support for appending element(s). Forwards $(D insert()).
     */
    auto opOpAssign(string op : "~" , KK...)(auto ref KK keys)
    {
        return insert(keys);
    }

    /**
     * Reserves slots for at least N supplemental keys.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     * Params:
     *      value = The count of additional slots to reserve.
     */
    void reserve()(size_t value) @nogc
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count + value);
        const size_t ol = _slots.length;

        if (nl > _slots.length)
        {
            _slots.length = nl;
            _slots[ol..nl] = null;
            reHash();
        }
    }

    /**
     * Minimizes the memory usage.
     *
     * Should be used after removal and only if $(D count() < slotCount()).
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     */
    void minimize()()
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count-1);

        if (nl < _slots.length)
        {
            Array!(K) old;
            foreach (immutable i; 0.._slots.length)
                if (auto b = _slots[i])
                    old ~= b._key;
            clear;
            foreach (immutable i; 0..old.length)
            {
                insert(old[i]);
                static if (is(K==struct))
                    destruct(old[i]);
            }
            destruct(old);
        }
    }

    /**
     * Tests the presence of a key in the set.
     *
     * Params:
     *      key = The key to test.
     * Returns:
     *      $(D null) if the key is not present otherwise a pointer to the key.
     */
    K* opBinaryRight(string op : "in", KK)(auto ref KK key)
    {
        K* result;
        Slot.FindResult fr = find(key);
        if (fr.slot)
            result = &fr.slot._key;
        return result;
    }

    /**
     * Provides an access to the keys.
     *
     * Params:
     *      index = The slot index. Must be in the $(D 0..slotCount()) range.
     * Returns:
     *      A pointer the nth key.
     */
    K* slot(const size_t index)
    {
        return &_slots[index]._key;
    }

    /**
     * Returns an input range that consists of each non-null key.
     */
    auto byKey()
    {
        return RangeForLpSet!HashSetT(&this);
    }

    /**
     * Returns the keys count.
     *
     * This matches to $(D byKey.walkLength).
     */
    size_t count() @nogc {return _count;}

    /**
     * Returns the slots count.
     */
    size_t slotCount() @nogc {return _slots.length;}
}
///
@nogc unittest
{
    HashSet_LP!string commands;
    // can insert up to 16 elements without reallocation
    commands.reserve(15);
    assert(commands.slotCount == 16);
    // appends elements
    commands.insert("move");
    commands.insert("insert");
    commands.insert("delete");
    // test for inclusion
    assert("move" in commands);
    // remove something
    commands.remove("insert");
    assert(commands.count == 2);
    assert("insert" !in commands);
    // empties and frees memory
    commands.clear;
    // manually managed implies to destruct by hand
    import iz.memory;
    destruct(commands);
}

@nogc unittest
{
    HashSet_LP!string hss;

    hss.reserve(7);
    assert(hss.slotCount == 8);

    foreach(i; 0 .. hss._slots.length)
        assert(hss._slots[i] is null);

    assert(hss.insert("cat"));
    assert("dog" !in hss);
    assert("cat" in hss);

    assert(hss.insert("rat"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);

    assert(hss.insert("fly"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);

    assert(hss.insert("bee"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);

    assert(hss.insert("ant"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" !in hss);

    assert(hss.insert("fox"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" in hss);

    assert(hss.insert("bat"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" in hss);
    assert("bat" in hss);

    assert(hss.insert("cow"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" in hss);
    assert("bat" in hss);
    assert("cow" in hss);

    assert(hss.remove("bat"));
    assert("bat" !in hss);

    hss.clear;
    assert(hss.count == 0);
    assert(hss.slotCount == 0);

    hss.reserve(8);
    import std.algorithm: among;
    assert(hss.insert("cow"));
    assert(hss.insert("yak"));
    auto r = hss.byKey;
    assert(!r.empty);
    assert((r.front).among("cow","yak"));
    r.popFront;
    assert(!r.empty);
    assert((r.front).among("cow","yak"));
    r.popFront;
    assert(r.empty);
    assert(hss.slotCount == 16);

    assert(hss.count == 2);
    hss.minimize;
    assert(hss.slotCount < 16);

    destruct(hss);
}

@nogc unittest
{
    HashSet_LP!string co;
    assert(co.count == 0);
    co.insert("abcd");
    assert(co.count == 1);
    co.insert("abcd");
    assert(co.count == 1);
    co.insert("abcd");
    assert(co.count == 1);
    co.insert("abcde");
    assert(co.count == 2);
    co.insert("abcde");
    assert(co.count == 2);
    co.insert("abcde");
    assert(co.count == 2);
    co.insert("abcde");
    assert(co.count == 2);
    destruct(co);
}

@nogc unittest
{
    {
        HashSet_LP!string co = HashSet_LP!string("ab", "ab", "cd");
        assert(co.count == 2);
        destruct(co);
    }
    {
        static a = ["ab", "cd"];
        HashSet_LP!string co = HashSet_LP!string("ab", a);
        assert(co.count == 2);
        destruct(co);
    }
    {
        HashSet_LP!int co = HashSet_LP!int(1);
        assert(co.count == 1);
        assert(2 !in co);
        assert(1 in co);
        destruct(co);
    }
}

/**
 * A manually managed hashmap that uses linear probing to solve the collisions.
 *
 * Params:
 *      K = The key type.
 *      V = The value type
 *      hasherFun = The hashing function, a $(D size_t(K value);) function,
 *          literal delegate or lambda.
 */
struct HashMap_LP(K, V, alias hasherFun = fnv1)
{
    static assert (is(typeof( (){size_t r = hasherFun(K.init);}  )),
        "invalid hash function");
    static assert (!is(K == void),
        "invalid Key type");
    static assert (!is(V == void),
        "invalid Value type");

private:

    alias MapT = typeof(this);
    alias Slot = LinearProbeSlot!(K,V);
    @NoGc Array!(Slot*) _slots;
    size_t _count;

    pragma(inline, true)
    size_t hasher(KK)(auto ref KK key) @nogc
    {
        return hasherFun(key) & (_slots.length - 1);
    }

    void reHash()()
    {
        _count = 0;
        auto old = _slots.dup;
        assert(old == _slots);
        _slots[] = null;
        foreach (immutable i; 0..old.length)
        {
            if (auto b = old[i])
            {
                insert(b._key, b._value);
                destruct(b);
            }
        }
        destruct(old);
    }

    pragma(inline, true)
    size_t nextHash(size_t value) @nogc @safe
    {
        return (value + 1) & (_slots.length - 1);
    }

    //pragma(inline, true)
    Slot.FindResult find(KK)(auto ref KK key)
    {
        Slot.FindResult fr;
        const size_t hb = hasher(key);
        fr.endHash = hb;

        if (_slots.length)
        {
            fr.slot = _slots[hb];
            while (true)
            {
                if (fr.slot && *fr.slot != key)
                {
                    fr.endHash = nextHash(fr.endHash);
                    if (fr.endHash == hb)
                        break;
                    fr.slot = _slots[fr.endHash];
                }
                else break;
            }
        }
        return fr;
    }

public:

    alias MapPair = Tuple!(K, V);

    ~this() @nogc
    {
        foreach(immutable i; 0.._slots.length)
            if (Slot* slt = _slots[i])
                destruct(slt);
        destruct(_slots);
    }

    this(this) @nogc
    {
        foreach (immutable i; 0.._slots.length)
            if (Slot* slt = _slots[i])
        {
            Slot* n = construct!Slot(slt._key, slt._value);
            n.__postblit();
            _slots[i] = n;
        }
    }

    /**
     * Tries to insert a key-value pair in the map.
     *
     * Params:
     *      mode = If set to $(D imExpand) then reserves a slot else if set to
     *      $(D imReserved) assumes a previous call to $(D reserve()).
     *      key = The key.
     *      value = The corresponding value.
     * Throws:
     *      An $(D OutOfMemoryError) if an internal call to $(D reserve()) fails.
     * Returns:
     *      If the key is added or if it's already included then returns $(D true),
     *      otherwise $(D false).
     */
    bool insert(alias mode = imExpand)(ref K key, auto ref V value) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        bool result;
        static if (mode)
            reserve(1);
        Slot.FindResult fr = find(key);
        if (fr.slot is null || *fr.slot != key)
        {
            assert(key !in this);
            Slot* n = construct!Slot(key, value);
            if (fr.slot is null)
            {
                assert(key !in this);
                _slots[fr.endHash] = n;
                result = true;
                ++_count;
            }
            else destruct(n);
        }
        else if (*fr.slot == key)
        {
            result = true;
            destruct(fr.slot);
            Slot* n = construct!Slot(key, value);
            _slots[fr.endHash] = n;
        }
        return result;
    }

    /// ditto
    bool insert(alias mode = imExpand)(K key, V value) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        return insert!mode(key, value);
    }

    /**
     * Tries to remove a key from the set.
     *
     * Returns:
     *      $(D true) if the key was included otherwise $(D false).
     */
    bool remove()(auto ref K key)
    {
        bool result;
        Slot.FindResult fr = find(key);
        if (fr.slot)
        {
            result = true;
            destruct(fr.slot);
            _slots[fr.endHash] = null;
            reHash;
        }
        return result;
    }

    /**
     * Clears and empties the set.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     */
    void clear() @nogc
    {
        foreach (immutable i; 0.._slots.length)
            if (auto b = _slots[i])
                destruct(b);
        _slots[] = null;
        _slots.length = 0;
        _count = 0;
    }

    /**
     * Support for appending an element.
     *
     * Forwards $(D insert()) with a default initialized value.
     *
     * Params:
     *      key = The key to insert.
     * Returns:
     *      If the key is added or if it's already included then returns $(D true),
     *      otherwise $(D false).
     */
    bool opOpAssign(string op : "~")(auto ref K key)
    {
        return insert(key, V.init);
    }

    /**
     * Support for inserting using the array syntax. Forwards $(D insert()).
     */
    void opIndexAssign(KK)(auto ref V value, auto ref KK key)
    {
        insert(key, value);
    }

    /**
     * Support for assigning to the value when the value is itself an AA.
     *
     * Note that this function only exists to port code that uses the runtime AA
     * with the syntax $(D aa[k_for_value][k_for_value_of_value] = stuff;).
     */
    void opIndexAssign(V2, KK1, KK2)(auto ref V2 value, auto ref KK1 key1, auto ref KK2 key2)
    {
        if (auto v = key1 in this)
        {
            v.insert(key2, value);
        }
        else
        {
            V v;
            v.insert(key2, value);
            insert(key1, v);
        }
    }

    /**
     * Support for the assignment operators on a value.
     */
    void opIndexOpAssign(string op, VV, KK)(auto ref VV value, auto ref KK key)
    {
        if (auto p = key in this)
        {
            mixin("(*p)" ~ op ~ "= value;");
        }
        else
        {
            V v;
            mixin("v" ~ op ~ "= value;");
            insert(key, v);
        }
    }

    /**
     * Returns an input range consisting of each key.
     */
    auto byKey() return @nogc
    {
        return RangeForLpSet!(MapT,rngMapByKey)(&this);
    }

    /**
     * Returns an input range consisting of each value.
     */
    auto byValue() return @nogc
    {
        return RangeForLpSet!(MapT,rngMapByValue)(&this);
    }

    /**
     * Returns an input range consisting of each non-null key-value pair.
     */
    auto byKeyValue() return @nogc
    {
        return RangeForLpSet!MapT(&this);
    }

    /**
     * Reserves slots for at least N supplemental key-value pairs.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     * Params:
     *      value = The count of additional slots to reserve.
     */
    void reserve()(size_t value) @nogc
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count + value);
        const size_t ol = _slots.length;

        if (nl > _slots.length)
        {
            _slots.length = nl;
            _slots[ol..nl] = null;
            reHash();
        }
    }

    /**
     * Minimizes the memory usage.
     *
     * Should be used after removal and only if $(D count() < slotCount()).
     */
    void minimize()()
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count-1);

        if (nl < _slots.length)
        {
            Array!(MapPair) old;
            foreach (immutable i; 0.._slots.length)
                if (auto b = _slots[i])
                    old ~= tuple(b._key, b._value);
            clear;
            foreach (immutable i; 0..old.length)
            {
                insert(old[i][0..2]);
                static if (is(K == struct))
                    destruct(old[i]);
            }
            destruct(old);
        }
    }

    /**
     * Tests the presence of a key in the set.
     *
     * Params:
     *      key = The key to test.
     * Returns:
     *      $(D null) if the key is not present otherwise a pointer to the
     *          value that mapped.
     */
    V* opBinaryRight(string op : "in", KK)(auto ref KK key)
    {
        V* result;
        Slot.FindResult fr = find(key);
        if (fr.slot)
            result = &fr.slot._value;
        return result;
    }

    /**
     * Provides an access to the key-value pairs.
     *
     * Params
     *      index = The pair index. Must be in the $(D 0..slotCount()) range.
     * Returns:
     *      A pointer to a tuple that contains, when not null, the
     *      key and the value of the element pointed by $(D_PARAM index).
     */
    MapPair* slot(const size_t index) @nogc
    {
        if (auto r = _slots[index])
            return r.ptr;
        else
            return null;
    }

    /// ditto
    auto ref V opIndex(KK)(auto ref KK key)
    if (!is(KK == size_t))
    {
        return *(key in this);
    }

    /**
     * Returns the key-value pairs count.
     *
     * This matches to $(D byKeyValue.walkLength).
     */
    size_t count() @nogc {return _count;}

    /**
     * Returns the slots counts.
     */
    size_t slotCount() @nogc {return _slots.length;}
}
///
@nogc unittest
{
    HashMap_LP!(string, size_t) stock;
    // can insert up to 16 elements without reallocation
    stock.reserve(15);
    assert(stock.slotCount == 16);
    // appends elements, various syntax allowed
    stock.insert("pen", 8);
    stock["gum"] += 32;
    stock["ruler"] += 12;
    stock ~= "tape roll";
    // test for inclusion, various syntax allowed
    assert("gum" in stock);
    assert(stock["ruler"] == 12);
    assert(stock["tape roll"] == 0);
    // remove something
    stock.remove("ruler");
    assert("ruler" !in stock);
    // empties and frees memory
    stock.clear;
    // manually managed implies to destruct by hand
    import iz.memory;
    destruct(stock);
}

@nogc unittest
{
    HashMap_LP!(string, int) hmsi;
    hmsi.reserve(15);

    hmsi.insert("cat", 0);
    hmsi.insert("dog", 1);
    hmsi.insert("pig", 2);
    assert(hmsi.count == 3);
    assert(*("cat" in hmsi) == 0);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert("bat" !in hmsi);
    assert("bug" !in hmsi);

    hmsi["bat"] = 3;
    hmsi["bug"] = 4;
    assert(hmsi.count == 5);
    assert(*("cat" in hmsi) == 0);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert(*("bat" in hmsi) == 3);
    assert(*("bug" in hmsi) == 4);

    assert(hmsi.remove("cat"));
    assert(hmsi.count == 4);
    assert("cat" !in hmsi);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert(*("bat" in hmsi) == 3);
    assert(*("bug" in hmsi) == 4);

    hmsi["owl"] = 5;
    assert(hmsi.count == 5);
    assert("cat" !in hmsi);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert(*("bat" in hmsi) == 3);
    assert(*("bug" in hmsi) == 4);
    assert(*("owl" in hmsi) == 5);

    hmsi.clear;
    assert(hmsi.count == 0);
    assert("cat" !in hmsi);
    assert("dog" !in hmsi);
    assert("pig" !in hmsi);
    assert("bat" !in hmsi);
    assert("bug" !in hmsi);
    assert("owl" !in hmsi);

    hmsi["bat"] = 3;
    hmsi["bug"] = 4;
    assert(hmsi.count == 2);
    assert(hmsi["bat"] == 3);
    assert(hmsi["bug"] == 4);
    hmsi["bat"] = 4;
    hmsi["bug"] = 3;
    assert(hmsi.count == 2);
    assert(hmsi["bat"] == 4);
    assert(hmsi["bug"] == 3);

    destruct(hmsi);
}

@nogc unittest
{
    HashMap_LP!(int, int) hmii;
    hmii.reserve(32);
    hmii[1] = 3;
    hmii[2] = 4;
    foreach(kv; hmii.byKeyValue)
    {
        assert(kv[0] == 1 || kv[0] == 2);
        assert(kv[1] == 3 || kv[1] == 4);
    }
    destruct(hmii);
}

/**
 * The bucket used in HashSet_AB and HashMap_AB.
 */
struct ArrayBucket(K, V = void)
{

private:

    static if (!isMap)
    {
        alias ArrayT = Array!K;
    }
    else
    {
        alias Pair = Tuple!(K, V);
        alias ArrayT = Array!(Pair);
    }

    ArrayT _array;

public:

    enum isMap = !is(V == void);

    ~this() @nogc
    {
        static if (isMap)
        {
            foreach (immutable i; 0.._array.length)
            {
                static if (is(K == struct))
                    destruct(_array[i][0]);
                static if (is(V == struct))
                    destruct(_array[i][1]);
            }
        }
        destruct(_array);
    }

    this(this) @nogc
    {
        _array.__postblit;
    }

    ref const(ArrayT) array() const @nogc nothrow
    {return _array;}

    static if (isMap)
    void insert(ref K key, ref V value) @nogc nothrow
    {
        _array ~= tuple(key, value);
    }
    else
    void insert(ref K key) @nogc nothrow
    {
        _array ~= key;
    }

    bool remove()(ref K key)
    {
        bool result;
        foreach (immutable i; 0.._array._length)
        {
            static if (!isMap)
            {
                static if (hasElaborateSelfEquals!K)
                {
                    if (!_array[i].opEquals(key))
                        continue;
                }
                else
                {
                    if (_array[i] != key)
                        continue;
                }
            }
            else
            {
                static if (hasElaborateSelfEquals!K)
                {
                    if (!_array[i][0].opEquals(key))
                        continue;
                }
                else
                {
                    if (_array[i][0] != key)
                        continue;
                }
            }
            result = true;
            if (i == 0)
            {
                if (_array.length > 1)
                    _array = _array[1..$];
                else
                    _array.length = 0;
            }
            else if (i == _array.length-1)
            {
                _array.length = _array.length - 1;
            }
            else _array = _array[0..i] ~ _array[i+1..$];
            break;
        }
        return result;
    }

    pragma(inline, true)
    void clear() @nogc nothrow
    {
        _array.length = 0;
    }

    ptrdiff_t indexOfKey()(ref K key)
    {
        ptrdiff_t result = -1;
        foreach (immutable i; 0.._array.length)
        {
            static if (isMap)
            {
                static if (hasElaborateSelfEquals!K)
                {
                    if (_array[i][0].opEquals(key))
                    {
                        result = i;
                        break;
                    }
                }
                else
                {
                    if (_array[i][0] == key)
                    {
                        result = i;
                        break;
                    }
                }
            }
            else
            {
                static if (hasElaborateSelfEquals!K)
                {
                    if (_array[i].opEquals(key))
                    {
                        result = i;
                        break;
                    }
                }
                else
                {
                    if (_array[i] == key)
                    {
                        result = i;
                        break;
                    }
                }
            }
        }
        return result;
    }

    pragma(inline, true)
    K* getKey(KK)(ref KK key)
    {
        K* result;
        if (const size_t j =  _array.length)
            foreach (immutable i; 0..j)
        {
            static if (isMap)
            {
                static if (hasElaborateSelfEquals!K)
                {
                    if (_array[i][0].opEquals(key))
                    {
                        result = &_array[i][0];
                        break;
                    }
                }
                else
                {
                    if (_array[i][0] == key)
                    {
                        result = &_array[i][0];
                        break;
                    }
                }
            }
            else
            {
                static if (hasElaborateSelfEquals!K)
                {
                    if (_array[i].opEquals(key))
                    {
                        result = &_array[i];
                        break;
                    }
                }
                else
                {
                    if (_array[i] == key)
                    {
                        result = &_array[i];
                        break;
                    }
                }
            }
        }
        return result;
    }

    static if (isMap)
    V* getValue(KK)(ref KK key)
    {
        V* result;
        if (const size_t j =  _array.length)
            foreach (immutable i; 0..j)
        {
            static if (hasElaborateSelfEquals!K)
            {
                if (_array[i][0].opEquals(key))
                {
                    result = &_array[i][1];
                        break;
                }
            }
            else
            {
                if (_array[i][0] == key)
                {
                    result = &_array[i][1];
                        break;
                }
            }
        }
        return result;
    }

    static if (isMap)
    ptrdiff_t indexOfValue()(ref V value)
    {
        ptrdiff_t result = -1;
        foreach(immutable i; 0.._array.length)
        {
            if (_array[i][1] == value)
            {
                result = i;
                break;
            }
        }
        return result;
    }

    size_t length() @nogc const pure nothrow {return _array.length;}
}

private struct RangeForAbSet(T, alias rngKind = rngNoMap)
{

private:

    T* _hashSetOrMap;
    alias B = ReturnType!(T.bucket);
    B _currBucket;
    bool _empty;
    size_t _bucketIndex;
    size_t _keyIndex;

public:

    this(T* hashSetOrMap) @nogc nothrow
    {
        assert(hashSetOrMap);
        _hashSetOrMap = hashSetOrMap;
        popFront;
    }

    bool empty() @nogc nothrow
    {
        return _currBucket is null;
    }

    void popFront() @nogc nothrow
    {
        if (_currBucket)
        {
            ++_keyIndex;
            if (_keyIndex >= _currBucket.length)
            {
                _currBucket = null;
                _keyIndex = 0;
                ++_bucketIndex;
            }
        }
        if (!_currBucket)
        {
            while (_bucketIndex < _hashSetOrMap._buckets.length)
            {
                _currBucket = (*_hashSetOrMap).bucket(_bucketIndex);
                if (_currBucket.length)
                    break;
                else
                    ++_bucketIndex;
            }
            if (_bucketIndex == _hashSetOrMap._buckets.length)
                _currBucket = null;
        }
    }

    auto ref front() @nogc
    {
        static if (rngKind == rngNoMap || rngKind == rngMapByKeyValue)
            return _currBucket._array[_keyIndex];
        else static if (rngKind == rngMapByKey)
            return _currBucket._array[_keyIndex][0];
        else static if (rngKind == rngMapByValue)
            return _currBucket._array[_keyIndex][1];
    }
}

/**
 * A manually managed hashset that uses buckets to solve the collisions.
 *
 * Params:
 *      K = the key type.
 *      hasherFun = The hashing function, a $(D size_t(K value);) function,
 *          literal delegate or lambda.
 */
struct HashSet_AB(K, alias hasherFun = fnv1)
{
    static assert (is(typeof( (){size_t r = hasherFun(K.init);}  )),
        "invalid hash function");
    static assert (!is(K == void),
        "invalid Key type");

private:

    alias HashSetT = typeof(this);
    alias BucketT = ArrayBucket!K;
    @NoGc Array!BucketT _buckets;
    size_t _count;

    pragma(inline, true)
    size_t hasher(KK)(auto ref KK key) @nogc
    {
        return hasherFun(key) & (_buckets.length - 1);
    }

    void reHash()()
    {
        _count = 0;
        Array!BucketT old = _buckets;
        foreach (immutable i; 0.._buckets.length)
            _buckets[i].clear;
        foreach (immutable i; 0..old.length)
        {
            foreach (immutable j; 0..old[i]._array.length)
                insert!false(old[i]._array[j]);
        }
        destruct(old);
    }

public:

    /**
     * Constructs using either a list of keys, arrays of keys, or both.
     */
    this(A...)(A a)
    {
        import std.meta: aliasSeqOf;
        import std.range: iota;

        foreach (i; aliasSeqOf!(iota(0, A.length)))
        {
            static if (is(A[i] == K))
                continue;
            else static if (isArray!(A[i]))
                continue;
            else static if (is(A[i] == Array!K))
                continue;
            else static assert(0, A[i].stringof ~ " not supported");
        }
        foreach (i; aliasSeqOf!(iota(0, A.length)))
                insert(a[i]);
    }

    ~this() @nogc
    {
        destruct(_buckets);
    }

    /**
     * Tries to insert key(s) in the set.
     *
     * Params:
     *      mode = If true (imExpand) then reserves a slot else (imReserved)
     *          assumes a previous call to $(D reserve()).
     *      key = either single keys, array of keys, or both.
     * Throws:
     *      An $(D OutOfMemoryError) if an internal call to $(D reserve()) fails.
     * Returns:
     *      If the key is added or if it's already included then returns $(D true),
     *      otherwise $(D false).
     */
    bool insert(alias mode = true)(ref K key) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        bool result;
        if (!_buckets.length || key !in this)
        {
            result = true;
            static if (mode)
                reserve(1);
            const size_t h = hasher(key);
            assert(h < _buckets.length);
            _buckets[h].insert(key);
            ++_count;
        }
        return result;
    }

    /// ditto
    bool insert(alias mode = true)(K key) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        return insert!mode(key);
    }

    /// ditto
    bool insert(KK)(auto ref KK keys)
    if ((isArray!KK && is(Unqual!(ElementEncodingType!KK) == K)) || is(KK == Array!K))
    {
        bool result = true;
        reserve(keys.length);
        foreach(k; keys)
            result &= insert!false(k);
        return result;
    }

    /// ditto
    bool insert(KK...)(auto ref KK keys)
    {
        bool result = true;
        reserve(KK.length);
        foreach(k; keys)
            result &= insert!false(k);
        return result;
    }

    /**
     * Tries to remove a key from the set.
     *
     * Returns:
     *      $(D true) if the key was included otherwise $(D false).
     */
    bool remove()(auto ref K key)
    {
        const size_t h = hasher(key);
        const bool result = _buckets[h].remove(key);
        if (result)
            --_count;
        return result;
    }

    /**
     * Reserves buckets for at least N supplemental keys.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     * Params:
     *      value = The count of additional slots to reserve.
     */
    void reserve()(size_t value)
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count + value);
        if (nl > _buckets.length)
        {
            _buckets.length = nl;
            reHash();
        }
    }

    /**
     * Minimizes the memory used by the set.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     */
    void minimize()()
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count-1);

        if (nl < _buckets.length)
        {
            Array!BucketT old = _buckets;
            clear;
             _buckets.length = nl;
            foreach (immutable i; 0..old.length)
                foreach (immutable j; 0..old[i]._array.length)
            {
                insert!false(old[i]._array[j]);
            }
            destruct(old);
        }
    }

    /**
     * Empties the set.
     */
    void clear() @nogc
    {
        _buckets.length = 0;
        _buckets.length = 2;
        _count = 0;
    }

    /**
     * Tests the presence of a key in the set.
     *
     * Params:
     *      key = The key to test.
     * Returns:
     *      $(D null) if the key is not present otherwise a pointer to the key.
     */
    pragma(inline, true)
    K* opBinaryRight(string op : "in", KK)(auto ref KK key)
    {
        return _buckets[hasher(key)].getKey(key);
    }

    /**
     * Provides an access to the buckets.
     *
     * Params:
     *      index = The bucket index. Must be in the $(D 0..bucketCount()) range.
     * Returns:
     *      A never null, pointer to a bucket.
     */
    BucketT* bucket(const size_t index) pure nothrow @nogc
    {
        return &_buckets[index];
    }

    /**
     * Returns: an input range that allows to iterate the keys.
     */
    auto byKey()
    {
        return RangeForAbSet!HashSetT(&this);
    }

    /**
     * Returns: the elements count.
     */
    size_t count() pure nothrow @nogc {return _count;}

    /**
     * Returns: the buckets count.
     */
    size_t bucketCount() pure nothrow @nogc {return _buckets.length;}

    /**
     * Returns: the collisions count.
     */
    size_t collisions() pure nothrow @nogc
    {
        size_t result;
        foreach(immutable i; 0.._buckets.length)
            result += _buckets[i]._array.length > 1 ?
                _buckets[i]._array.length - 1 : 0;
        return result;
    }
}
///
@nogc unittest
{
    HashSet_AB!string commands;
    // can insert up to 16 elements without reallocation
    commands.reserve(15);
    assert(commands.bucketCount == 16);
    // appends elements
    commands.insert("move");
    commands.insert("insert");
    commands.insert("delete");
    // test for inclusion
    assert("move" in commands);
    // remove something
    commands.remove("insert");
    assert(commands.count == 2);
    assert("insert" !in commands);
    // empties and frees memory
    commands.clear;
    // manually managed implies to destruct by hand
    import iz.memory;
    destruct(commands);
}

@nogc unittest
{
    HashSet_AB!string hss;

    assert(hss.insert("cat"));
    assert(hss.bucketCount == 2);
    assert("dog" !in hss);
    assert("cat" in hss);

    assert(hss.insert("rat"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);

    assert(hss.insert("fly"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);

    assert(hss.insert("bee"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);

    assert(hss.insert("ant"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" !in hss);

    assert(hss.insert("fox"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" in hss);

    assert(hss.insert("bat"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" in hss);
    assert("bat" in hss);

    assert(hss.insert("cow"));
    assert("dog" !in hss);
    assert("cat" in hss);
    assert("rat" in hss);
    assert("fly" in hss);
    assert("bee" in hss);
    assert("ant" in hss);
    assert("fox" in hss);
    assert("bat" in hss);
    assert("cow" in hss);

    assert(hss.remove("bat"));
    assert("bat" !in hss);

    hss.clear;
    assert(hss.count == 0);

    import std.algorithm: among;
    hss.reserve(20);
    assert(hss.insert("cow"));
    assert(hss.insert("yak"));
    auto r = hss.byKey;
    assert(!r.empty);
    assert((r.front).among("cow","yak"));
    r.popFront;
    assert(!r.empty);
    assert((r.front).among("cow","yak"));
    r.popFront;
    assert(r.empty);

    assert(hss.bucketCount == 32);
    hss.minimize;
    assert(hss.bucketCount == 2);

    destruct(hss);
}

@nogc unittest
{
    HashSet_AB!int hsi;
    assert(hsi.byKey.empty);
    destruct(hsi);
}

/**
 * A manually managed hashmap that uses buckets to solve the collisions.
 *
 * Params:
 *      K = the key type.
 *      V = the value type.
 *      hasherFun = The hashing function, a $(D size_t(K value);) function,
 *          literal delegate or lambda.
 */
struct HashMap_AB(K, V, alias hasherFun = fnv1)
{
    static assert (is(typeof( (){size_t r = hasherFun(K.init);}  )),
        "invalid hash function");
    static assert (!is(K == void),
        "invalid Key type");
    static assert (!is(V == void),
        "invalid Value type");

private:

    alias HashMapT = typeof(this);
    alias BucketT = ArrayBucket!(K,V);
    @NoGc Array!BucketT _buckets;
    size_t _count;

    pragma(inline, true)
    size_t hasher(KK)(auto ref KK key) @nogc
    {
        return hasherFun(key) & (_buckets.length - 1);
    }

    void reHash()()
    {
        //_count = 0;
        Array!BucketT old = _buckets;
        foreach (immutable i; 0.._buckets.length)
            _buckets[i].clear;
        foreach (immutable i; 0..old.length)
        {
            foreach (immutable j; 0..old[i]._array.length)
            {
                const h = hasher(old[i]._array[j][0]);
                _buckets[h].insert(old[i]._array[j][0], old[i]._array[j][1]);
            }
        }
        destruct(old);
    }

public:

    ~this() @nogc
    {
        destruct(_buckets);
    }

    /**
     * Tries to insert a key-value pair in the set.
     *
     * Params:
     *      mode = If set to $(D imExpand) then reserves a slot else
     *          if set to $(D imReserved) assumes a previous call to $(D reserve()).
     *      key = The key.
     *      value = The value.
     * Throws:
     *      An $(D OutOfMemoryError) if an internal call to $(D reserve()) fails.
     * Returns:
     *      If the key is added or if it's already included then returns $(D true),
     *      otherwise $(D false).
     */
    bool insert(alias mode = imExpand)(ref K key, auto ref V value) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        bool result;
        if (!_buckets.length)
            reserve(1);
        if (key !in this)
        {
            result = true;
            static if (mode)
                reserve(1);
            const size_t h = hasher(key);
            assert(h < _buckets.length);
            _buckets[h].insert(key, value);
            ++_count;
        }
        else
        {
            result = true;
            const size_t h = hasher(key);
            assert(h < _buckets.length);
            _buckets[h].remove(key);
            _buckets[h].insert(key, value);
        }
        return result;
    }

    /// ditto
    bool insert(alias mode = imExpand)(K key, V value) @nogc
    if (isImplicitlyConvertible!(typeof(mode), bool))
    {
        return insert!mode(key, value);
    }

    /**
     * Tries to remove a key from the set.
     *
     * Returns:
     *      $(D true) if the key was included otherwise $(D false).
     */
    bool remove()(auto ref K key) @nogc
    {
        const size_t h = hasher(key);
        const bool result = _buckets[h].remove(key);
        if (result)
            --_count;
        return result;
    }

    /**
     * Reserves buckets for at least N supplemental key-value pairs.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     * Params:
     *      value = The count of additional slots to reserve.
     */
    void reserve()(size_t value) @nogc
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count + value);
        if (nl > _buckets.length)
        {
            _buckets.length = nl;
            reHash();
        }
    }

    /**
     * Minimizes the memory used by the map.
     *
     * Throws:
     *      An $(D OutOfMemoryError) if the reallocation fails.
     */
    void minimize()()
    {
        import std.math: nextPow2;
        const size_t nl = nextPow2(_count-1);

        if (nl < _buckets.length)
        {
            Array!BucketT old = _buckets;
            clear;
             _buckets.length = nl;
            foreach (immutable i; 0..old.length)
                foreach (immutable j; 0..old[i]._array.length)
            {
                insert!false(old[i]._array[j][0], old[i]._array[j][1]);
            }
            destruct(old);
        }
    }

    /**
     * Empties the map.
     */
    void clear() @nogc
    {
        _buckets.length = 0;
        _buckets.length = 2;
        _count = 0;
    }

    /**
     * Retrieves the value associated to a key.
     *
     * Params:
     *      key = The key.
     * Returns:
     *      $(D null) if the key is not present otherwise a pointer to associated value.
     */
    V* opBinaryRight(string op : "in", KK)(auto ref KK key)
    {
        V* result;
        if (_buckets.length)
            result = _buckets[hasher(key)].getValue(key);
        return result;
    }

    /**
     * Support for appending an element.
     *
     * Forwards $(D insert()) with a default initialized value.
     *
     * Params:
     *      key = The key to insert.
     * Returns:
     *      If the key is added or if it's already included then returns $(D true),
     *      otherwise $(D false).
     */
    bool opOpAssign(string op : "~")(auto ref K key)
    {
        return insert(key, V.init);
    }

    /**
     * Provides an access to the buckets.
     *
     * Params:
     *      index = The bucket index. Must be in the $(D 0..bucketCount()) range.
     * Returns:
     *      A never null pointer to a bucket.
     */
    BucketT* bucket(const size_t index) pure nothrow @nogc
    {
        return &_buckets[index];
    }

    /**
     * Support for retrieving a value using the array syntax.
     */
    auto ref V opIndex(KK)(auto ref KK key)
    {
        import std.stdio;
        return *(key in this);
    }

    /**
     * Support for assigning using the array syntax.
     */
    void opIndexAssign(KK)(auto ref V value, auto ref KK key)
    {
        insert(key, value);
    }

    /**
     * Support for assigning to the value when the value is itself an AA.
     *
     * Note that this function only exists to port code that uses the runtime AA
     * with the syntax $(D aa[k_for_value][k_for_value_of_value] = stuff;).
     */
    void opIndexAssign(V2, KK1, KK2)(auto ref V2 value, auto ref KK1 key1, auto ref KK2 key2)
    {
        if (auto v = key1 in this)
        {
            v.insert(key2, value);
        }
        else
        {
            V v; v.reserve(1);
            v.insert(key2, value);
            insert!imReserved(key1, v);
        }
    }

    /**
     * Support for the assignment operators on a value.
     */
    void opIndexOpAssign(string op, VV, KK)(auto ref VV value, auto ref KK key)
    {
        if (auto p = key in this)
        {
            mixin("(*p) " ~ op ~ "= value;");
        }
        else
        {
            V v;
            mixin("v" ~ op ~ "= value;");
            insert(key, v);
        }
    }

    /**
     * Returns: an input range that allows to iterate the key-value pairs.
     */
    auto byKeyValue()
    {
        return RangeForAbSet!HashMapT(&this);
    }

    /**
     * Returns: an input range that allows to iterate the keys.
     */
    auto byKey()
    {
        return RangeForAbSet!(HashMapT,rngMapByKey)(&this);
    }

    /**
     * Returns: an input range that allows to iterate the values.
     */
    auto byValue()
    {
        return RangeForAbSet!(HashMapT,rngMapByValue)(&this);
    }

    /**
     * Returns: the elements count.
     */
    size_t count() pure nothrow @nogc {return _count;}

    /**
     * Returns the buckets count.
     */
    size_t bucketCount() pure nothrow @nogc {return _buckets.length;}

    /**
     * Returns: the collisions count.
     */
    size_t collisions() pure nothrow @nogc
    {
        size_t result;
        foreach(immutable i; 0.._buckets.length)
            result += _buckets[i]._array.length > 1 ?
                _buckets[i]._array.length - 1 : 0;
        return result;
    }
}
///
@nogc unittest
{
    HashMap_AB!(string, size_t) stock;
    // can insert up to 16 elements without reallocation
    stock.reserve(15);
    assert(stock.bucketCount == 16);
    // appends elements, various syntax allowed
    stock.insert("pen", 8);
    stock["gum"] += 32;
    stock["ruler"] += 12;
    stock ~= "tape roll";
    // test for inclusion, various syntax allowed
    assert("gum" in stock);
    assert(stock["ruler"] == 12);
    assert(stock["tape roll"] == 0);
    // remove something
    stock.remove("ruler");
    assert("ruler" !in stock);
    // empties and frees memory
    stock.clear;
    // manually managed implies to destruct by hand
    import iz.memory;
    destruct(stock);
}

@nogc unittest
{
    HashMap_AB!(string, int) hmsi;

    hmsi.insert("cat", 0);
    hmsi.insert("dog", 1);
    hmsi.insert("pig", 2);
    assert(hmsi.count == 3);
    assert(*("cat" in hmsi) == 0);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert("bat" !in hmsi);
    assert("bug" !in hmsi);

    hmsi["bat"] = 3;
    hmsi["bug"] = 4;
    assert(hmsi.count == 5);
    assert(*("cat" in hmsi) == 0);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert(*("bat" in hmsi) == 3);
    assert(*("bug" in hmsi) == 4);

    assert(hmsi.remove("cat"));
    assert(hmsi.count == 4);
    assert("cat" !in hmsi);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert(*("bat" in hmsi) == 3);
    assert(*("bug" in hmsi) == 4);

    hmsi["owl"] = 5;
    assert(hmsi.count == 5);
    assert("cat" !in hmsi);
    assert(*("dog" in hmsi) == 1);
    assert(*("pig" in hmsi) == 2);
    assert(*("bat" in hmsi) == 3);
    assert(*("bug" in hmsi) == 4);
    assert(*("owl" in hmsi) == 5);

    hmsi.clear;
    assert(hmsi.count == 0);
    assert("cat" !in hmsi);
    assert("dog" !in hmsi);
    assert("pig" !in hmsi);
    assert("bat" !in hmsi);
    assert("bug" !in hmsi);
    assert("owl" !in hmsi);

    hmsi["bat"] = 3;
    hmsi["bug"] = 4;
    assert(hmsi.count == 2);
    assert(hmsi["bat"] == 3);
    assert(hmsi["bug"] == 4);
    hmsi["bat"] = 4;
    hmsi["bug"] = 3;
    assert(hmsi.count == 2);
    assert(hmsi["bat"] == 4);
    assert(hmsi["bug"] == 3);

    destruct(hmsi);
}

@nogc unittest
{
    HashMap_AB!(int, int) aa;
    assert(1 !in aa);
}

@nogc unittest
{
    HashMap_AB!(int, int) aa;
    aa[0] = 0;
}

@nogc unittest
{
    HashMap_AB!(string, void*) aa0;
    aa0[""] = null;
    HashMap_AB!(void*, string) aa1;
    //aa1[null] = "";
}

unittest
{
    HashSet_AB!(const(char)[]) aa0;
    string s = "s";
    aa0.insert(s);
}

@nogc unittest
{
    struct Foo {}
    HashMap_AB!(Foo,Foo) a1;
    class Bar
    {
        override bool opEquals(Object o) const pure @nogc nothrow {return o is this;}
    }
    {HashMap_AB!(Bar,Bar) a2;}
    {HashMap_LP!(Bar,Bar) a2;}
    {
        HashSet_AB!(Bar) a2;
        Bar b = construct!Bar;
        a2.insert(b);
        a2.remove(b);
        destruct!true(b);
    }
    {
        HashSet_LP!(Bar) a2;
        Bar b = construct!Bar;
        a2.insert(b);
        a2.remove(b);
        destruct!true(b);
    }
}

//TODO-cHashMaps: proper support for post++/--
version (none) unittest
{
    {
        HashMap_LP!(string, size_t) aa;
        aa["0"] += 1;
        assert(aa["0"] == 1);
        aa["0"]++ ;
        assert(aa["0"] == 2);
    }
    {
        //HashMap_AB!(string, size_t) aa;
        //aa["0"]++;
        //assert(aa["0"] == 1);
        //aa["0"]++ ;
        //assert(aa["0"] == 2);
        //aa["1"]++;
        //assert(aa["1"] == 1);
        //aa["1"]-- ;
        //assert(aa["1"] == 0);
    }
    {
        //HashMap_LP!(string, size_t) aa;
        //aa["0"]++;
        //assert(aa["0"] == 1);
        //aa["0"]++;
        //assert(aa["0"] == 2);
        //aa["1"]++;
        //assert(aa["1"] == 1);
        //aa["1"]-- ;
        //assert(aa["1"] == 0);
    }
}

unittest
{
    HashMap_AB!(string, int) aa;
    aa.insert("0",2);
    aa["0"] += 0;
    aa.insert("1",2);
    assert("1" in aa);
    aa["1"] += 0;
    assert(aa.count == 2);
    auto rng = aa.byValue;
    assert(!rng.empty);
    assert(rng.front == 2);
    rng.popFront;
    assert(!rng.empty);
    assert(rng.front == 2);
    rng.popFront;
    assert(rng.empty);
    destruct(aa);
}

unittest
{
    {
        alias AA1 = HashMap_AB!(string, int);
        HashMap_AB!(string, AA1) aa;
        // note: runtime AA syntax is `aa["0"]["0"] = 42;`
        aa["0", "0"] = 42;
        auto aaOf_0 = aa["0"];
        assert(aaOf_0["0"] == 42);
        destruct(aa);
    }
    {
        alias AA1 = HashMap_LP!(string, int);
        HashMap_LP!(string, AA1) aa;
        aa["0", "0"] = 42;
        auto aaOf_0 = aa["0"];
        assert(aaOf_0["0"] == 42);
        destruct(aa);
    }
}

