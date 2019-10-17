/**
 * The iz serialization system.
 */
module iz.serializer;

import
    std.range, std.typetuple, std.conv, std.traits, std.stdio;
import
    iz.memory, iz.containers, iz.strings, iz.rtti, iz.sugar;

public
{
    import iz.types, iz.properties, iz.referencable, iz.streams;
}

// Serializable types validator & misc. ---------------------------------------+

/**
 * Makes a reference serializable.
 *
 * The reference must be stored in the ReferenceMan.
 *
 * A "referenced variable" is typically something that is assigned
 * at the run-time but not owned by the entity that wants to keep track of it,
 * or that is serialized by another entity.
 *
 * Note that this class is not needed to serialize a reference to an object or to
 * a delegate, since those reference types are automatically handled by the serializer.
 */
// class inherited from the old serialization system, not really needed anymore.
class SerializableReference: PropertyPublisher
{

    mixin PropertyPublisherImpl;
    mixin inheritedDtor;

protected:

    ubyte _cnt;
    Array!char _id, _tp;
    void delegate(Object) _onRestored;

    void doSet()
    {
        _cnt++;
        if (_cnt == 2)
        {
            _cnt = 0;
            if (_onRestored)
                _onRestored(this);
        }
    }

@PropHints(PropHint.initCare):

    @Get char[] type()
    {
        return _tp[];
    }

    @Set void type(char[] value)
    {
        _tp = value;
        doSet;
    }

    @Get char[] identifier()
    {
         return _id[];
    }

    @Set void identifier(char[] value)
    {
        _id = value;
        doSet;
    }

public:

    ///
    this() {collectPublications!SerializableReference;}

    ~this()
    {
        destruct(_tp);
        destruct(_id);
    }

    /**
     * Sets the internal fields according to a referenced.
     *
     * Usually called before the serialization.
     */
    void storeReference(RT)(ref RT reference)
    {
        _tp = RT.stringof.dup;
        _id = ReferenceMan.referenceID!RT(reference).dup;
    }

    /**
     * Returns the reference according to the internal fields.
     *
     * Usually called after the deserialization of after the
     * the reference owner is notified by onRestored().
     */
    auto restoreReference(RT)()
    {
        return ReferenceMan.reference!RT(_id);
    }

    /**
     * Defines the event called when the the identifier string and the
     * type string are restored, so that the reference owner can
     * retrieve the matching reference in the ReferenceMan.
     */
    void onRestored(void delegate(Object) value)
    {
        _onRestored = value;
    }
    /// ditto
    void delegate(Object) onRestored()
    {
        return _onRestored;
    }
}

package bool isSerObjectType(T)()
{
    static if (is(T : Stream)) return false;
    else static if (is(T : Object)) return true;
    else return false;
}

package bool isSerObjectType(RtType type)
{
    with(RtType) return type == _object;
}

package bool isSerArrayType(T)()
{
    static if (!isArray!T) return false;
    else static if (isMultiDimensionalArray!T) return false;
    else static if (true)
    {
        alias TT = typeof(T.init[0]);
        static if (isSomeFunction!TT) return false;
        else static if (isNarrowString!TT) return true;
        else static if (!isBasicRtType!TT) return false;
        else return true;
    }
    else return true;
}

/**
 * Only a subset of the type representable as a Rtti are serializable.
 * This template only evaluates to true if it's the case.
 */
bool isSerializable(T)()
{
    static if (isBasicRtType!T) return true;
    else static if (isSerArrayType!T) return true;
    else static if (is(T : Stream)) return true;
    else static if (isSerObjectType!T) return true;
    else static if (is(T==delegate)) return true;
    else static if (is(PointerTarget!T==function)) return true;
    else static if (is(T==GenericEnum)) return true;
    else static if (is(T==GenericStruct)) return true;
    else return false;
}

unittest
{
    struct S{}
    struct V{uint _value; alias _value this;}
    struct VS{V _value; alias _value this;}
    static assert( isSerializable!ubyte );
    static assert( isSerializable!double );
    static assert( isSerializable!(ushort[]) );
    static assert( isSerializable!Object );
    static assert( !isSerializable!S );
    static assert( !isSerializable!VS );
    static assert( isSerializable!MemoryStream);
    static assert( isSerializable!GenericDelegate);
    static assert( !isSerializable!(int[int]) );
}
// -----------------------------------------------------------------------------

// Intermediate Serialization Tree --------------------------------------------+

/// Represents a serializable property without genericity.
struct SerNodeInfo
{
    /// the rtti of the property
    const(Rtti)* rtti;
    /// a pointer to a $(D PropDescriptor)
    Ptr     descriptor;
    /// the raw value
    Array!ubyte value;
    /// the name of the property
    Array!char  name;
    /// the property level in the IST
    uint    level;
    /// indicates if any error occured during processing
    bool    isDamaged;
    /// hint to rebuild the IST
    bool    isLastChild;

    ~this()
    {
        destruct(value);
        destruct(name);
    }
}

/**
 * Stores an IST in an associative array.
 */
struct IstNodeCache
{

private:

    IstNode _root;
    HashMap_AB!(const(char)[], IstNode) _aa;

public:

    @disable this(this);

    ~this()
    {
        destruct(_aa);
    }

    /// Updates the cache.
    void setRoot(IstNode root)
    {
        assert(root);
        _root = root;
        _aa.clear;

        size_t count;
        deepIterate!((a => ++count), "children")(root);
        _aa.reserve(count);
        deepIterate!(a => _aa.insert!imReserved(a.identifiersChain(), a), "children")(root);
    }

    /// Returns: The node with that matches to an identifier chain.
    IstNode find(const(char[]) identChain)
    {
        if (IstNode* n = identChain in _aa)
            return *n;
        else
            return null;
    }
}

/// IST node
class IstNode
{

    mixin TreeItem;

private:

    SerNodeInfo _info;

public:

    ~this()
    {
        _info.name.length = 0;
        _info.value.length = 0;
        destruct(_info);
        deleteChildren;
    }

    /**
     * Returns a new unmanaged IstNode.
     */
    IstNode addNewChildren()
    {
        auto result = construct!IstNode;
        addChild(result);
        return result;
    }

    /**
     * Sets the infomations describing the property associated
     * to this IST node.
     */
    void setDescriptor(T)(PropDescriptor!T* descriptor)
    {
        if (descriptor)
            setNodeInfo!T(&_info, descriptor);
    }

    /**
     * Returns a pointer to the information describing the property
     * associated to this IST node.
     */
    SerNodeInfo* info()
    {
        return &_info;
    }

    /**
     * Returns the parents identifier chain.
     */
    string parentIdentifiersChain()
    {
        if (!level)
            return "";

        string[] items;
        IstNode curr = cast(IstNode) parent;
        while (curr)
        {
            items ~= curr.info.name[];
            curr = cast(IstNode) curr.parent;
        }
        return items.retro.join(".");
    }

    /**
     * Returns the identifier chain.
     */
    string identifiersChain()
    {
        if (!level)
            return info.name[].idup;
        else
            return parentIdentifiersChain ~ "." ~ info.name[].idup;
    }
}
//----

// Value as text to ubyte[] converters ----------------------------------------+

private static char[] invalidText = "invalid".dup;

/// Converts the raw data contained in a SerNodeInfo to its string representation.
char[] value2text(SerNodeInfo* nodeInfo)
{
    char[] v2t_1(T)(){return to!string(*cast(T*)nodeInfo.value.ptr).dup;}
    char[] v2t_2(T)(){return to!string(cast(T[])nodeInfo.value[]).dup;}
    char[] v2t(T)(){if (!nodeInfo.rtti.dimension) return v2t_1!T; else return v2t_2!T;}
    //
    with (RtType) final switch(nodeInfo.rtti.type)
    {
        case _invalid, _aa, _pointer, _union: return invalidText;
        case _bool:     return v2t!bool;
        case _ubyte:    return v2t!ubyte;
        case _byte:     return v2t!byte;
        case _ushort:   return v2t!ushort;
        case _short:    return v2t!short;
        case _uint:     return v2t!uint;
        case _int:      return v2t!int;
        case _ulong:    return v2t!ulong;
        case _long:     return v2t!long;
        case _float:    return v2t!float;
        case _double:   return v2t!double;
        case _real:     return v2t!real;
        case _char:     return v2t!char;
        case _wchar:    return v2t!wchar;
        case _dchar:    return v2t!dchar;
        case _enum:     return v2t_2!char;
        case _object:   return cast(char[]) nodeInfo.value;
        case _stream:   return to!(char[])(nodeInfo.value[]);
        case _funptr:   return v2t_2!char;
        case _struct:
            if (nodeInfo.rtti.structInfo.type == StructType._binary)
                return v2t_2!ubyte;
            else
                return cast(char[]) nodeInfo.value;
    }
}

/// Converts the literal representation to a ubyte array according to type.
ubyte[] text2value(char[] text, const SerNodeInfo* nodeInfo)
{
    ubyte[] t2v_1(T)()
    {
        auto res = new ubyte[](T.sizeof);
        *cast(T*) res.ptr = to!T(text);
        return res;
    }
    ubyte[] t2v_2(T)()
    {
        auto v = to!(T[])(text);
        auto res = new ubyte[](v.length * T.sizeof);
        moveMem(res.ptr, v.ptr, res.length);
        return res;
    }
    ubyte[] t2v(T)(){
        if (!nodeInfo.rtti.dimension) return t2v_1!T; else return t2v_2!T;
    }
    //
    with(RtType) final switch(nodeInfo.rtti.type)
    {
        case _invalid, _aa, _pointer, _union: return cast(ubyte[])invalidText;
        case _bool:     return t2v!bool;
        case _ubyte:    return t2v!ubyte;
        case _byte:     return t2v!byte;
        case _ushort:   return t2v!ushort;
        case _short:    return t2v!short;
        case _uint:     return t2v!uint;
        case _int:      return t2v!int;
        case _ulong:    return t2v!ulong;
        case _long:     return t2v!long;
        case _float:    return t2v!float;
        case _double:   return t2v!double;
        case _real:     return t2v!real;
        case _char:     return t2v!char;
        case _wchar:    return t2v_2!wchar;
        case _dchar:    return t2v!dchar;
        case _enum:     return cast(ubyte[]) text;
        case _object:   return cast(ubyte[]) text;
        case _stream:   return t2v_2!ubyte;
        case _funptr:   return cast(ubyte[]) text;
        case _struct:
            if (nodeInfo.rtti.structInfo.type == StructType._binary)
                return t2v_2!ubyte;
            else
                return cast(ubyte[]) text;
    }
}
//----

// Descriptor to node & node to descriptor ------------------------------------+

/// Restores the raw value contained in a SerNodeInfo using the associated setter.
void nodeInfo2Declarator(SerNodeInfo* nodeInfo)
{
    void toDecl1(T)()  {
        auto descr = cast(PropDescriptor!T *) nodeInfo.descriptor;
        descr.set( *cast(T*) nodeInfo.value.ptr );
    }
    void toDecl2(T)() {
        auto descr = cast(PropDescriptor!(T[]) *) nodeInfo.descriptor;
        descr.set(cast(T[]) nodeInfo.value[]);
    }
    void toDecl(T)() {
        (!nodeInfo.rtti.dimension) ? toDecl1!T : toDecl2!T;
    }
    //
    with (RtType) final switch(nodeInfo.rtti.type)
    {
        case _invalid, _aa, _pointer, _union:  break;
        case _bool:     toDecl!bool; break;
        case _byte:     toDecl!byte; break;
        case _ubyte:    toDecl!ubyte; break;
        case _short:    toDecl!short; break;
        case _ushort:   toDecl!ushort; break;
        case _int:      toDecl!int; break;
        case _uint:     toDecl!uint; break;
        case _long:     toDecl!long; break;
        case _ulong:    toDecl!ulong; break;
        case _float:    toDecl!float; break;
        case _double:   toDecl!double; break;
        case _real:     toDecl!real; break;
        case _char:     toDecl!char; break;
        case _wchar:    toDecl!wchar; break;
        case _dchar:    toDecl!dchar; break;
        case _enum:
            import std.algorithm.searching;
            int i = cast(int) countUntil(nodeInfo.rtti.enumInfo.members, cast(string) nodeInfo.value);
            assert(i > -1);
            auto descr = cast(PropDescriptor!int *) nodeInfo.descriptor;
            descr.set(nodeInfo.rtti.enumInfo.values[i]);
            break;
        case _object:   break;
        case _struct:
            final switch(nodeInfo.rtti.structInfo.type)
            {
                case StructType._none: assert(0);
                case StructType._text:

                    void* structPtr = (cast(PropDescriptor!GenericStruct*) nodeInfo.descriptor).getter()().getThis;
                    void* oldCtxt = nodeInfo.rtti.structInfo.textTraits.setContext(structPtr);
                    assert(nodeInfo.rtti.structInfo.textTraits.loadFromText.ptr);
                    nodeInfo.rtti.structInfo.textTraits.loadFromText(cast(const(char)[]) nodeInfo.value);
                    nodeInfo.rtti.structInfo.textTraits.restoreContext(oldCtxt);
                    break;
                case StructType._binary:
                    void* structPtr = (cast(PropDescriptor!GenericStruct*) nodeInfo.descriptor).getter()().getThis;
                    void* oldCtxt = nodeInfo.rtti.structInfo.binTraits.setContext(structPtr);
                    assert(nodeInfo.rtti.structInfo.binTraits.loadFromBytes.ptr);
                    nodeInfo.rtti.structInfo.binTraits.loadFromBytes(cast(ubyte[])nodeInfo.value);
                    nodeInfo.rtti.structInfo.binTraits.restoreContext(oldCtxt);
                    break;
                case StructType._publisher:
                    break;
            }
            break;
        case _stream:
            MemoryStream str = construct!MemoryStream;
            scope(exit) destruct(str);
            str.write(cast(ubyte*)nodeInfo.value.ptr, nodeInfo.value.length);
            str.position = 0;
            auto descr = cast(PropDescriptor!Stream*) nodeInfo.descriptor;
            descr.set(str);
            break;
        case _funptr:
            char[] refId = cast(char[]) nodeInfo.value[];
            bool setFromGenericRef(T)()
            {
                bool result;
                if (T* funOrDg = ReferenceMan.reference!(T)(refId))
                {
                    auto descr = cast(PropDescriptor!T*) nodeInfo.descriptor;
                    descr.set(*funOrDg);
                    result = true;
                }
                return result;
            }

            bool setFuncFromTypeIdentifier()
            {
                bool result;
                if (void* fod = ReferenceMan.reference(nodeInfo.rtti.funptrInfo.identifier, refId))
                {
                    result =  true;
                    auto descr = cast(PropDescriptor!GenericFunction*) nodeInfo.descriptor;
                    descr.set(cast(GenericFunction)fod);
                }
                return result;
            }

            bool setDgFromTypeIdentifier()
            {
                bool result;
                if (void* fod = ReferenceMan.reference(nodeInfo.rtti.funptrInfo.identifier, refId))
                {
                    result = true;
                    auto descr = cast(PropDescriptor!GenericDelegate*) nodeInfo.descriptor;
                    descr.set(*cast(GenericDelegate*)fod);
                }
                return result;
            }

            if (nodeInfo.rtti.funptrInfo.hasContext)
            {
                if (!setDgFromTypeIdentifier)
                    setFromGenericRef!GenericDelegate;
            }
            else
            {
                if (!setFuncFromTypeIdentifier)
                    setFromGenericRef!GenericFunction;
            }
    }
}

/// Fills an SerNodeInfo according to an PropDescriptor
void setNodeInfo(T)(SerNodeInfo* nodeInfo, PropDescriptor!T* descriptor)
{
    scope(failure) nodeInfo.isDamaged = true;

    nodeInfo.rtti = descriptor.rtti;
    nodeInfo.descriptor = cast(Ptr) descriptor;
    nodeInfo.name = descriptor.name[];

    // simple, fixed-length (or convertible to), types
    static if (isBasicRtType!T)
    {
        nodeInfo.value.length = nodeInfo.rtti.type.size;
        *cast(T*) nodeInfo.value.ptr = descriptor.get();
    }

    // arrays types
    else static if (isSerArrayType!T)
    {
        T value = descriptor.get();
        nodeInfo.value.length = value.length * nodeInfo.rtti.type.size;
        moveMem(nodeInfo.value.ptr, cast(void*) value.ptr, nodeInfo.value.length);
    }

    // enums
    else static if (is(T == GenericEnum))
    {
        uint v = (*descriptor.as!uint).get();
        switch(nodeInfo.rtti.enumInfo.valueType.type.size)
        {
            case 1: v &= 0xFF; break;
            case 2: v &= 0xFFFF; break;
            default:
        }
        import std.algorithm.searching: countUntil;
        ptrdiff_t i = countUntil(nodeInfo.rtti.enumInfo.values, v);
        assert (i != -1);
        nodeInfo.value = cast(ubyte[]) nodeInfo.rtti.enumInfo.members[i];
    }

    // Serializable or Object
    else static if (isSerObjectType!T)
    {
        Object obj = cast(Object) descriptor.get();
        if (obj)
            nodeInfo.value = cast(ubyte[]) className(obj);
    }

    // struct, warning, T is set to a dummy struct type
    else static if (is(T == struct) || is(T==GenericStruct))
    {
        const(Rtti)* ti = nodeInfo.rtti;
        final switch(ti.structInfo.type)
        {
            case StructType._none, StructType._publisher:
                nodeInfo.value = cast(ubyte[]) ti.structInfo.identifier;
                break;
            case StructType._text:
                void* structPtr = descriptor.getter()().getThis;
                void* oldCtxt = ti.structInfo.textTraits.setContext(structPtr);
                assert(ti.structInfo.textTraits.saveToText.ptr);
                nodeInfo.value = cast(ubyte[]) ti.structInfo.textTraits.saveToText();
                ti.structInfo.textTraits.restoreContext(oldCtxt);
                break;
            case StructType._binary:
                void* structPtr = descriptor.getter()().getThis;
                void* oldCtxt = ti.structInfo.binTraits.setContext(structPtr);
                assert(ti.structInfo.binTraits.saveToBytes.ptr);
                nodeInfo.value = ti.structInfo.binTraits.saveToBytes();
                ti.structInfo.binTraits.restoreContext(oldCtxt);
        }
    }

    // stream
    else static if (is(T : Stream))
    {
        Stream value = descriptor.get();
        value.position = 0;
        static if (is(T == MemoryStream))
        {
            nodeInfo.value = (cast(MemoryStream)value).ubytes;
        }
        else
        {
            nodeInfo.value.length = cast(uint) value.size;
            value.read(nodeInfo.value.ptr, cast(uint) value.size);
        }
    }

    // delegate & function
    else static if (is(T == GenericDelegate) || is(T == GenericFunction))
    {
        auto dg = descriptor.get;
        const(char)[] id;
        id = ReferenceMan.referenceID(&dg);
        if (!id.length) id = ReferenceMan.referenceID!(is(T == GenericDelegate))
            (nodeInfo.rtti.funptrInfo.identifier, &dg);
        nodeInfo.value = cast(ubyte[]) id;
    }

    else static assert(0, "cannot set the ISTnode for a " ~ T.stringof);
}
//----

// Serialization formats ------------------------------------------------------+

/// Enumerates the possible serialization format
enum SerializationFormat : ubyte
{
    /// native binary format
    izbin,
    /// native text format
    iztxt,
    /// JSON chunks
    json
}

/// Enumerates the possible token handled by a serialization format
enum FormatToken
{
    /// Nothing was read.
    unknow,
    ///
    beg,
    /// An object is read or written.
    objBeg,
    /// An object has been object read or written.
    objEnd,
    /// A property is read or written.
    prop,
    /// Nothing to read anymore.
    end,
}

/// Prototype of a function that writes an IstNode representation to a Stream.
alias FormatWriter = void function(IstNode istNode, Stream stream, const FormatToken tok);

/// Prototype of a function that reads an IstNode representation from a Stream.
alias FormatReader = FormatToken function(Stream stream, IstNode istNode);

// JSON format ----------------------------------------------------------------+
private void writeJSON(IstNode istNode, Stream stream, const FormatToken tok)
{
    import std.json: JSONValue, toJSON;
    version(assert) const bool pretty = true; else const bool pretty = false;
    //
    switch (tok)
    {
        case FormatToken.beg:
            //stream.writeChar('{');
            return;
        case FormatToken.end:
            //stream.writeChar('}');
            return;
        case FormatToken.objEnd:
            foreach(i; 0 .. istNode.level) stream.writeChar('\t');
            if (pretty)
            {
                stream.position = stream.position - 2;
                stream.writeChar(' ');
                stream.writeChar(' ');
            }
            else
            {
                stream.position = stream.position - 1;
                stream.writeChar(' ');
            }
            stream.writeChar(']');
            stream.writeChar('}');
            stream.writeChar(',');
            stream.writeChar('\n');
            return;
        default:
    }
    //
    auto level  = JSONValue(istNode.level);
    auto type   = JSONValue(typeString(istNode.info.rtti));
    auto name   = JSONValue(istNode.info.name[].idup);

    char[] txt;
    if (tok == FormatToken.objBeg)
    {
        auto prop   = JSONValue(["type": type, "name": name]);
        txt    = (toJSON(prop, pretty)[0..$-1] ~ ",\"value\" : [").dup;
    }
    else
    {
        auto value  = JSONValue(value2text(istNode.info).idup);
        auto prop   = JSONValue(["type": type, "name": name, "value": value]);
        txt = (toJSON(prop, pretty) ~ ",").dup;
    }



    //
    stream.write(txt.ptr, txt.length);
}

private FormatToken readJSON(Stream stream, IstNode istNode)
{
    import std.json: JSONValue, parseJSON, JSON_TYPE;
    // cache property
    size_t cnt, len;
    char c;
    bool skip;
    auto immutable stored = stream.position;
    while (true)
    {
        if (stream.position == stream.size)
            return FormatToken.end;
        ++len;
        stream.read(&c, 1);
        if (c == '\\')
            continue;
        if (c == '"')
            skip = !skip;
        if (!skip)
        {
            cnt += (c == '{');
            cnt -= (c == '}');
        }
        if (cnt == 0)
            break;
    }
    stream.position = stored;
    char[] cache;
    cache.length = len;
    stream.read(cache.ptr, cache.length);
    //
    const JSONValue prop = parseJSON(cache);

    const(JSONValue)* type = "type" in prop;
    if (type && type.type == JSON_TYPE.STRING)
        istNode.info.rtti = getRtti(type.str);
    else
        istNode.info.isDamaged = true;

    const(JSONValue)* name = "name" in prop;
    if (name && name.type == JSON_TYPE.STRING)
        istNode.info.name = name.str.dup;
    else
        istNode.info.isDamaged = true;

    const(JSONValue)* value = "value" in prop;
    if (value && value.type == JSON_TYPE.STRING)
        istNode.info.value = text2value(value.str.dup, istNode.info);
    else
        istNode.info.isDamaged = true;

    return FormatToken.prop;
}
// ----

// Text format ----------------------------------------------------------------+
private void writeText(IstNode istNode, Stream stream, const FormatToken tok)
{
    switch (tok)
    {
        case FormatToken.beg, FormatToken.end:
            return;
        case FormatToken.objEnd:
            foreach(i; 0 .. istNode.level) stream.writeChar('\t');
            stream.writeChar('}');
            stream.writeChar('\n');
            return;
        default:
    }
    // indentation
    foreach(i; 0 .. istNode.level) stream.writeChar('\t');
    // type
    char[] type = typeString(istNode.info.rtti).dup;
    type = replace(type, " ", "-");
    stream.write(type.ptr, type.length);
    stream.writeChar(' ');
    // name
    char[] name = istNode.info.name[];
    stream.write(name.ptr, name.length);
    // name value separators
    char[] name_value = " = \"".dup;
    stream.write(name_value.ptr, name_value.length);
    // value
    char[] value = value2text(istNode.info);
    with (RtType) if (istNode.info.rtti.type >= _char &&
        istNode.info.rtti.type <= _dchar && istNode.info.rtti.dimension > 0)
    {
        value = escape(value, [['\n','n'],['"','"']]);
    }
    stream.write(value.ptr, value.length);
    char[] eol = "\"\n".dup;
    stream.write(eol.ptr, eol.length);
    if (tok == FormatToken.objBeg)
    {
        foreach(i; 0 .. istNode.level) stream.writeChar('\t');
        value = "{\n".dup;
        stream.write(value.ptr, value.length);
    }
}

private FormatToken readText(Stream stream, IstNode istNode)
{
    char[] text, identifier;
    char curr, old;

    // cache property
    while (true)
    {
        if (stream.position == stream.size)
            return FormatToken.end;

        old = curr;
        curr = stream.readChar;
        if (curr == '\n' && old == '}')
            return FormatToken.objEnd;
        if (curr == '"' && old == '\\')
        {
            text ~= curr;
            continue;
        }
        if (curr == '\n' && old == '"')
            break;
        else if (curr == '"' && stream.position == stream.size)
            break;
        text ~= curr;
    }

    // skip indentation
    skipWord(text, whiteChars);
    // type & name;
    identifier = nextWord(text);
    identifier = replace(identifier, "-", " ");
    istNode.info.rtti = getRtti(identifier);
    assert(istNode.info.rtti, identifier);
    // name
    istNode.info.name = nextWord(text).idup;
    // name value separator
    identifier = nextWord(text);
    if (identifier != "=") istNode.info.isDamaged = true;
    // value
    skipWordUntil(text, '"');
    identifier = text[1..$-1];
    with (RtType) if (istNode.info.rtti.type >= _char &&
        istNode.info.rtti.type <= _dchar && istNode.info.rtti.dimension > 0)
    {
        identifier = unEscape(identifier, [['\n','n'],['"','"']]);
    }
    istNode.info.value = text2value(identifier, istNode.info);

    // object begins ?
    auto savedPos = stream.position;
    while (true)
    {
        old = curr;
        curr = stream.readChar;
        if (curr == '\n' && old == '{')
            return FormatToken.objBeg;
        else if (curr == '\n')
        {
            stream.position = savedPos;
            break;
        }
    }
    return FormatToken.prop;
}
//----

// Binary format --------------------------------------------------------------+
version(BigEndian) private ubyte[] swapBE(const ref ubyte[] input, size_t div)
{
    if (div == 1) return input.dup;
    auto result = new ubyte[](input.length);
    switch(div) {
        default: break;
        case 2: foreach(immutable i; 0 .. input.length / div) {
            result[i*2+0] = input[i*2+1];
            result[i*2+1] = input[i*2+0];
        } break;
        case 4: foreach(immutable i; 0 .. input.length / div) {
            result[i*4+0] = input[i*4+3];
            result[i*4+1] = input[i*4+2];
            result[i*4+2] = input[i*4+1];
            result[i*4+3] = input[i*4+0];
        } break;
        case 8: foreach(immutable i; 0 .. input.length / div) {
            result[i*8+0] = input[i*8+7];
            result[i*8+1] = input[i*8+6];
            result[i*8+2] = input[i*8+5];
            result[i*8+3] = input[i*8+4];
            result[i*8+4] = input[i*8+3];
            result[i*8+5] = input[i*8+2];
            result[i*8+6] = input[i*8+1];
            result[i*8+7] = input[i*8+0];
        } break;
    }
    return result;
}

private void writeBin(IstNode istNode, Stream stream, const FormatToken tok)
{
    switch (tok)
    {
        case FormatToken.beg, FormatToken.end:
            return;
        case FormatToken.objEnd:
            stream.writeUbyte(0xFE);
            return;
        default:
    }

    ubyte[] data;
    uint datalength;
    // total size
    auto sizePos = stream.position;
    stream.writeUint(0);
    // header
    stream.writeUbyte(0x99);
    // type
    stream.writeUbyte(cast(ubyte) istNode.info.rtti.type);
    // as array
    stream.writeBool(istNode.info.rtti.dimension > 0);
    // opt type identifier stringz
    if (istNode.info.rtti.type == RtType._enum ||
        istNode.info.rtti.type == RtType._struct ||
        istNode.info.rtti.type == RtType._funptr ||
        istNode.info.rtti.type == RtType._object)
    {
        data = cast(ubyte[]) istNode.info.rtti.enumInfo.identifier;
        stream.write(data.ptr, data.length);
    }
    stream.writeUbyte(0x0);
    // namez
    data = cast(ubyte[]) istNode.info.name[];
    stream.write(data.ptr, data.length);
    stream.writeUbyte(0);
    // value length then value
    version(LittleEndian)
        data = istNode.info.value;
    else
        data = swapBE(istNode.info.value, type2size[istNode.info.type]);

    datalength = cast(uint) data.length;
    stream.writeUint(cast(uint) datalength);
    stream.write(data.ptr, data.length);

    // sub obj
    stream.writeBool(tok == FormatToken.objBeg);
    //footer
    stream.writeUbyte(0xA0);
    // size
    auto savedEnd = stream.position;
    stream.position = sizePos;
    stream.writeUint(cast(uint) (savedEnd - sizePos));
    stream.position = savedEnd;
}

private FormatToken readBin(Stream stream, IstNode istNode)
{
    ubyte bin;
    ubyte[] data;
    uint datalength;
    uint sze;
    import std.string: fromStringz;
    //
    if (stream.position == stream.size)
        return FormatToken.end;
    bin = stream.readUbyte;
    if (bin == 0xFE)
        return FormatToken.objEnd;
    stream.position = stream.position-1;
    // cache property
    sze = stream.readUint;
    data.length = sze - 4;
    stream.read(data.ptr, data.length);
    //
    assert(data[0] == 0x99);
    assert(data[$-1] == 0xA0);
    FormatToken result = (data[$-2] == 1) ? FormatToken.objBeg : FormatToken.prop;
    // type and array
    string tstr;
    uint offs = 3;
    if (data[1] == RtType._enum || data[1] == RtType._struct ||
        data[1] == RtType._funptr || data[1] == RtType._object)
    {
        tstr = fromStringz( &(cast(string)data)[3] );
        offs += tstr.length;
    }
    else tstr = typeString(cast(RtType) data[1]);
    offs += 1;
    if (data[2]) tstr ~= "[]";
    istNode.info.rtti = getRtti(tstr);
    assert(istNode.info.rtti, `"` ~ tstr ~ `"` );
    // namez
    string name = fromStringz(&(cast(string)data)[offs]);
    offs += (name.length + 1);
    istNode.info.name = name;
    // value length then value
    version(LittleEndian)
    {
        datalength = *cast(uint*) (data.ptr + offs);
        istNode.info.value = data[offs + 4 .. offs + 4 + datalength];
    }
    else
    {
        datalength = *cast(uint*) (data.ptr + offs);
        data = data[offs + 4 .. offs + 4 + datalength];
        istNode.info.value = swapBE(data, istNode.info.type.size);
    }
    return result;
}
//----

/// The serialization format used when not specified.
alias defaultFormat = SerializationFormat.iztxt;

private FormatWriter writeFormat(SerializationFormat format)
{
    with(SerializationFormat) final switch(format)
    {
        case izbin: return &writeBin;
        case iztxt: return &writeText;
        case json:  return &writeJSON;
    }
}

private FormatReader readFormat(SerializationFormat format)
{
    with(SerializationFormat) final switch(format)
    {
        case izbin: return &readBin;
        case iztxt: return &readText;
        case json:  return &readJSON;
    }
}
//----

// Main Serializer class ------------------------------------------------------+

/**
 * Prototype of the event triggered when a serializer misses a property descriptor.
 * Params:
 *      node = The information used to determine the descriptor to return.
 *      descriptor = What the serializer want. If set to null then node is not restored.
 *      stop = the callee can set this value to true to stop the restoration
 *      process. According to the serialization context, this value can be noop.
 */
alias WantDescriptorEvent = void delegate(IstNode node, ref Ptr descriptor, out bool stop);


/**
 * Prototype of the event called when a serializer fails to get an aggregate to
 * deserialize.
 *
 * Params:
 *      node = The information the callee uses to set the undefined aggregate.
 *      aggregate = The object or a pointer to the struct where the deserialization
 *          continues.
 *      fromReference = When set to true, the serializer tries to find the aggregate
 *          in the ReferenceMan.
 */
alias WantAggregateEvent = void delegate(IstNode node, ref void* aggregate, out bool fromRefererence);


//TODO-cserializer: error handling (using isDamaged + errors when reading a format).
//TODO-cserializer: convert FP to string using format("%.{9|17}g",value).

/**
 * The Serializer class is specialized to store and restore the members of
 * an Object.
 *
 * PropertyPublisher:
 * A Serializer serializes trees of classes and struct that implement the
 * PropertyPublisher interface. Their publications define what is saved or
 * restored. Object descriptors leading to an owned Object define the structure.
 * Basics types and array of basic types are handled. Special cases exist to
 * manage Stream properties, delegates or objects that are stored in the ReferenceMan.
 * It's even possible to handle more complex types by using or writing custom
 * PropertyPublishers, such as those defined in iz.classes.
 *
 * Ownership:
 * Sub objects can be fully serialized or not. This is determined by the ownership.
 * A sub object is considered as owned when its member 'declarator' matches to
 * to the member 'declarator' of the descriptor that returns this sub object.
 * When not owned, the sub object publications are not stored, instead, the
 * serializer writes its unique identifier, as found in the ReferenceMan.
 * When deserializing, the opposite process happens: the serializer tries to
 * restore the reference using the ReferenceMan. Ownership is automatically set
 * by the $(D PropertyPubliserImpl) analyzers.
 *
 * Representation:
 * The serializer uses an intermediate serialization tree (IST) that ensures a
 * certain flexibilty against a traditional single-shot sequential serialization.
 * As expected for a serializer, object trees can be stored or restored by
 * a simple and single call to $(D publisherToStream()) in pair with
 * $(D streamToPublisher()) but the IST also allows to convert a Stream or
 * to find and restores a specific property.
 *
 * Errors:
 * Two events ($(D onWantDescriptor) and  $(D onWantAggregate)) allow to handle
 * the errors that could be encountered when restoring.
 * They permit a PropertyPublisher to be modified without any risk of deserialization
 * failure. Data saved from an older version can be recovered by deserializing in
 * a temporary property and converted to a new type. They can also be skipped,
 * without stopping the whole processing. Missing objects can be created when
 * The serializer ask for, since in this case, the original Object type and the
 * original variable names are passed as hint.
 *
 * Hints:
 * The serializer handles the $(D PropHints) of the publications. If $(D PropHint.dontSet)
 * is in the hints then a property is not restored. If $(D PropHint.dontGet) is in the
 * hints then the property is not stored. These two hints allow to deprecate some
 * publications, without breaking the restoration. The hint $(D PropHint.initCare)
 * indicates that a property equal to its initializer is written. For floating point
 * types there is an exception that is the intializer is considered to be 0.
 */
class Serializer
{

private:

    IstNodeCache _nodesCache;
    /// the IST root
    IstNode _rootNode;
    /// the current parent node, always represents a PropertyPublisher
    IstNode _parentNode;
    /// the last created node
    IstNode _previousNode;
    /// the PropertyPublisher linked to _rootNode
    Object  _rootPublisher;

    Object  _declarator;

    WantAggregateEvent _onWantAggregate;
    WantDescriptorEvent _onWantDescriptor;
    void delegate(void*) _onFinishAggregate;

    SerializationFormat _format;

    Stream _stream;
    PropDescriptor!Object _rootDescr;

    bool _mustWrite;
    bool _mustRead;

    void addIstNodeForDescriptor(T)(PropDescriptor!T * descriptor)
    if (isSerializable!T && !isSerObjectType!T)
    in
    {
        assert(descriptor);
        assert(descriptor.name.length);
    }
    body
    {
        if (PropHint.dontGet in descriptor.hints)
            return;
        if (PropHint.initCare !in descriptor.hints)
        {
            static if (isNumeric!T)
            {
                if (descriptor.get() == 0)
                    return;
            }
            else static if (is(T == bool))
            {
                if (descriptor.get() == false)
                    return;
            }
            else static if (is(T == GenericEnum))
            {
                //if (descriptor.get() == 0)
                //    return;
            }
            else static if (isArray!T)
            {
                if (!descriptor.get().length)
                    return;
            }
            else static if (is(T == GenericFunction) ||is(T == GenericDelegate))
            {
                auto dg = descriptor.get();
                if (dg == null)
                    return;
                const(char)[] id = ReferenceMan.referenceID(&dg);
                if (!id.length) id = ReferenceMan.referenceID!(is(T == GenericDelegate))
                    (descriptor.rtti.funptrInfo.identifier, &dg);
                if (id.length == 0)
                    return;
            }
        }
        IstNode propNode = _parentNode.addNewChildren;
        propNode.setDescriptor(descriptor);

        if (_mustWrite)
        {
            const bool isPub = isPublisingStruct(descriptor.rtti) || descriptor.rtti.type == RtType._object;
            if (isPub) writeFormat(_format)(propNode, _stream, FormatToken.objBeg);
            else writeFormat(_format)(propNode, _stream, FormatToken.prop);
            if (isPub) writeFormat(_format)(propNode, _stream, FormatToken.objEnd);
        }

        _previousNode = propNode;
    }

    bool restoreFromEvent(IstNode node, out bool stop)
    {
        if (!_onWantDescriptor)
            return false;
        _onWantDescriptor(node, node.info.descriptor, stop);
        if (node.info.descriptor)
        {
            nodeInfo2Declarator(node.info);
            return true;
        }
        else if (isSerObjectType(node.info.rtti.type))
            return true;
        else
            return false;
    }

    bool descriptorMatchesNode(T)(PropDescriptor!T* descr, IstNode node)
    if (isSerializable!T)
    {
        if (!descr || !node.info.name.length || descr.name != node.info.name ||
            getRtti!T !is descr.rtti) return false;
        else
            return true;
    }

    void addPropertyPublisher(PD)(PD* pubDescriptor)
    {
        assert(pubDescriptor);

        if (PropHint.dontGet in pubDescriptor.hints)
            return;

        static if (is(PD == PropDescriptor!Object))
        {
            PropertyPublisher publisher;
            publisher = cast(PropertyPublisher) pubDescriptor.get();
            enum declIsClass = true;
        }
        else
        {
            const(Rtti)* prtti = pubDescriptor.rtti;
            if (prtti.type != RtType._struct)
                return;
            if (prtti.structInfo.type != StructType._publisher)
                return;

            const(PubTraits)* publisher = prtti.structInfo.pubTraits;
            enum declIsClass = false;
        }

        // write/Set object node
        if (!_parentNode) _parentNode = _rootNode;
        else _parentNode = _parentNode.addNewChildren;
        _parentNode.setDescriptor(pubDescriptor);


        if (/*_mustWrite*/false)
        {
            writeFormat(_format)(_parentNode, _stream, FormatToken.objBeg);
            writeFormat(_format)(_parentNode, _stream, FormatToken.prop);
            writeFormat(_format)(_parentNode, _stream, FormatToken.objEnd);
        }

        // reference: if not a PropPublisher
        if(!publisher)
            return;

        static if (declIsClass)
        {
            // reference: current publisher is not owned at all
            if (_parentNode !is _rootNode && publisher.declarator is null)
                return;

            // reference: current publisher is not owned by the declarator
            if (_parentNode !is _rootNode && pubDescriptor.declarator !is publisher.declarator)
                return;
        }

        // not a reference: current publisher is owned (it has initialized the target),
        // so write its members
        foreach(immutable i; 0 .. publisher.publicationCount())
        {
            auto descr = cast(GenericDescriptor*) publisher.publicationFromIndex(i);
            const(Rtti)* rtti = descr.rtti;
            //
            void addValueProp(T)()
            {
                if (!rtti.dimension) addIstNodeForDescriptor(descr.as!T);
                else addIstNodeForDescriptor(descr.as!(T[]));
            }
            with(RtType) final switch(rtti.type)
            {
                case _invalid, _aa, _pointer, _union: assert(0);
                case _bool:   addValueProp!bool; break;
                case _byte:   addValueProp!byte; break;
                case _ubyte:  addValueProp!ubyte; break;
                case _short:  addValueProp!short; break;
                case _ushort: addValueProp!ushort; break;
                case _int:    addValueProp!int; break;
                case _uint:   addValueProp!uint; break;
                case _long:   addValueProp!long; break;
                case _ulong:  addValueProp!ulong; break;
                case _float:  addValueProp!float; break;
                case _real:   addValueProp!real; break;
                case _double: addValueProp!double; break;
                case _char:   addValueProp!char; break;
                case _wchar:  addValueProp!wchar; break;
                case _dchar:  addValueProp!dchar; break;
                case _enum:   addIstNodeForDescriptor(descr.as!GenericEnum); break;
                case _object:
                    auto _oldParentNode = _parentNode;
                    addPropertyPublisher(descr.as!Object);
                    _parentNode = _oldParentNode;
                    break;
                case _stream:
                    addIstNodeForDescriptor(descr.as!Stream);
                    break;
                case _funptr:
                    if (rtti.funptrInfo.hasContext)
                        addIstNodeForDescriptor(descr.as!GenericDelegate);
                    else
                        addIstNodeForDescriptor(descr.as!GenericFunction);
                    break;
                case _struct:
                    final switch (rtti.structInfo.type)
                    {
                        case StructType._none:
                            assert(0);
                        case StructType._publisher:
                            auto _oldParentNode = _parentNode;
                            PropDescriptor!GenericStruct* des = cast(PropDescriptor!GenericStruct*) descr;
                            void* structPtr = des.getter()().getThis;
                            void* oldCtxt = rtti.structInfo.pubTraits.setContext(structPtr);
                            addPropertyPublisher(des); // struct detected with rtti
                            rtti.structInfo.pubTraits.restoreContext(oldCtxt);
                            _parentNode = _oldParentNode;
                            break;
                        case StructType._text, StructType._binary:
                            addIstNodeForDescriptor(descr.as!GenericStruct);
                    }
            }
        }
    }

public:

    ///
    this()
    {
        _rootNode = construct!IstNode;
    }

    ~this()
    {
        _rootNode.deleteChildren;
        destruct(_rootNode);
        destruct(_nodesCache);
    }

//---- serialization ----------------------------------------------------------+

    /**
     * Saves the IST to a Stream.
     *
     * Params:
     *      outputStream = The stream where te data are written.
     *      format = The data format.
     */
    void istToStream(Stream outputStream, SerializationFormat format = defaultFormat)
    {
        _format = format;
        _stream = outputStream;
        _mustWrite = true;
        //
        writeFormat(_format)(null, _stream, FormatToken.beg);
        void writeNodesFrom(IstNode parent)
        {
            writeFormat(_format)(parent, _stream, FormatToken.objBeg);
            foreach(node; parent.children)
            {
                auto child = cast(IstNode) node;
                if (child.childrenCount)
                    writeNodesFrom(child);
                else writeFormat(_format)(child, _stream, FormatToken.prop);
            }
            writeFormat(_format)(parent, _stream, FormatToken.objEnd);
        }
        writeNodesFrom(_rootNode);
        writeFormat(_format)(null, _stream, FormatToken.end);
        //
        _mustWrite = false;
        _stream = null;
    }

    /**
     * Builds the IST from a PropertyPublisher and stores each publication
     * of the publisher in a stream.
     *
     * Storage is performed just after a publication is detected.
     *
     * Params:
     *      root = Either a PropertyPublisher or an object that's been mixed
     *      with the PropertyPublisherImpl template.
     *      outputStream = The stream where the data are written.
     *      format = The serialized data format.
     */
    void publisherToStream(Object root, Stream outputStream,
        SerializationFormat format = defaultFormat)
    {
        _format = format;
        _stream = outputStream;
        //_mustWrite = true;
        _rootNode.deleteChildren;
        _previousNode = null;
        _parentNode = null;
        PropDescriptor!Object rootDescr = PropDescriptor!Object(&root, "root");
        addPropertyPublisher(&rootDescr);
        istToStream(outputStream, format);
        //_mustWrite = false;
        _stream = null;
    }

    /// ditto
    void publisherToStream(S)(ref S root, Stream outputStream,
        SerializationFormat format = defaultFormat)
    if (is(S == struct))
    {
        const(Rtti)* rtti = getRtti!S;
        if (rtti.structInfo.type != StructType._publisher)
            return;
        rtti.structInfo.pubTraits.setContext(&root);

        _format = format;
        _stream = outputStream;
        //_mustWrite = true;
        _rootNode.deleteChildren;
        _previousNode = null;
        _parentNode = null;
        PropDescriptor!S rootDescr = PropDescriptor!S(&root, "root");
        addPropertyPublisher(&rootDescr);
        istToStream(outputStream, format);
        //_mustWrite = false;
        _stream = null;
    }

    /**
     * Builds the IST from a PropertyPublisher.
     */
    void publisherToIst(Object root)
    {
        _mustWrite = false;
        _rootNode.deleteChildren;
        _previousNode = null;
        _parentNode = null;
        PropDescriptor!Object rootDescr = PropDescriptor!Object(cast(Object*)&root, "root");
        addPropertyPublisher(&rootDescr);
        _mustWrite = false;
        _stream = null;
    }

    /**
     * Builds the IST from a struct that has the traits of a property publisher.
     */
    void publisherToIst(S)(ref S root)
    if (is(S == struct))
    {
        const(Rtti)* rtti = getRtti!S;
        if (rtti.structInfo.type != StructType._publisher)
            return;
        rtti.structInfo.pubTraits.setContext(&root);

        _mustWrite = false;
        _rootNode.deleteChildren;
        _previousNode = null;
        _parentNode = null;
         PropDescriptor!S rootDescr = PropDescriptor!S(&root, "root");
        addPropertyPublisher(&rootDescr);
        _mustWrite = false;
        _stream = null;
    }


//------------------------------------------------------------------------------
//---- deserialization --------------------------------------------------------+

    /**
     * Restores the IST to a $(D PropertyPublisher).
     *
     * Can be called after $(D streamToIst), which builds the IST without defining
     * the $(D PropDescriptor) that matches to a node. The descriptors are
     * dynamically set using the publications of the root. If the procedure doesn't
     * detect the descriptor that matches to an IST node then the events
     * $(D onWantObject) and $(D onWantDescriptor) are called.
     *
     * Params:
     *      root = The object from where the restoration starts. It must be
     *      a $(D PropertyPublisher) or struct that contains $(D PropertyPublisherImpl).
     */
    void istToPublisher(R)(ref R root)
    if (is(R == class) || is(R == struct))
    {
        void restoreFrom(T)(IstNode node, ref T target)
        {
            static if (is(T : Object))
                if (!target) return;

            foreach(child; node.children)
            {
                IstNode childNode = cast(IstNode) child;

                bool stop;
                void* gdPtr = target.publicationFromName(childNode.info.name[]);
                if (!gdPtr)
                {
                    restoreFromEvent(childNode, stop);
                    if (stop)
                        return;
                    stop = false;
                    gdPtr = node.info.descriptor;
                }

                if (!gdPtr) continue;

                GenericDescriptor* gd = cast(GenericDescriptor*) gdPtr;
                if (PropHint.dontSet in gd.hints)
                    continue;
                if (gd.rtti is childNode.info.rtti)
                {
                    childNode.info.descriptor = gd;
                    nodeInfo2Declarator(childNode.info);
                    if (childNode.info.rtti.type == RtType._object)
                    {
                        auto od = cast(PropDescriptor!Object*) gd;
                        void* o = cast(void*) od.get();
                        bool fromRef;

                        if (!o && _onWantAggregate)
                            _onWantAggregate(childNode, o, fromRef);

                        if (fromRef || !o)
                        {
                            void* po = ReferenceMan.reference(
                                childNode.info.rtti.classInfo.identifier,
                                childNode.identifiersChain
                            );
                            if (po)
                                od.set(cast(Object) po);
                        }
                        else
                        {
                            if (PropertyPublisher pub = cast(PropertyPublisher) cast(Object) o)
                                restoreFrom(childNode, pub);
                        }
                    }
                    else if (childNode.info.rtti.type == RtType._struct &&
                        childNode.info.rtti.structInfo.type == StructType._publisher)
                    {
                        auto sd = cast(PropDescriptor!GenericStruct*) gd;
                        void* structPtr = sd.getter()().getThis;
                        bool fromRef;

                        if (!structPtr && _onWantAggregate)
                            _onWantAggregate(childNode, structPtr, fromRef);

                        if (fromRef || !structPtr)
                        {
                            void* ps = ReferenceMan.reference(
                                childNode.info.rtti.classInfo.identifier,
                                childNode.identifiersChain
                            );
                            if (ps)
                                sd.set(*cast(GenericStruct*) ps);
                        }
                        else
                        {
                            void* oldCtxt = sd.rtti.structInfo.pubTraits.setContext(structPtr);
                            auto pubStruct = sd.rtti.structInfo.pubTraits;
                            restoreFrom(childNode, pubStruct);
                            sd.rtti.structInfo.pubTraits.restoreContext(oldCtxt);
                        }
                    }
                }
                else
                {
                    restoreFromEvent(childNode, stop);
                    if (stop)
                        return;
                    stop = false;
                }
            }
        }
        static if(is(R == class))
        {
            if (auto pub = cast(PropertyPublisher) root)
                restoreFrom(_rootNode, pub);
            if (_onFinishAggregate)
                _onFinishAggregate(&root);
        }
        else static if (is(R == struct))
        {
            const(Rtti)* rtti = getRtti!R;
            if (rtti.structInfo.type != StructType._publisher)
                return;
            restoreFrom(_rootNode, root);
            if (_onFinishAggregate)
                _onFinishAggregate(&root);
        }
    }

    /**
     * Builds the IST from a $(D Stream) and restores from root.
     *
     * This method calls successively $(D streamToIst()) then $(D istToPublisher()).
     *
     * Params:
     *      inputStream: The $(D Stream) that contains the data previously serialized.
     *      root = The object from where the restoration starts. It must be
     *      a $(D PropertyPublisher) or struct that contains $(D PropertyPublisherImpl).
     */
    void streamToPublisher(R)(Stream inputStream, ref R root,
        SerializationFormat format = defaultFormat)
    if (is(R == struct))
    {
        streamToIst(inputStream, format);
        istToPublisher(root);
    }

    /// ditto
    void streamToPublisher(R)(Stream inputStream, R root,
        SerializationFormat format = defaultFormat)
    if (is(R == class))
    {
        streamToIst(inputStream, format);
        istToPublisher(root);
    }

    /**
     * Builds the IST from a $(D Stream).
     *
     * After the call the IST nodes are not yet linked to their $(D PropDescriptor).
     * The deserialization process can be achieved manually, using $(D findNode())
     * in pair with $(D restoreProperty()) or automatically, using $(D istToPublisher()).
     * This function can also be used to convert from a format to another.
     *
     * Params:
     *      inputStream = The $(D Stream) containing the serialized data.
     *      format = The format of the serialized data.
     */
    void streamToIst(Stream inputStream, SerializationFormat format = defaultFormat)
    {
        _rootNode.deleteChildren;
        _mustRead = false;
        _stream = inputStream;
        _format = format;

        IstNode newNde = _rootNode;
        IstNode parent = _rootNode;

        FormatToken tok = readFormat(_format)(_stream, newNde);
        assert(tok == FormatToken.objBeg, to!string(tok));

        while(tok != FormatToken.end)
        {
            newNde = construct!IstNode;
            tok = readFormat(_format)(_stream, newNde);
            switch (tok)
            {
                case FormatToken.prop:
                    parent.addChild(newNde);
                    newNde.info.level = cast(uint) newNde.level;
                    break;
                case FormatToken.objBeg:
                    parent.addChild(newNde);
                    parent = newNde;
                    newNde.info.level = cast(uint) newNde.level;
                    break;
                case FormatToken.objEnd:
                    assert(parent);
                    if (parent)
                        parent = parent.parent;
                    destruct(newNde);
                    break;
                default:
                    destruct(newNde);
            }
        }
        //
        _stream = null;
    }

    /**
     * Finds the tree node matching to an identifier chain.
     *
     * Params:
     *      cache = Set to true to use a cache. In this case $(D updateCache())
     *      must be called before.
     *      descriptorName = The chain of properties name that identifies the node.
     * Returns:
     *      A reference to the node that matches to the property or nulll.
     */
    IstNode findNode(bool cache = false)(const(char)[] descriptorName)
    {
        if (_rootNode.info.name == descriptorName)
            return _rootNode;

        static if (cache)
            return _nodesCache.find(descriptorName);
        else
        {
            IstNode result;
            deepIterate!((IstNode n)
            {
                if (n.identifiersChain() == descriptorName)
                {
                    result = n;
                    return true;
                }
                else
                {
                    result = null;
                    return false;
                }
            }, "children")(_rootNode);
            return result;
        }
    }

    /**
     * Restores the IST from an arbitrary tree node.
     *
     * The process is lead by the nodeInfo associated to the node.
     * If the descriptor is not defined then the $(D wantDescriptorEvent) is called.
     * It means that this method can be used to deserialize to an arbitrary descriptor,
     * for example after a call to $(D streamToIst()).
     *
     * Params:
     *      node = The IST node from where the restoration starts.
     *      It can be determined by a call to $(D findNode()).
     *      recursive = When set to true the restoration is recursive.
     */
    void nodeToPublisher(IstNode node, bool recursive = false)
    {
        bool restore(IstNode current)
        {
            bool result = true;
            GenericDescriptor* des = cast(GenericDescriptor*) current.info.descriptor;
            if (des && PropHint.dontSet in des.hints)
                return result;
            if (current.info.descriptor && current.info.name ==
                (cast(PropDescriptor!byte*)current.info.descriptor).name)
                    nodeInfo2Declarator(current.info);
            else
            {
                bool stop;
                result = restoreFromEvent(current, stop);
                result &= !stop;
            }
            return result;
        }
        bool restoreLoop(IstNode current)
        {
            if (!restore(current)) return false;
            foreach(child; current.children)
            {
                auto childNode = cast(IstNode) child;
                if (!restore(childNode)) return false;
                if (recursive && (childNode.info.rtti.type == RtType._object ||
                    (childNode.info.rtti.type == RtType._struct &&
                    childNode.info.rtti.structInfo.type == StructType._publisher)))
                    if (!restoreLoop(childNode)) return false;
            }
            return true;
        }
        restoreLoop(node);
    }

    /**
     * Restores the property associated to an IST node using the setter of the
     * PropDescriptor passed as parameter.
     *
     * Params:
     *      node = An IstNode. Can be determined by a call to $(D findNode()).
     *      descriptor = The PropDescriptor whose setter is used to restore the node data.
     *      if not specified then the $(D onWantDescriptor) event is called.
     */
    void nodeToProperty(T)(IstNode node, PropDescriptor!T* descriptor = null)
    {
        if (descriptor && PropHint.dontSet in descriptor.hints)
            return;
        if (descriptorMatchesNode!T(descriptor, node))
        {
            node.info.descriptor = descriptor;
            nodeInfo2Declarator(node.info);
        }
        else
        {
            bool noop;
            restoreFromEvent(node, noop);
        }
    }

//------------------------------------------------------------------------------
//---- miscellaneous  ---------------------------------------------------------+

    /**
     * Updates the cache optionally used in $(D findNode()).
     */
    void updateCache()
    {
        _nodesCache.setRoot(this._rootNode);
    }

    /// The IST can be modified, build, cleaned from the root node
    @property IstNode serializationTree(){return _rootNode;}

    /// Event called when the serializer misses a property descriptor.
    @property WantDescriptorEvent onWantDescriptor(){return _onWantDescriptor;}

    /// ditto
    @property void onWantDescriptor(WantDescriptorEvent value){_onWantDescriptor = value;}

    /// Event called when the serializer misses an aggregate
    @property WantAggregateEvent onWantAggregate(){return _onWantAggregate;}

    /// ditto
    @property void onWantAggregate(WantAggregateEvent value){_onWantAggregate = value;}

    /// Event called when the serializer finishes to destream an aggregate.
    @property void onFinishAggregate(void delegate(void*) value){_onFinishAggregate = value;}

    /// ditto
    @property void delegate(void*) onFinishAggregate(){return _onFinishAggregate;}

//------------------------------------------------------------------------------

}
///
unittest
{
    // defines two serializable classes
    class B: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        @SetGet uint data1 = 1, data2 = 2;
        this(){collectPublications!B;}
        void reset(){data1 = 0; data2 = 0;}
    }
    class A: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        @SetGet B sub1, sub2;
        this()
        {
            sub1 = construct!B;
            sub2 = construct!B;
            // sub1 and sub2 are fully serialized because they already exist
            // when the analyzers run, otherwise they would be considered as
            // reference and their members would not be serialized.
            collectPublications!A;
        }
        ~this(){destructEach(sub1, sub2);}
    }

    MemoryStream stream = construct!MemoryStream;
    Serializer serializer = construct!Serializer;
    A a = construct!A;
    // serializes
    serializer.publisherToStream(a, stream);

    // reset the fields
    a.sub1.reset;
    a.sub2.reset;
    stream.position = 0;
    // deserializes
    serializer.streamToPublisher(stream, a);
    // check the restored values
    assert(a.sub1.data1 == 1);
    assert(a.sub2.data1 == 1);
    assert(a.sub1.data2 == 2);
    assert(a.sub2.data2 == 2);
    // cleanup
    destructEach(a, serializer, stream);
}

//----

// Miscellaneous helpers ------------------------------------------------------+
/**
 * Serializes a $(D PropertyPublisher) to a file.
 *
 * This helper function works in pair with $(D fileToPublisher()).
 *
 * Params:
 *      pub = The $(D PropertyPublisher) to save.
 *      filename = The target file, always created or overwritten.
 *      format = Optional, the serialization format, by default iztext.
 */
void publisherToFile(Object pub, const(char)[] filename,
    SerializationFormat format = defaultFormat,
    WantAggregateEvent wae = null, WantDescriptorEvent wde = null)
{
    MemoryStream str = construct!MemoryStream;
    Serializer ser = construct!Serializer;
    scope(exit) destructEach(str, ser);
    ser.onWantAggregate = wae;
    ser.onWantDescriptor = wde;

    ser.publisherToStream(pub, str, format);
    str.saveToFile(filename);
}

/**
 * Deserializes a file to a $(D PropertyPublisher).
 *
 * This helper function works in pair with $(D publisherToFile()).
 *
 * Params:
 *      filename = The source file.
 *      pub = The target $(D PropertyPublisher).
 *      format = optional, the serialization format, by default iztext.
 */
void fileToPublisher(const(char)[] filename, Object pub,
    SerializationFormat format = defaultFormat,
    WantAggregateEvent wae = null, WantDescriptorEvent wde = null)
{
    MemoryStream str = construct!MemoryStream;
    Serializer ser = construct!Serializer;
    scope(exit) destructEach(str, ser);
    ser.onWantAggregate = wae;
    ser.onWantDescriptor = wde;

    str.loadFromFile(filename);
    ser.streamToPublisher(str, pub, format);
}
//----

version(unittest)
{
    import std.stdio;

    unittest
    {
        char[] text;
        ubyte[] value;
        SerNodeInfo inf;
        //
        value = [13];
        text = "13".dup;
        inf.rtti = getRtti!(typeof(value[0]));
        inf.value = value ;
        assert(value2text(&inf) == text);
        assert(text2value(text, &inf) == value);
        //
        value = [13,14];
        text = "[13, 14]".dup;
        inf.rtti = getRtti!(typeof(value));
        inf.value = value ;
        assert(value2text(&inf) == text);
        assert(text2value(text, &inf) == value);
        //
        void testType(T)(T t)
        {
            char[] asText;
            T v = t;
            SerNodeInfo info;
            PropDescriptor!T descr;
            //
            descr.define(&v, "property");
            setNodeInfo!T(&info, &descr);
            //
            asText = to!string(v).dup;
            assert(value2text(&info) == asText, T.stringof);
            static if (!isArray!T)
                assert(*cast(T*)(text2value(asText, &info)).ptr == v, T.stringof);
            static if (isArray!T)
                assert(cast(ubyte[])text2value(asText, &info)==cast(ubyte[])v,T.stringof);
        }

        struct ImpConv{uint _field; alias _field this;}
        auto ic = ImpConv(8);

        testType('c');
        testType("azertyuiop".dup);
        testType!uint(ic);
        testType(cast(byte)8);      testType(cast(byte[])[8,8]);
        testType(cast(ubyte)8);     testType(cast(ubyte[])[8,8]);
        testType(cast(short)8);     testType(cast(short[])[8,8]);
        testType(cast(ushort)8);    testType(cast(ushort[])[8,8]);
        testType(cast(int)8);       testType(cast(int[])[8,8]);
        testType(cast(uint)8);      testType(cast(uint[])[8,8]);
        testType(cast(long)8);      testType(cast(long[])[8,8]);
        testType(cast(ulong)8);     testType(cast(ulong[])[8,8]);
        testType(cast(float).8f);   testType(cast(float[])[.8f,.8f]);
        testType(cast(double).8);   testType(cast(double[])[.8,.8]);
    }

    unittest
    {
        //foreach(fmt;EnumMembers!SerializationFormat)
        //    testByFormat!fmt();

        testByFormat!(SerializationFormat.iztxt)();
        testByFormat!(SerializationFormat.izbin)();
        //testByFormat!(SerializationFormat.json)();
    }

    class Referenced1 {}

    class ReferencedUser: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        mixin inheritedDtor;

        SerializableReference fSerRef;
        Referenced1 fRef;

        void doRestore(Object sender)
        {
            fRef = fSerRef.restoreReference!Referenced1;
        }

        this()
        {
            fSerRef = construct!SerializableReference;
            fSerRef.onRestored = &doRestore;
            collectPublications!ReferencedUser;
        }

        ~this() {destruct(fSerRef);}

        @Get SerializableReference theReference()
        {
            fSerRef.storeReference!Referenced1(fRef);
            return fSerRef;
        }
        @Set void theReference(SerializableReference value)
        {
            // when a sub publisher is owned the setter is a noop.
            // actually the serializer uses the descriptor getter
            // to know where the members of the sub pub. are located.
        }
    }

    class ClassA: ClassB
    {
        mixin inheritedDtor;
        private:
            ClassB _aB1, _aB2;
            PropDescriptor!Object* aB1descr, aB2descr;
        public:
            this()
            {
                assert(!this.publicationFromName("aB1"));
                assert(!this.publicationFromName("aB2"));
                _aB1 = construct!ClassB;
                _aB2 = construct!ClassB;
                aB1descr = construct!(PropDescriptor!Object)(cast(Object*)&_aB1, "aB1");
                aB2descr = construct!(PropDescriptor!Object)(cast(Object*)&_aB2, "aB2");
                aB1descr.declarator = this;
                aB2descr.declarator = this;

                // add publications by hand.
                _publishedDescriptors ~= cast(void*) aB1descr;
                _publishedDescriptors ~= cast(void*) aB2descr;

                // without the scanners ownership must be set manually
                setTargetObjectOwner(aB1descr, this);
                setTargetObjectOwner(aB2descr, this);
                assert(targetObjectOwnedBy(aB1descr, this));
                assert(targetObjectOwnedBy(aB2descr, this));
            }
            ~this()
            {
                destructEach(_aB1, _aB2);
                callInheritedDtor;
            }
            override void reset()
            {
                super.reset;
                _aB1.reset;
                _aB2.reset;
            }
    }

    class ClassB : PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        mixin inheritedDtor;

        private:
            Array!int  _anIntArray;
            Array!char _someChars;
            @SetGet float  aFloat;

        public:

            this()
            {
                collectPublications!ClassB;
                aFloat = 0.123456f;
                _someChars = "azertyuiop";
                anIntArray = [0, 1, 2, 3];
            }

            ~this()
            {
                destructEach(_anIntArray, _someChars);
                callInheritedDtor;
            }

            void reset()
            {
                _anIntArray.length = 0;
                _someChars.length = 0;
                aFloat = 0.0f;
            }

            @Set anIntArray(int[] value){_anIntArray = value;}
            @Get int[] anIntArray(){return _anIntArray[];}
            @Set someChars(char[] value){_someChars = value;}
            @Get char[] someChars(){return _someChars[];}
    }

    void testByFormat(SerializationFormat format)()
    {
        ReferenceMan.clear;

        MemoryStream str  = construct!MemoryStream;
        Serializer ser    = construct!Serializer;
        ClassB b = construct!(ClassB);
        ClassA a = construct!(ClassA);
        scope(exit) destructEach(str, ser, b, a);

        // basic sequential store/restore ---+
        ser.publisherToStream(b,str,format);
        //str.saveToFile("a.txt");
        b.reset;
        assert(b.anIntArray == []);
        assert(b.aFloat == 0.0f);
        assert(b.someChars == "");
        str.position = 0;
        ser.streamToPublisher(str,b,format);

        str.clear;
        ser.serializationTree.saveToStream(str);
        //str.saveToFile("f.txt");

        assert(b.anIntArray == [0, 1, 2, 3], to!string(b.anIntArray));
        assert(b.aFloat == 0.123456f);
        assert(b.someChars == "azertyuiop");
        //----

        // arbitrarily find a prop ---+
        assert(ser.findNode!false("root.anIntArray"));
        assert(ser.findNode!false("root.aFloat"));
        assert(ser.findNode!false("root.someChars"));
        assert(!ser.findNode!false("root."));
        assert(!ser.findNode!false("aFloat"));
        assert(!ser.findNode!false("root.someChar"));
        assert(!ser.findNode!false(""));

        ser.updateCache;
        assert(ser.findNode!true("root.anIntArray"));
        assert(ser.findNode!true("root.aFloat"));
        assert(ser.findNode!true("root.someChars"));
        assert(!ser.findNode!true("root."));
        assert(!ser.findNode!true("aFloat"));
        assert(!ser.findNode!true("root.someChar"));
        assert(!ser.findNode!true(""));
        //----

        // restore elsewhere that in the declarator ---+
        float outside;
        auto node = ser.findNode("root.aFloat");
        auto aFloatDescr = PropDescriptor!float(&outside, "aFloat");
        ser.nodeToProperty(node, &aFloatDescr);
        assert(outside == 0.123456f);
        //----

        // nested declarations with super.declarations ---+
        str.clear;
        ser.publisherToStream(a,str,format);
        a.reset;
        assert(a.anIntArray == []);
        assert(a.aFloat == 0.0f);
        assert(a.someChars == "");
        assert(a._aB1.anIntArray == []);
        assert(a._aB1.aFloat == 0.0f);
        assert(a._aB1.someChars == "");
        assert(a._aB2.anIntArray == []);
        assert(a._aB2.aFloat == 0.0f);
        assert(a._aB2.someChars == "");
        str.position = 0;

        ser.streamToPublisher(str,a,format);
        assert(a.anIntArray == [0, 1, 2, 3]);
        assert(a.aFloat ==  0.123456f);
        assert(a.someChars == "azertyuiop");
        assert(a._aB1.anIntArray == [0, 1, 2, 3]);
        assert(a._aB1.aFloat ==  0.123456f);
        assert(a._aB1.someChars == "azertyuiop");
        assert(a._aB2.anIntArray == [0, 1, 2, 3]);
        assert(a._aB2.aFloat ==  0.123456f);
        assert(a._aB2.someChars == "azertyuiop");
        //----

        // store & restore a serializable reference ---+
        auto ref1 = construct!(Referenced1);
        auto ref2 = construct!(Referenced1);
        auto usrr = construct!(ReferencedUser);
        scope(exit) destructEach(ref1, ref2, usrr);

        assert(ReferenceMan.storeReference!Referenced1(ref1, "referenced.ref1"));
        assert(ReferenceMan.storeReference!Referenced1(ref2, "referenced.ref2"));
        assert(ReferenceMan.referenceID!Referenced1(ref1) == "referenced.ref1");
        assert(ReferenceMan.referenceID!Referenced1(ref2) == "referenced.ref2");
        assert(ReferenceMan.reference!Referenced1("referenced.ref1") == ref1);
        assert(ReferenceMan.reference!Referenced1("referenced.ref2") == ref2);

        str.clear;
        usrr.fRef = ref1;
        ser.publisherToStream(usrr, str, format);

        usrr.fRef = ref2;
        assert(usrr.fRef == ref2);
        str.position = 0;
        ser.streamToPublisher(str, usrr, format);
        assert(usrr.fRef == ref1);

        usrr.fRef = null;
        assert(usrr.fRef is null);
        str.position = 0;
        ser.streamToPublisher(str, usrr, format);
        assert(usrr.fRef is ref1);

        str.clear;
        usrr.fRef = null;
        ser.publisherToStream(usrr, str, format);
        usrr.fRef = ref2;
        assert(usrr.fRef is ref2);
        str.position = 0;
        ser.streamToPublisher(str, usrr, format);
        assert(usrr.fRef is null);
        //----

        // auto store, stream to ist, restores manually ---+
        str.clear;
        ser.publisherToStream(b,str,format);
        b.reset;
        assert(b.anIntArray == []);
        assert(b.aFloat == 0.0f);
        assert(b.someChars == "");
        str.position = 0;
        ser.streamToIst(str,format);
        //
        auto node_anIntArray = ser.findNode("root.anIntArray");
        if(node_anIntArray) ser.nodeToProperty(node_anIntArray,
             b.publication!(int[])("anIntArray"));
        else assert(0);
        auto node_aFloat = ser.findNode("root.aFloat");
        if(node_aFloat) ser.nodeToProperty(node_aFloat,
            b.publication!float("aFloat"));
        else assert(0);
        auto node_someChars = ser.findNode("root.someChars");
        if(node_someChars) ser.nodeToProperty(node_someChars,
           b.publication!(char[])("someChars"));
        else assert(0);
        assert(b.anIntArray == [0, 1, 2, 3]);
        assert(b.aFloat == 0.123456f);
        assert(b.someChars == "azertyuiop");
        //----

        // decomposed de/serialization phases with event ---+
        void wantDescr(IstNode node, ref Ptr matchingDescriptor, out bool stop)
        {
            immutable string chain = node.parentIdentifiersChain;
            if (chain == "root")
                matchingDescriptor = a.publicationFromName(node.info.name[]);
            else if (chain == "root.aB1")
                matchingDescriptor = a._aB1.publicationFromName(node.info.name[]);
            else if (chain == "root.aB2")
                matchingDescriptor = a._aB2.publicationFromName(node.info.name[]);
        }

        str.clear;
        ser.publisherToIst(a);
        ser.istToStream(str,format);
        a.reset;
        assert(a.anIntArray == []);
        assert(a.aFloat == 0.0f);
        assert(a.someChars == "");
        assert(a._aB1.anIntArray == []);
        assert(a._aB1.aFloat == 0.0f);
        assert(a._aB1.someChars == "");
        assert(a._aB2.anIntArray == []);
        assert(a._aB2.aFloat == 0.0f);
        assert(a._aB2.someChars == "");
        str.position = 0;
        ser.onWantDescriptor = &wantDescr;
        ser.streamToIst(str,format);
        auto nd = ser.findNode("root");
        assert(nd);
        ser.nodeToPublisher(nd, true);
        assert(a.anIntArray == [0, 1, 2, 3]);
        assert(a.aFloat ==  0.123456f);
        assert(a.someChars == "azertyuiop");
        assert(a._aB1.anIntArray == [0, 1, 2, 3]);
        assert(a._aB1.aFloat ==  0.123456f);
        assert(a._aB1.someChars == "azertyuiop");
        assert(a._aB2.anIntArray == [0, 1, 2, 3]);
        assert(a._aB2.aFloat ==  0.123456f);
        assert(a._aB2.someChars == "azertyuiop");
        ser.onWantDescriptor = null;
        // ----

        // struct serialized as basicType ---+
        import iz.enumset: EnumSet, Set8;
        enum A {a0,a1,a2}
        alias SetofA = EnumSet!(A,Set8);

        class Bar: PropertyPublisher
        {
            mixin PropertyPublisherImpl;

            private:
                SetofA _set;
                PropDescriptor!SetofA setDescr;
            public:
                this()
                {
                    setDescr.define(&_set,"set");
                    with(A) _set = SetofA(a1,a2);
                    collectPublications!Bar;
                }
                // struct can only be serialized using a representation
                // whose type is iteself serializable
                @Set void set(ubyte value){_set = value;}
                @Get ubyte set(){return _set.container;}
        }

        str.clear;
        auto bar = construct!Bar;
        scope(exit) bar.destruct;

        ser.publisherToStream(bar, str, format);
        bar._set = [];
        str.position = 0;
        ser.streamToPublisher(str, bar, format);
        assert( bar._set == SetofA(A.a1,A.a2), to!string(bar.set));
        // ----
    }

    // test fields renamed between two versions ---+
    class Source: PropertyPublisher
    {
        @GetSet private uint _a = 78;
        @GetSet private char[] _b = "foobar".dup;
        mixin PropertyPublisherImpl;
        this(){collectPublications!Source;}
    }

    class Target: PropertyPublisher
    {
        @GetSet private int _c;
        @GetSet private ubyte[] _d;
        mixin PropertyPublisherImpl;
        this(){collectPublications!Target;}
    }

    unittest
    {
        Source source = construct!Source;
        Target target = construct!Target;
        Serializer ser = construct!Serializer;
        MemoryStream str = construct!MemoryStream;
        scope(exit) destructEach(source, ser, str, target);

        ser.publisherToStream(source, str, SerializationFormat.izbin);
        str.position = 0;

        void error(IstNode node, ref Ptr matchingDescriptor, out bool stop)
        {
            if (node.info.name == "a")
                matchingDescriptor = target.publication!(int)("c");
            else if (node.info.name == "b")
                matchingDescriptor = target.publication!(ubyte[])("d");
            stop = false;
        }
        ser.onWantDescriptor = &error;
        ser.streamToPublisher(str, target, SerializationFormat.izbin);
        assert(target._c == 78);
        assert(cast(char[])target._d == "foobar");
    }
    //----

    // test the RuntimeTypeInfo-based serialization ----+
    enum E:byte {e0 = 1, e1 = 2}

    class SubPublisher: PropertyPublisher
    {
        // fully serialized (initializer is MainPub)
        mixin PropertyPublisherImpl;
        @SetGet char[] _someChars = "awhyes".dup;
        this(){collectPublicationsFromFields!SubPublisher;}
    }

    class RefPublisher: PropertyPublisher
    {
        // only ref is serialized (initializer is not MainPub)
        mixin PropertyPublisherImpl;
        this(){collectPublicationsFromFields!RefPublisher;}
        @SetGet uint _a;
    }

    class MainPublisher: PropertyPublisher
    {
        mixin PropertyPublisherImpl;
        mixin inheritedDtor;

        // target when _subPublisher wont be found
        SubPublisher _anotherSubPubliser;

        // the sources for the references
        void delegate(uint) _delegateSource;
        RefPublisher _refPublisherSource;
        string dgTest;

        @SetGet E _e;
        @SetGet ubyte _a = 12;
        @SetGet byte _b = 21;
        @SetGet byte _c = 31;
        @SetGet void delegate(uint) _delegate;
        MemoryStream _stream;

        Array!char _t;
        @Set t(char[] value){_t = value;}
        @Get char[] t(){return _t;}

        @SetGet RefPublisher _refPublisher; //initially null, so it's a ref.
        @SetGet SubPublisher _subPublisher; //initially assigned so 'this' is the owner.

        this()
        {
            _t = "line1\"inside dq\"\nline2\nline3".dup;
            _refPublisherSource = construct!RefPublisher; // not published
            _subPublisher = construct!SubPublisher;
            _anotherSubPubliser = construct!SubPublisher;
            _stream = construct!MemoryStream;
            _stream.writeUbyte(0XFE);
            _stream.writeUbyte(0XFD);
            _stream.writeUbyte(0XFC);
            _stream.writeUbyte(0XFB);
            _stream.writeUbyte(0XFA);
            _stream.writeUbyte(0XF0);

            // collect publications before ref are assigned
            collectPublications!MainPublisher;

            _delegateSource = &delegatetarget;
            _delegate = _delegateSource;
            _refPublisher = _refPublisherSource; // e.g assingation during runtime

            assert(_refPublisher.declarator !is this);
            assert(_refPublisher.declarator is null);

            auto dDescr = publication!GenericDelegate("delegate", false);
            assert(dDescr);

            auto strDesc = publicationFromName("stream");
            assert(strDesc);

            ReferenceMan.storeReference(_refPublisherSource,
                "root.refPublisher");
            ReferenceMan.storeReference(&_delegateSource,
                "root.delegate");

            alias DGT = typeof(_delegateSource);
            assert(*cast(DGT*) ReferenceMan.reference(DGT.stringof, "root.delegate") ==
                _delegateSource);

            assert(publicationFromName("delegate") != null);
        }
        ~this()
        {
            destruct(_refPublisherSource);
            destruct(_anotherSubPubliser);
            destruct(_stream);
            destruct(_t);
            callInheritedDtor;
        }
        void delegatetarget(uint param){dgTest = "awyesss";}
        void reset()
        {
            _e = E.e1;
            _a = 0; _b = 0; _c = 0; _t = _t.init;
            _subPublisher.destruct;
            _subPublisher = null; // wont be found anymore during deser.
            _anotherSubPubliser._someChars = "".dup;
            _delegate = null;
            _refPublisher = null;
            _stream.size = 0;
        }
        @Get Stream stream()
        {
            return _stream;
        }
        @Set void stream(Stream str)
        {
            str.position = 0;
            _stream.loadFromStream(str);
            _stream.position = 0;
            assert(str.size > 0);
        }
    }

    unittest
    {
        MainPublisher c = construct!MainPublisher;
        Serializer ser = construct!Serializer;
        MemoryStream str = construct!MemoryStream;
        scope(exit) destructEach(c, ser, str);

        void objectNotFound(IstNode node, ref void* serializable, out bool fromReference)
        {
            if (node.info.name == "subPublisher")
            {
                serializable = cast(void*) c._anotherSubPubliser;
            }
            if (node.info.name == "refPublisher")
                fromReference = true;
        }

        ser.onWantAggregate = &objectNotFound;
        ser.publisherToStream(c, str);
        //str.saveToFile(r"test.txt");
        //
        c.reset;
        str.position = 0;
        ser.streamToPublisher(str, c);
        //
        assert(c._a == 12);
        assert(c._b == 21);
        assert(c._c == 31);
        assert(c._e == E.e0);
        assert(c._t == "line1\"inside dq\"\nline2\nline3", c._t);
        assert(c._refPublisher == c._refPublisherSource);
        assert(c._anotherSubPubliser._someChars == "awhyes");
        assert(c._delegate);
        assert(c._delegate.funcptr == c._delegateSource.funcptr);
        assert(c._delegate.ptr == c._delegateSource.ptr);

        c._delegate(123);
        assert(c.dgTest == "awyesss");

        assert(c._stream.readUbyte == 0xFE);
        assert(c._stream.readUbyte == 0xFD);
        assert(c._stream.readUbyte == 0xFC);
        assert(c._stream.readUbyte == 0xFB);
        assert(c._stream.readUbyte == 0xFA);
        assert(c._stream.readUbyte == 0xF0);
    }
    //----

    // test generic Reference restoring ---+
    class HasGenRef: PropertyPublisher
    {
        // the source, usually comes from outside
        Object source;
        // what's gonna be assigned
        Object target;
        mixin PropertyPublisherImpl;
        this()
        {
            collectPublications!HasGenRef;
            source = construct!Object;
            ReferenceMan.storeReference!void(cast(void*)source,"thiswillwork");
            target = source;
        }
        ~this()
        {
            destruct(source);
        }

        @Get const(char[]) objectReference()
        {
            // get ID from what's currently assigned
            return ReferenceMan.referenceID!void(cast(void*)target);
        }

        @Set void objectReference(const(char[]) value)
        {
            // ID -> Reference -> assign the variable
            target = cast(Object) ReferenceMan.reference!void(value);
        }
    }

    unittest
    {
        MemoryStream str = construct!MemoryStream;
        Serializer ser = construct!Serializer;
        HasGenRef obj = construct!HasGenRef;
        scope(exit) destructEach(ser, str, obj);

        ser.publisherToStream(obj, str);
        str.position = 0;
        obj.target = null;

        ser.streamToPublisher(str, obj);
        assert(obj.target == obj.source);
    }
    //----

    // test publishing struct
    unittest
    {
        static struct PubStruct
        {
            mixin PropertyPublisherImpl;

            @SetGet uint _ui = 8;
            @SetGet char[] _cs = "8".dup;
        }

        PubStruct ps;
        ps.collectPublications!PubStruct;

        assert(ps.publicationFromName("ui") != null);
        assert(ps.publicationFromName("cs") != null);

        MemoryStream str = construct!MemoryStream;
        Serializer ser = construct!Serializer;
        scope(exit) destructEach(str, ser);
        //
        ser.publisherToStream(ps, str);
        ps._cs = ps._cs.init;
        ps._ui = 0;
        //
        str.position = 0;
        ser.streamToPublisher(str, ps);
        assert(ps._cs == "8");
        assert(ps._ui == 8);
    }

    // test text struct
    unittest
    {
        static struct TextStruct
        {
            const(char)[] _value = "content backup".dup;
            const(char)[] saveToText(){return _value;}
            void loadFromText(const(char)[] value){_value = value.dup;}
        }

        class TextStructParent: PropertyPublisher
        {
            mixin PropertyPublisherImpl;
            @SetGet TextStruct _ts;
        }

        TextStructParent tsp = construct!TextStructParent;
        tsp.collectPublications!TextStructParent;
        assert(tsp.publicationFromName("ts") != null);
        assert(tsp.publicationFromIndex(0) != null);
        assert(tsp._ts._value == "content backup");

        MemoryStream str = construct!MemoryStream;
        Serializer ser = construct!Serializer;
        scope(exit) destructEach(str, ser, tsp);

        ser.publisherToStream(tsp, str);
        //str.saveToFile("textstruct.txt");
        tsp._ts._value = "";

        str.position = 0;
        ser.streamToPublisher(str, tsp);
        assert(tsp._ts._value == "content backup", tsp._ts._value);
    }

    // test bin struct
    unittest
    {
        static struct BinStruct
        {
            ubyte[] _value = cast(ubyte[])"content backup".dup;
            ubyte[] saveToBytes(){return _value;}
            void loadFromBytes(ubyte[] value){_value = value;}
        }

        class BinStructParent: PropertyPublisher
        {
            mixin PropertyPublisherImpl;
            @SetGet BinStruct _bs;
        }

        BinStructParent bsp = construct!BinStructParent;
        bsp.collectPublications!BinStructParent;
        assert(bsp.publicationFromName("bs") != null);
        assert(bsp.publicationFromIndex(0) != null);
        assert(bsp._bs._value == "content backup");

        MemoryStream str = construct!MemoryStream;
        Serializer ser = construct!Serializer;
        scope(exit) destructEach(str, ser, bsp);

        ser.publisherToStream(bsp, str);
        //str.saveToFile("binstruct.txt");
        bsp._bs._value = bsp._bs._value.init;

        str.position = 0;
        ser.streamToPublisher(str, bsp);
        assert(bsp._bs._value == "content backup");
    }

    // test nested publishing structs, detected from fields
    unittest
    {
        static struct Child
        {
            mixin PropertyPublisherImpl;
            @SetGet int _a = 8;
            @SetGet int _b = 9;
        }
        static struct Parent
        {
            mixin PropertyPublisherImpl;
            @SetGet Child _child1;
            @SetGet Child _child2;
        }

        Serializer ser = construct!Serializer;
        MemoryStream str = construct!MemoryStream;
        scope(exit) destructEach(str, ser);
        Parent parent;
        parent.collectPublications!Parent;
        parent._child1.collectPublications!Child;
        parent._child2.collectPublications!Child;

        assert(parent.publicationFromName("child1") != null);
        assert(parent.publicationFromName("child2") != null);
        assert(parent._child1.publicationFromName("a") != null);
        assert(parent._child2.publicationFromName("b") != null);

        ser.publisherToStream(parent, str);
        parent._child1._a = 0;
        parent._child1._b = 0;
        parent._child2._a = 0;
        parent._child2._b = 0;

        str.position = 0;
        ser.streamToPublisher(str, parent);
        assert(parent._child1._a == 8);
        assert(parent._child1._b == 9);
        assert(parent._child2._a == 8);
        assert(parent._child2._b == 9);
    }

    // test nested publishing structs, detected from get/set pair
    unittest
    {
        static struct Child
        {
            mixin PropertyPublisherImpl;
            @SetGet int _a = 8;
            @SetGet int _b = 9;
        }
        static struct Parent
        {
            mixin PropertyPublisherImpl;
            Child _child1;
            @Get ref Child child1(){return _child1;}
            @Set void child1(Child value) {}
            ~this() {destruct(_child1);}
        }

        Serializer ser = construct!Serializer;
        MemoryStream str = construct!MemoryStream;
        scope(exit) destructEach(str, ser);
        Parent parent;
        parent.collectPublications!Parent;
        parent._child1.collectPublications!Child;

        assert(parent.publicationFromName("child1") != null);
        assert(parent._child1.publicationFromName("a") != null);
        assert(parent._child1.publicationFromName("b") != null);

        ser.publisherToStream(parent, str);
        parent._child1._a = 0;
        parent._child1._b = 0;

        str.position = 0;
        ser.streamToPublisher(str, parent);
        assert(parent._child1._a == 8);
        assert(parent._child1._b == 9);
    }

    // PropHints
    unittest
    {
        class Foo: PropertyPublisher
        {
            mixin PropertyPublisherImpl;

            @PropHints(PropHint.dontGet)
            @SetGet int _i = 1;
            @PropHints(PropHint.dontSet)
            @SetGet int _j = 1;
            @PropHints(PropHint.initCare)
            @SetGet int _k = 0;

            this()
            {
                collectPublications!Foo;
            }
        }

        Foo foo = construct!Foo;
        MemoryStream str = construct!MemoryStream;
        Serializer ser = construct!Serializer;
        scope(exit) destructEach(foo, ser, str);

        ser.publisherToStream(foo, str);
        assert(ser.findNode("root.i") is null);  // dontGet, so not in IST

        assert(ser.findNode("root.k") !is null); // _k was equal to 0
        assert(ser.findNode("root.j") !is null); // in IST...

        foo._i = 0;
        foo._j = 0;
        foo._k = 1;
        str.position = 0;
        ser.streamToPublisher(str, foo);
        assert(foo._k == 0);
        assert(foo._i == 0);
        assert(foo._j == 0); //...but not restored
    }
}

