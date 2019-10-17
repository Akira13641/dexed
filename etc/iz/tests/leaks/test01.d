module test01;

void main()
{
    import iz.classes, iz.memory, iz.types, std.meta;

    foreach(member; __traits(allMembers, iz.classes))
    {
        alias T = AliasSeq!(__traits(getMember, iz.classes, member))[0];
        static if (is(T == class) && hasDefaultConstructor!T &&
            !__traits(isAbstractClass, T))
        {
            enum okay = is(typeof((){auto a = construct!T();}));
            static if (okay)
            {
                T instance = construct!T();
                destruct(instance);
            }
            else pragma(msg, T);
        }
    }
}
