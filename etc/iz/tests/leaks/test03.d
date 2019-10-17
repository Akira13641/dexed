module test03;

import
    iz.memory, iz.containers;

void main() @nogc
{
    static ubyte[8] a = [0,1,2,3,4,5,6,7];
    static ubyte[8] b = [0,1,2,1,4,5,6,7];
    static ubyte[8] c = [0,1,2,1,4,5,7,7];
    {
        HashSet_LP!(ubyte[8]) commands;
        commands.reserve(15);
        commands.insert(a);
        commands.insert(b);
        HashSet_LP!(ubyte[8]) clone = commands;
        commands.clear;
        commands.insert(a);
        commands.insert(b);
        commands.minimize;
        commands.insert(b);
        commands.remove(a);
        destruct(commands);
        destruct(clone);
    }
    {
        HashSet_AB!(ubyte[8]) commands;
        commands.reserve(15);
        commands.insert(a);
        commands.insert(b);
        HashSet_AB!(ubyte[8]) clone = commands;
        commands.clear;
        commands.insert(a);
        commands.insert(b);
        commands.minimize;
        commands.insert(b);
        commands.remove(a);
        destruct(commands);
        destruct(clone);
    }
}
