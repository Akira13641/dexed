module test02;

import
    iz.memory, iz.containers;
import
    std.conv: to;

struct Element
{
    Array!char text;
    ~this() @nogc {destruct(text);}
}

Array!Element elements;
Array!(size_t*) intPtrs;

void main()
{
    elements.length = 1000;
    foreach(immutable i; 0..elements.length)
        elements[i].text = to!string(i, 16);
    elements.length = 500;
    destruct(elements);

    foreach(immutable i; 0..2000)
        intPtrs ~= newPtr!size_t;
    foreach(immutable i; 0..2000)
        destruct(intPtrs[i]);
    destruct(intPtrs);
}
