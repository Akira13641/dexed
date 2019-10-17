module test05;

import std.stdio;

import iz.containers, iz.memory;

struct Item
{
    @NoGc Array!string values;
    ~this()@nogc{}
    this(this) @nogc {values.__postblit;}
}

void main(string[] args)
{
    Array!Item items;
    items.length = 1;
    items[0].values = ["a","b","c"];
    Array!Item old = items;
    items.length = 0;
    old.length = 0;
    destruct(items);
    destruct(old);
}
