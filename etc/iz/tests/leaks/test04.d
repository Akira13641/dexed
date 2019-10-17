module test04;

import iz.memory, iz.containers;

struct Item
{
    void test() @nogc {p = getMem(12);}
    ~this() @nogc {freeMem(p);}
    void* p;
}

Array!Item items;
Array!(Item*) pitems;

void main(string[] args)
{
    items.length = 1;
    items[0].test;
    items.length = 0;

    pitems.length = 1;
    pitems[0] = construct!Item;
    destruct(pitems[0]);

    destruct(items);
    destruct(pitems);
}
