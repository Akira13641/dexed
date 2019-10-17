## iz [![CI Status](https://travis-ci.org/BBasile/iz.svg)](https://travis-ci.org/BBasile/iz) [![codecov](https://codecov.io/gh/BBasile/iz/branch/master/graph/badge.svg)](https://codecov.io/gh/BBasile/iz) ![](https://img.shields.io/github/tag/BBasile/iz.svg)

### Introduction

_iz_ is a general purpose library for the D programming language.
It includes [streams, containers, a serializer, property binder, Pascal-like sets, Pascal-like properties](https://iz.dpldocs.info/iz.html) and more.

### Runtime incompatibilities

#### (_dont_) Destroy

This library experiments manually managed lifetime.
Most of the classes declared in _iz_ are not compatible with `new` and `destroy`, instead [`construct` and `destruct`](https://iz.dpldocs.info/iz.memory.html#function) must be used.

- `destroy` calls the destructors from the most derived to the base using the dynamic type information of each generation and without considering the static type of the argument (which may be correct).
- `destruct` calls the destructor defined for the static type of the argument unless it is passed after a cast to `Object`. Even in this case, only the most derived destructor gets looked-up using the type information. Once found, the other destructors are called by inheritance (i.e like what does `super()` to constructors). This system, since not built into the language, is provided with a [mixin template](https://iz.dpldocs.info/iz.memory.inheritedDtor.html).

As a consequence, `destroy` or the GC will segfault, due to a double call to the destructors, if _iz_ classes that do not directly inherit from `Object` (i.e non-empty inheritance list) are not accurately handled with `construct` and `destruct`.

#### D arrays

Still because of manual memory management, D arrays (e.g `int[] a;`) are not really usable in _iz_ classes.

A D arrays consists of two members. One of them is a pointer that's managed by the garbage collector.
When a D array is used in an Iz class, the GC may thinks that the array is not used anymore.
By default this library uses [GC-free arrays](https://iz.dpldocs.info/iz.containers.Array.html in its classes and other aggregates.
In the same fashion, the library contains hashsets and hashmaps that are based on the GC-free array.
Any container based on custom allocators will also be compatible with _iz_ class system.

In order to use default D arrays two workarounds exist:
- call `GC.disable` at the beginning of the program.
- use `std.experimental.allocator` functions `makeArray()`, `shrinkArray()` and `expandArray()` with `Mallocator.instance` and never `.length` and `Appender`. Uses local copies in order to work safely with Phobos algorithms.

Note that D arrays are not affected when used as local variables.
The lifetime of the aggregates is often known and deterministic so the associated memory can then be considered as polluting the GC heap.

### Pascal influences

#### EnumSet

[Iz's Enumset](https://iz.dpldocs.info/iz.enumset.EnumSet.html) is similar to the Pascla `set of` construct.

In Pascal we write

```pascal
procedure test();
type
 Option = (o1, o2, o3);
 Options = set of Option;
var
  Options opts;
begin
  opts += [o1];
  assert(o1 in opts);
end;
```

in D with iz:
```d
void test()
{
    import iz.enumset;
    enum Option {o1, o2, o3}
    EnumSet!(Option, Set8) opts;
    opts += [Option.o1];
    assert(o1 in opts);
}
```
With the same performances, i.e lookup is O(1).

#### Properties and serialization

[Iz's serializer](https://iz.dpldocs.info/iz.serializer.Serializer.html) is influenced by Object streaming, as existing in Delphi or FreePascal.

In Pascal we write

```pascal
uses classes;

TFoo = class(TComponent)
  private _field: integer;
  published field: integer read _field write _field;
end;

procedure writeOne()
var
  str: TMemoryStream;
  one: TFoo;
begin
  one := TFoo.create(nil);
  str := TMemoryStream.create;
  str.WriteComponent(one);
  str.SaveToFile("a.txt");
  one.free; str.free;
end;
```

in D with iz:
```d
import iz.properties, iz.serializer, iz.memory;

class Foo : PropertyPublisher
{
    mixin PropertyPublisherImpl;
    private @SetGet int _field;
    this(){collectPublications!Foo;}
}

void writeOne()
{
    Foo one = construct!Foo;
    publisherToFile(one, "a.txt");
    destruct(one);
}
```

### Big example

You can look at [Kheops](https://github.com/BBasile/kheops/tree/master/src/kheops) source code, which is strongly based on iz (manual memory management, tree structure, serialization, properties, etc are used).

### Build and setup

- using [Coedit](https://github.com/BBasile/Coedit): open the file _iz.coedit_ in the _project_ menu, item _open_, compile, install in the libman.
- using DUB, `dub build --build=release`.
- using shell scripts, see the _scripts_ folder.

### Other

- Iz's memory management [is verified to be leak free on a daily basis](https://github.com/BBasile/iz/blob/master/tests/valgrinder.d).
- warning, unstable API, subject to heavy changes.
- Boost Software License 1.0

