module acd.arithmetic.internal;

import containers.internal.mixins : AllocatorState;
import std.experimental.allocator.common : stateSize;

import std.meta;
import std.traits;

template perhapsNotAllocator(alias T)
{
    import std.meta;
    static if (stateSize!(typeof(T)) > 0)
        alias perhapsNotAllocator = T;
    else
        alias perhapsNotAllocator = AliasSeq!();
}

// TODO: use allocator here, or prealloc statically.
struct DynamicArray(T)
{
    private T[] data;
    private size_t _length;

    size_t length() const
    {
        return _length;
    }

    this(size_t initialCapacity)
    {
        data = new T[initialCapacity];
        _length = 0;
    }

    void opOpAssign(string op)(auto ref T value) if (op == "~")
    {
        if (_length == data.length)
            data.length *= 2;
        data[_length++] = value;
    }

    // reroute indexing to the underlying array
    ref T opIndex(size_t index)
    {
        assert(index < _length);
        return data[index];
    }

    ref T opIndexAssign(T value, size_t index)
    {
        assert(index < _length);
        return data[index] = value;
    }

    void clear()
    {
        _length = 0;
    }

    T[] sliceTemp()
    {
        return data[0 .. _length];
    }	
}

TTo reinterpret(TTo, TFrom)(TFrom value)
{
    static assert(TFrom.sizeof == TTo.sizeof);
    return *cast(TTo*) &value;
}

template numParameters(alias TDelegate, TArgument, size_t maxParameters = 8)
{
    static foreach (i; 0 .. maxParameters)
    {
        static if (__traits(compiles, delegate(Repeat!(i, TArgument) args) => TDelegate(args)))
            enum numParameters = i;
    }
}
