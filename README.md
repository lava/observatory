# Observatory benchmark utility library

This library contains various tools for observing the runtime
behaviour of a program, in particular related to performance.

It is explicitly *not* intended to be a framework. That means
unlike gbenchmark, etc., the user should not be expected to
set up test benches in order to use this library.

Currently, three utilities have been implemented:

 1) [Perf Counters](include/observatory/counters.hpp) 

 2) [Pyplot Sink](include/observatory/pyplot_sink.hpp) 

 3) [Perf Samplers](include/observatory/perf_sampler.hpp)


While these should work fine, the library was pretty much
salvaged from a bunch of ad-hoc performance experiments
and thus the level of polish is relatively low.


# Design Notes

## Compiler optimizations

Consider this example:

    int foo() {
        ScopedTimer c(&result);

        int x = 0;
        for (int i=0; i<10000; ++i) x += i;
        return x;
    }

The compiler is free to reorder this to

    int foo() {
        ScopedTimer c(&result);
        c.~ScopedTimer();

        int x = 0;
        for (int i=0; i<10000; ++i) x += i;
        return x;
    }

because there is no data dependency between measuring the time point
and computing the return value. (Actually, in my version of gcc this
is done quite aggressively as soon as optimizations are turned on, and
even marking `result` as volatile does not revert it.)

Because this issue is quite subtle to detect and can completely invalidate
benchmarking results, most time measurements within `observatory` take an
additional arbitrary parameter on which to generate a data dependency:

    int foo() {
        ScopedTimer c(&result);
        int x = 0;
        for (int i=0; i<10000; ++i) x += i;
        c.split_after(x);
        return x;
    }
