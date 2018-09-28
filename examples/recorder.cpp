#include <iostream>
#include <observatory/instrumentation/perf_sampler.hpp>

int foo() {
    for (int i=0; i<64; ++i) {
        std::ofstream f("foo.txt", std::ios_base::out | std::ios_base::app);
        f << i;
    }
}


int main() {
    observatory::BoundedRecorder rec(256*1024);
    rec.enable();
    foo();
    rec.disable();
    rec.drain("perf.data");
}
