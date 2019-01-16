#include <observatory/instrumentation/perf_sampler.hpp>

#include <sys/syscall.h>
#include <iostream>

void f() {
    unsigned long x = 0;
    for (int i=0; i<1000; ++i) {
        x += syscall(__NR_gettid);
    }
    std::cout << x << std::endl;
}

struct perf_record_comm {
  struct perf_event_header header;
  uint32_t pid, tid;
  char comm[];
  // struct sample_id sample_id;
} __attribute__((packed));

int main() {
    observatory::ThreadAwareRecorder tr("perf.data", 100, observatory::CounterType::Cycles);

    tr.enable();

    f();

    tr.disable("first execution");
    tr.enable();

    f();

    tr.disable("second execution");
    tr.dump();
}
