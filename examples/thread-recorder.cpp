#include <iostream>
#include <thread>

#include <observatory/instrumentation/perf_sampler.hpp>


void untraced_function() {
	unsigned long x = 0;
	for (int i=0; i<1000; ++i) {
		x += syscall(__NR_getpid);
	}
	std::cout << x << std::endl;
}

void traced_function() {
        unsigned long x = 0;
	for (int i=0; i<1000; ++i) {
		x += syscall(__NR_gettid);
	}
	std::cout << x << std::endl;
}

int main() {
	observatory::ThreadAwareRecorder tr(100, observatory::CounterType::Cycles);

        // Traces for this shouldn't show up in the perf.data
        std::thread t0([&tr] {
		untraced_function();
        });

        // Traces for this should show up in the perf.data
        std::thread t1([&tr] {
                tr.enable();
		traced_function();
		tr.disable();
        });

	t0.join();
	t1.join();

	tr.drain("perf.data");
}
