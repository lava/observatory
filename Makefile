PREFIX ?= /usr

examples: examples/thread-recorder examples/named-threads examples/recorder examples/sink

examples/thread-recorder: examples/thread-recorder.cpp include/observatory/instrumentation/perf_sampler.hpp
	g++ $(CFLAGS) examples/thread-recorder.cpp -Iinclude/ -oexamples/thread-recorder -pthread

examples/named-threads: examples/named-threads.cpp include/observatory/instrumentation/perf_sampler.hpp
	g++ $(CFLAGS) examples/named-threads.cpp -Iinclude/ -oexamples/named-threads -pthread

examples/recorder: include/observatory/instrumentation/counters.hpp
	g++ $(CFLAGS) examples/recorder.cpp -Iinclude/ -oexamples/recorder

examples/sink: examples/sink.cpp include/observatory/instrumentation/pyplot_sink.hpp
	g++ $(CFLAGS) examples/sink.cpp -Iinclude/ -oexamples/sink

install:
	@# Create directory
	install -d include/ $(DESTDIR)/$(PREFIX)/include
	@# Fill directory contents
	cp -R --no-target-directory include/ $(DESTDIR)/$(PREFIX)/include


