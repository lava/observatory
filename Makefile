PREFIX ?= /usr

examples: examples/thread-recorder examples/recorder

examples/thread-recorder: examples/thread-recorder.cpp include/observatory/instrumentation/sampling_counters.hpp
	g++ $(CFLAGS) examples/thread-recorder.cpp -Iinclude/ -oexamples/thread-recorder -pthread

examples/recorder: include/observatory/instrumentation/sampling_counters.hpp
	g++ $(CFLAGS) examples/recorder.cpp -Iinclude/ -oexamples/recorder

install:
	@# Create directory
	install -d include/ $(DESTDIR)/$(PREFIX)/include
	@# Fill directory contents
	cp -R --no-target-directory include/ $(DESTDIR)/$(PREFIX)/include


