PREFIX ?= /usr/local

examples: examples/thread-recorder examples/named-threads examples/recorder examples/sink examples/ggsink

examples/thread-recorder: examples/thread-recorder.cpp include/observatory/perf_sampler.hpp
	g++ $(CFLAGS) examples/thread-recorder.cpp -Iinclude/ -oexamples/thread-recorder -pthread

examples/named-threads: examples/named-threads.cpp include/observatory/perf_sampler.hpp
	g++ $(CFLAGS) examples/named-threads.cpp -Iinclude/ -oexamples/named-threads -pthread

examples/recorder: include/observatory/counters.hpp
	g++ $(CFLAGS) examples/recorder.cpp -Iinclude/ -oexamples/recorder

examples/pysink: examples/pyplot_sink.cpp include/observatory/pyplot_sink.hpp
	g++ $(CFLAGS) examples/sink.cpp -Iinclude/ -o$@

examples/ggsink: examples/ggplot_sink.cpp include/observatory/ggplot_sink.hpp
	g++ $(CFLAGS) examples/ggplot_sink.cpp -Iinclude/ -o$@

install:
	@# Create directory
	install -d include/ $(DESTDIR)$(PREFIX)/include
	@# Fill directory contents
	cp -R --no-target-directory include/ $(DESTDIR)$(PREFIX)/include


