#include <observatory/pyplot_sink.hpp>

int main() {
    observatory::PyplotSink::datapoint("test.py", "series1", 1, 4.0, "hello!");
    observatory::PyplotSink::datapoint("test.py", "series1", 1, 4.5, "hello!");
    observatory::PyplotSink::datapoint("test.py", "series1", 2, 8.0, "hello!");
    observatory::PyplotSink::datapoint("test.py", "series1", 2, 9.0, "hello!");

    // RAII api
    observatory::PyplotSink sink("test.py");
    sink.datapoint("series2", 1, 3.0);
    sink.datapoint("series2", 1, 3.0);
    sink.datapoint("series2", 2, 7.9);
    sink.datapoint("series2", 2, 10.2);
}
