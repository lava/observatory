#include <chrono>
#include <fstream>
#include <iomanip>

// The `PyplotSink` is a class that accepts data points and
// generates a python script plotting the data. 
//
// A data point for this class is defined as a `x` and `y`
// coordinate, together with a `tag` identifying the data
// series.
//
// The output file is persistent, so data can be appended
// throughout multiple runs of the same program.


namespace observatory {

// todo - Add an option to specify xlabel as a string, for binned data
// todo - Add an interface to add multiple measurements at once.
class PyplotSink {
public:
    // Implicitly added: Current time, [...]
    // Note: `note` should not contain newlines.
    static void datapoint(const std::string& filename, const std::string& tag, long long x, double y, const char* note = nullptr);

    // RAII interface, avoids re-opening the file every time.
    // todo - add xlabel/ylabel/title/description as constructor parameters
    // todo - probably add flush() and/or close(), also maybe additional constructors
    // to set default tag/x/y/note at construction time.
    PyplotSink(const std::string& filename);
    void datapoint(const std::string& tag, long long x, double y, const char* note = nullptr);

private:
    static std::ofstream prepareFile_(const std::string& filename);
    static void datapoint_(std::ostream& file,  const std::string& tag, long long x, double y, const char* note = nullptr);
    static void preamble_(std::ostream& str);

    std::ofstream output_;
};


inline
PyplotSink::PyplotSink(const std::string& filename)
  : output_(PyplotSink::prepareFile_(filename))
{}


inline
void PyplotSink::datapoint(const std::string& tag, long long x, double y, const char* note)
{
    this->datapoint_(output_, tag, x, y, note);
}


inline void PyplotSink::datapoint(
    const std::string& filename,
    const std::string& tag,
    long long int x,
    double y,
    const char* note)
{
    std::ofstream file = PyplotSink::prepareFile_(filename);
    PyplotSink::datapoint_(file, tag, x, y, note);
    file.close();
}


namespace internal {

inline std::ostream& operator<<(std::ostream& str, std::chrono::system_clock::time_point time_point)
{
    time_t time = std::chrono::system_clock::to_time_t(time_point);
    // ctime() prints the time in a fixed-width 24-byte long
    // format string followed by a newline
    char buffer[26];
    ctime_r(&time, buffer);
    buffer[24] = 0; 
    return str << buffer;
}

} // namespace internal 


inline void PyplotSink::datapoint_(
    std::ostream& file,
    const std::string& tag,
    long long int x,
    double y,
    const char* note)
{
    using namespace internal;
    auto now = std::chrono::system_clock::now();
    file << "\n# Collected " << now << '\n';
    if (note) {
        file << "# " << note << '\n';
    }
    file << "benchmark_data['" << tag << "'][" << x << "].append(" << std::setprecision(16) << std::fixed << y << ")\n";
}


inline std::ofstream PyplotSink::prepareFile_(const std::string& filename)
{
    std::ofstream file(filename, std::ios_base::out | std::ios_base::app);
    bool empty = file.tellp() == 0;

    if (empty) {
        PyplotSink::preamble_(file);
    }

    return file;  
}


inline void PyplotSink::preamble_(std::ostream& str)
{
    str <<
R"_(#!/usr/bin/python

from __future__ import print_function
import atexit
import collections
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm

benchmark_data = collections.defaultdict(lambda: collections.defaultdict(list))

# PROCEDURE DIVISION.

def plot(data):
    plot_data = {}

    ntags = len(data)
    plot_colors = iter(cm.rainbow(np.linspace(0,1,ntags)))
    scatter_colors = iter(cm.rainbow(np.linspace(0,1,ntags)))

    for tag, data_ in data.iteritems():
        plot_data[tag] = {}
        plot_data[tag]["x"] = []
        plot_data[tag]["avg"] = []
        plot_data[tag]["rawx"] = []
        plot_data[tag]["rawy"] = []

        for x, ys in sorted(data_.items()):
            # todo - filter outliers
            mean = np.mean(ys)

            plot_data[tag]["x"].append(x)
            plot_data[tag]["avg"].append(mean)
            plot_data[tag]["rawx"] += [x]*len(ys)
            plot_data[tag]["rawy"] += ys

    plt.title("Insert title")
    plt.xlabel("Insert x-axis label")
    plt.ylabel("Insert y-axis label")
    # todo - this appears below the visible area by default, there
    # should be some way to adjust the plot boundaries.
    plt.annotate("Insert explanation if desired.", (0,0), (0, -40),
        xycoords='axes fraction',
        textcoords='offset points',
        va='top')

    for tag, pltdata in plot_data.iteritems():
        # https://matplotlib.org/api/markers_api.html#module-matplotlib.markers
        plt.scatter(pltdata["rawx"], pltdata["rawy"], marker='x', s=1, c=next(scatter_colors))
        plt.plot(pltdata["x"], pltdata["avg"], ' o', label=tag, color=next(plot_colors))

    plt.legend()
    plt.show()


atexit.register(lambda: plot(benchmark_data))

# DATA DIVISION.

)_";
}


} // namespace observatory
