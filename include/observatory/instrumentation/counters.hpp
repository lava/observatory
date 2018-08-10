// This header includes methods that can be injected into
// code to easily collect measurements from existing codebases
// (or even running processes later)
// 
// It should stay header-only and as dependency-free as reasonably
// possible.

#pragma once


#include <chrono>
#include <functional>
#include <iostream>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>

#ifndef OBSERVATORY_DISABLE_CPU_COUNTERS

#include <string.h>
#include <sys/types.h>
#include <linux/perf_event.h>
#include <sys/syscall.h>
#include <unistd.h>


namespace observatory {

std::ostream& operator<<(std::ostream& str, std::chrono::system_clock::time_point time_point);
std::ostream& operator<<(std::ostream& str, std::chrono::high_resolution_clock::duration duration);

namespace internal {
namespace perf {

// Trivial wrapper around perf_event_open() syscall to provide type-checking
// for args.
static int
sys_perf_event_open(struct perf_event_attr *attr, pid_t pid, int cpu,
                    int group_fd, unsigned long flags)
{
    return syscall(__NR_perf_event_open, attr, pid, cpu, group_fd, flags);
}

static int createHardwareCounter(enum perf_hw_id type)
{
    struct perf_event_attr pea = {0};
    pea.type = PERF_TYPE_HARDWARE;
    pea.config = type;

    // pid == 0: counter is attached to the current task
    //      TODO - does the above mean current thread, or process?
    //      TODO - can we actually create multiple counters of the
    //             same kind for the same thread/process?
    // cpu == -1: count on all cpu's
    // group_fd == -1: create counter in a new group
    //      TODO - the counters for MultiCounter should be created in
    //             the same group, so they're bound together.
    int fd = sys_perf_event_open(&pea, 0, -1, -1, PERF_FLAG_FD_CLOEXEC);
    if (fd < 0) {
            printf("Error %d: %s\n", errno, strerror(errno));
            if (errno == EACCES) {
                    printf("Try setting /proc/sys/kernel/perf_event_paranoid to 1\n");
            }
            if (errno == ENOSYS) {
                    printf("No CONFIG_PERF_EVENTS=y kernel support configured?\n");
            }
    }

    return fd;
}


static uint64_t readCounter(int fd)
{
    uint64_t value = -1;

    // todo - error handling
    read(fd, &value, sizeof(value));

    return value;
}

}}} // namespace benchmark::internal::perf

#endif // !OBSERVATORY_DISABLE_CPU_COUNTERS


namespace observatory {

struct CpuCounter {
    enum struct Type {
        Instructions = PERF_COUNT_HW_INSTRUCTIONS,
        Cycles = PERF_COUNT_HW_CPU_CYCLES,
    };

    CpuCounter(Type type);
    ~CpuCounter();

    void reset();

    uint64_t split() const;

private:
    int fd_;
    uint64_t initial_;
};

inline CpuCounter::CpuCounter(Type type)
  : fd_(internal::perf::createHardwareCounter(static_cast<perf_hw_id>(type)))
  , initial_(internal::perf::readCounter(fd_))
{}

inline CpuCounter::~CpuCounter()
{
    ::close(fd_);
}

inline void CpuCounter::reset()
{
    initial_ = internal::perf::readCounter(fd_);
}

inline uint64_t CpuCounter::split() const
{
  uint64_t count = internal::perf::readCounter(fd_);
  return count - initial_;
}

// todo - templatize on clock type?
struct TimeCounter {
    typedef std::chrono::high_resolution_clock::time_point timepoint;

    TimeCounter();

    void reset();

    std::chrono::nanoseconds split() const;

private:
    timepoint initial_;
};

inline TimeCounter::TimeCounter()
  : initial_(std::chrono::high_resolution_clock::now())
{}

inline void TimeCounter::reset()
{
  initial_ = std::chrono::high_resolution_clock::now();
}

inline std::chrono::nanoseconds TimeCounter::split() const
{
  auto now = std::chrono::high_resolution_clock::now();
  return now - initial_;
}


class ScopedSideChannelCollector
{
public:
    ScopedSideChannelCollector(const char* filename, const std::string& annotation);
    ~ScopedSideChannelCollector(); // todo - rule of three

    template<typename T>
    void wait_for(const T& t) { asm volatile ("" : : "r"(&t)); }

private:
    const char* filename_;
    int exceptions_;
    std::string annotation_;

    TimeCounter walltime_;
    CpuCounter cycles_;
    CpuCounter instructions_;
};


inline ScopedSideChannelCollector::ScopedSideChannelCollector(const char* filename, const std::string& annotation)
  : filename_(filename)
#ifdef __cpp_lib_uncaught_exceptions
  , exceptions_(std::uncaught_exceptions())
#endif
  , annotation_(annotation)
  , cycles_(CpuCounter::Type::Cycles)
  , instructions_(CpuCounter::Type::Instructions)
{}


inline ScopedSideChannelCollector::~ScopedSideChannelCollector()
{
#ifdef __cpp_lib_uncaught_exceptions
    if (std::uncaught_exceptions() > exceptions_) {
        return;
    }
#endif

    // Measure in decreasing order of sensitivity.
    uint64_t instructions = instructions_.split();
    uint64_t cycles = cycles_.split();
    auto walltime = walltime_.split();

    std::ofstream file(filename_, std::ios_base::out | std::ios_base::app);
    auto now = std::chrono::system_clock::now();

    file << '\n';
    file << "# " << annotation_ << '\n';
    file << "# " << now << '\n';
    file << "instructions = " << instructions << '\n';
    file << "cycles = " << cycles << '\n';
    file << "walltime = " << walltime << std::endl;
}


class MultiCounter
{
public:
    MultiCounter();

    void reset();

    struct Measurement {
        uint64_t instructions;
        uint64_t cycles;
        std::chrono::high_resolution_clock::duration walltime;
    };

    template<typename T>
    Measurement split_after(const T& t);

private:
    TimeCounter walltime_;
    CpuCounter cycles_;
    CpuCounter instructions_;
};

inline MultiCounter::MultiCounter()
  : cycles_(CpuCounter::Type::Cycles)
  , instructions_(CpuCounter::Type::Instructions)
{}

inline void MultiCounter::reset()
{
    // We can't do this atomically, so at least we order it
    // from least to most sensitive.
    walltime_.reset();
    cycles_.reset();
    instructions_.reset();
}


template<typename T>
MultiCounter::Measurement MultiCounter::split_after(const T& t)
{
    asm volatile ("" : : "r"(&t));

    uint64_t instructions = instructions_.split();
    uint64_t cycles = cycles_.split();
    auto walltime = walltime_.split();

    return Measurement {instructions, cycles, walltime};
}

class PyplotScriptFormatter {
public:
    static std::ostream& preamble(std::ostream& str);
    static std::ostream& datapoint(std::ostream& str, const char* tag, long long int xpos, const MultiCounter::Measurement& m);
};

inline std::ostream& PyplotScriptFormatter::preamble(std::ostream& str)
{
    return str <<
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
    quantity = "cycles"

    ntags = len(data)
    plot_colors = iter(cm.rainbow(np.linspace(0,1,ntags)))
    scatter_colors = iter(cm.rainbow(np.linspace(0,1,ntags)))

    for tag, data_ in data.iteritems():
        plot_data[tag] = {}
        plot_data[tag]["x"] = []
        plot_data[tag]["avg"] = []
        plot_data[tag]["raw"] = []

        for xpos, lst in sorted(data_.items()):
            pdata = []
            raw = []

            for elem in lst:
                pdata.append(elem[quantity])
                raw.append( (xpos, elem[quantity]) )

            # todo - filter outliers
            mean = np.mean(pdata)

            plot_data[tag]["x"].append(xpos)
            plot_data[tag]["avg"].append(mean)
            plot_data[tag]["raw"] += raw

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
        plt.scatter(pltdata["raw"], marker='x', s=1, c=next(scatter_colors))
        plt.plot(pltdata["x"], pltdata["avg"], ' o', label=tag,
                 color=next(plot_colors))

    plt.legend()
    plt.show()


atexit.register(lambda: plot(benchmark_data))

# DATA DIVISION.

)_";
}


inline std::ostream& PyplotScriptFormatter::datapoint(
    std::ostream& str,
    const char* tag,
    long long int xpos,
    const MultiCounter::Measurement& m)
{
    auto now = std::chrono::system_clock::now();
    str << "\n# Collected " << now << '\n';
    str << "benchmark_data['" << tag << "'][" << xpos << "].append({\n";
    str << "  'cycles': " << m.cycles << ",\n";
    str << "  'instructions': " << m.instructions << ",\n";
    str << "  'walltime': " << m.walltime << ",\n";
    return str << "})\n";
}

class SideChannelCollector
{
public:
    // All measurements for the same quantity and xpos are considered
    // to be sampled from the same distribution, and statistics are computed
    // over them (i.e. mean, variance)
    SideChannelCollector(const char* filename, const char* quantity, int xpos = 0);

    void reset();

    // todo - provide overloads to override quantity and/or xpos
    template<typename T>
    void split_after(const T& t);

    // todo - probably `double` would be better for xpos
    template<typename T>
    void split_after(const T& t, long long int xpos);

private:
    const char* filename_;
    const char* quantity_;
    int xpos_;

    MultiCounter counter_;

    // todo - add template magic etc.
    typedef PyplotScriptFormatter Formatter;
};


inline SideChannelCollector::SideChannelCollector(
    const char* filename,
    const char* quantity,
    int xpos)
  : filename_(filename)
  , quantity_(quantity)
  , xpos_(xpos)
{
    this->reset();
}


inline void SideChannelCollector::reset()
{
    counter_.reset();
    // Attempt to clobber memory to generate a rwbarrier,
    // so initialization cannot be moved too early.
    // (should probably rather go into the individual counters)
    // todo - verify that this is actually effective.
    asm volatile("" : : : "memory");
}


template<typename T>
void SideChannelCollector::split_after(const T& t)
{
	this->split_after(t, xpos_);
}


template<typename T>
void SideChannelCollector::split_after(const T& t, long long int xpos)
{
    MultiCounter::Measurement m = counter_.split_after(t);

    // todo - would it be useful to add a file i/o mutex?
    std::ofstream file(filename_, std::ios_base::out | std::ios_base::app);
    bool empty = file.tellp() == 0;

    if (empty) {
        Formatter::preamble(file);
    }

    Formatter::datapoint(file, quantity_, xpos, m);

    file.close();

    this->reset();
}

inline
std::ostream& operator<<(std::ostream& str, std::chrono::high_resolution_clock::duration duration)
{
    return str << std::chrono::duration_cast<std::chrono::duration<double, std::ratio<1>>>(duration).count();
}

inline
std::ostream& operator<<(std::ostream& str, std::chrono::system_clock::time_point time_point)
{
    time_t time = std::chrono::system_clock::to_time_t(time_point);
    // ctime() prints the time in a fixed-width 24-byte long
    // format string followed by a newline
    char buffer[26];
    ctime_r(&time, buffer);
    buffer[24] = 0; 
    return str << buffer;
}

} // namespace observatory
