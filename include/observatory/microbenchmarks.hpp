// This header contains code for writing special-purpose
// benchmarks.

namespace observatory {

struct DataSeries {
    std::string name;

    std::vector<double> xpos;
    std::vector<uint64_t> cycles;
    std::vector<uint64_t> instructions;
    std::vector<std::chrono::steady_clock::duration> walltime;

    DataSeries(const std::string& name, size_t intendedSize):
      name(name)
    {
      // todo - only reserve for the data that's actually used
      xpos.reserve(intendedSize);
      cycles.reserve(intendedSize);
      instructions.reserve(intendedSize);
      walltime.reserve(intendedSize);
    }
};


struct BenchmarkContext {
    // todo - use more templates to make this type-safe, and don't
    // mix input and output arguments

    // input
    int iteration;

    // output
    double xpos;

    // i/o
    void* opaque;
};


struct BenchmarkOptions {
    // todo - these two should probably a bit more general
    int repetitions;

    bool include_cpu_counters;
    bool include_walltime;

    std::function<void(BenchmarkContext&)> globalSetup;
    std::function<void(BenchmarkContext&)> globalTeardown;

    // This 
    std::function<void(BenchmarkContext&)> iterationSetup;
    std::function<void(BenchmarkContext&)> iterationTeardown;

    // maybe
    //std::function<bool(void)> discardResult;
};

template<typename BenchmarkFn>
DataSeries benchmark_series(const std::string& name, BenchmarkFn f, const BenchmarkOptions& options);

// Convenience function

template<typename BenchmarkFn>
DataSeries benchmark_series(const std::string& name, BenchmarkFn f)
{
    BenchmarkOptions options;
    options.repetitions = 1;
    options.include_walltime = true;
    options.include_cpu_counters = true;
    return benchmark_series(name, f, options);
}

template<typename BenchmarkFn>
DataSeries benchmark_series(const std::string& name, BenchmarkFn f, std::unordered_map<std::string, const char*> keywords)
{
    BenchmarkOptions options;
    if (keywords.count("repetitions")) {
        options.repetitions = std::atoi(keywords["repetitions"]);
    } else {
        options.repetitions = 1;
    }

    // TODO - parse other keywords
    options.include_walltime = true;
    options.include_cpu_counters = true;

    return benchmark_series(name, f, options);
}

// General case

// The other option (still todo) would be e.g. `benchmark_comparison()`, i.e. where the
// x axis does not represent some changing numeric quantity but rather a set of discrete options.
template<typename BenchmarkFn>
DataSeries benchmark_series(const std::string& name, BenchmarkFn f, const BenchmarkOptions& options)
{
    BenchmarkContext context;

    if (options.globalSetup) {
        options.globalSetup(context);
    }

    DataSeries result(name, options.repetitions);
    int cycles_fd = 0;
    int instructions_fd = 0;

    if (options.include_cpu_counters) {
        cycles_fd = internal::perf::createHardwareCounter(PERF_COUNT_HW_CPU_CYCLES);
        instructions_fd = internal::perf::createHardwareCounter(PERF_COUNT_HW_INSTRUCTIONS);
    }

    for (int i=0; i<options.repetitions; ++i) {
        std::chrono::steady_clock::time_point walltime_before, walltime_after;
        uint64_t cycles_before, cycles_after;
        uint64_t instructions_before, instructions_after;

        context.iteration = i;
        context.xpos = i;

        if (options.iterationSetup) {
            options.iterationSetup(context);
        }

        // initializing these in order of sensitivity
        if (options.include_walltime) {
            walltime_before = std::chrono::steady_clock::now();
        }

        if (options.include_cpu_counters) {
            // todo - we could probably get a pretty accurate estimate of instructions
            // it takes to read the cycles counter and subtract that
            cycles_before = internal::perf::readCounter(cycles_fd);
            instructions_before = internal::perf::readCounter(instructions_fd);
        }

        auto x = f(context);
        
        // Force the compiler to actually execute f by using the address of the result
        // as an input argument here, so the result can't be optimized away.
        asm volatile ("" : : "r"(&x));

        // todo - is it more useful to read these in the same order as they were
        // initialized, or in the opposite order?
        if (options.include_cpu_counters) {
            instructions_after = internal::perf::readCounter(instructions_fd);
            cycles_after = internal::perf::readCounter(cycles_fd);
        }

        if (options.include_walltime) {
            walltime_after = std::chrono::steady_clock::now();
            result.walltime.push_back(walltime_after - walltime_before);
        }

        if (options.iterationTeardown) {
            options.iterationTeardown(context);
        }

        if (options.include_cpu_counters) {
            result.cycles.push_back(cycles_after - cycles_before);
            result.instructions.push_back(instructions_after - instructions_before);
        }

        result.xpos.push_back(context.xpos);
    }

    if (options.globalTeardown) {
        options.globalTeardown(context);
    }

    return result;
}

} // namespace benchmark

#include <iostream>

namespace benchmark {


std::ostream& operator<<(std::ostream& str, std::chrono::steady_clock::duration duration)
{
    return str << std::chrono::duration_cast<std::chrono::duration<double, std::ratio<1>>>(duration).count();
}

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

void plot_data(const DataSeries& series)
{
    bool walltime = series.walltime.size() > 0;
    bool cycles = series.cycles.size() > 0;
    bool instructions = series.cycles.size() > 0;

    // Header
    // todo - insert dynamic number of spaces
    std::cout << "i\txpos\t" << (walltime ? "Time\t" : "") << (cycles ? "Cycles\t" : "") << (instructions ? "Instructions\t" : "") << std::endl;


    for (size_t i=0; i<series.xpos.size(); ++i) {
        std::cout << i << ":\t";
        std::cout << series.xpos.at(i) << "\t";

        if (walltime) {
            std::cout << series.walltime.at(i) << "s\t";
        }

        if (cycles) {
            std::cout << series.cycles.at(i) << "\t";
        }

        if (instructions) {
            std::cout << series.instructions.at(i) << "\t";
        }

        std::cout << std::endl;
    }
}

} // namespace observatory {
