#pragma once

#include <fstream>
#include <iomanip>

// The `GgplotSink` is a class that accepts data points and
// generates an R script plotting the data. 
//
// A data point for this class is defined as a `x` and `y`
// coordinate, together with a `tag` identifying the data
// series.
//
// The output file is persistent, so data can be appended
// throughout multiple runs of the same program.
//
// TODO: Create a uniform interface for different types of
// sinks

namespace observatory {

class GgplotSink {
public:
    GgplotSink(const std::string& filename);
    void datapoint(const std::string& tag, long long x, double y, const char* note = nullptr);

private:
    static std::ofstream prepareFile_(const std::string& filename);
    static void datapoint_(std::ostream& file,  const std::string& tag, long long x, double y, const char* note = nullptr);
    static void preamble_(std::ostream& str);

    std::ofstream output_;

};

GgplotSink::GgplotSink(const std::string& filename)
  : output_(prepareFile_(filename))
{}

inline
void GgplotSink::datapoint(const std::string& tag, long long x, double y, const char* note)
{
    this->datapoint_(output_, tag, x, y, note);
}

inline void GgplotSink::datapoint_(
    std::ostream& file,
    const std::string& tag,
    long long int x,
    double y,
    const char* note)
{
    file << tag << "," << x << "," << std::setprecision(16) << std::fixed << y << "\n";
}


inline std::ofstream GgplotSink::prepareFile_(const std::string& filename)
{
    std::ofstream file(filename, std::ios_base::out | std::ios_base::app);
    bool empty = file.tellp() == 0;

    if (empty) {
	// TODO: set chmod+x
        GgplotSink::preamble_(file);
    }

    return file;  
}

inline void GgplotSink::preamble_(std::ostream& str)
{
    str <<
R"_(#!/usr/bin/Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(tcltk)
})

# https://stackoverflow.com/a/15373917/92560
thisFile <- function() {
        cmdArgs <- commandArgs(trailingOnly = FALSE)
        needle <- "--file="
        match <- grep(needle, cmdArgs)
        if (length(match) > 0) {
                # Rscript
                return(normalizePath(sub(needle, "", cmdArgs[match])))
        } else {
                # 'source'd via R console
                return(normalizePath(sys.frames()[[1]]$ofile))
        }
}

# Read the data from the bottom of this file
filename <- thisFile()
pattern <- paste0("# --- 8< ----------------------", "-")
lines <- readLines(filename)
idx <- grep(pattern, lines)
print(idx)
data <- read_csv(lines, skip=idx,
        col_types = cols(
                tag=col_character(),
                x=col_integer(),
                y=col_double()))

# Plot the data
plt <- ggplot(data = data, mapping = aes(x =x, y =y)) + geom_point(mapping = aes(color=tag))

# Display output (wtf, R!)
X11()
print(plt)
while (!is.null(dev.list())) Sys.sleep(1)
q()

# Data starts below this line.
# --- 8< -----------------------
tag,x,y
)_";
}

} // namespace observatory
