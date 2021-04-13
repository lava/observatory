#include <chrono>

#pragma once

/// A simple `stopwatch` class that avoids the most common pitfalls.
/// It is not configurable, to avoid being accidentally misconfigured.
/// It has no third-party dependencies to be copy/pastable in ad-hoc benchmarks.
class stopwatch {
public:
  using clock = std::chrono::steady_clock;
  using duration = clock::duration;
  using time_point = clock::time_point;

  /// Initialize stopwatch with current time.
  stopwatch();

  /// Reset start time to current time.
  /// @param t Optionally can be passed an arbitrary argument `t` to ensure
  ///          all computations affecting `t` have been performed before the
  ///          time is measured.
  /// @returns Duration since the .
  template<typename T>
  duration reset(const T& t = int{0});

  /// Generate a split measurement, with a read dependency on the
  /// passed parameter.
  /// @param t The stopwatch will ensure all computations affecting `t` have
  ///          been performed before the time is measured.
  template<typename T>
  duration split(const T&) const;

private:
  std::chrono::steady_clock::time_point start_;
};


stopwatch::stopwatch()
  : start_(clock::now())
{}


template<typename T>
auto stopwatch::reset(const T& t) -> duration
{
  // Generate a read-access to prevent operations modifying `t` from being
  // moved *after* the call to reset.
  asm volatile ("" : : "r"(&t));
  auto previous = start_;
  start_ = clock::now();
  // Clobber memory to prevent expensive unrelated operations being moved
  // *before* the call to reset. This is slightly contrary to the goal of
  // measuring the best code the compiler can possibly generate, but placing
  // the call to `reset()` in the correct location is much more intuitive
  // than ensure no missed data dependencies on the measured code path.
  asm volatile("" : : : "memory");
  return start_ - previous;
}

template<typename T>
auto stopwatch::split(const T& t) const -> duration
{
  asm volatile ("" : : "r"(&t));
  auto now = clock::now();
  return now - start_;
}