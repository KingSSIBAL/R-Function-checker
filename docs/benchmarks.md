# Benchmarks

This document is a place to record performance benchmarks of the C++ backend compared to base R equivalents.

## Goals

- Track performance regressions or improvements over time.
- Provide realistic numbers for documentation and teaching materials.
- Help guide optimization work toward real bottlenecks.

## Example benchmark layout

For each benchmark, record:

- R and package version
- Platform (OS, CPU, RAM)
- Code snippet
- Results (e.g., median runtime over several runs)

You can use packages such as `bench` or `microbenchmark` for consistent measurements.
