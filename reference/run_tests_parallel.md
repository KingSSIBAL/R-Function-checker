# Run tests in parallel using multiple CPU cores

Executes test cases in parallel for improved performance on multi-core
systems. Automatically falls back to sequential for small test sets.

Parallel Execution Strategy:

- Create cluster with n cores (leave 1 free for system)

- Export student_fun, instructor_fun, tolerance to workers

- Distribute test indices across workers

- Each worker: runs test, catches errors, returns result

- Main process: collects results, cleans up cluster

## Usage

``` r
run_tests_parallel(
  student_fun,
  instructor_fun,
  test_data,
  tolerance,
  use_parallel = TRUE,
  data_cache = list()
)
```

## Arguments

- student_fun:

  Student's function implementation

- instructor_fun:

  Instructor's reference implementation

- test_data:

  Validated test case data

- tolerance:

  Numeric tolerance for comparisons

- use_parallel:

  Whether to actually use parallel (TRUE) or fall back (FALSE)

- data_cache:

  Named list of loaded data objects (optional)

## Value

List of test results, each containing:

- student: student output or error object

- expected: expected output or error object

- index: test number (1-based)

## Details

Performance Characteristics:

- Setup cost: ~100-200ms (cluster creation)

- Per-test overhead: ~10ms (serialization)

- Speedup: typically 2-4x on 4-core system

- Worth it for: ≥10 tests

Core Allocation Strategy:

- Detect available cores

- Use cores - 1 (keep system responsive)

- Cap at 4 cores (diminishing returns beyond this)

- Example: 8-core system → use 4 cores

## Resource Management

Cluster is always stopped via on.exit(), even if errors occur. This
prevents orphaned R processes.
