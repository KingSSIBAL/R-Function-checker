# Run tests sequentially (one after another)

Executes test cases one at a time in order. Used for:

- Small test sets (\<10 tests)

- When use_parallel = FALSE

- Debugging (easier to trace)

Advantages over Parallel:

- No setup overhead

- Easier debugging (linear execution)

- More predictable

- Better for small test sets

## Usage

``` r
run_tests_sequential(
  student_fun,
  instructor_fun,
  test_data,
  tolerance,
  data_cache = list()
)
```

## Arguments

- student_fun:

  Student's function

- instructor_fun:

  Instructor's reference function

- test_data:

  Validated test case data

- tolerance:

  Numeric tolerance

- data_cache:

  Named list of loaded data objects (optional)

## Value

List of test results (same structure as parallel version)

## Details

Execution Flow: For each test: 1. Extract input arguments 2. Inject data
if needed 3. Call student function (catch errors) 4. Call instructor
function (catch errors) 5. Package results 6. Move to next test

Error Handling: Errors are captured, not thrown. This allows:

- All tests to run (one failure doesn't stop others)

- Detailed error reporting for each test

- Graceful handling of student code errors
