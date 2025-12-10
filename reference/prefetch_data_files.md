# Prefetch and cache data files for test cases

Downloads all data files specified in test_data\$data_files and stores
them in a named list for quick access during test execution.

## Usage

``` r
prefetch_data_files(test_data)
```

## Arguments

- test_data:

  Validated test case data

## Value

Named list of loaded data objects (keyed by filename)

## Details

This function is called once before running tests to avoid downloading
the same file multiple times. The returned cache is passed to the test
runner.
