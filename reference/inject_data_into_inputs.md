# Inject data into test case inputs

Replaces data file references in test inputs with actual loaded data.

## Usage

``` r
inject_data_into_inputs(input_args, data_cache)
```

## Arguments

- input_args:

  List of arguments for a single test case

- data_cache:

  Named list of loaded data objects

## Value

Modified input_args with data objects injected

## Details

Data can be specified in two ways:

1.  As a named `data` argument: list(data = "file.csv")

2.  As any argument value: list(df = "file.csv")

The function checks each argument to see if it's a string that matches a
cached data filename, and replaces it with the loaded data.
