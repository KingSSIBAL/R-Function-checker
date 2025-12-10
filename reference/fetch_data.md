# Fetch a data file from the repository

Downloads a data file from the repository's data/ folder and loads it
into R.

Supported Formats:

- **CSV**: Loaded with read.csv() into data.frame

- **RDS**: Loaded with readRDS() (any R object)

- **RData/rda**: Loaded with load() into environment

- **TXT**: Loaded with readLines() into character vector

- **XLSX/XLS**: Loaded with readxl::read_excel() into tibble/data.frame

Security:

- Filename validated in C++ (no path traversal)

- Only allowed extensions: csv, rds, rda, RData, txt, xlsx, xls

- Downloaded to temporary file (cleaned up by R)

## Usage

``` r
fetch_data(filename)
```

## Arguments

- filename:

  Name of the data file (e.g., "dataset.csv")

## Value

The loaded data object (type depends on file format)

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch a CSV file
df <- fetch_data("sample_data.csv")

# Fetch an RDS file
model <- fetch_data("trained_model.rds")
} # }
```
