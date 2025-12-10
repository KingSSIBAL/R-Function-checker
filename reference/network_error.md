# Custom error for network-related issues

Creates a structured error object for network failures. This allows
calling code to handle network issues specifically, separate from other
types of errors.

## Usage

``` r
network_error(message, call = NULL)
```

## Arguments

- message:

  Human-readable error message describing the network issue

- call:

  The call that generated the error (usually NULL or sys.call())

## Value

An S3 object of class c("network_error", "error", "condition")

## Details

Common network issues this error represents:

- No internet connection

- Server unreachable

- Timeout (\>30 seconds)

- DNS resolution failure

- Firewall blocking connection

## Examples

``` r
if (FALSE) { # \dontrun{
if (!curl::has_internet()) {
  stop(network_error("No internet connection detected"))
}
} # }
```
