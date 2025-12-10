# Derive key from factors

Derives an encryption key from multiple input factors. Useful for
creating deterministic keys from known values.

## Usage

``` r
derive_key(..., length = 32L)
```

## Arguments

- ...:

  Character strings to use as key factors

- length:

  Key length in bytes (default 32)

## Value

Hex-encoded derived key

## Examples

``` r
if (FALSE) { # \dontrun{
# Derive key from username and password
key <- derive_key("username", "password", "salt")

# Use derived key for encryption
encrypted <- encrypt_text("data", key = key)
} # }
```
