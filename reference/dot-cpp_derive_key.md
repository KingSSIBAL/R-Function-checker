# Derive encryption key

Derives a key from multiple factors using S-box transformation.

## Usage

``` r
.cpp_derive_key(factors, key_length = 32L)
```

## Arguments

- factors:

  Character vector of key factors

- key_length:

  Desired key length (default 32 for 256-bit)

## Value

Hex-encoded derived key
