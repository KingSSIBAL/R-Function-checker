# Encrypt text

Encrypts text using the autograder's encryption system. This function
provides a simple interface for encrypting sensitive data.

## Usage

``` r
encrypt_text(text, key = "", encoding = c("hex", "base64"))
```

## Arguments

- text:

  Character string to encrypt

- key:

  Optional custom key (uses default if not provided)

- encoding:

  Output encoding: "hex" (default) or "base64"

## Value

Encrypted string in specified encoding

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic encryption
encrypted <- encrypt_text("my secret data")

# With custom key
encrypted <- encrypt_text("my secret", key = "my-custom-key")

# Base64 encoding
encrypted <- encrypt_text("my secret", encoding = "base64")
} # }
```
