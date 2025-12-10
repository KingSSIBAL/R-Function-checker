# Generate encryption key

Generates a cryptographically secure random key for encryption.

## Usage

``` r
generate_key(length = 32L)
```

## Arguments

- length:

  Key length in bytes (default 32 for 256-bit key)

## Value

Hex-encoded key string

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a new key
my_key <- generate_key()

# Use the key for encryption
encrypted <- encrypt_text("secret", key = my_key)
decrypted <- decrypt_text(encrypted, key = my_key)
} # }
```
