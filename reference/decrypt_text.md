# Decrypt text

Decrypts text that was encrypted using encrypt_text().

## Usage

``` r
decrypt_text(ciphertext, key = "", encoding = c("hex", "base64"))
```

## Arguments

- ciphertext:

  Encrypted string

- key:

  Optional custom key (must match encryption key)

- encoding:

  Input encoding: "hex" (default) or "base64"

## Value

Decrypted plaintext string

## Examples

``` r
if (FALSE) { # \dontrun{
# Decrypt hex-encoded text
plaintext <- decrypt_text(encrypted_data)

# Decrypt base64-encoded text
plaintext <- decrypt_text(encrypted_data, encoding = "base64")
} # }
```
