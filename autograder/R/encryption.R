# ============================================================================
# AUTOGRADER PACKAGE - ENCRYPTION PUBLIC API
# ============================================================================
#
# File: encryption.R
# Purpose: Public encryption functions for instructor use
#
# Note: The Rcpp wrapper functions (.cpp_*) are auto-generated in RcppExports.R
#
# Author: Reijel Agub (rcagub@up.edu.ph)
# Version: 0.4.0
# License: MIT
#
# ============================================================================

# ============================================================================
# PUBLIC ENCRYPTION API
# ============================================================================

#' Encrypt text
#' 
#' Encrypts text using the autograder's encryption system.
#' This function provides a simple interface for encrypting sensitive data.
#' 
#' @param text Character string to encrypt
#' @param key Optional custom key (uses default if not provided)
#' @param encoding Output encoding: "hex" (default) or "base64"
#' 
#' @return Encrypted string in specified encoding
#' 
#' @examples
#' \dontrun{
#' # Basic encryption
#' encrypted <- encrypt_text("my secret data")
#' 
#' # With custom key
#' encrypted <- encrypt_text("my secret", key = "my-custom-key")
#' 
#' # Base64 encoding
#' encrypted <- encrypt_text("my secret", encoding = "base64")
#' }
#' 
#' @keywords internal
encrypt_text <- function(text, key = "", encoding = c("hex", "base64")) {
    encoding <- match.arg(encoding)
    
    if (!is.character(text) || length(text) != 1) {
        stop("text must be a single character string", call. = FALSE)
    }
    
    if (encoding == "base64") {
        .cpp_encrypt_base64(text, key)
    } else {
        .cpp_encrypt(text, key)
    }
}

#' Decrypt text
#' 
#' Decrypts text that was encrypted using encrypt_text().
#' 
#' @param ciphertext Encrypted string
#' @param key Optional custom key (must match encryption key)
#' @param encoding Input encoding: "hex" (default) or "base64"
#' 
#' @return Decrypted plaintext string
#' 
#' @examples
#' \dontrun{
#' # Decrypt hex-encoded text
#' plaintext <- decrypt_text(encrypted_data)
#' 
#' # Decrypt base64-encoded text
#' plaintext <- decrypt_text(encrypted_data, encoding = "base64")
#' }
#' 
#' @keywords internal
decrypt_text <- function(ciphertext, key = "", encoding = c("hex", "base64")) {
    encoding <- match.arg(encoding)
    
    if (!is.character(ciphertext) || length(ciphertext) != 1) {
        stop("ciphertext must be a single character string", call. = FALSE)
    }
    
    if (encoding == "base64") {
        .cpp_decrypt_base64(ciphertext, key)
    } else {
        .cpp_decrypt(ciphertext, key)
    }
}

#' Generate encryption key
#' 
#' Generates a cryptographically secure random key for encryption.
#' 
#' @param length Key length in bytes (default 32 for 256-bit key)
#' 
#' @return Hex-encoded key string
#' 
#' @examples
#' \dontrun{
#' # Generate a new key
#' my_key <- generate_key()
#' 
#' # Use the key for encryption
#' encrypted <- encrypt_text("secret", key = my_key)
#' decrypted <- decrypt_text(encrypted, key = my_key)
#' }
#' 
#' @keywords internal
generate_key <- function(length = 32L) {
    if (!is.numeric(length) || length < 1 || length > 256) {
        stop("length must be a positive integer between 1 and 256", call. = FALSE)
    }
    .cpp_generate_key(as.integer(length))
}

#' Derive key from factors
#' 
#' Derives an encryption key from multiple input factors.
#' Useful for creating deterministic keys from known values.
#' 
#' @param ... Character strings to use as key factors
#' @param length Key length in bytes (default 32)
#' 
#' @return Hex-encoded derived key
#' 
#' @examples
#' \dontrun{
#' # Derive key from username and password
#' key <- derive_key("username", "password", "salt")
#' 
#' # Use derived key for encryption
#' encrypted <- encrypt_text("data", key = key)
#' }
#' 
#' @keywords internal
derive_key <- function(..., length = 32L) {
    factors <- c(...)
    
    if (!is.character(factors) || length(factors) == 0) {
        stop("At least one character factor is required", call. = FALSE)
    }
    
    .cpp_derive_key(factors, as.integer(length))
}
