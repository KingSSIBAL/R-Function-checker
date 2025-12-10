// ============================================================================
// AUTOGRADER - ENCRYPTION MODULE HEADER
// ============================================================================
//
// File: crypto/encryption.hpp
// Purpose: Encryption and decryption functionality
//
// Provides:
//   - AES S-box transformations
//   - Key derivation functions
//   - XOR cipher operations
//   - URL encryption/decryption helpers
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_ENCRYPTION_HPP
#define AUTOGRADER_ENCRYPTION_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <array>
#include "types.h"
#include "exceptions.h"

namespace autograder {
namespace crypto {

// ============================================================================
// AES S-BOX CONSTANTS
// ============================================================================

/**
 * @brief Standard AES S-box (Substitution Box) lookup table
 * 
 * The S-box is one of the most important components of AES encryption.
 * It provides non-linear byte substitution, which is essential for security.
 * 
 * Mathematical Background:
 *   - Based on multiplicative inverse in Galois Field GF(2^8)
 *   - Followed by affine transformation
 *   - Each input byte maps to exactly one output byte (bijective)
 * 
 * @note This is the official AES S-box from FIPS 197 standard
 */
extern const std::array<uint8_t, 256> AES_SBOX;

/**
 * @brief Inverse AES S-box for decryption
 */
extern const std::array<uint8_t, 256> AES_SBOX_INV;

// ============================================================================
// ENCRYPTION CLASS
// ============================================================================

/**
 * @class Encryptor
 * @brief Handles encryption and decryption operations
 * 
 * Provides multiple encryption methods:
 *   - Simple XOR cipher
 *   - AES S-box based encryption
 *   - Key derivation
 * 
 * Usage:
 *   Encryptor enc;
 *   auto encrypted = enc.encrypt("secret data");
 *   auto decrypted = enc.decrypt(encrypted);
 */
class Encryptor {
private:
    std::string key_;
    EncryptionAlgorithm algorithm_;

    // Apply S-box substitution to a byte
    static uint8_t sbox_substitute(uint8_t byte);
    
    // Apply inverse S-box substitution
    static uint8_t sbox_substitute_inv(uint8_t byte);

public:
    /**
     * @brief Default constructor with auto-derived key
     */
    Encryptor();
    
    /**
     * @brief Constructor with custom key
     * @param key The encryption key
     * @param algorithm The encryption algorithm to use
     */
    explicit Encryptor(const std::string& key, 
                       EncryptionAlgorithm algorithm = EncryptionAlgorithm::AES_SBOX);
    
    /**
     * @brief Set the encryption key
     * @param key The encryption key
     */
    void set_key(const std::string& key);
    
    /**
     * @brief Get the current encryption key
     */
    const std::string& get_key() const;
    
    /**
     * @brief Set the encryption algorithm
     */
    void set_algorithm(EncryptionAlgorithm algo);
    
    /**
     * @brief Get the current encryption algorithm
     */
    EncryptionAlgorithm get_algorithm() const;
    
    /**
     * @brief Encrypt a string
     * @param plaintext The text to encrypt
     * @return CryptoResult containing the encrypted data
     */
    CryptoResult encrypt(const std::string& plaintext) const;
    
    /**
     * @brief Decrypt a string
     * @param ciphertext The encrypted text
     * @return CryptoResult containing the decrypted data
     */
    CryptoResult decrypt(const std::string& ciphertext) const;
    
    /**
     * @brief Encrypt data to hex string
     * @param plaintext The text to encrypt
     * @return Hex-encoded encrypted string
     */
    std::string encrypt_to_hex(const std::string& plaintext) const;
    
    /**
     * @brief Decrypt from hex string
     * @param hex_ciphertext Hex-encoded encrypted string
     * @return Decrypted plaintext
     */
    std::string decrypt_from_hex(const std::string& hex_ciphertext) const;
    
    /**
     * @brief Encrypt data to base64 string
     * @param plaintext The text to encrypt
     * @return Base64-encoded encrypted string
     */
    std::string encrypt_to_base64(const std::string& plaintext) const;
    
    /**
     * @brief Decrypt from base64 string
     * @param b64_ciphertext Base64-encoded encrypted string
     * @return Decrypted plaintext
     */
    std::string decrypt_from_base64(const std::string& b64_ciphertext) const;

private:
    // Internal encryption methods
    std::string xor_encrypt(const std::string& data) const;
    std::string xor_decrypt(const std::string& data) const;
    std::string sbox_encrypt(const std::string& data) const;
    std::string sbox_decrypt(const std::string& data) const;
};

// ============================================================================
// KEY DERIVATION
// ============================================================================

/**
 * @class KeyDeriver
 * @brief Provides key derivation functionality
 */
class KeyDeriver {
public:
    /**
     * @brief Derive a key from multiple factors
     * @param factors Vector of strings to combine
     * @param key_length Desired key length (default: 32 for 256-bit)
     * @return Derived key string
     */
    static std::string derive(const std::vector<std::string>& factors, 
                              size_t key_length = 32);
    
    /**
     * @brief Derive the default autograder key
     * @return The derived key
     */
    static std::string derive_default();
    
    /**
     * @brief Apply S-box transformation to key material
     * @param input Input string
     * @return Transformed string
     */
    static std::string sbox_transform(const std::string& input);
    
    /**
     * @brief Simple hash function for key derivation
     * @param input Input string
     * @param output_length Desired output length
     * @return Hash output
     */
    static std::string simple_hash(const std::string& input, size_t output_length = 32);
};

// ============================================================================
// ENCODING UTILITIES
// ============================================================================

/**
 * @class Encoder
 * @brief Provides encoding/decoding utilities
 */
class Encoder {
public:
    /**
     * @brief Convert bytes to hex string
     * @param data Input data
     * @return Hex-encoded string
     */
    static std::string to_hex(const std::string& data);
    static std::string to_hex(const ByteVector& data);
    
    /**
     * @brief Convert hex string to bytes
     * @param hex Hex-encoded string
     * @return Decoded data
     */
    static std::string from_hex(const std::string& hex);
    static ByteVector from_hex_to_bytes(const std::string& hex);
    
    /**
     * @brief Convert bytes to base64 string
     * @param data Input data
     * @return Base64-encoded string
     */
    static std::string to_base64(const std::string& data);
    static std::string to_base64(const ByteVector& data);
    
    /**
     * @brief Convert base64 string to bytes
     * @param b64 Base64-encoded string
     * @return Decoded data
     */
    static std::string from_base64(const std::string& b64);
    static ByteVector from_base64_to_bytes(const std::string& b64);
};

// ============================================================================
// URL ENCRYPTION HELPER
// ============================================================================

/**
 * @class UrlEncryptor
 * @brief Specialized encryptor for URL protection
 * 
 * Provides convenient methods for encrypting and decrypting URLs
 * used by the autograder to fetch test cases.
 */
class UrlEncryptor {
private:
    Encryptor encryptor_;

public:
    /**
     * @brief Default constructor
     */
    UrlEncryptor();
    
    /**
     * @brief Encrypt a URL
     * @param url The URL to encrypt
     * @return Encrypted URL (hex-encoded)
     */
    std::string encrypt_url(const std::string& url);
    
    /**
     * @brief Decrypt a URL
     * @param encrypted_url Encrypted URL (hex-encoded)
     * @return Decrypted URL
     */
    std::string decrypt_url(const std::string& encrypted_url);
    
    /**
     * @brief Get the GitHub base URL
     * @return The base URL for the repository
     */
    std::string get_github_base();
    
    /**
     * @brief Build full URL for a function
     * @param function_name The function name
     * @return Full URL to fetch the function
     */
    std::string build_function_url(const std::string& function_name);
};

// ============================================================================
// FREE FUNCTIONS FOR CONVENIENCE
// ============================================================================

/**
 * @brief Quick encrypt function
 * @param plaintext Text to encrypt
 * @param key Optional encryption key (uses default if empty)
 * @return Encrypted hex string
 */
std::string quick_encrypt(const std::string& plaintext, 
                          const std::string& key = "");

/**
 * @brief Quick decrypt function
 * @param ciphertext Encrypted hex string
 * @param key Optional encryption key (uses default if empty)
 * @return Decrypted text
 */
std::string quick_decrypt(const std::string& ciphertext, 
                          const std::string& key = "");

/**
 * @brief Generate a random key
 * @param length Key length in bytes
 * @return Random key string
 */
std::string generate_random_key(size_t length = 32);

} // namespace crypto
} // namespace autograder

#endif // AUTOGRADER_ENCRYPTION_HPP
