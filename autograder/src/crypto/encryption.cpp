// ============================================================================
// AUTOGRADER - ENCRYPTION MODULE IMPLEMENTATION
// ============================================================================
//
// File: crypto/encryption.cpp
// Purpose: Implementation of encryption and decryption functionality
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include "encryption.hpp"
#include <cstring>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <random>
#include <chrono>

namespace autograder {
namespace crypto {

// ============================================================================
// AES S-BOX CONSTANTS
// ============================================================================

const std::array<uint8_t, 256> AES_SBOX = {{
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16
}};

const std::array<uint8_t, 256> AES_SBOX_INV = {{
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
}};

// ============================================================================
// ENCRYPTOR IMPLEMENTATION
// ============================================================================

uint8_t Encryptor::sbox_substitute(uint8_t byte) {
    return AES_SBOX[byte];
}

uint8_t Encryptor::sbox_substitute_inv(uint8_t byte) {
    return AES_SBOX_INV[byte];
}

Encryptor::Encryptor() 
    : key_(KeyDeriver::derive_default())
    , algorithm_(EncryptionAlgorithm::AES_SBOX) {}

Encryptor::Encryptor(const std::string& key, EncryptionAlgorithm algorithm)
    : key_(key)
    , algorithm_(algorithm) {
    if (key_.empty()) {
        key_ = KeyDeriver::derive_default();
    }
}

void Encryptor::set_key(const std::string& key) {
    key_ = key;
}

const std::string& Encryptor::get_key() const {
    return key_;
}

void Encryptor::set_algorithm(EncryptionAlgorithm algo) {
    algorithm_ = algo;
}

EncryptionAlgorithm Encryptor::get_algorithm() const {
    return algorithm_;
}

std::string Encryptor::xor_encrypt(const std::string& data) const {
    std::string result = data;
    size_t key_len = key_.length();
    
    for (size_t i = 0; i < data.length(); ++i) {
        result[i] = data[i] ^ key_[i % key_len];
    }
    
    return result;
}

std::string Encryptor::xor_decrypt(const std::string& data) const {
    // XOR is symmetric
    return xor_encrypt(data);
}

std::string Encryptor::sbox_encrypt(const std::string& data) const {
    std::string result = data;
    size_t key_len = key_.length();
    
    for (size_t i = 0; i < data.length(); ++i) {
        // First XOR with key
        uint8_t byte = static_cast<uint8_t>(data[i]) ^ key_[i % key_len];
        // Then apply S-box
        result[i] = static_cast<char>(sbox_substitute(byte));
    }
    
    return result;
}

std::string Encryptor::sbox_decrypt(const std::string& data) const {
    std::string result = data;
    size_t key_len = key_.length();
    
    for (size_t i = 0; i < data.length(); ++i) {
        // First apply inverse S-box
        uint8_t byte = sbox_substitute_inv(static_cast<uint8_t>(data[i]));
        // Then XOR with key
        result[i] = static_cast<char>(byte ^ key_[i % key_len]);
    }
    
    return result;
}

CryptoResult Encryptor::encrypt(const std::string& plaintext) const {
    try {
        std::string result;
        
        switch (algorithm_) {
            case EncryptionAlgorithm::NONE:
                result = plaintext;
                break;
            case EncryptionAlgorithm::XOR_SIMPLE:
                result = xor_encrypt(plaintext);
                break;
            case EncryptionAlgorithm::AES_SBOX:
            case EncryptionAlgorithm::AES_256_CBC:
            default:
                result = sbox_encrypt(plaintext);
                break;
        }
        
        return CryptoResult(true, result);
    } catch (const std::exception& e) {
        return CryptoResult(std::string("Encryption failed: ") + e.what());
    }
}

CryptoResult Encryptor::decrypt(const std::string& ciphertext) const {
    try {
        std::string result;
        
        switch (algorithm_) {
            case EncryptionAlgorithm::NONE:
                result = ciphertext;
                break;
            case EncryptionAlgorithm::XOR_SIMPLE:
                result = xor_decrypt(ciphertext);
                break;
            case EncryptionAlgorithm::AES_SBOX:
            case EncryptionAlgorithm::AES_256_CBC:
            default:
                result = sbox_decrypt(ciphertext);
                break;
        }
        
        return CryptoResult(true, result);
    } catch (const std::exception& e) {
        return CryptoResult(std::string("Decryption failed: ") + e.what());
    }
}

std::string Encryptor::encrypt_to_hex(const std::string& plaintext) const {
    auto result = encrypt(plaintext);
    if (!result.success) {
        throw EncryptionException(result.error_message);
    }
    return Encoder::to_hex(result.data);
}

std::string Encryptor::decrypt_from_hex(const std::string& hex_ciphertext) const {
    std::string ciphertext = Encoder::from_hex(hex_ciphertext);
    auto result = decrypt(ciphertext);
    if (!result.success) {
        throw DecryptionException(result.error_message);
    }
    return result.data;
}

std::string Encryptor::encrypt_to_base64(const std::string& plaintext) const {
    auto result = encrypt(plaintext);
    if (!result.success) {
        throw EncryptionException(result.error_message);
    }
    return Encoder::to_base64(result.data);
}

std::string Encryptor::decrypt_from_base64(const std::string& b64_ciphertext) const {
    std::string ciphertext = Encoder::from_base64(b64_ciphertext);
    auto result = decrypt(ciphertext);
    if (!result.success) {
        throw DecryptionException(result.error_message);
    }
    return result.data;
}

// ============================================================================
// KEY DERIVER IMPLEMENTATION
// ============================================================================

std::string KeyDeriver::derive(const std::vector<std::string>& factors, 
                                size_t key_length) {
    std::string combined;
    for (const auto& factor : factors) {
        combined += factor;
    }
    
    std::string transformed = sbox_transform(combined);
    
    // Extend or truncate to desired length
    while (transformed.length() < key_length) {
        transformed += sbox_transform(transformed);
    }
    
    return transformed.substr(0, key_length);
}

std::string KeyDeriver::derive_default() {
    return derive({
        "AUTOGRADER_SECURE_KEY_2025",
        "v0.4.0",
        "R-FUNC-CHK",
        "MODULAR-CPP"
    }, 32);
}

std::string KeyDeriver::sbox_transform(const std::string& input) {
    std::string output;
    output.reserve(input.length());
    
    for (size_t i = 0; i < input.length(); ++i) {
        output += static_cast<char>(AES_SBOX[static_cast<uint8_t>(input[i])]);
    }
    
    return output;
}

std::string KeyDeriver::simple_hash(const std::string& input, size_t output_length) {
    std::string result(output_length, '\0');
    
    // Simple hash: mix input bytes into result
    for (size_t i = 0; i < input.length(); ++i) {
        size_t idx = i % output_length;
        result[idx] = static_cast<char>(
            static_cast<uint8_t>(result[idx]) ^ 
            AES_SBOX[static_cast<uint8_t>(input[i])]
        );
    }
    
    // Apply S-box transformation for better mixing
    return sbox_transform(result);
}

// ============================================================================
// ENCODER IMPLEMENTATION
// ============================================================================

std::string Encoder::to_hex(const std::string& data) {
    std::ostringstream ss;
    ss << std::hex << std::setfill('0');
    
    for (unsigned char c : data) {
        ss << std::setw(2) << static_cast<int>(c);
    }
    
    return ss.str();
}

std::string Encoder::to_hex(const ByteVector& data) {
    std::ostringstream ss;
    ss << std::hex << std::setfill('0');
    
    for (uint8_t byte : data) {
        ss << std::setw(2) << static_cast<int>(byte);
    }
    
    return ss.str();
}

std::string Encoder::from_hex(const std::string& hex) {
    if (hex.length() % 2 != 0) {
        throw InvalidInputException("Invalid hex string length");
    }
    
    std::string result;
    result.reserve(hex.length() / 2);
    
    for (size_t i = 0; i < hex.length(); i += 2) {
        int byte;
        std::istringstream ss(hex.substr(i, 2));
        ss >> std::hex >> byte;
        result += static_cast<char>(byte);
    }
    
    return result;
}

ByteVector Encoder::from_hex_to_bytes(const std::string& hex) {
    std::string str = from_hex(hex);
    return ByteVector(str.begin(), str.end());
}

// Base64 encoding table
static const char BASE64_CHARS[] = 
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789+/";

std::string Encoder::to_base64(const std::string& data) {
    std::string result;
    result.reserve(((data.size() + 2) / 3) * 4);
    
    int i = 0;
    unsigned char char_array_3[3];
    unsigned char char_array_4[4];
    
    const unsigned char* bytes = reinterpret_cast<const unsigned char*>(data.data());
    size_t len = data.size();
    
    while (len--) {
        char_array_3[i++] = *(bytes++);
        if (i == 3) {
            char_array_4[0] = (char_array_3[0] & 0xfc) >> 2;
            char_array_4[1] = ((char_array_3[0] & 0x03) << 4) + ((char_array_3[1] & 0xf0) >> 4);
            char_array_4[2] = ((char_array_3[1] & 0x0f) << 2) + ((char_array_3[2] & 0xc0) >> 6);
            char_array_4[3] = char_array_3[2] & 0x3f;
            
            for (i = 0; i < 4; i++) {
                result += BASE64_CHARS[char_array_4[i]];
            }
            i = 0;
        }
    }
    
    if (i) {
        for (int j = i; j < 3; j++) {
            char_array_3[j] = '\0';
        }
        
        char_array_4[0] = (char_array_3[0] & 0xfc) >> 2;
        char_array_4[1] = ((char_array_3[0] & 0x03) << 4) + ((char_array_3[1] & 0xf0) >> 4);
        char_array_4[2] = ((char_array_3[1] & 0x0f) << 2) + ((char_array_3[2] & 0xc0) >> 6);
        
        for (int j = 0; j < i + 1; j++) {
            result += BASE64_CHARS[char_array_4[j]];
        }
        
        while (i++ < 3) {
            result += '=';
        }
    }
    
    return result;
}

std::string Encoder::to_base64(const ByteVector& data) {
    return to_base64(std::string(data.begin(), data.end()));
}

static inline bool is_base64(unsigned char c) {
    return (std::isalnum(c) || c == '+' || c == '/');
}

std::string Encoder::from_base64(const std::string& b64) {
    size_t len = b64.size();
    int i = 0;
    int in_ = 0;
    unsigned char char_array_4[4], char_array_3[3];
    std::string result;
    
    while (len-- && b64[in_] != '=' && is_base64(b64[in_])) {
        char_array_4[i++] = b64[in_]; in_++;
        if (i == 4) {
            for (i = 0; i < 4; i++) {
                char_array_4[i] = static_cast<unsigned char>(
                    std::find(BASE64_CHARS, BASE64_CHARS + 64, char_array_4[i]) - BASE64_CHARS
                );
            }
            
            char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
            char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
            char_array_3[2] = ((char_array_4[2] & 0x3) << 6) + char_array_4[3];
            
            for (i = 0; i < 3; i++) {
                result += char_array_3[i];
            }
            i = 0;
        }
    }
    
    if (i) {
        for (int j = 0; j < i; j++) {
            char_array_4[j] = static_cast<unsigned char>(
                std::find(BASE64_CHARS, BASE64_CHARS + 64, char_array_4[j]) - BASE64_CHARS
            );
        }
        
        char_array_3[0] = (char_array_4[0] << 2) + ((char_array_4[1] & 0x30) >> 4);
        char_array_3[1] = ((char_array_4[1] & 0xf) << 4) + ((char_array_4[2] & 0x3c) >> 2);
        
        for (int j = 0; j < i - 1; j++) {
            result += char_array_3[j];
        }
    }
    
    return result;
}

ByteVector Encoder::from_base64_to_bytes(const std::string& b64) {
    std::string str = from_base64(b64);
    return ByteVector(str.begin(), str.end());
}

// ============================================================================
// URL ENCRYPTOR IMPLEMENTATION
// ============================================================================

UrlEncryptor::UrlEncryptor() 
    : encryptor_() {}

std::string UrlEncryptor::encrypt_url(const std::string& url) {
    return encryptor_.encrypt_to_hex(url);
}

std::string UrlEncryptor::decrypt_url(const std::string& encrypted_url) {
    return encryptor_.decrypt_from_hex(encrypted_url);
}

std::string UrlEncryptor::get_github_base() {
    // For development, return URL directly
    // In production, this would be decrypted
    return "https://raw.githubusercontent.com/KingSSIBAL/R-Function-checker/main/repo";
}

std::string UrlEncryptor::build_function_url(const std::string& function_name) {
    return get_github_base() + "/functions/" + function_name + ".R";
}

// ============================================================================
// FREE FUNCTIONS
// ============================================================================

std::string quick_encrypt(const std::string& plaintext, const std::string& key) {
    Encryptor enc(key.empty() ? KeyDeriver::derive_default() : key);
    return enc.encrypt_to_hex(plaintext);
}

std::string quick_decrypt(const std::string& ciphertext, const std::string& key) {
    Encryptor enc(key.empty() ? KeyDeriver::derive_default() : key);
    return enc.decrypt_from_hex(ciphertext);
}

std::string generate_random_key(size_t length) {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(0, 255);
    
    std::string key;
    key.reserve(length);
    
    for (size_t i = 0; i < length; ++i) {
        key += static_cast<char>(dis(gen));
    }
    
    return key;
}

} // namespace crypto
} // namespace autograder
