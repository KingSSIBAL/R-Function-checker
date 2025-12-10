# Crypto Module

AES S-box based encryption and decryption for secure URL storage.

## Files

| File | Purpose |
|------|---------|
| `encryption.hpp` | Encryption API and class definitions |
| `encryption.cpp` | Implementation |
| `encrypted_config.hpp` | **Auto-generated** encrypted URL configuration |

## ⚠️ Important

**`encrypted_config.hpp` is auto-generated!**

Do not edit manually. Use `tools/encrypt_url_helper.R` to regenerate.

## Components

### AES S-box

Standard AES S-box lookup table for byte substitution:

```cpp
extern const std::array<uint8_t, 256> AES_SBOX;
extern const std::array<uint8_t, 256> AES_INV_SBOX;
```

### Encryptor Class

```cpp
class Encryptor {
public:
    explicit Encryptor(const std::string& key);
    
    CryptoResult encrypt(const std::string& plaintext);
    CryptoResult decrypt(const std::string& ciphertext);
    
    std::string encrypt_to_hex(const std::string& plaintext);
    std::string decrypt_from_hex(const std::string& hex);
    
    std::string encrypt_to_base64(const std::string& plaintext);
    std::string decrypt_from_base64(const std::string& base64);
};
```

### KeyDeriver Class

```cpp
class KeyDeriver {
public:
    static std::string derive(const std::vector<std::string>& factors, 
                              size_t key_length = 32);
    static std::string derive_default();
};
```

### Encoder Class

```cpp
class Encoder {
public:
    static std::string to_hex(const std::string& data);
    static std::string from_hex(const std::string& hex);
    static std::string to_base64(const std::string& data);
    static std::string from_base64(const std::string& base64);
};
```

## Usage

### Encrypting

```cpp
#include "crypto/encryption.hpp"

// Quick encrypt with default key
std::string encrypted = autograder::crypto::quick_encrypt("secret data");

// With custom key
autograder::crypto::Encryptor enc("my-secret-key");
auto result = enc.encrypt("plaintext");
if (result.success) {
    std::string ciphertext = result.data;
}
```

### Decrypting

```cpp
autograder::crypto::Encryptor enc("my-secret-key");
auto result = enc.decrypt(ciphertext);
if (result.success) {
    std::string plaintext = result.data;
}
```

### Key Derivation

```cpp
std::vector<std::string> factors = {"factor1", "factor2", "factor3"};
std::string key = autograder::crypto::KeyDeriver::derive(factors, 32);
```

## Algorithm

1. **Key Derivation**: Concatenate factors → Apply S-box transform → Extend to desired length
2. **Encryption**: XOR with key → Apply S-box substitution
3. **Decryption**: Apply inverse S-box → XOR with key

This provides obfuscation suitable for hiding URLs from casual inspection. It is not cryptographically secure against determined attackers.
