// ============================================================================
// AUTOGRADER - ENCRYPTED CONFIGURATION (AUTO-GENERATED)
// Generated: 2025-12-10 12:44:19
// DO NOT EDIT MANUALLY
// ============================================================================

#ifndef AUTOGRADER_ENCRYPTED_CONFIG_HPP
#define AUTOGRADER_ENCRYPTED_CONFIG_HPP

#include <cstdint>
#include <string>
#include <vector>
#include "encryption.h"
#include "types.h"

namespace autograder {
namespace config {

static const bool USE_AUTHENTICATION = true;

static const uint8_t ENCRYPTED_URL[] = {
    0xe9, 0xc4, 0x20, 0xbf, 0x66, 0x80, 0x91, 0x18, 0x9c, 0xef, 0x6c, 0x2e, 0x01, 0x8f, 0xc4, 0x45,
    0xaf, 0x95, 0xb4, 0xa4, 0xe4, 0x7a, 0x09, 0x7f, 0xe3, 0x98, 0x4c, 0x02, 0x6e, 0x05, 0x00, 0x7f,
    0x28, 0x66, 0x7f, 0x55, 0x8b, 0x85, 0x70, 0x52, 0xcc, 0x2c, 0x19, 0x32, 0x83, 0x2f, 0xee, 0x8f,
    0xa2, 0x98, 0x11, 0x9c, 0x3a, 0x56, 0x2c, 0x51, 0x5a, 0xea, 0x02, 0xef, 0x83, 0xf5, 0x72, 0xf9,
    0x98, 0x2a, 0x2f
};
static const size_t ENCRYPTED_URL_LEN = 67;

static const uint8_t ENCRYPTED_TOKEN[] = {
    0x69, 0x2a, 0x20, 0xce, 0x03, 0xaa, 0x86, 0x7f, 0x76, 0x92, 0x60, 0x86, 0xcf, 0x6a, 0xd5, 0xc3,
    0x05, 0xcd, 0x41, 0x75, 0x0f, 0xa9, 0xa8, 0x4a, 0x4d, 0x32, 0x7f, 0x3c, 0x20, 0xfc, 0x83, 0x85,
    0x66, 0x0a, 0xfc, 0x98, 0x61, 0xc3, 0x6d, 0x83, 0x67, 0xa0, 0xbb, 0xde, 0x67, 0x31, 0x95, 0x1b,
    0x77, 0x19, 0xec, 0x30, 0xb8, 0x88, 0xd6, 0x50, 0xb6, 0x70, 0x21, 0x6e, 0x5a, 0xcb, 0x2c, 0xfc,
    0xe6, 0x4f, 0xcb, 0xe8, 0x70, 0xda, 0x8d, 0xd2, 0xd8, 0xa8, 0xb8, 0x7a, 0x93, 0x3b, 0x62, 0x84,
    0x27, 0x5f, 0x8c, 0x3f, 0xa7, 0x14, 0x1b, 0x2c, 0xa8, 0x35, 0x00, 0xeb, 0xda
};
static const size_t ENCRYPTED_TOKEN_LEN = 93;

static const char* KEY_FACTORS[] = {
    "AUTOGRADER_SECURE_KEY_2025",
    "v0.4.0",
    "R-FUNC-CHK",
    "MODULAR-CPP"
};
static const size_t KEY_FACTORS_COUNT = 4;

inline std::string get_repository_url() {
    std::vector<std::string> factors;
    for (size_t i = 0; i < KEY_FACTORS_COUNT; ++i) factors.push_back(KEY_FACTORS[i]);
    std::string key = crypto::KeyDeriver::derive(factors, 32);
    std::string encrypted(reinterpret_cast<const char*>(ENCRYPTED_URL), ENCRYPTED_URL_LEN);
    crypto::Encryptor enc(key);
    auto result = enc.decrypt(encrypted);
    return result.success ? result.data : "";
}

inline std::string get_auth_token() {
    if (!USE_AUTHENTICATION || ENCRYPTED_TOKEN_LEN == 0) return "";
    std::vector<std::string> factors;
    for (size_t i = 0; i < KEY_FACTORS_COUNT; ++i) factors.push_back(KEY_FACTORS[i]);
    std::string key = crypto::KeyDeriver::derive(factors, 32);
    std::string encrypted(reinterpret_cast<const char*>(ENCRYPTED_TOKEN), ENCRYPTED_TOKEN_LEN);
    crypto::Encryptor enc(key);
    auto result = enc.decrypt(encrypted);
    return result.success ? result.data : "";
}

inline bool is_authentication_enabled() { return USE_AUTHENTICATION; }
inline AuthMode get_auth_mode() { return USE_AUTHENTICATION ? AuthMode::GITHUB_TOKEN : AuthMode::NONE; }

inline Config get_config() {
    Config cfg;
    cfg.base_url = get_repository_url();
    cfg.auth_mode = get_auth_mode();
    cfg.auth_token = get_auth_token();
    return cfg;
}

} // namespace config
} // namespace autograder

#endif // AUTOGRADER_ENCRYPTED_CONFIG_HPP

