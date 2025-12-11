# ============================================================================
# C++ CONFIG FILE GENERATOR
# ============================================================================

generate_config_content <- function(encrypted_url, encrypted_token, 
                                    key_factors, use_auth) {
    # Format token array
    if (length(encrypted_token) > 0 && !is.null(encrypted_token)) {
        token_cpp <- bytes_to_cpp(encrypted_token, "ENCRYPTED_TOKEN")
    } else {
        token_cpp <- "static const uint8_t ENCRYPTED_TOKEN[] = { 0x00 };\nstatic const size_t ENCRYPTED_TOKEN_LEN = 0;\n"
    }
    
    paste0(
'// ============================================================================
// AUTOGRADER - ENCRYPTED CONFIGURATION (AUTO-GENERATED)
// Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '
// DO NOT EDIT MANUALLY
// ============================================================================

#ifndef AUTOGRADER_ENCRYPTED_CONFIG_H
#define AUTOGRADER_ENCRYPTED_CONFIG_H

#include <cstdint>
#include <string>
#include <vector>
#include "encryption.h"
#include "types.h"

namespace autograder {
namespace config {

static const bool USE_AUTHENTICATION = ', tolower(as.character(use_auth)), ';

', bytes_to_cpp(encrypted_url, "ENCRYPTED_URL"), '
', token_cpp, '
static const char* KEY_FACTORS[] = {
', paste0('    "', key_factors, '"', collapse = ",\n"), '
};
static const size_t KEY_FACTORS_COUNT = ', length(key_factors), ';

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

#endif // AUTOGRADER_ENCRYPTED_CONFIG_H
')
}

find_package_dir <- function() {
    script_dir <- getwd()
    if (basename(script_dir) == "tools") {
        script_dir <- script_dir
    } else if (file.exists(file.path(script_dir, "tools"))) {
        script_dir <- file.path(script_dir, "tools")
    }
    
    parent_dir <- dirname(script_dir)
    config_path <- file.path(parent_dir, "autograder", "src", "crypto", "encrypted_config.hpp")
    normalizePath(config_path, mustWork = FALSE)
}
