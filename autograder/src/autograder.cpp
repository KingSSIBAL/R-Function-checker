#include <Rcpp.h>
#include <string>
#include <vector>
#include <cstring>
#include <stdexcept>

using namespace Rcpp;

// ============================================================================
// AES-256 ENCRYPTION IMPLEMENTATION (Simplified)
// ============================================================================

// AES S-box
static const uint8_t sbox[256] = {
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
};

// Simple XOR-based decryption using derived key
std::string simple_decrypt(const std::string& encoded, const std::string& key) {
    std::string result = encoded;
    size_t key_len = key.length();
    
    for (size_t i = 0; i < encoded.length(); ++i) {
        // Apply S-box transformation followed by XOR
        uint8_t byte = static_cast<uint8_t>(encoded[i]);
        byte = sbox[byte];
        result[i] = byte ^ key[i % key_len];
    }
    
    return result;
}

// Derive key from multiple environment factors
std::string derive_key() {
    // Combine multiple factors to create key
    std::string key_base = "AUTOGRADER_SECURE_KEY_2025";
    
    // Add some variability based on package version or build
    std::string var1 = "v0.2.0";
    std::string var2 = "R-FUNC-CHK";
    
    std::string derived = key_base + var1 + var2;
    
    // Simple hash-like transformation
    std::string key;
    for (size_t i = 0; i < derived.length(); ++i) {
        key += static_cast<char>(sbox[static_cast<uint8_t>(derived[i])]);
    }
    
    return key.substr(0, 32); // 256-bit key
}

// Encrypt URL parts (to be done offline and hardcoded)
static const std::string ENC_PART_1 = "\x8e\x9a\x8c\x9e\xae\xb2\xaf\xaf"; // Encrypted "https://"
static const std::string ENC_PART_2 = "\x9d\x85\xa3\xae\x87\x91\x8c\x9a"; // Encrypted segments
// ... Additional encrypted parts would go here

// For demonstration, using base64 as placeholder
// In production, encrypt these with the derived key offline
std::string get_github_base() {
    // This would normally decrypt the URL parts
    // For now, returning the URL directly for functionality
    // In production, you'd encrypt these strings offline
    return "https://raw.githubusercontent.com/KingSSIBAL/R-Function-checker/main/repo";
}

// ============================================================================
// CUSTOM ERROR TYPES
// ============================================================================

class NetworkError : public std::runtime_error {
public:
    explicit NetworkError(const std::string& msg) : std::runtime_error(msg) {}
};

class FunctionNotFoundError : public std::runtime_error {
public:
    explicit FunctionNotFoundError(const std::string& msg) : std::runtime_error(msg) {}
};

class InvalidInputError : public std::runtime_error {
public:
    explicit InvalidInputError(const std::string& msg) : std::runtime_error(msg) {}
};

// ============================================================================
// INPUT SANITIZATION
// ============================================================================

bool is_valid_function_name(const std::string& name) {
    if (name.empty() || name.length() > 100) {
        return false;
    }
    
    // Check for path traversal attempts
    if (name.find("..") != std::string::npos ||
        name.find("/") != std::string::npos ||
        name.find("\\") != std::string::npos ||
        name.find("~") != std::string::npos) {
        return false;
    }
    
    // Only allow alphanumeric, underscore, and dash
    for (char c : name) {
        if (!std::isalnum(c) && c != '_' && c != '-') {
            return false;
        }
    }
    
    return true;
}

// ============================================================================
// OPTIMIZED COMPARISON (Pure C++)
// ============================================================================

//' Fast comparison of R objects using C++
//' @keywords internal
// [[Rcpp::export(".cpp_compare_fast")]]
LogicalVector cpp_compare_fast(SEXP obj1, SEXP obj2, double tolerance = 1e-10) {
    
    // Check types first
    int type1 = TYPEOF(obj1);
    int type2 = TYPEOF(obj2);
    
    if (type1 != type2) {
        return LogicalVector::create(false);
    }
    
    // Handle numeric vectors (most common case)
    if (type1 == REALSXP) {
        NumericVector v1(obj1);
        NumericVector v2(obj2);
        
        if (v1.size() != v2.size()) {
            return LogicalVector::create(false);
        }
        
        // Early termination on first difference
        for (int i = 0; i < v1.size(); ++i) {
            if (NumericVector::is_na(v1[i]) && NumericVector::is_na(v2[i])) {
                continue;
            }
            if (NumericVector::is_na(v1[i]) || NumericVector::is_na(v2[i])) {
                return LogicalVector::create(false);
            }
            if (std::abs(v1[i] - v2[i]) > tolerance) {
                return LogicalVector::create(false);
            }
        }
        return LogicalVector::create(true);
    }
    
    // Handle integer vectors
    if (type1 == INTSXP) {
        IntegerVector v1(obj1);
        IntegerVector v2(obj2);
        
        if (v1.size() != v2.size()) {
            return LogicalVector::create(false);
        }
        
        for (int i = 0; i < v1.size(); ++i) {
            if (v1[i] != v2[i]) {
                return LogicalVector::create(false);
            }
        }
        return LogicalVector::create(true);
    }
    
    // Handle character vectors
    if (type1 == STRSXP) {
        CharacterVector v1(obj1);
        CharacterVector v2(obj2);
        
        if (v1.size() != v2.size()) {
            return LogicalVector::create(false);
        }
        
        for (int i = 0; i < v1.size(); ++i) {
            if (v1[i] != v2[i]) {
                return LogicalVector::create(false);
            }
        }
        return LogicalVector::create(true);
    }
    
    // Handle logical vectors
    if (type1 == LGLSXP) {
        LogicalVector v1(obj1);
        LogicalVector v2(obj2);
        
        if (v1.size() != v2.size()) {
            return LogicalVector::create(false);
        }
        
        for (int i = 0; i < v1.size(); ++i) {
            if (v1[i] != v2[i]) {
                return LogicalVector::create(false);
            }
        }
        return LogicalVector::create(true);
    }
    
    // For complex types, fall back to R's identical
    Function identical("identical");
    return LogicalVector::create(as<bool>(identical(obj1, obj2)));
}

// ============================================================================
// SECURE FETCH WITH BETTER ERROR HANDLING
// ============================================================================

//' Securely fetch function content with improved error handling
//' @keywords internal
// [[Rcpp::export(".cpp_fetch_function_content")]]
CharacterVector cpp_fetch_function_content(const std::string& function_name) {
    
    // Input sanitization
    if (!is_valid_function_name(function_name)) {
        throw InvalidInputError(
            "Invalid function name. Use only letters, numbers, underscores, and hyphens."
        );
    }
    
    // Build URL internally
    std::string base_url = get_github_base();
    std::string url = base_url + "/functions/" + function_name + ".R";
    
    // Get R functions
    Function download_file("download.file");
    Function readLines("readLines");
    Function tempfile("tempfile");
    Function file_exists("file.exists");
    
    // Create temp file
    CharacterVector temp = tempfile();
    std::string temp_path = as<std::string>(temp);
    
    try {
        // Try to download with timeout
        download_file(url, temp_path, 
                     Named("mode", "w"), 
                     Named("quiet", true),
                     Named("timeout", 30));
        
        // Verify file was created
        LogicalVector exists = file_exists(temp_path);
        if (!exists[0]) {
            throw NetworkError("Download failed: file not created");
        }
        
        // Read content
        CharacterVector content = readLines(temp_path, Named("warn", false));
        
        if (content.size() == 0) {
            throw FunctionNotFoundError(
                std::string("Function '") + function_name + 
                "' not found or file is empty.\n" +
                "Available functions can be listed with list_problems()."
            );
        }
        
        return content;
        
    } catch (const InvalidInputError& e) {
        forward_exception_to_r(e);
    } catch (const FunctionNotFoundError& e) {
        forward_exception_to_r(e);
    } catch (const NetworkError& e) {
        std::string msg = "Network error: Unable to fetch function. ";
        msg += "Check your internet connection and try again.";
        throw NetworkError(msg);
    } catch (std::exception& e) {
        std::string msg = "Unexpected error fetching function '";
        msg += function_name + "': " + e.what();
        throw std::runtime_error(msg);
    }
    
    return CharacterVector::create();
}

//' Securely fetch problems list
//' @keywords internal
// [[Rcpp::export(".cpp_fetch_problems_list")]]
CharacterVector cpp_fetch_problems_list() {
    
    std::string base_url = get_github_base();
    std::string url = base_url + "/functions/_problems.R";
    
    Function download_file("download.file");
    Function readLines("readLines");
    Function tempfile("tempfile");
    
    CharacterVector temp = tempfile();
    std::string temp_path = as<std::string>(temp);
    
    try {
        download_file(url, temp_path, 
                     Named("mode", "w"), 
                     Named("quiet", true),
                     Named("timeout", 30));
        
        CharacterVector content = readLines(temp_path, Named("warn", false));
        return content;
        
    } catch (std::exception& e) {
        // Return empty if problems list doesn't exist
        return CharacterVector::create();
    }
}

//' Legacy comparison function (kept for compatibility)
//' @keywords internal
// [[Rcpp::export(".cpp_compare_identical")]]
LogicalVector cpp_compare_identical(SEXP obj1, SEXP obj2) {
    return cpp_compare_fast(obj1, obj2, 1e-10);
}
