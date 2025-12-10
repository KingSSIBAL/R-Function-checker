// ============================================================================
// AUTOGRADER - NETWORK MODULE HEADER
// ============================================================================
//
// File: network/fetcher.hpp
// Purpose: Secure network operations for fetching test cases
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#ifndef AUTOGRADER_FETCHER_HPP
#define AUTOGRADER_FETCHER_HPP

#include <Rcpp.h>
#include <string>
#include <vector>
#include <functional>
#include "types.h"
#include "exceptions.h"
#include "validator.h"
#include "encryption.h"

namespace autograder {
namespace network {

// ============================================================================
// FETCHER CLASS
// ============================================================================

/**
 * @class Fetcher
 * @brief Secure network operations for fetching remote content
 * 
 * Provides:
 *   - Secure download with validation
 *   - Timeout handling
 *   - Error classification
 *   - Content verification
 */
class Fetcher {
public:
    /**
     * @brief Default constructor
     */
    Fetcher();
    
    /**
     * @brief Constructor with custom configuration
     * @param config Configuration options
     */
    explicit Fetcher(const Config& config);
    
    /**
     * @brief Set configuration
     * @param config Configuration options
     */
    void set_config(const Config& config);
    
    /**
     * @brief Get current configuration
     * @return Current configuration
     */
    const Config& get_config() const;
    
    /**
     * @brief Fetch function content from repository
     * 
     * Downloads the R file containing a function and its test cases.
     * 
     * @param function_name Name of the function to fetch
     * @return CharacterVector containing the file lines
     * @throws InvalidInputException if function name is invalid
     * @throws FunctionNotFoundException if function doesn't exist
     * @throws NetworkException if download fails
     */
    Rcpp::CharacterVector fetch_function_content(const std::string& function_name);
    
    /**
     * @brief Fetch problems list from repository
     * 
     * Downloads _problems.R which contains the list of available functions.
     * 
     * @return CharacterVector containing the file lines, empty on failure
     */
    Rcpp::CharacterVector fetch_problems_list();
    
    /**
     * @brief Download a file from URL
     * @param url The URL to download from
     * @return Downloaded content as string
     * @throws NetworkException if download fails
     */
    std::string download(const std::string& url);
    
    /**
     * @brief Download a file to a local path
     * @param url The URL to download from
     * @param local_path Local file path to save to
     * @return true if successful
     */
    bool download_to_file(const std::string& url, const std::string& local_path);
    
    /**
     * @brief Check if internet connection is available
     * @return true if connected
     */
    static bool has_internet();
    
    /**
     * @brief Build full URL for a function
     * @param function_name The function name
     * @return Full URL
     */
    std::string build_function_url(const std::string& function_name) const;
    
    /**
     * @brief Build URL for problems list
     * @return Full URL for _problems.R
     */
    std::string build_problems_url() const;

    /**
     * @brief Build URL for a data file
     * @param filename The data file name (e.g., "dataset.csv")
     * @return Full URL for the data file
     */
    std::string build_data_url(const std::string& filename) const;

    /**
     * @brief Fetch a data file from repository
     * 
     * Downloads a data file (CSV, RDS, RData) from the data/ folder.
     * 
     * @param filename Name of the data file
     * @return Path to the downloaded temporary file
     * @throws NetworkException if download fails
     */
    std::string fetch_data_file(const std::string& filename);

private:
    Config config_;
    crypto::UrlEncryptor url_encryptor_;
    
    // Internal download using R's download.file (legacy public repos)
    Rcpp::CharacterVector download_to_temp(const std::string& url);
    
    // Authenticated download using curl (private repos)
    void download_authenticated(const std::string& url, const std::string& dest_path);
};

// ============================================================================
// REPOSITORY CLASS
// ============================================================================

/**
 * @class Repository
 * @brief High-level interface for repository operations
 * 
 * Provides convenient methods for working with the test case repository.
 */
class Repository {
public:
    /**
     * @brief Default constructor
     */
    Repository();
    
    /**
     * @brief Constructor with base URL
     * @param base_url Repository base URL
     */
    explicit Repository(const std::string& base_url);
    
    /**
     * @brief Set the base URL
     * @param url Base URL
     */
    void set_base_url(const std::string& url);
    
    /**
     * @brief Get the base URL
     * @return Base URL
     */
    const std::string& get_base_url() const;
    
    /**
     * @brief Get list of available problems
     * @return Vector of problem names
     */
    StringVector get_problems();
    
    /**
     * @brief Check if a function exists
     * @param function_name Function name to check
     * @return true if function exists
     */
    bool function_exists(const std::string& function_name);
    
    /**
     * @brief Get function code
     * @param function_name Function name
     * @return R code as CharacterVector
     */
    Rcpp::CharacterVector get_function_code(const std::string& function_name);

private:
    std::string base_url_;
    Fetcher fetcher_;
    StringVector cached_problems_;
    bool problems_cached_;
};

} // namespace network
} // namespace autograder

#endif // AUTOGRADER_FETCHER_HPP
