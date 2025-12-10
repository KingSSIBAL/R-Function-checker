// ============================================================================
// AUTOGRADER - NETWORK MODULE IMPLEMENTATION
// ============================================================================
//
// File: network/fetcher.cpp
// Purpose: Implementation of secure network operations
//
// Author: Reijel Agub (rcagub@up.edu.ph)
// Version: 0.4.0
// License: MIT
//
// ============================================================================

#include "fetcher.h"
#include "encrypted_config.h"

namespace autograder {
namespace network {

// ============================================================================
// FETCHER IMPLEMENTATION
// ============================================================================

Fetcher::Fetcher() 
    : config_() {}

Fetcher::Fetcher(const Config& config) 
    : config_(config) {}

void Fetcher::set_config(const Config& config) {
    config_ = config;
}

const Config& Fetcher::get_config() const {
    return config_;
}

std::string Fetcher::build_function_url(const std::string& function_name) const {
    return config_.base_url + "/functions/" + function_name + ".R";
}

std::string Fetcher::build_problems_url() const {
    return config_.base_url + "/functions/_problems.R";
}

bool Fetcher::has_internet() {
    try {
        Rcpp::Function curl_has_internet = 
            Rcpp::Environment::namespace_env("curl")["has_internet"];
        return Rcpp::as<bool>(curl_has_internet());
    } catch (...) {
        // If curl isn't available, assume we have internet
        return true;
    }
}

Rcpp::CharacterVector Fetcher::download_to_temp(const std::string& url) {
    Rcpp::Function tempfile("tempfile");
    Rcpp::Function file_exists("file.exists");
    Rcpp::Function read_lines("readLines");
    
    // Create temporary file
    Rcpp::CharacterVector temp = tempfile();
    std::string temp_path = Rcpp::as<std::string>(temp);
    
    // Check if we need authenticated download
    if (config_.auth_mode == AuthMode::GITHUB_TOKEN && !config_.auth_token.empty()) {
        // Use curl for authenticated download
        download_authenticated(url, temp_path);
    } else {
        // Legacy: use R's download.file for public repos
        Rcpp::Function download_file("download.file");
        download_file(url, temp_path,
                      Rcpp::Named("mode", "w"),
                      Rcpp::Named("quiet", true),
                      Rcpp::Named("timeout", config_.timeout_seconds));
    }
    
    // Verify file exists
    Rcpp::LogicalVector exists = file_exists(temp_path);
    if (!exists[0]) {
        throw NetworkException("Download failed: file not created");
    }
    
    // Read content
    return read_lines(temp_path, Rcpp::Named("warn", false));
}

void Fetcher::download_authenticated(const std::string& url, const std::string& dest_path) {
    // Use curl package for authenticated requests
    // This allows setting Authorization header for private repos
    try {
        Rcpp::Environment curl_env = Rcpp::Environment::namespace_env("curl");
        Rcpp::Function curl_fetch_disk = curl_env["curl_fetch_disk"];
        Rcpp::Function new_handle = curl_env["new_handle"];
        Rcpp::Function handle_setheaders = curl_env["handle_setheaders"];
        
        // Create curl handle
        Rcpp::RObject handle = new_handle();
        
        // Set authorization header
        std::string auth_header = "token " + config_.auth_token;
        handle_setheaders(handle, 
                          Rcpp::Named("Authorization", auth_header),
                          Rcpp::Named("Accept", "application/vnd.github.v3.raw"),
                          Rcpp::Named("User-Agent", "autograder-r-package/0.4.0"));
        
        // Fetch to disk
        curl_fetch_disk(url, dest_path, Rcpp::Named("handle", handle));
        
    } catch (const std::exception& e) {
        throw NetworkException(std::string("Authenticated download failed: ") + e.what());
    }
}

Rcpp::CharacterVector Fetcher::fetch_function_content(const std::string& function_name) {
    // Validate input
    ValidationResult validation_result = 
        validation::Validator::validate_function_name(function_name);
    
    if (!validation_result.valid) {
        throw InvalidInputException(validation_result.error_message);
    }
    
    // Build URL
    std::string url = build_function_url(function_name);
    
    try {
        // Download content
        Rcpp::CharacterVector content = download_to_temp(url);
        
        // Verify content is not empty
        if (content.size() == 0) {
            throw FunctionNotFoundException(function_name);
        }
        
        return content;
        
    } catch (const InvalidInputException& e) {
        throw;
    } catch (const FunctionNotFoundException& e) {
        throw;
    } catch (const std::exception& e) {
        // Classify error
        std::string msg = e.what();
        if (msg.find("404") != std::string::npos || 
            msg.find("not found") != std::string::npos) {
            throw FunctionNotFoundException(function_name);
        }
        throw NetworkException(std::string("Network error: ") + e.what());
    }
}

Rcpp::CharacterVector Fetcher::fetch_problems_list() {
    std::string url = build_problems_url();
    
    try {
        return download_to_temp(url);
    } catch (...) {
        // Return empty vector on failure
        return Rcpp::CharacterVector::create();
    }
}

std::string Fetcher::build_data_url(const std::string& filename) const {
    return config_.base_url + "/data/" + filename;
}

std::string Fetcher::fetch_data_file(const std::string& filename) {
    // Validate filename - only allow safe characters
    for (char c : filename) {
        if (!std::isalnum(c) && c != '_' && c != '-' && c != '.') {
            throw InvalidInputException("Invalid data filename: contains unsafe characters");
        }
    }
    
    // Check file extension
    std::string ext;
    size_t dot_pos = filename.rfind('.');
    if (dot_pos != std::string::npos) {
        ext = filename.substr(dot_pos);
    }
    
    // Only allow safe extensions
    if (ext != ".csv" && ext != ".rds" && ext != ".RDS" && 
        ext != ".rda" && ext != ".RData" && ext != ".txt" &&
        ext != ".xlsx" && ext != ".xls") {
        throw InvalidInputException("Invalid data file type. Allowed: csv, rds, rda, RData, txt, xlsx, xls");
    }
    
    // Build URL
    std::string url = build_data_url(filename);
    
    try {
        // Create temp file with appropriate extension
        Rcpp::Function tempfile("tempfile");
        Rcpp::CharacterVector temp = tempfile(Rcpp::Named("fileext", ext));
        std::string temp_path = Rcpp::as<std::string>(temp);
        
        // Download
        if (config_.auth_mode == AuthMode::GITHUB_TOKEN && !config_.auth_token.empty()) {
            download_authenticated(url, temp_path);
        } else {
            Rcpp::Function download_file("download.file");
            download_file(url, temp_path,
                          Rcpp::Named("mode", "wb"),
                          Rcpp::Named("quiet", true),
                          Rcpp::Named("timeout", config_.timeout_seconds));
        }
        
        // Verify file exists
        Rcpp::Function file_exists("file.exists");
        Rcpp::LogicalVector exists = file_exists(temp_path);
        if (!exists[0]) {
            throw NetworkException("Data file download failed: " + filename);
        }
        
        return temp_path;
        
    } catch (const InvalidInputException& e) {
        throw;
    } catch (const std::exception& e) {
        std::string msg = e.what();
        if (msg.find("404") != std::string::npos) {
            throw NetworkException("Data file not found: " + filename);
        }
        throw NetworkException(std::string("Failed to download data file: ") + e.what());
    }
}

std::string Fetcher::download(const std::string& url) {
    Rcpp::CharacterVector content = download_to_temp(url);
    
    // Combine lines into single string
    std::string result;
    for (int i = 0; i < content.size(); ++i) {
        if (i > 0) result += "\n";
        result += Rcpp::as<std::string>(content[i]);
    }
    
    return result;
}

bool Fetcher::download_to_file(const std::string& url, const std::string& local_path) {
    try {
        Rcpp::Function download_file("download.file");
        
        download_file(url, local_path,
                      Rcpp::Named("mode", "wb"),
                      Rcpp::Named("quiet", true),
                      Rcpp::Named("timeout", config_.timeout_seconds));
        
        Rcpp::Function file_exists("file.exists");
        Rcpp::LogicalVector exists = file_exists(local_path);
        return exists[0];
        
    } catch (...) {
        return false;
    }
}

// ============================================================================
// REPOSITORY IMPLEMENTATION
// ============================================================================

Repository::Repository() 
    : base_url_(config::get_repository_url())
    , problems_cached_(false) {}

Repository::Repository(const std::string& base_url) 
    : base_url_(base_url)
    , problems_cached_(false) {}

void Repository::set_base_url(const std::string& url) {
    base_url_ = url;
    // Invalidate cache
    problems_cached_ = false;
    cached_problems_.clear();
}

const std::string& Repository::get_base_url() const {
    return base_url_;
}

StringVector Repository::get_problems() {
    if (problems_cached_) {
        return cached_problems_;
    }
    
    try {
        Config config;
        config.base_url = base_url_;
        fetcher_.set_config(config);
        
        Rcpp::CharacterVector code = fetcher_.fetch_problems_list();
        
        if (code.size() > 0) {
            // Parse and evaluate _problems.R
            Rcpp::Environment env = Rcpp::new_env();
            Rcpp::Function parse("parse");
            Rcpp::Function eval_func("eval");
            
            // Combine code into single string
            std::string code_str;
            for (int i = 0; i < code.size(); ++i) {
                code_str += Rcpp::as<std::string>(code[i]) + "\n";
            }
            
            Rcpp::ExpressionVector parsed = parse(Rcpp::Named("text", code_str));
            eval_func(parsed, env);
            
            // Extract problems vector
            if (env.exists("problems")) {
                Rcpp::CharacterVector problems = env["problems"];
                cached_problems_.clear();
                for (int i = 0; i < problems.size(); ++i) {
                    cached_problems_.push_back(Rcpp::as<std::string>(problems[i]));
                }
                problems_cached_ = true;
            }
        }
    } catch (...) {
        // Return default list on failure
    }
    
    if (cached_problems_.empty()) {
        cached_problems_ = {"fibonacci", "factorial", "sum_vector"};
    }
    
    return cached_problems_;
}

bool Repository::function_exists(const std::string& function_name) {
    StringVector problems = get_problems();
    for (const auto& p : problems) {
        if (p == function_name) {
            return true;
        }
    }
    return false;
}

Rcpp::CharacterVector Repository::get_function_code(const std::string& function_name) {
    Config config;
    config.base_url = base_url_;
    fetcher_.set_config(config);
    
    return fetcher_.fetch_function_content(function_name);
}

} // namespace network
} // namespace autograder
