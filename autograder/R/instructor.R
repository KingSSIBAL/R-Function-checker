# ============================================================================
# AUTOGRADER PACKAGE - INSTRUCTOR SETUP FUNCTIONS
# ============================================================================
#
# File: instructor.R
# Purpose: Functions to help instructors configure and build the package
#
# These functions require the source package (not installed package) and
# R build tools (Rtools on Windows, Xcode on Mac).
#
# Author: Autograder Team
# Version: 0.4.0
# License: MIT
#
# ============================================================================

# ============================================================================
# ENCRYPTION CONSTANTS (must match C++)
# ============================================================================

.AES_SBOX <- as.integer(c(
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
))

.DEFAULT_KEY_FACTORS <- c("AUTOGRADER_SECURE_KEY_2025", "v0.4.0", "R-FUNC-CHK", "MODULAR-CPP")

# ============================================================================
# INTERNAL ENCRYPTION HELPERS
# ============================================================================

.sbox_transform <- function(input) {
  bytes <- as.integer(charToRaw(input))
  transformed <- sapply(bytes, function(b) .AES_SBOX[b + 1])
  as.raw(transformed)
}

.derive_key <- function(factors, key_length = 32) {
  combined <- paste(factors, collapse = "")
  transformed <- .sbox_transform(combined)
  while (length(transformed) < key_length) {
    extended <- .sbox_transform(rawToChar(transformed))
    transformed <- c(transformed, extended)
  }
  transformed[1:key_length]
}

.encrypt_string <- function(plaintext, key) {
  plain_bytes <- as.integer(charToRaw(plaintext))
  key_bytes <- as.integer(key)
  key_len <- length(key_bytes)
  
  # Vectorized XOR encryption with S-box substitution
  key_indices <- ((seq_along(plain_bytes) - 1) %% key_len) + 1
  xored <- bitwXor(plain_bytes, key_bytes[key_indices])
  encrypted <- .AES_SBOX[xored + 1]
  as.raw(encrypted)
}

.bytes_to_cpp <- function(bytes, var_name) {
  hex_values <- sprintf("0x%02x", as.integer(bytes))
  rows <- split(hex_values, ceiling(seq_along(hex_values) / 16))
  formatted_rows <- sapply(rows, function(row) {
    paste0("    ", paste(row, collapse = ", "))
  })
  paste0(
    "static const uint8_t ", var_name, "[] = {\n",
    paste(formatted_rows, collapse = ",\n"),
    "\n};\n",
    "static const size_t ", var_name, "_LEN = ", length(bytes), ";\n"
  )
}

# ============================================================================
# CONFIG FILE GENERATOR
# ============================================================================

.generate_config_content <- function(encrypted_url, encrypted_token, 
                                     key_factors, use_auth) {
  if (length(encrypted_token) > 0 && !is.null(encrypted_token)) {
    token_cpp <- .bytes_to_cpp(encrypted_token, "ENCRYPTED_TOKEN")
  } else {
    token_cpp <- "static const uint8_t ENCRYPTED_TOKEN[] = { 0x00 };\nstatic const size_t ENCRYPTED_TOKEN_LEN = 0;\n"
  }
  
  paste0(
    '// ============================================================================
// AUTOGRADER - ENCRYPTED CONFIGURATION (AUTO-GENERATED)
// Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '
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

static const bool USE_AUTHENTICATION = ', tolower(as.character(use_auth)), ';

', .bytes_to_cpp(encrypted_url, "ENCRYPTED_URL"), '
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

#endif // AUTOGRADER_ENCRYPTED_CONFIG_HPP
')
}

# ============================================================================
# PRE-FLIGHT CHECKS
# ============================================================================

#' Check if R build tools are available
#' 
#' @description
#' Verifies that the system has the required tools to build R packages
#' with C++ code (Rtools on Windows, Xcode on Mac, gcc on Linux).
#' 
#' @return Logical TRUE if build tools are available
#' 
#' @examples
#' \dontrun{
#' autograder_check_build_tools()
#' }
#' 
#' @keywords internal
autograder_check_build_tools <- function() {
  if (!requireNamespace("pkgbuild", quietly = TRUE)) {
    cli::cli_alert_warning("Package 'pkgbuild' not installed. Installing...")
    utils::install.packages("pkgbuild", quiet = TRUE)
  }
  
  has_tools <- pkgbuild::has_build_tools(debug = FALSE)
  
  if (has_tools) {
    cli::cli_alert_success("Build tools are available")
  } else {
    cli::cli_alert_danger("Build tools NOT found!")
    
    if (.Platform$OS.type == "windows") {
      cli::cli_alert_info("Install Rtools from: https://cran.r-project.org/bin/windows/Rtools/")
    } else if (Sys.info()["sysname"] == "Darwin") {
      cli::cli_alert_info("Install Xcode Command Line Tools: xcode-select --install")
    } else {
      cli::cli_alert_info("Install gcc/g++: sudo apt-get install r-base-dev")
    }
  }
  
  invisible(has_tools)
}

#' Test GitHub token authentication
#' 
#' @description
#' Tests if a GitHub token can successfully access a private repository.
#' Run this before configuring to ensure your credentials work.
#' 
#' @param url Repository URL (raw.githubusercontent.com format)
#' @param token GitHub personal access token
#' @param verbose Print detailed output (default TRUE)
#' 
#' @return Invisible list with success status and details
#' 
#' @examples
#' \dontrun{
#' autograder_test_token(
#'   url = "https://raw.githubusercontent.com/user/private-repo/main/repo",
#'   token = "github_pat_xxxxx"
#' )
#' }
#' 
#' @keywords internal
autograder_test_token <- function(url, token, verbose = TRUE) {
  if (verbose) {
    cli::cli_h1("GitHub Token Test")
  }
  
  # Validate inputs
  if (missing(token) || is.null(token) || nchar(trimws(token)) == 0) {
    cli::cli_alert_danger("No token provided!")
    return(invisible(list(success = FALSE, error = "No token provided")))
  }
  
  if (missing(url) || is.null(url) || nchar(trimws(url)) == 0) {
    cli::cli_alert_danger("No repository URL provided!")
    return(invisible(list(success = FALSE, error = "No URL provided")))
  }
  
  if (verbose) {
    cli::cli_alert_info("URL: {.url {url}}")
    cli::cli_alert_info("Token: {substr(token, 1, 10)}... (masked)")
  }
  
  # Build test URL
  base_url <- sub("/$", "", url)
  test_url <- paste0(base_url, "/functions/_problems.R")
  
  if (verbose) cli::cli_alert("Testing connection...")
  
  # Test using curl
  result <- tryCatch({
    if (!requireNamespace("curl", quietly = TRUE)) {
      stop("Package 'curl' is required")
    }
    
    h <- curl::new_handle()
    curl::handle_setheaders(h,
      "Authorization" = paste("token", token),
      "Accept" = "application/vnd.github.v3.raw",
      "User-Agent" = "autograder-r-package"
    )
    
    response <- curl::curl_fetch_memory(test_url, handle = h)
    status_code <- response$status_code
    
    if (verbose) cli::cli_alert_info("HTTP Status: {status_code}")
    
    if (status_code == 200) {
      list(success = TRUE, status_code = status_code)
    } else {
      errors <- list(
        "401" = "Unauthorized - Token invalid or expired",
        "403" = "Forbidden - Token lacks 'Contents: Read' permission",
        "404" = "Not Found - Repo doesn't exist or token can't access it"
      )
      error_msg <- errors[[as.character(status_code)]] %||% paste("HTTP", status_code)
      list(success = FALSE, error = error_msg, status_code = status_code)
    }
  }, error = function(e) {
    list(success = FALSE, error = conditionMessage(e))
  })
  
  if (verbose) {
    cli::cli_rule()
    if (result$success) {
      cli::cli_alert_success("TOKEN TEST PASSED!")
      cli::cli_alert_info("You can now run {.fn autograder_configure}")
    } else {
      cli::cli_alert_danger("TOKEN TEST FAILED!")
      cli::cli_alert_warning("Error: {result$error}")
      cli::cli_h3("Troubleshooting")
      cli::cli_ul(c(
        "Verify token is correct and not expired",
        "Ensure token has 'Contents: Read' permission",
        "Check repository URL is correct",
        "For fine-grained tokens, verify repo is selected"
      ))
    }
  }
  
  invisible(result)
}

# ============================================================================
# MAIN CONFIGURATION FUNCTION
# ============================================================================

#' Configure autograder with repository URL and token
#' 
#' @description
#' One-command setup for instructors. This function:
#' 1. Validates inputs and tests the token

#' 2. Encrypts the URL and token
#' 3. Updates the C++ config file
#' 4. Optionally rebuilds and installs the package
#' 
#' @param url Repository URL (raw.githubusercontent.com format)
#' @param token GitHub personal access token (NULL for public repos)
#' @param pkg_path Path to autograder package source directory
#' @param install If TRUE, automatically rebuild and install the package
#' @param key_factors Key derivation factors (advanced, rarely needed)
#' 
#' @return Invisible list with operation result
#' 
#' @examples
#' \dontrun{
#' # For private repository
#' autograder_configure(
#'   url = "https://raw.githubusercontent.com/user/private-repo/main/repo",
#'   token = "github_pat_xxxxx",
#'   pkg_path = "path/to/autograder",
#'   install = TRUE
#' )
#' 
#' # For public repository
#' autograder_configure(
#'   url = "https://raw.githubusercontent.com/user/public-repo/main/repo",
#'   pkg_path = "path/to/autograder"
#' )
#' }
#' 
#' @keywords internal
autograder_configure <- function(url, 
                                  token = NULL,
                                  pkg_path = NULL,
                                  install = TRUE,
                                  key_factors = .DEFAULT_KEY_FACTORS) {
  
  cli::cli_h1("Autograder Configuration")
  
  # Determine auth mode

  use_auth <- !is.null(token) && nchar(trimws(token)) > 0
  
  cli::cli_alert_info("Mode: {.strong {if (use_auth) 'SECURE (Token Auth)' else 'PUBLIC (No Auth)'}}")
  
  # Find package path
  if (is.null(pkg_path)) {
    # Try common locations
    possible_paths <- c(
      "autograder",
      "../autograder",
      file.path(getwd(), "autograder"),
      file.path(dirname(getwd()), "autograder")
    )
    
    for (p in possible_paths) {
      if (file.exists(file.path(p, "DESCRIPTION"))) {
        pkg_path <- normalizePath(p)
        break
      }
    }
    
    if (is.null(pkg_path)) {
      cli::cli_alert_danger("Could not find autograder package source!")
      cli::cli_alert_info("Please specify {.arg pkg_path}")
      return(invisible(list(success = FALSE, error = "Package not found")))
    }
  }
  
  pkg_path <- normalizePath(pkg_path, mustWork = FALSE)
  config_path <- file.path(pkg_path, "src", "encrypted_config.h")
  
  cli::cli_alert_info("Package path: {.path {pkg_path}}")
  
  # Validate package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    cli::cli_alert_danger("Not a valid R package: {.path {pkg_path}}")
    return(invisible(list(success = FALSE, error = "Invalid package path")))
  }
  
  # Check build tools if installing
  if (install) {
    cli::cli_h2("Checking Build Tools")
    if (!autograder_check_build_tools()) {
      return(invisible(list(success = FALSE, error = "Build tools not available")))
    }
  }
  
  # Test token for private repos
  if (use_auth) {
    cli::cli_h2("Testing Token")
    test_result <- autograder_test_token(url, token, verbose = TRUE)
    if (!test_result$success) {
      cli::cli_alert_danger("Token test failed. Aborting configuration.")
      return(invisible(list(success = FALSE, error = "Token test failed")))
    }
  }
  
  # Encrypt
  cli::cli_h2("Encrypting Configuration")
  cli::cli_alert("Deriving encryption key...")
  key <- .derive_key(key_factors)
  
  cli::cli_alert("Encrypting URL...")
  encrypted_url <- .encrypt_string(url, key)
  
  encrypted_token <- NULL
  if (use_auth) {
    cli::cli_alert("Encrypting token...")
    encrypted_token <- .encrypt_string(token, key)
  }
  
  # Generate config
  cli::cli_alert("Generating C++ config file...")
  cpp_content <- .generate_config_content(encrypted_url, encrypted_token, key_factors, use_auth)
  
  # Write config
  tryCatch({
    writeLines(cpp_content, config_path)
    cli::cli_alert_success("Updated: {.path {config_path}}")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to write config: {conditionMessage(e)}")
    return(invisible(list(success = FALSE, error = conditionMessage(e))))
  })
  
  # Install
  if (install) {
    cli::cli_h2("Building & Installing Package")
    cli::cli_alert("This may take 1-2 minutes...")
    
    tryCatch({
      if (!requireNamespace("devtools", quietly = TRUE)) {
        cli::cli_alert_warning("Installing devtools...")
        utils::install.packages("devtools", quiet = TRUE)
      }
      
      devtools::install(pkg_path, quiet = FALSE, upgrade = "never")
      cli::cli_alert_success("Package installed successfully!")
      
    }, error = function(e) {
      cli::cli_alert_danger("Installation failed: {conditionMessage(e)}")
      cli::cli_alert_info("You can manually install with: devtools::install('{pkg_path}')")
      return(invisible(list(success = FALSE, error = conditionMessage(e))))
    })
  } else {
    cli::cli_h2("Next Steps")
    cli::cli_alert_info("Run the following to complete setup:")
    cli::cli_code("devtools::install('{pkg_path}')")
  }
  
  cli::cli_rule()
  cli::cli_alert_success("Configuration complete!")
  
  invisible(list(
    success = TRUE, 
    config_path = config_path,
    mode = "secure"
  ))
}

# ============================================================================
# INTERACTIVE SETUP WIZARD
# ============================================================================

#' Interactive setup wizard for autograder
#' 
#' @description
#' Guides instructors through the configuration process step by step
#' with prompts and validation at each step.
#' 
#' @param pkg_path Path to autograder package source (optional, will prompt)
#' 
#' @return Invisible result of configuration
#' 
#' @examples
#' \dontrun{
#' autograder_setup_wizard()
#' }
#' 
#' @keywords internal
autograder_setup_wizard <- function(pkg_path = NULL) {
  
  cli::cli_h1("Autograder Setup Wizard")
  cli::cli_text("")
  cli::cli_text("This wizard will help you configure the autograder package")
  cli::cli_text("for your course.")
  cli::cli_text("")
  cli::cli_rule()
  
  # Check for interactive session

if (!interactive()) {
    cli::cli_alert_danger("This wizard requires an interactive R session.")
    return(invisible(list(success = FALSE)))
  }
  
  # Step 1: Check build tools
  cli::cli_h2("Step 1: Checking Prerequisites")
  
  if (!autograder_check_build_tools()) {
    cli::cli_alert_danger("Please install build tools first and run the wizard again.")
    return(invisible(list(success = FALSE)))
  }
  
  # Step 2: Repository type
  cli::cli_h2("Step 2: Repository Type")
  cli::cli_text("")
  cli::cli_text("Is your repository public or private?")
  cli::cli_ol(c(
    "Public (anyone can see the code)",
    "Private (requires GitHub token)"
  ))
  cli::cli_text("")
  
  repo_type <- readline("Enter 1 or 2: ")
  use_private <- repo_type == "2"
  
  # Step 3: Get URL
  cli::cli_h2("Step 3: Repository URL")
  cli::cli_text("")
  cli::cli_text("Enter your repository URL in this format:")
  cli::cli_code("https://raw.githubusercontent.com/USERNAME/REPO/BRANCH/FOLDER")
  cli::cli_text("")
  cli::cli_text("Example: https://raw.githubusercontent.com/myname/myrepo/main/repo")
  cli::cli_text("")
  
  url <- readline("Repository URL: ")
  
  if (nchar(trimws(url)) == 0) {
    cli::cli_alert_danger("URL is required!")
    return(invisible(list(success = FALSE)))
  }
  
  # Validate URL format
  if (!grepl("^https://", url)) {
    cli::cli_alert_warning("URL should start with https://")
  }
  
  # Step 4: Get token (if private)
  token <- NULL
  if (use_private) {
    cli::cli_h2("Step 4: GitHub Token")
    cli::cli_text("")
    cli::cli_text("Enter your GitHub Personal Access Token.")
    cli::cli_text("Create one at: {.url https://github.com/settings/tokens}")
    cli::cli_text("")
    cli::cli_alert_info("Required permission: 'Contents: Read'")
    cli::cli_text("")
    
    # Use invisible input if available
    if (requireNamespace("getPass", quietly = TRUE)) {
      token <- getPass::getPass("GitHub Token (hidden): ")
    } else {
      cli::cli_alert_warning("Install 'getPass' package for hidden input")
      token <- readline("GitHub Token: ")
    }
    
    if (nchar(trimws(token)) == 0) {
      cli::cli_alert_danger("Token is required for private repositories!")
      return(invisible(list(success = FALSE)))
    }
    
    # Test token
    cli::cli_text("")
    cli::cli_alert("Testing token...")
    test_result <- autograder_test_token(url, token, verbose = FALSE)
    
    if (!test_result$success) {
      cli::cli_alert_danger("Token test failed: {test_result$error}")
      retry <- readline("Try again? (y/n): ")
      if (tolower(retry) != "y") {
        return(invisible(list(success = FALSE)))
      }
      return(autograder_setup_wizard(pkg_path))  # Restart wizard
    }
    cli::cli_alert_success("Token verified!")
  }
  
  # Step 5: Package path
  cli::cli_h2("Step 5: Package Location")
  
  if (is.null(pkg_path)) {
    # Try to find it
    possible_paths <- c(
      "autograder",
      "../autograder", 
      file.path(getwd(), "autograder")
    )
    
    found_path <- NULL
    for (p in possible_paths) {
      if (file.exists(file.path(p, "DESCRIPTION"))) {
        found_path <- normalizePath(p)
        break
      }
    }
    
    if (!is.null(found_path)) {
      cli::cli_alert_info("Found package at: {.path {found_path}}")
      use_found <- readline("Use this path? (y/n): ")
      if (tolower(use_found) == "y") {
        pkg_path <- found_path
      }
    }
    
    if (is.null(pkg_path)) {
      cli::cli_text("Enter the path to the autograder package source:")
      pkg_path <- readline("Package path: ")
    }
  }
  
  # Step 6: Confirm and execute
  cli::cli_h2("Step 6: Confirmation")
  cli::cli_text("")
  cli::cli_alert_info("URL: {.url {url}}")
  cli::cli_alert_info("Mode: {if (use_private) 'Private' else 'Public'}")
  cli::cli_alert_info("Package: {.path {pkg_path}}")
  cli::cli_text("")
  
  confirm <- readline("Proceed with configuration? (y/n): ")
  
  if (tolower(confirm) != "y") {
    cli::cli_alert_warning("Configuration cancelled.")
    return(invisible(list(success = FALSE)))
  }
  
  # Run configuration
  result <- autograder_configure(
    url = url,
    token = token,
    pkg_path = pkg_path,
    install = TRUE
  )
  
  invisible(result)
}

# ============================================================================
# RSTUDIO ADDIN FUNCTION
# ============================================================================

#' RStudio Addin: Configure Autograder
#' 
#' @description
#' Opens a dialog for configuring autograder (used by RStudio Addins menu).
#' 
#' @keywords internal
autograder_addin_configure <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("This addin requires RStudio")
  }
  
  if (!rstudioapi::isAvailable()) {
    stop("This addin must be run from within RStudio")
  }
  
  # Use miniUI for dialog if available, otherwise fall back to wizard
  if (requireNamespace("miniUI", quietly = TRUE) && 
      requireNamespace("shiny", quietly = TRUE)) {
    .run_config_dialog()
  } else {
    autograder_setup_wizard()
  }
}

.run_config_dialog <- function() {
  # Shiny gadget for configuration
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Configure Autograder"),
    miniUI::miniContentPanel(
      shiny::selectInput("repo_type", "Repository Type:",
                         choices = c("Public" = "public", "Private" = "private")),
      shiny::textInput("url", "Repository URL:", 
                       placeholder = "https://raw.githubusercontent.com/..."),
      shiny::conditionalPanel(
        condition = "input.repo_type == 'private'",
        shiny::passwordInput("token", "GitHub Token:")
      ),
      shiny::textInput("pkg_path", "Package Path:", 
                       value = if (file.exists("autograder/DESCRIPTION")) "autograder" else ""),
      shiny::hr(),
      shiny::actionButton("test", "Test Connection", class = "btn-info"),
      shiny::verbatimTextOutput("test_result")
    )
  )
  
  server <- function(input, output, session) {
    output$test_result <- shiny::renderText({
      shiny::req(input$test)
      shiny::isolate({
        if (input$repo_type == "private" && nchar(input$token) > 0) {
          result <- autograder_test_token(input$url, input$token, verbose = FALSE)
          if (result$success) "Token verified!" else paste("Failed:", result$error)
        } else if (input$repo_type == "public") {
          "Public repo - no token needed"
        } else {
          "Enter token first"
        }
      })
    })
    
    shiny::observeEvent(input$done, {
      token <- if (input$repo_type == "private") input$token else NULL
      
      result <- autograder_configure(
        url = input$url,
        token = token,
        pkg_path = input$pkg_path,
        install = TRUE
      )
      
      shiny::stopApp(result)
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Configure Autograder", width = 500, height = 400))
}
