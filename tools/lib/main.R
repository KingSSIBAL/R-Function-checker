# ============================================================================
# MAIN FUNCTIONS & CONFIGURATION
# ============================================================================

# Load configuration from .env
.env_config <- load_env_file()

USE_AUTHENTICATION <- TRUE  # Always use secure mode
REPOSITORY_URL <- .env_config$REPO_URL %||% "https://api.github.com/repos/KingSSIBAL/R-Function-checker/contents"
GITHUB_TOKEN <- .env_config$GITHUB_TOKEN %||% ""
KEY_FACTORS <- if (!is.null(.env_config$KEY_FACTORS)) {
    strsplit(.env_config$KEY_FACTORS, ",")[[1]]
} else {
    c("AUTOGRADER_SECURE_KEY_2025", "v0.4.0", "R-FUNC-CHK", "MODULAR-CPP")
}
CONFIG_FILE_PATH <- find_package_dir()

# ============================================================================
# PUBLIC API
# ============================================================================

#' Test GitHub token authentication
#' @export
test_token <- function(url = REPOSITORY_URL, token = GITHUB_TOKEN, verbose = TRUE) {
    .test_token_impl(url, token, verbose)
}

#' Encrypt URL and token, update package configuration
#' @export
encrypt_and_update <- function(url = REPOSITORY_URL, token = GITHUB_TOKEN,
                                use_auth = USE_AUTHENTICATION, 
                                key_factors = KEY_FACTORS,
                                config_path = CONFIG_FILE_PATH, verbose = TRUE) {
    if (verbose) {
        cat("\n╔════════════════════════════════════════════════════════════════════╗\n")
        cat("║          AUTOGRADER URL ENCRYPTION HELPER v0.4.0                   ║\n")
        cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
        cat("🔐 MODE: SECURE (Private Repo)\n\n")
    }
    
    if (use_auth && nchar(token) == 0) {
        if (verbose) cat("❌ ERROR: No token provided for secure mode!\n")
        return(invisible(list(success = FALSE, error = "No token")))
    }
    
    if (verbose) cat("📦 Deriving key and encrypting...\n")
    key <- derive_key(key_factors)
    encrypted_url <- encrypt_string(url, key)
    encrypted_token <- if (use_auth && nchar(token) > 0) encrypt_string(token, key) else NULL
    
    cpp_content <- generate_config_content(encrypted_url, encrypted_token, key_factors, use_auth)
    
    if (!dir.exists(dirname(config_path))) dir.create(dirname(config_path), recursive = TRUE)
    
    tryCatch({
        writeLines(cpp_content, config_path)
        if (verbose) {
            cat("\n✅ SUCCESS! Updated: ", config_path, "\n\n")
            cat("Next: devtools::install(\"autograder\")\n")
        }
        invisible(list(success = TRUE, config_path = config_path))
    }, error = function(e) {
        if (verbose) cat("❌ ERROR: ", conditionMessage(e), "\n")
        invisible(list(success = FALSE, error = conditionMessage(e)))
    })
}

#' Quick setup for secure mode
#' @export
setup_secure_mode <- function(url, token) {
    encrypt_and_update(url = url, token = token, use_auth = TRUE)
}

#' Show help
#' @export
show_help <- function() {
    cat("\n╔════════════════════════════════════════════════════════════════════╗\n")
    cat("║       AUTOGRADER ENCRYPTION HELPER                                 ║\n")
    cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
    cat("  test_token()         - Test GitHub token\n")
    cat("  encrypt_and_update() - Encrypt and update package\n\n")
    cat("  Mode: SECURE | ")
    cat("Token: ", ifelse(nchar(GITHUB_TOKEN) > 0, "✓ Loaded", "✗ Not set"), "\n\n")
}
