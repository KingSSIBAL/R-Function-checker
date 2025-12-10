# ============================================================================
# TOKEN TESTING FUNCTIONS
# ============================================================================

.test_token_impl <- function(url, token, verbose = TRUE) {
    if (verbose) {
        cat("\n")
        cat("╔════════════════════════════════════════════════════════════════════╗\n")
        cat("║              GITHUB TOKEN TEST                                     ║\n")
        cat("╚════════════════════════════════════════════════════════════════════╝\n\n")
    }
    
    # Validate inputs
    if (is.null(token) || nchar(trimws(token)) == 0) {
        if (verbose) cat("❌ ERROR: No token provided!\n")
        return(invisible(list(success = FALSE, error = "No token provided")))
    }
    
    if (is.null(url) || nchar(trimws(url)) == 0) {
        if (verbose) cat("❌ ERROR: No repository URL provided!\n")
        return(invisible(list(success = FALSE, error = "No URL provided")))
    }
    
    if (verbose) {
        cat("🔗 URL: ", url, "\n")
        cat("🔑 Token: ", paste0(substr(token, 1, 10), "..."), " (masked)\n\n")
    }
    
    # Build test URL
    base_url <- sub("/$", "", url)
    test_url <- paste0(base_url, "/functions/_problems.R")
    
    if (verbose) cat("🌐 Testing connection...\n")
    
    # Test using curl
    result <- tryCatch({
        cmd <- sprintf(
            'curl -s -o NUL -w "%%{http_code}" -H "Authorization: token %s" -H "Accept: application/vnd.github.v3.raw" "%s"',
            token, test_url
        )
        
        status_code <- as.integer(trimws(system(cmd, intern = TRUE, ignore.stderr = TRUE)))
        
        if (verbose) cat("   HTTP Status: ", status_code, "\n")
        
        if (status_code == 200) {
            list(success = TRUE, status_code = status_code)
        } else {
            errors <- c(
                "401" = "401 Unauthorized - Token invalid or expired",
                "403" = "403 Forbidden - Token lacks 'Contents: Read' permission",
                "404" = "404 Not Found - Repo doesn't exist or token can't access it"
            )
            list(success = FALSE, error = errors[[as.character(status_code)]] %||% paste("HTTP", status_code))
        }
    }, error = function(e) {
        list(success = FALSE, error = conditionMessage(e))
    })
    
    if (verbose) {
        cat("\n═══════════════════════════════════════════════════════════════════════\n")
        if (result$success) {
            cat("                    ✅ TOKEN TEST PASSED!                              \n")
            cat("═══════════════════════════════════════════════════════════════════════\n\n")
            cat("Next: Run encrypt_and_update() to encrypt and build.\n")
        } else {
            cat("                    ❌ TOKEN TEST FAILED!                              \n")
            cat("═══════════════════════════════════════════════════════════════════════\n\n")
            cat("Error: ", result$error, "\n\n")
            cat("Troubleshooting:\n")
            cat("  • Verify token is correct and not expired\n")
            cat("  • Ensure token has 'Contents: Read' permission\n")
            cat("  • Check repository URL is correct\n")
            cat("  • For fine-grained tokens, verify repo is selected\n\n")
        }
    }
    
    invisible(result)
}
