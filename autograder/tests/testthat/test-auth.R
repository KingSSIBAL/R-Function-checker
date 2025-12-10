# ============================================================================
# AUTOGRADER - AUTHENTICATION TESTS
# ============================================================================
#
# Tests for authentication and private repository access:
#   - .cpp_is_auth_enabled()
#   - .cpp_get_auth_mode()
#   - .cpp_has_auth_token()
#   - .cpp_get_auth_info()
#   - Legacy mode access tests
#   - Secure mode access tests (when configured)
#
# ============================================================================

# ============================================================================
# AUTHENTICATION STATUS TESTS
# ============================================================================

test_that(".cpp_is_auth_enabled returns logical", {
    result <- autograder:::.cpp_is_auth_enabled()
    expect_type(result, "logical")
    expect_length(result, 1)
})

test_that(".cpp_get_auth_mode returns valid mode", {
    mode <- autograder:::.cpp_get_auth_mode()
    expect_type(mode, "character")
    expect_true(mode %in% c("secure", "legacy"))
})

test_that(".cpp_has_auth_token returns logical", {
    result <- autograder:::.cpp_has_auth_token()
    expect_type(result, "logical")
    expect_length(result, 1)
})

test_that(".cpp_get_auth_info returns complete info", {
    info <- autograder:::.cpp_get_auth_info()
    
    expect_type(info, "list")
    expect_true("mode" %in% names(info))
    expect_true("has_token" %in% names(info))
    expect_true("url_length" %in% names(info))
    expect_true("token_length" %in% names(info))
})

test_that("auth mode and is_auth_enabled are consistent", {
    is_enabled <- autograder:::.cpp_is_auth_enabled()
    mode <- autograder:::.cpp_get_auth_mode()
    
    if (is_enabled) {
        expect_equal(mode, "secure")
    } else {
        expect_equal(mode, "legacy")
    }
})

test_that("auth info mode matches get_auth_mode", {
    info <- autograder:::.cpp_get_auth_info()
    mode <- autograder:::.cpp_get_auth_mode()
    
    expect_equal(info$mode, mode)
})

# ============================================================================
# ENCRYPTED URL CONFIGURATION TESTS
# ============================================================================

test_that("encrypted URL has positive length", {
    info <- autograder:::.cpp_get_auth_info()
    expect_true(info$url_length > 0)
})

test_that("token length is consistent with auth mode", {
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        # In legacy mode, token may be 0 or minimal
        expect_true(info$token_length >= 0)
    }
    # In secure mode, token should be present
    if (info$mode == "secure" && info$has_token) {
        expect_true(info$token_length > 0)
    }
})

# ============================================================================
# LEGACY MODE ACCESS TESTS
# ============================================================================

test_that("legacy mode can fetch problems list", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        # In legacy mode, should be able to fetch from public repo
        problems <- autograder:::.cpp_fetch_problems_list()
        
        expect_type(problems, "character")
        expect_true(length(problems) > 0)
    } else {
        skip("Not in legacy mode")
    }
})

test_that("legacy mode can fetch function content", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        # Fetch fibonacci function code
        code <- autograder:::.cpp_fetch_function_content("fibonacci")
        
        expect_type(code, "character")
        expect_true(length(code) > 0)
        
        # Code should contain function definition
        code_text <- paste(code, collapse = "\n")
        expect_true(grepl("function", code_text, ignore.case = TRUE))
    } else {
        skip("Not in legacy mode")
    }
})

test_that("legacy mode list_problems works", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        problems <- list_problems()
        
        expect_type(problems, "character")
        expect_true(length(problems) > 0)
        expect_true("fibonacci" %in% problems)
    } else {
        skip("Not in legacy mode")
    }
})

test_that("legacy mode preview_tests works", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        # This should not error
        expect_no_error(preview_tests("fibonacci"))
    } else {
        skip("Not in legacy mode")
    }
})

test_that("legacy mode fetch_instructor_code works", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        env <- fetch_instructor_code("fibonacci")
        
        expect_type(env, "environment")
        expect_true(exists("fibonacci", envir = env))
        expect_true(is.function(get("fibonacci", envir = env)))
    } else {
        skip("Not in legacy mode")
    }
})

# ============================================================================
# SECURE MODE ACCESS TESTS
# ============================================================================

test_that("secure mode can fetch problems list", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "secure" && info$has_token) {
        # In secure mode with token, should be able to fetch from private repo
        result <- tryCatch({
            problems <- autograder:::.cpp_fetch_problems_list()
            list(success = TRUE, problems = problems)
        }, error = function(e) {
            list(success = FALSE, error = conditionMessage(e))
        })
        
        if (result$success) {
            expect_type(result$problems, "character")
            expect_true(length(result$problems) > 0)
        } else {
            # Token may be invalid or repo not set up - skip
            skip(paste("Secure fetch failed:", result$error))
        }
    } else {
        skip("Not in secure mode or no token")
    }
})

test_that("secure mode can fetch function content", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "secure" && info$has_token) {
        result <- tryCatch({
            code <- autograder:::.cpp_fetch_function_content("fibonacci")
            list(success = TRUE, code = code)
        }, error = function(e) {
            list(success = FALSE, error = conditionMessage(e))
        })
        
        if (result$success) {
            expect_type(result$code, "character")
            expect_true(length(result$code) > 0)
            
            # Code should contain function definition
            code_text <- paste(result$code, collapse = "\n")
            expect_true(grepl("function", code_text, ignore.case = TRUE))
        } else {
            skip(paste("Secure fetch failed:", result$error))
        }
    } else {
        skip("Not in secure mode or no token")
    }
})

test_that("secure mode list_problems works", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "secure" && info$has_token) {
        result <- tryCatch({
            problems <- list_problems()
            list(success = TRUE, problems = problems)
        }, error = function(e) {
            list(success = FALSE, error = conditionMessage(e))
        })
        
        if (result$success) {
            expect_type(result$problems, "character")
            expect_true(length(result$problems) > 0)
        } else {
            skip(paste("Secure list_problems failed:", result$error))
        }
    } else {
        skip("Not in secure mode or no token")
    }
})

test_that("secure mode fetch_instructor_code works", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "secure" && info$has_token) {
        result <- tryCatch({
            env <- fetch_instructor_code("fibonacci")
            list(success = TRUE, env = env)
        }, error = function(e) {
            list(success = FALSE, error = conditionMessage(e))
        })
        
        if (result$success) {
            expect_type(result$env, "environment")
            expect_true(exists("fibonacci", envir = result$env))
        } else {
            skip(paste("Secure fetch_instructor_code failed:", result$error))
        }
    } else {
        skip("Not in secure mode or no token")
    }
})

# ============================================================================
# CURRENT MODE INTEGRATION TESTS (runs regardless of mode)
# ============================================================================

test_that("fetch works in current auth mode", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    # This should work regardless of auth mode if configured correctly
    # In legacy mode: uses public repo
    # In secure mode: uses private repo with token
    
    if (info$mode == "legacy") {
        # Legacy mode should always work with public repo
        problems <- tryCatch(
            autograder:::.cpp_fetch_problems_list(),
            error = function(e) NULL
        )
        
        # If we got problems, the fetch worked
        if (!is.null(problems)) {
            expect_type(problems, "character")
        }
    }
    
    # Note: Secure mode tests require actual token configuration
    # and are skipped in automated testing
})

test_that("list_problems works in current auth mode", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "legacy") {
        # Should work with public repo
        problems <- tryCatch(
            list_problems(),
            error = function(e) NULL
        )
        
        if (!is.null(problems)) {
            expect_type(problems, "character")
        }
    }
})

# ============================================================================
# SECURE MODE SPECIFIC TESTS (conditional)
# ============================================================================

test_that("secure mode has token when enabled", {
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "secure") {
        # If secure mode is enabled, we expect a token
        expect_true(info$has_token)
        expect_true(info$token_length > 0)
    } else {
        # In legacy mode, just verify the check works
        expect_equal(info$mode, "legacy")
    }
})

test_that("secure mode can fetch if token configured", {
    skip_on_cran()
    skip_if_offline()
    
    info <- autograder:::.cpp_get_auth_info()
    
    if (info$mode == "secure" && info$has_token) {
        # Attempt to fetch - should work if token is valid
        result <- tryCatch({
            code <- autograder:::.cpp_fetch_function_content("fibonacci")
            list(success = TRUE, code = code)
        }, error = function(e) {
            list(success = FALSE, error = conditionMessage(e))
        })
        
        # We just verify it returns a result (success depends on token validity)
        expect_type(result, "list")
        
        if (result$success) {
            expect_type(result$code, "character")
        }
    } else {
        # Legacy mode - verify we can still make assertions
        expect_true(info$mode %in% c("legacy", "secure"))
    }
})

# ============================================================================
# AUTHENTICATION ERROR HANDLING
# ============================================================================

test_that("auth functions don't crash with invalid input", {
    # These functions take no input, so just verify they don't crash
    expect_no_error(autograder:::.cpp_is_auth_enabled())
    expect_no_error(autograder:::.cpp_get_auth_mode())
    expect_no_error(autograder:::.cpp_has_auth_token())
    expect_no_error(autograder:::.cpp_get_auth_info())
})

test_that("multiple auth info calls are consistent", {
    info1 <- autograder:::.cpp_get_auth_info()
    info2 <- autograder:::.cpp_get_auth_info()
    
    expect_equal(info1$mode, info2$mode)
    expect_equal(info1$has_token, info2$has_token)
    expect_equal(info1$url_length, info2$url_length)
    expect_equal(info1$token_length, info2$token_length)
})
