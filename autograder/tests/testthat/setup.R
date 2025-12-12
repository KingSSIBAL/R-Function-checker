# ============================================================================
# Test Setup - Runs before all tests
# ============================================================================

# Disable rate limiting during tests
# Tests make many fetch calls which would otherwise hit rate limits
configure_rate_limit(max_calls = 10000L, window_seconds = 60L)

# Clear caches before tests
if (exists("clear_instructor_cache")) {
  tryCatch(
    clear_instructor_cache(),
    error = function(e) NULL
  )
}

# Set test options
options(autograder.strict_validation = TRUE)
