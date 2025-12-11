# ============================================================================
# AUTOGRADER - CACHE TESTS
# ============================================================================
#
# Tests for memoise caching functionality
#
# ============================================================================

# ============================================================================
# CACHE INFO TESTS
# ============================================================================

test_that("cache_info returns expected structure", {
  info <- cache_info()
  
  expect_true(is.list(info))
  expect_true("instructor_cache_size" %in% names(info))
  expect_true("cache_dir" %in% names(info))
  expect_true("cache_dir_size_bytes" %in% names(info))
  expect_true("cache_timeout_seconds" %in% names(info))
})

test_that("cache_info values are sensible", {
  info <- cache_info()
  
  expect_true(info$instructor_cache_size >= 0)
  expect_true(is.character(info$cache_dir))
  expect_true(info$cache_dir_size_bytes >= 0)
  expect_true(info$cache_timeout_seconds > 0)
})

# ============================================================================
# SET_CACHE_TIMEOUT TESTS
# ============================================================================

test_that("set_cache_timeout validates input", {
  expect_error(set_cache_timeout("not a number"))
  expect_error(set_cache_timeout(-100))
  expect_error(set_cache_timeout(c(100, 200)))
})

test_that("set_cache_timeout changes option", {
  old <- getOption("autograder.cache_timeout")
  
  set_cache_timeout(7200)
  expect_equal(getOption("autograder.cache_timeout"), 7200)
  
  # Restore
  set_cache_timeout(old)
  expect_equal(getOption("autograder.cache_timeout"), old)
})

test_that("set_cache_timeout returns old value", {
  old <- getOption("autograder.cache_timeout")
  
  result <- set_cache_timeout(9999)
  expect_equal(result, old)
  
  # Restore
  set_cache_timeout(old)
})

# ============================================================================
# CLEAR_ALL_CACHES TESTS
# ============================================================================

test_that("clear_all_caches runs without error", {
  expect_message(clear_all_caches(), "cleared")
})

test_that("clear_all_caches clears instructor cache", {
  # Add something to cache
  assign("test_item", "value", envir = autograder:::.instructor_cache)
  
  # Clear
  suppressMessages(clear_all_caches())
  
  # Check it's gone
  expect_false(exists("test_item", envir = autograder:::.instructor_cache))
})

# ============================================================================
# MEMOISED FUNCTION TESTS
# ============================================================================

test_that(".get_memoised_fetch_problems returns a function", {
  fn <- .get_memoised_fetch_problems()
  expect_true(is.function(fn))
})

test_that(".get_memoised_fetch_data returns a function", {
  fn <- .get_memoised_fetch_data()
  expect_true(is.function(fn))
})
