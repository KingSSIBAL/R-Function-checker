# =============================================================================
# INSTRUCTOR FUNCTION
# =============================================================================

fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  for (i in 3:n) {
    fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
}

# =============================================================================
# TEST CASES
# =============================================================================

test_cases <- list(
  inputs = list(
    list(1),
    list(5),
    list(10),
    list(15),
    list(0),      # Hidden edge case
    list(20)      # Hidden larger test
  ),
  
  descriptions = c(
    "Base case: n = 1",
    "Small input: n = 5",
    "Medium input: n = 10",
    "Larger input: n = 15",
    "Edge case: n = 0",
    "Large input: n = 20"
  ),
  
  points = c(
    1,   # Base case
    2,   # Small
    2,   # Medium
    3,   # Larger
    1,   # Edge case
    1    # Hidden large
  ),
  
  hidden = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,   # Hidden edge case
    TRUE    # Hidden large test
  ),
  
  expected_type = "numeric",
  tolerance = 1e-10
)
