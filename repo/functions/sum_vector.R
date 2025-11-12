# =============================================================================
# INSTRUCTOR FUNCTION
# =============================================================================

sum_vector <- function(x) {
  if (length(x) == 0) return(0)
  total <- 0
  for (val in x) {
    total <- total + val
  }
  total
}

# =============================================================================
# TEST CASES
# =============================================================================

test_cases <- list(
  inputs = list(
    list(c(1, 2, 3)),
    list(c(10, 20, 30)),
    list(numeric(0)),
    list(c(-5, 5, -10, 10)),
    list(c(1.5, 2.5, 3.0))   # Hidden with floats
  ),
  
  descriptions = c(
    "Simple sum: [1,2,3]",
    "Larger numbers: [10,20,30]",
    "Empty vector",
    "Mixed positive/negative",
    "Floating point numbers"
  ),
  
  points = c(2, 2, 2, 2, 2),
  
  hidden = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  
  expected_type = "numeric",
  tolerance = 1e-8
)
