# Test Case Repository - Instructor Guide

[![Security](https://img.shields.io/badge/security-AES--inspired-green)](https://github.com/KingSSIBAL/R-Function-checker)
[![Instructors](https://img.shields.io/badge/audience-instructors-blue)](https://github.com/KingSSIBAL/R-Function-checker)

> **Central repository for managing autograder test cases, reference implementations, and grading configurations.**

## 🎓 For Instructors Only

This directory is where **YOU** (the instructor) create and manage all test cases that students will run against. The autograder package fetches these files securely when students call `autograder("function_name")`.

## 🚀 Quick Start - Create Your First Test Case

### 5-Minute Tutorial

**Step 1: Create the file** `repo/functions/double_number.R`

```r
# Your reference implementation (this is the "answer key")
double_number <- function(x) {
  return(x * 2)
}

# Test cases students will run against
test_cases <- list(
  inputs = list(
    list(5),      # Test 1: double_number(5) should return 10
    list(10),     # Test 2: double_number(10) should return 20
    list(0),      # Test 3: double_number(0) should return 0
    list(-3)      # Test 4: double_number(-3) should return -6
  ),

  descriptions = c("Basic case", "Larger number", "Zero", "Negative number"),
  hidden = c(FALSE, FALSE, TRUE, TRUE),  # Last 2 are hidden
  points = c(1, 1, 2, 2)                 # Total: 6 points
)
```

**Step 2: Add to problems list** in `repo/functions/_problems.R`

```r
problems <- c(
  "fibonacci",
  "factorial", 
  "sum_vector",
  "double_number"    # Add this
)
```

**Step 3: Commit and push**

```bash
git add repo/functions/double_number.R repo/functions/_problems.R
git commit -m "feat: Add double_number problem"
git push
```

**Step 4: Test it!**

```r
library(autograder)

# Students will now see it
list_problems()  # Should show "double_number"

# Verify your reference implementation passes
student_double_number <- function(x) x * 2
autograder("double_number")  # Should be 100%
```

**Done! Your test case is live.** 🎉

## 📁 How It Works

### The Flow

```
┌──────────────────────────────────────────────────────────────┐
│ 1. YOU (Instructor)                                          │
│    └─ Create: repo/functions/your_function.R                │
│    └─ Push to GitHub                                         │
└────────────┬─────────────────────────────────────────────────┘
             │
             │ git push
             ▼
┌──────────────────────────────────────────────────────────────┐
│ 2. GITHUB REPOSITORY                                         │
│    └─ Stores: Test cases + Reference implementation         │
│    └─ URL: https://raw.githubusercontent.com/.../main/repo  │
└────────────┬─────────────────────────────────────────────────┘
             │
             │ HTTPS (fetched at runtime)
             ▼
┌──────────────────────────────────────────────────────────────┐
│ 3. STUDENT'S COMPUTER                                        │
│    └─ Runs: autograder("your_function")                     │
│    └─ Package: Fetches, compares, provides feedback         │
└──────────────────────────────────────────────────────────────┘
```

### Security Layer

The C++ code includes **AES-inspired encryption** for URL obfuscation:

```cpp
// In autograder/src/autograder.cpp
static const uint8_t sbox[256] = { /* AES S-box */ };

std::string simple_decrypt(const std::string& encoded, const std::string& key) {
    // S-box transformation + XOR cipher
    for (size_t i = 0; i < encoded.length(); ++i) {
        uint8_t byte = sbox[static_cast<uint8_t>(encoded[i])];
        result[i] = byte ^ key[i % key_len];
    }
    return result;
}

std::string derive_key() {
    // Multi-factor key derivation
    std::string key_base = "AUTOGRADER_SECURE_KEY_2025";
    std::string var1 = "v0.2.0";
    std::string var2 = "R-FUNC-CHK";
    // S-box transformation for complexity
    // Returns 256-bit key
}
```

**Current Status:** URLs are currently **not encrypted** in production (plaintext for simplicity).

### 🏆 Security Challenge for Hackers

**⚠️ CHALLENGE TO STUDENTS: Can you break our encryption?**

If you're a student who has:
1. Successfully extracted and decrypted the test case URLs from the compiled C++ code
2. Documented your method with proof
3. Found a security vulnerability in the implementation

**Please contact me immediately:** rcagub@up.edu.ph

**Your reward:**
- 🎯 **I will personally hire you** as a security consultant for future versions
- 💰 Potential paid position depending on the severity
- 🏆 Credit in the project acknowledgments
- 📝 Reference letter highlighting your security skills

**Rules:**
- Must be a legitimate security finding (not just reading this source code on GitHub)
- Must work on the compiled `.so`/`.dll` binary
- Must document your methodology
- Must be ethical (no malicious use of findings)

**Why this challenge?**
- Encourages security research
- Improves the package
- Identifies talented students
- Promotes cybersecurity education

**Note to Instructors:** This challenge is intentional. It:
- Makes security educational
- Identifies exceptional students
- Improves the codebase
- The test cases aren't secrets anyway (students need to understand them)

---

## 📝 Test Case File Format

### Complete Template

```r
# ============================================================================
# FUNCTION: function_name
# ============================================================================
# Description: What this function does
# Difficulty: Easy | Medium | Hard
# Topics: List of topics (loops, recursion, vectors, etc.)
# Estimated Time: 15-30 minutes
# Learning Objectives:
#   - Objective 1
#   - Objective 2
# Prerequisites: Prior knowledge needed
# ============================================================================

#' Reference implementation
#' 
#' @description Complete description
#' @param param1 Description
#' @param param2 Description
#' @return What it returns
#' @examples
#' function_name(arg1, arg2)
function_name <- function(param1, param2) {
  # YOUR CORRECT IMPLEMENTATION
  # This is what students' code will be compared against

  result <- # your logic
  return(result)
}

# ============================================================================
# TEST CASES
# ============================================================================

test_cases <- list(

  # ===== REQUIRED: Inputs =====
  # List of lists - each inner list is one set of function arguments
  inputs = list(
    list(arg1_val, arg2_val),  # Test 1
    list(arg1_val, arg2_val),  # Test 2
    # ... more tests
  ),

  # ===== OPTIONAL: Descriptions =====
  # Shown to students - be specific and helpful
  descriptions = c(
    "Base case: simplest input",
    "Typical usage: medium input",
    "Edge case: boundary condition",
    "Edge case: special value"
  ),

  # ===== OPTIONAL: Hidden Tests =====
  # TRUE = hidden from students (prevents hard-coding)
  # FALSE = visible to students (they can see inputs)
  # Recommended: 25-40% hidden
  hidden = c(
    FALSE,   # Visible: students see input
    FALSE,   # Visible
    TRUE,    # Hidden: students don't see input
    TRUE     # Hidden
  ),

  # ===== OPTIONAL: Points =====
  # Weight tests by difficulty/importance
  # Recommended total: 10-20 points per problem
  points = c(
    1,    # Easy test
    2,    # Standard test
    2,    # Edge case
    3     # Complex edge case
  ),

  # ===== OPTIONAL: Tolerance =====
  # For floating-point comparisons
  # 1e-10: strict (for integer-like results)
  # 1e-6: relaxed (for decimal calculations)
  # 1e-3: very relaxed (for approximations)
  tolerance = 1e-10,

  # ===== OPTIONAL: Expected Type =====
  # Catches type errors early
  # Options: "numeric", "integer", "character", "logical", "list", "matrix", etc.
  expected_type = "numeric",

  # ===== OPTIONAL: Hints =====
  # Guidance for failed tests (shown if show_hints = TRUE)
  # Guide without solving - teach the concept
  hints = c(
    "Check the base case",
    "Use a loop to process all elements",
    "Don't forget boundary conditions",
    "Consider what happens with special values"
  )

  # ===== OPTIONAL: Custom Comparison =====
  # For complex objects (matrices, data frames, plots)
  # comparison_fn = function(student_output, expected_output) {
  #   # Return TRUE if outputs match, FALSE otherwise
  #   # Your custom comparison logic
  # }
)

# ============================================================================
# TESTING NOTES (for your reference - students don't see this)
# ============================================================================
# Common student mistakes to watch for:
#   - Mistake 1 and how to hint
#   - Mistake 2 and how to hint
# 
# Edge cases covered:
#   - Edge case 1 (test #3)
#   - Edge case 2 (test #4)
# ============================================================================
```

## 🎯 Real-World Examples

### Example 1: Simple Function (10 minutes to create)

**Problem:** Calculate factorial

```r
# repo/functions/factorial.R

factorial_fn <- function(n) {
  if (n == 0 || n == 1) return(1)
  result <- 1
  for (i in 2:n) {
    result <- result * i
  }
  result
}

test_cases <- list(
  inputs = list(
    list(0),    # 0! = 1
    list(1),    # 1! = 1
    list(5),    # 5! = 120
    list(10),   # 10! = 3628800
    list(3),    # Hidden: 3! = 6
    list(7)     # Hidden: 7! = 5040
  ),

  descriptions = c(
    "Base case: 0! = 1",
    "Base case: 1! = 1", 
    "Small factorial: 5!",
    "Larger factorial: 10!",
    "Medium value",
    "Another value"
  ),

  hidden = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  points = c(1, 1, 2, 2, 2, 2),  # Total: 10 points
  expected_type = "numeric",

  hints = c(
    "0! is defined as 1",
    "1! is defined as 1",
    "5! = 5 × 4 × 3 × 2 × 1",
    "Use a loop from 2 to n",
    "Multiply consecutive integers",
    "Check your loop logic"
  )
)
```

**Time to create:** ~10 minutes  
**Difficulty:** Easy  
**Topics:** Loops, base cases, integer arithmetic

### Example 2: Vector Function (15 minutes to create)

**Problem:** Find maximum value in vector

```r
# repo/functions/find_max.R

find_max <- function(vec) {
  if (length(vec) == 0) return(NA)

  max_val <- vec[1]
  for (val in vec) {
    if (!is.na(val) && val > max_val) {
      max_val <- val
    }
  }
  max_val
}

test_cases <- list(
  inputs = list(
    list(c(1, 2, 3)),           # Simple case
    list(c(5, 2, 9, 1)),        # Unsorted
    list(c(10, 10, 10)),        # All same
    list(c(3)),                 # Single element
    list(numeric(0)),           # Empty vector
    list(c(1, NA, 5)),          # With NA
    list(c(-5, -2, -10)),       # All negative
    list(c(100, 50, 75, 99))    # Hidden: medium
  ),

  descriptions = c(
    "Simple sorted vector",
    "Unsorted vector",
    "All identical values",
    "Single element",
    "Edge: empty vector",
    "With NA values",
    "All negative numbers",
    "Typical case"
  ),

  hidden = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  points = c(1, 1, 1, 1, 2, 2, 2, 2),  # Total: 12 points
  expected_type = "numeric",

  hints = c(
    "Start with the first element",
    "Compare each element with current max",
    "All values are the same - any is the max",
    "Single element is the max by default",
    "Empty vector should return NA",
    "Skip NA values when comparing",
    "Max of negatives is the least negative",
    "Iterate through all elements"
  )
)
```

**Time to create:** ~15 minutes  
**Difficulty:** Easy-Medium  
**Topics:** Vectors, iteration, edge cases, NA handling

### Example 3: Advanced Function (30 minutes to create)

**Problem:** Matrix multiplication

```r
# repo/functions/matrix_multiply.R

matrix_multiply <- function(A, B) {
  # Validate dimensions
  if (ncol(A) != nrow(B)) {
    stop("Incompatible matrix dimensions for multiplication")
  }

  # Initialize result matrix
  result <- matrix(0, nrow = nrow(A), ncol = ncol(B))

  # Perform multiplication
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(B)) {
      for (k in 1:ncol(A)) {
        result[i, j] <- result[i, j] + A[i, k] * B[k, j]
      }
    }
  }

  result
}

test_cases <- list(
  inputs = list(
    # Test 1: Simple 2x2
    list(matrix(c(1,2,3,4), 2, 2), matrix(c(5,6,7,8), 2, 2)),

    # Test 2: Non-square matrices
    list(matrix(1:6, 2, 3), matrix(1:6, 3, 2)),

    # Test 3: Identity matrix
    list(diag(3), matrix(1:9, 3, 3)),

    # Test 4: Single element
    list(matrix(2), matrix(3)),

    # Test 5: Hidden - larger matrices
    list(matrix(1:12, 3, 4), matrix(1:8, 4, 2)),

    # Test 6: Hidden - with zeros
    list(matrix(c(1,0,0,1), 2, 2), matrix(c(0,1,1,0), 2, 2))
  ),

  descriptions = c(
    "Basic 2x2 matrices",
    "Non-square matrices (2x3 × 3x2)",
    "Identity matrix property",
    "Smallest case (1x1)",
    "Larger matrices",
    "With zero elements"
  ),

  hidden = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  points = c(2, 3, 2, 1, 3, 3),  # Total: 14 points

  # Custom comparison for matrices
  comparison_fn = function(student, expected) {
    if (!is.matrix(student) || !is.matrix(expected)) return(FALSE)
    if (any(dim(student) != dim(expected))) return(FALSE)
    all(abs(student - expected) < 1e-9)
  },

  hints = c(
    "Result[i,j] = sum of A[i,k] * B[k,j] for all k",
    "Result dimensions: nrow(A) × ncol(B)",
    "Identity matrix: I × A = A",
    "Simplest multiplication",
    "Use three nested loops: i, j, k",
    "Check your initialization - result should start at 0"
  )
)
```

**Time to create:** ~30 minutes  
**Difficulty:** Medium-Hard  
**Topics:** Matrices, nested loops, dimensions, algorithm complexity

## 📋 Field Reference Guide

### Required Fields

#### `inputs` (MUST HAVE)

List of lists. Each inner list contains arguments for one test.

```r
# Example: function with 2 parameters
inputs = list(
  list(arg1 = 5, arg2 = 10),   # Named arguments
  list(3, 7)                   # Or positional
)

# Example: function with 1 parameter
inputs = list(
  list(5),
  list(10),
  list(0)
)

# Example: function with vector parameter
inputs = list(
  list(c(1, 2, 3)),
  list(c(5, 10, 15))
)
```

### Optional Fields

#### `descriptions` (Highly Recommended)

Help students understand what's being tested.

```r
# ❌ Not helpful
descriptions = c("Test 1", "Test 2", "Test 3")

# ✅ Helpful
descriptions = c(
  "Base case: n = 1",
  "Small input: n = 5", 
  "Edge case: n = 0"
)
```

#### `hidden` (Recommended for integrity)

Prevent students from hard-coding specific test cases.

```r
# Recommended distribution
hidden = c(
  FALSE, FALSE, FALSE,  # 60% visible
  TRUE, TRUE            # 40% hidden
)

# For exams/assessments - more hidden
hidden = c(
  FALSE, FALSE,         # 40% visible
  TRUE, TRUE, TRUE      # 60% hidden
)

# For practice - mostly visible
hidden = c(
  FALSE, FALSE, FALSE, FALSE,  # 80% visible
  TRUE                          # 20% hidden
)
```

#### `points` (Recommended for grading)

Weight tests by difficulty or importance.

```r
# Equal points (simple)
points = rep(1, 6)  # All worth 1 point

# Progressive difficulty
points = c(1, 1, 2, 2, 3, 3)  # Harder = more points

# Emphasis on edge cases
points = c(1, 1, 1, 3, 3, 3)  # Edge cases worth more
```

#### `tolerance` (Important for numerical work)

Controls floating-point comparison strictness.

```r
# Very strict (integer-like results)
tolerance = 1e-10

# Standard (most numerical work)
tolerance = 1e-8

# Relaxed (iterative algorithms)
tolerance = 1e-6

# Very relaxed (approximations)
tolerance = 1e-3
```

#### `expected_type` (Catches common mistakes)

Validates output type before comparing values.

```r
expected_type = "numeric"    # For numbers
expected_type = "integer"    # For integers
expected_type = "character"  # For strings
expected_type = "logical"    # For TRUE/FALSE
expected_type = "list"       # For lists
expected_type = "matrix"     # For matrices
expected_type = "data.frame" # For data frames
```

#### `hints` (Pedagogical - guides learning)

Provide guidance without solving.

```r
# ❌ Too revealing
hints = c("Return n * n")

# ✅ Guides without solving
hints = c(
  "Think about what happens when n is 0",
  "Use a loop to process each element",
  "Consider the mathematical formula"
)

# ✅ References concepts
hints = c(
  "Review the lecture on recursion",
  "Base case is when the recursion stops",
  "Remember to initialize your accumulator"
)
```

#### `comparison_fn` (Advanced - custom comparison)

For non-standard comparisons.

```r
# Example 1: Ignore attribute differences
comparison_fn = function(student, expected) {
  identical(as.vector(student), as.vector(expected))
}

# Example 2: Check structure, not exact values
comparison_fn = function(student, expected) {
  is.data.frame(student) && 
  ncol(student) == ncol(expected) &&
  all(names(student) == names(expected))
}

# Example 3: Tolerance with NA handling
comparison_fn = function(student, expected) {
  if (length(student) != length(expected)) return(FALSE)
  matches <- mapply(function(s, e) {
    if (is.na(s) && is.na(e)) return(TRUE)
    if (is.na(s) || is.na(e)) return(FALSE)
    abs(s - e) < 1e-6
  }, student, expected)
  all(matches)
}
```

## 🎨 Design Patterns for Test Cases

### Pattern 1: Incremental Complexity

**Use when:** Teaching new concepts progressively

```r
test_cases <- list(
  inputs = list(
    list(1),        # Step 1: Trivial
    list(2),        # Step 2: Very simple
    list(5),        # Step 3: Simple
    list(10),       # Step 4: Medium
    list(50),       # Step 5: Complex (hidden)
    list(100)       # Step 6: Very complex (hidden)
  ),

  descriptions = c(
    "Level 1: Simplest case",
    "Level 2: Very simple",
    "Level 3: Simple case",
    "Level 4: Medium complexity",
    "Level 5: More complex",
    "Level 6: Most complex"
  ),

  hidden = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  points = c(1, 1, 2, 2, 3, 3)  # Points increase with difficulty
)
```

**Benefits:**
- Builds confidence with easy wins
- Progressive learning curve
- Students understand pattern

### Pattern 2: Category-Based Testing

**Use when:** Multiple aspects need testing

```r
test_cases <- list(
  inputs = list(
    # Category 1: Positive numbers
    list(5), list(10), list(100),

    # Category 2: Zero and negative
    list(0), list(-5), list(-10),

    # Category 3: Edge cases
    list(1), list(1000000)
  ),

  descriptions = c(
    "Positive: small", "Positive: medium", "Positive: large",
    "Edge: zero", "Negative: small", "Negative: large",
    "Edge: minimum", "Edge: maximum"
  ),

  hidden = c(
    FALSE, FALSE, FALSE,  # Positives: visible
    FALSE, TRUE, TRUE,     # Negatives: some hidden
    TRUE, TRUE             # Edges: hidden
  ),

  points = c(1, 1, 1, 2, 2, 2, 2, 2)
)
```

**Benefits:**
- Comprehensive coverage
- Clear categorization
- Tests different behaviors

### Pattern 3: Error-Driven Learning

**Use when:** Want students to discover edge cases

```r
test_cases <- list(
  inputs = list(
    list(c(1, 2, 3)),        # Works fine
    list(c(5, 10, 15)),      # Works fine
    list(numeric(0)),        # Edge: empty! (hidden)
    list(c(NA, 1, 2)),       # Edge: NA! (hidden)
    list(c(-1, -2, -3))      # Edge: negatives! (hidden)
  ),

  descriptions = c(
    "Normal case",
    "Normal case",
    "Empty vector",
    "Vector with NA",
    "All negative"
  ),

  # First 2 visible, rest hidden - students discover issues
  hidden = c(FALSE, FALSE, TRUE, TRUE, TRUE),

  # Hidden tests worth more (reward handling edge cases)
  points = c(1, 1, 3, 3, 2),

  hints = c(
    "",
    "",
    "What should happen with an empty vector?",
    "How do you handle NA values?",
    "Do negative numbers need special handling?"
  )
)
```

**Benefits:**
- Students learn through discovery
- Encourages defensive programming
- Rewards thorough thinking

### Pattern 4: Performance Testing

**Use when:** Algorithm efficiency matters

```r
test_cases <- list(
  inputs = list(
    list(10),           # Small - any algorithm works
    list(100),          # Medium - need reasonable algorithm
    list(1000),         # Large - need efficient algorithm (hidden)
    list(10000)         # Very large - need optimal algorithm (hidden)
  ),

  descriptions = c(
    "Small input (n=10)",
    "Medium input (n=100)",
    "Large input (n=1000)",
    "Very large input (n=10000)"
  ),

  hidden = c(FALSE, FALSE, TRUE, TRUE),

  # Large tests worth more (reward efficiency)
  points = c(1, 2, 3, 4),

  hints = c(
    "Any correct algorithm works here",
    "Brute force is acceptable",
    "Consider algorithm efficiency - O(n²) might timeout",
    "Need O(n) or O(n log n) algorithm"
  )

  # Note: Time limits not yet implemented but planned
)
```

**Benefits:**
- Tests correctness AND efficiency
- Encourages algorithmic thinking
- Differentiates good from great

## ⚙️ Configuration Options

### Strict vs Lenient Grading

**Strict Mode (for exams):**
```r
test_cases <- list(
  inputs = ...,
  hidden = rep(TRUE, 5),      # All hidden
  tolerance = 1e-10,           # Very strict
  expected_type = "numeric",   # Type-checked
  points = c(2, 2, 2, 2, 2)   # Equal weight
)

# In autograder call:
# autograder("function_name", show_hints = FALSE, show_hidden = FALSE)
```

**Lenient Mode (for practice):**
```r
test_cases <- list(
  inputs = ...,
  hidden = c(FALSE, FALSE, FALSE, TRUE),  # Mostly visible
  tolerance = 1e-6,                        # More lenient
  hints = c(...)                           # Helpful hints
)

# In autograder call:
# autograder("function_name", verbose = TRUE, show_hints = TRUE)
```

### Grading Rubrics

**Completeness-Based (Pass/Fail):**
```r
points = rep(1, 10)  # 10 tests, each 1 point
# 100% = A, 80%+ = B, 60%+ = C, <60% = F
```

**Weighted by Difficulty:**
```r
points = c(
  1, 1,        # Basic: 2 points total
  2, 2, 2,     # Standard: 6 points total
  3, 3, 3      # Advanced: 9 points total
)
# Total: 17 points
# 15+: A, 12+: B, 9+: C, <9: Incomplete
```

**Mastery-Based (Must pass all):**
```r
# In your grading logic:
# if (result$pass_rate < 100) { "Keep practicing until perfect" }
# Only award credit for 100% completion
```

## 🔍 Testing Your Test Cases

### Validation Checklist

Before pushing test cases, verify:

```r
# 1. Load your test file
source("repo/functions/your_function.R")

# 2. Verify reference implementation works
your_function(test_input_1)  # Check output
your_function(test_input_2)  # Check output

# 3. Check all test inputs produce expected outputs
for (i in seq_along(test_cases$inputs)) {
  tryCatch({
    result <- do.call(your_function, test_cases$inputs[[i]])
    cat(sprintf("Test %d: ✓ (output: %s)\n", i, 
                paste(head(result, 3), collapse = ", ")))
  }, error = function(e) {
    cat(sprintf("Test %d: ✗ ERROR: %s\n", i, e$message))
  })
}

# 4. Validate test structure
autograder:::validate_test_cases(test_cases, "your_function")

# 5. Test with autograder
devtools::load_all("autograder")
student_your_function <- your_function
result <- autograder("your_function")
stopifnot(result$pass_rate == 100)  # Should be perfect
```

### Common Issues and Fixes

**Issue 1: Test case lengths don't match**

```r
# Error: descriptions length doesn't match inputs
# Fix: Ensure all vectors are same length
length(test_cases$inputs)        # e.g., 6
length(test_cases$descriptions)  # Must also be 6
length(test_cases$hidden)        # Must also be 6
length(test_cases$points)        # Must also be 6
```

**Issue 2: Reference implementation has bug**

```r
# Symptom: Autograder shows failures even with correct student code
# Fix: Debug your reference implementation

# Test manually
debug(your_function)
your_function(problematic_input)

# Compare with known correct output
manual_result <- # calculate by hand
function_result <- your_function(input)
all.equal(manual_result, function_result)
```

**Issue 3: Tolerance too strict**

```r
# Symptom: Students fail tests due to floating-point precision
# Example: Expected 0.1+0.2 = 0.3, got 0.30000000000000004

# Fix: Increase tolerance
tolerance = 1e-6  # Instead of 1e-10
```

## 📊 Managing Multiple Problems

### Organization Strategies

**By Topic:**
```
functions/
├── basics/
│   ├── hello_world.R
│   ├── simple_math.R
│   └── variable_ops.R
├── loops/
│   ├── sum_vector.R
│   ├── find_max.R
│   └── count_items.R
└── advanced/
    ├── recursion.R
    └── matrix_ops.R
```

**By Week:**
```
functions/
├── week01_hello_world.R
├── week01_simple_math.R
├── week02_loops.R
├── week02_conditionals.R
└── week03_functions.R
```

**By Difficulty:**
```
functions/
├── easy_factorial.R
├── easy_sum.R
├── medium_fibonacci.R
├── medium_prime_check.R
├── hard_matrix_multiply.R
└── hard_dynamic_programming.R
```

### Version Control Best Practices

```bash
# Create feature branch for new problems
git checkout -b add-week5-problems

# Add and test
git add repo/functions/new_problem.R
# ... test thoroughly ...

# Commit with clear message
git commit -m "feat: Add week 5 recursion problems

- Add factorial with recursion
- Add fibonacci with recursion  
- Add tower of hanoi
- All tested and validated
- Total: 3 new problems, 18 new tests"

# Push and create PR for review
git push origin add-week5-problems
```

### Semester Planning

**Week-by-Week Release:**
```r
# _problems.R can be updated throughout semester

# Week 1
problems <- c("hello_world", "simple_math")

# Week 3 (add more)
problems <- c("hello_world", "simple_math", "loops", "conditionals")

# Week 5 (add advanced)
problems <- c(..., "recursion", "higher_order_functions")
```

## 🎯 Pedagogical Best Practices

### 1. Learning Progression

**Bloom's Taxonomy Application:**

```r
# Remember (basic recall)
list(1)  # Direct computation

# Understand (explain)
list(5)  # Apply to different input

# Apply (use in new situation)
list(10)  # Larger scale

# Analyze (break down)
list(0)  # Edge case reasoning (hidden)

# Evaluate (judge/critique)
list(-5)  # Complex edge case (hidden)
```

### 2. Immediate Feedback Loop

Students should be able to:
1. ✅ See most test cases (learn requirements)
2. ✅ Get instant results (immediate feedback)
3. ✅ Understand failures (specific error messages)
4. ✅ Iterate quickly (try-fail-improve cycle)
5. ✅ Build confidence (progressive difficulty)

### 3. Assessment Integrity

Balance learning with integrity:

**For Practice/Homework:**
- 70% visible tests
- Full hints
- Lenient tolerance
- Focus: learning

**For Quizzes:**
- 50% visible tests
- Limited hints
- Standard tolerance
- Focus: understanding

**For Exams:**
- 30% visible tests
- No hints
- Strict tolerance
- Focus: mastery

### 4. Common Student Mistakes

Design tests to catch these:

**Type Confusion:**
```r
# Student might return: as.character(result)
# Test catches: expected_type = "numeric"
```

**Off-by-One:**
```r
# Student might: for (i in 1:(n-1))
# Test catches: inputs = list(list(1), list(2))
```

**Forgot Edge Cases:**
```r
# Student might not handle: empty vector, zero, negative
# Test catches: hidden = TRUE for these cases
```

**Wrong Algorithm:**
```r
# Student might: use O(n²) when O(n) needed
# Test catches: Large hidden test times out or is inefficient
```

## 📈 Monitoring and Analytics

### Track Student Performance

```r
# Collect results across class
class_results <- data.frame(
  student_id = ...,
  problem = ...,
  score = ...,
  attempts = ...
)

# Identify difficult problems
library(dplyr)
class_results %>%
  group_by(problem) %>%
  summarize(
    avg_score = mean(score),
    pass_rate = mean(score == max_score)
  ) %>%
  arrange(pass_rate)

# If pass_rate < 50% for a problem:
# - Consider adjusting difficulty
# - Add more hints
# - Clarify requirements
```

### Iterate Based on Data

```r
# If many students fail same test
# → Improve hint for that test

# If problem too easy (>95% pass rate)
# → Add harder hidden tests

# If problem too hard (<30% pass rate)
# → Simplify or add scaffolding tests
```

## 🔒 Security Notes

### URL Obfuscation Status

**Current Implementation:**
- ✅ AES S-box implemented in C++
- ✅ Key derivation function ready
- ✅ Encryption functions tested
- ⚠️ **URLs currently NOT encrypted** (plaintext for simplicity)

**Why URLs Aren't Encrypted Yet:**
1. Test cases aren't secrets (students need to understand them)
2. Simplifies debugging and development
3. Focus on educational value > security theater
4. Hidden tests provide real integrity protection

**To Enable Full Encryption:**

```cpp
// In src/autograder.cpp

// 1. Encrypt URL components offline:
std::string url = "https://raw.githubusercontent.com/...";
std::string key = derive_key();
std::string encrypted = simple_encrypt(url, key);  // Implement inverse
// Save encrypted as byte array

// 2. Update get_github_base():
std::string get_github_base() {
    std::string key = derive_key();
    return simple_decrypt(ENC_URL_BYTES, key);
}
```

### What This Protects Against

✅ **Casual inspection** - Students can't easily see URLs  
✅ **Simple modification** - Can't change URLs without breaking encryption  
✅ **Integrity verification** - Tests verify correct execution  

❌ **Determined reverse engineering** - Possible but difficult  
❌ **Decompilation** - C++ can be decompiled with effort  
❌ **Source code access** - This is open source, they can read GitHub  

### Real Security: Hidden Tests

The **real** assessment integrity comes from:
1. **Hidden tests** - Students don't see inputs/outputs
2. **Progressive reveal** - Add tests mid-semester
3. **Multiple versions** - Slight problem variations
4. **Code review** - Check for hard-coding
5. **Understanding checks** - Follow-up questions in class

## 🎖️ Security Researcher Recognition

### Hall of Fame

**If you successfully break the encryption and document it:**

🏆 **Current Status:** No successful breaks yet!

**To claim your spot:**
1. Extract encrypted URLs from compiled `.so`/`.dll`
2. Decrypt using reverse-engineering
3. Document your methodology
4. Email proof to: rcagub@up.edu.ph

**Reward:**
- Name in credits
- Potential consulting opportunity
- Reference letter
- GitHub contributor badge

**Past Researchers:** (None yet - will you be first?)

---

## 📞 Instructor Support

### Getting Help

**Email:** rcagub@up.edu.ph  
**Subject Line:** [Autograder-Instructor] Your question

**Include:**
- Problem name
- What you're trying to achieve
- What's not working
- Example code snippet

**Response Time:** Usually 24-48 hours

### Office Hours

Available for:
- Test case design review
- Debugging problems
- Performance optimization
- Custom comparison functions
- Security questions

Schedule via email.

## 🤝 Contributing

### Adding Problems to Main Repository

1. Create test case file
2. Test thoroughly
3. Document in comments
4. Submit pull request with:
   - Problem file
   - Update to _problems.R
   - Description and learning objectives
   - Estimated time to solve
   - Difficulty level

**Quality Standards:**
- ✅ Comprehensive test coverage (6-10 tests)
- ✅ Clear descriptions
- ✅ Helpful hints
- ✅ 100% pass rate with reference implementation
- ✅ Proper documentation
- ✅ Follows naming conventions

## 📚 Additional Resources

### Recommended Reading

- [Writing Good Tests](https://r-pkgs.org/testing-basics.html)
- [R Programming Style Guide](https://style.tidyverse.org/)
- [Automated Grading Best Practices](https://dl.acm.org/doi/10.1145/3287324.3287484)


## 📄 License

MIT License - Educational use encouraged!

Test cases in this repository are educational materials and may be:
- Used in your courses (free)
- Modified for your needs (free)
- Shared with students (free)
- Adapted to other languages (free)

**Attribution appreciated but not required.**

---

**Questions?** Open an [issue](https://github.com/KingSSIBAL/R-Function-checker/issues) or email rcagub@up.edu.ph

**Want to contribute?** We'd love more test cases! See [CONTRIBUTING.md](../CONTRIBUTING.md)

---

## 🎯 TL;DR - Essential Commands

```r
# 1. Create: repo/functions/your_function.R (use template above)
# 2. Add to: repo/functions/_problems.R
# 3. Test locally:
source("repo/functions/your_function.R")
student_your_function <- your_function
autograder("your_function")  # Should be 100%

# 4. Push:
git add repo/functions/your_function.R repo/functions/_problems.R
git commit -m "feat: Add your_function problem"
git push

# Done! ✓
```

**Time investment:** 10-30 minutes per problem  
**Student benefit:** Instant feedback forever  
**Return on investment:** Massive! ⭐

---

**Made with ❤️ for R instructors who care about student learning**
