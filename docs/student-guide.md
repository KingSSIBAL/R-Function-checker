# 📦 Autograder Installation Guide for Windows Students

Welcome! This guide will help you install and use the **Autograder** package to test your R programming assignments.

---

## 📋 What You'll Need

Before starting, make sure you have:
- ✅ **R** installed (version 3.5.0 or later)
  - Download from: https://cran.r-project.org/bin/windows/base/
- ✅ **RStudio** (recommended but optional)
  - Download from: https://posit.co/download/rstudio-desktop/
- ✅ **The autograder package file** (`.tar.gz` or `.zip`)
- ✅ **Internet connection** (to fetch test cases)

---

## ✔️ Check Your R Installation

Before installing the autograder package, verify you have R properly set up:

### Step 1: Open R or RStudio Console

**Option A: Using RStudio (Recommended)**
1. Open RStudio
2. Look at the bottom-left panel (Console)

**Option B: Using R directly**
1. Click Start Menu → Search for "R"
2. Open "R x.x.x (64-bit)" or "R x.x.x (32-bit)"
3. You'll see the R Console

### Step 2: Check Your R Version

In the R Console, type this command and press Enter:

```r
R.version.string
```

You should see something like:
```
[1] "R version 4.3.2 (2023-10-31 ucrt)"
```

✅ **If you see version 3.5.0 or higher, you're good!**  
❌ **If your version is older, download the latest R from https://cran.r-project.org/bin/windows/base/**

---

## 🚀 Installation Steps

### Step 1: Download the Autograder Package File

Your instructor will provide you with a file named:
- `autograder_0.4.0.tar.gz` (source package), or
- `autograder_0.4.0.zip` (binary package)

**Save this file somewhere you'll remember**, like your **Downloads** folder or **Desktop**.

### Step 2: Open R or RStudio

**Using RStudio:**
1. Open RStudio
2. At the bottom-left, you'll see the Console

**Using R directly:**
1. Open the R application from Start Menu

### Step 3: Install the Package

In the R Console, run this command:

**If you downloaded the `.tar.gz` file:**
```r
install.packages("C:/Users/YourUsername/Downloads/autograder_0.4.0.tar.gz", 
                 repos = NULL, 
                 type = "source")
```

**If you downloaded the `.zip` file:**
```r
install.packages("C:/Users/YourUsername/Downloads/autograder_0.4.0.zip", 
                 repos = NULL, 
                 type = "binary")
```

⚠️ **Important: Replace `YourUsername` with YOUR actual Windows username!**

### Step 4: Wait for Installation

The console will show messages as it installs. This may take 30 seconds to 2 minutes.

You'll see:
```
package 'autograder' successfully unpacked and MD5 sums checked
```

✅ **Installation is complete!**

---

## 📚 Verify Installation

To confirm autograder is installed correctly, run this command:

```r
library(autograder)
```

You should see:
```
Autograder v0.4.0 loaded.

Use list_problems() to see available assignments.
Use preview_tests('<function_name>') to preview test cases.
Use autograder('<function_name>') to grade your work.
```

✅ **If you see this message, you're ready to go!**

---

## 🎯 How to Use Autograder

### 1️⃣ See Available Problems

```r
list_problems()
```

Output:
```
Available problems:
  - fibonacci
  - factorial
  - sum_vector
```

### 2️⃣ Preview Test Cases (Before You Code)

```r
preview_tests("fibonacci")
```

This shows you what the tests expect WITHOUT revealing all details.

### 3️⃣ Write Your Solution

In your R script or RStudio editor, write your function:

```r
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  if (n > 2) {
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
  }
  fib
}
```

⚠️ **Important: Always name your function `student_<problem_name>`**

### 4️⃣ Test Your Solution

Run this command:

```r
autograder("fibonacci")
```

You'll see detailed feedback:

```
Loading fibonacci...

=== Running Tests ===

[Test 1] Base case: n = 1 (1 pt): PASS
[Test 2] Small input: n = 5 (2 pt): PASS
[Test 3] Medium input: n = 10 (2 pt): PASS
[Test 4] Larger input: n = 15 (3 pt): PASS
[Test 5] (1 pt): PASS
[Test 6] (1 pt): PASS

=== Summary ===
Score: 10/10 points (100.0%)
Tests: 6/6 passed (100.0%)

✓ ALL TESTS PASSED! Excellent work!
```

✅ **If all tests pass, you're done! Submit your code.**

---

## 🆘 Troubleshooting

### Problem: "package or namespace load failed"

**Solution:**
```r
remove.packages("autograder")
install.packages("C:/Users/YourUsername/Downloads/autograder_0.4.0.tar.gz", 
                 repos = NULL, 
                 type = "source")
library(autograder)
```

---

### Problem: "cannot open file 'C:/Users/...'" (File Not Found)

**Solution:**
1. Check the exact path to your downloaded file
2. In File Explorer, navigate to where you saved it
3. Right-click the file → Properties → Copy the full path
4. Paste it into the install command

Example:
```r
install.packages("C:/Users/Student/Desktop/autograder_0.4.0.tar.gz", 
                 repos = NULL, 
                 type = "source")
```

---

### Problem: "Function 'student_fibonacci' not found"

**Solution:**
1. Make sure you defined your function in the **current R session**
2. Make sure the function name is exactly `student_<problem_name>`
3. Run your function definition code first, then run `autograder()`

Example - Correct Order:
```r
# First: Define your function
student_fibonacci <- function(n) {
  # Your code here
}

# Second: Test it
autograder("fibonacci")
```

---

### Problem: "could not connect to GitHub"

**Solution:**
1. Check your **internet connection**
2. Try again in a few moments
3. If still failing, contact your instructor

---

### Problem: Tests fail with "Type Error"

**Solution:**
1. Check the expected output type
2. Make sure your function returns the **correct type**
   - `numeric` for numbers
   - `character` for text
   - `list` for lists
   - etc.

Example:
```r
# ❌ Wrong - returns a vector of length 1
student_fibonacci <- function(n) {
  return(list(1:n))  # Wrong type!
}

# ✅ Correct - returns a numeric vector
student_fibonacci <- function(n) {
  return(1:n)  # Correct type!
}
```

---

## 📞 Getting Help

If you're stuck:

1. ✅ **Check the error message carefully** - it often tells you exactly what's wrong
2. ✅ **Test your function manually** with simple inputs
3. ✅ **Review the expected behavior** using `preview_tests()`
4. ✅ **Ask your instructor** if you're still confused

---

## 💡 Tips for Success

### Tip 1: Test Incrementally
```r
# Test with simple input first
student_fibonacci(1)  # Should return: 1
student_fibonacci(5)  # Should return: 1 1 2 3 5
```

### Tip 2: Read Error Messages
The autograder gives you **helpful hints**:
```
✗ All tests failed. Check your implementation carefully.
  Suggestions:
    * Test your function manually with simple inputs
    * Review the function requirements
    * Check for syntax errors or typos
```

### Tip 3: Use RStudio Features
- **Ctrl + Enter** to run a line of code
- **Ctrl + Shift + C** to comment/uncomment code
- **Ctrl + L** to clear the console

### Tip 4: Save Your Work
Always save your R script with your solutions:
- File → Save As → `my_solutions.R`

---

## 📊 Problems with Data Files

Some problems may require your function to work with data files (CSV, Excel, RDS, etc.). The autograder handles data loading automatically!

### How It Works

When you see a test case like:
```r
# Test expects: analyze(data) where data is loaded from "scores.csv"
```

You don't need to load the file yourself! The autograder:
1. Fetches the data file from the instructor's repository
2. Loads it into R (as a data frame, list, or appropriate type)
3. Passes it to your function as an argument

### Writing Functions That Use Data

Your function receives the **already-loaded data**, not a file path:

```r
# ✅ Correct - receives a data frame directly
student_analyze_data <- function(data, analysis_type = "mean") {
  if (analysis_type == "mean") {
    return(mean(data$value))
  } else if (analysis_type == "sum") {
    return(sum(data$value))
  }
}

# ❌ Wrong - don't try to read the file yourself
student_analyze_data <- function(data, analysis_type = "mean") {
  df <- read.csv(data)  # DON'T DO THIS!
  # ...
}
```

### Supported Data Formats

| Format | Extensions | Loaded As |
|--------|-----------|-----------|
| CSV | `.csv` | Data frame |
| Excel | `.xlsx`, `.xls` | Data frame |
| R Data | `.rds` | Original R object |
| R Data | `.rdata`, `.rda` | Named variables |
| Text | `.txt` | Data frame |

---

## 🎓 Example Workflow

Here's a complete example of how to work through a problem:

```r
# Step 1: Load autograder
library(autograder)

# Step 2: See available problems
list_problems()

# Step 3: Preview the fibonacci tests
preview_tests("fibonacci")

# Step 4: Write your solution
student_fibonacci <- function(n) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)
  fib <- c(1, 1)
  if (n > 2) {
    for (i in 3:n) {
      fib[i] <- fib[i-1] + fib[i-2]
    }
  }
  fib
}

# Step 5: Test manually first
student_fibonacci(1)   # Returns: 1
student_fibonacci(5)   # Returns: 1 1 2 3 5

# Step 6: Submit for grading
autograder("fibonacci")

# Step 7: View results and fix any issues
```

---

## 📝 Quick Reference

| Command | What It Does |
|---------|-------------|
| `list_problems()` | Show all available assignments |
| `preview_tests("name")` | See test cases for an assignment |
| `autograder("name")` | Grade your solution |
| `library(autograder)` | Load the package |
| `remove.packages("autograder")` | Uninstall the package |

---

## ✅ You're Ready!

Congratulations! You now have everything you need to:
- ✅ Install the autograder package
- ✅ View assignment requirements
- ✅ Test your solutions
- ✅ Get feedback on your code

**Good luck with your assignments!** 🚀

---

## 📧 Questions?

If you run into issues or have questions:
1. Check the **Troubleshooting** section above
2. Ask your **classmates**
3. Contact your **instructor**

---

**Version:** Autograder v0.4.0  
**Last Updated:** November 12, 2025  
**For:** Windows Users
