# Contributing to R-Function-checker

Thank you for considering contributing to **R-Function-checker**! This project powers an autograder for R programming assignments using an R package with a modern C++ backend.

## Ways to contribute

- Report bugs or unexpected behavior
- Suggest new features or improvements
- Improve documentation (R docs, vignettes, guides)
- Add or improve tests (R or C++)
- Optimize performance or refactor code

## Getting started

1. Fork the repository and clone your fork.
2. Create a new branch for your work:
   - Example: `git checkout -b feature/better-error-messages`
3. Make your changes in small, focused commits.
4. Run checks and tests locally before opening a PR.

## Development environment

### R side

- Recommended: latest stable R (4.x)
- Suggested packages: `devtools`, `testthat`, `roxygen2`, `covr`
- From the repository root:
  - `cd autograder`
  - `devtools::load_all()`
  - `devtools::test()`

### C++ side

The C++ code is inside `autograder/src/` and uses C++17.

- Require a compiler with C++17 support (e.g., recent GCC/Clang, MSVC)
- Keep headers and implementations modular (see existing `core/`, `crypto/`, `compare/`, etc.)
- Prefer small, focused functions and clear error messages

## Code style

### R

- Use snake_case for function names and objects
- Add roxygen2 comments for exported functions
- Keep functions small and focused
- Write tests for new behavior in `autograder/tests/testthat/`

### C++

- Use clear, descriptive names
- Avoid duplicating logic across modules
- Prefer `const` correctness and references when possible
- Make error messages user-friendly (students and instructors will see them)

## Testing

Before opening a pull request:

- Run R tests:
  - From `autograder/`: `devtools::test()`
- Run R CMD check when possible:
  - `R CMD build autograder`
  - `R CMD check autograder_*.tar.gz`

If you modify C++ code, verify that compilation succeeds on your platform and that tests still pass.

## Opening a pull request

When your changes are ready:

1. Push your branch to your fork.
2. Open a pull request against the `main` branch of this repository.
3. In the PR description, include:
   - What you changed and why
   - How you tested it (commands, platforms)
   - Any potential breaking changes

## Reporting security issues

For vulnerabilities or security-sensitive issues (especially around authentication or encryption), **do not** open a public issue. Instead, email the maintainer directly at `rcagub@up.edu.ph` with:

- A clear description of the issue
- Steps or conditions required to reproduce
- Any suggested mitigations

Thank you for helping improve R-Function-checker!
