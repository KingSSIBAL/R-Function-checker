# Architecture Overview

This document describes the high-level architecture of **R-Function-checker** and how the R package and C++ backend interact.

## Components

- **R package (`autograder/`)**
  - Provides the user-facing API (`autograder()`, helpers for listing problems, previewing tests, etc.).
  - Handles assignment configuration, problem selection, and orchestration of test execution.
  - Bridges R objects to the C++ comparison and validation engine via Rcpp.

- **C++ backend (`autograder/src/`)**
  - Implements high-performance comparison, validation, and formatting logic.
  - Provides modular subsystems:
    - `core/`: type definitions, error/exception hierarchy.
    - `crypto/`: AES-based encryption helpers, key/IV handling, auth config.
    - `validation/`: input validation, path normalization, and safety checks.
    - `compare/`: optimized R object comparison routines.
    - `network/`: authenticated downloads and data fetching.
    - `format/`: formatting of feedback messages and diffs.

- **Problem repository (`repo/`)**
  - Contains problem definitions and test cases under `functions/`.
  - Contains data files for tests under `data/`.
  - Designed so instructors can host their own version (public or private) and the package can fetch from it.

- **Instructor tools (`tools/`)**
  - R utilities for setting up legacy vs secure mode.
  - Helpers for encrypting URLs and managing `.env` configuration.

## R–C++ interaction

- R functions collect inputs (student solutions, expected outputs, metadata) and pass them to C++ through compiled interfaces.
- C++ performs comparison, validation, and formatting, and returns structured results.
- R code then aggregates results, computes scores, and generates user-friendly feedback.

## Execution modes

- **Sequential execution**: Runs tests one after another, suitable for small test sets or debugging.
- **Parallel execution**: Distributes tests across workers when available, reducing total runtime for large test suites.

## Authentication and data access

- In legacy mode, test cases and data are fetched from a public repository.
- In secure mode, the tools configure a base URL and GitHub token via `.env`.
- The `network/` module uses these settings to download required files without exposing secrets in code.

## Extensibility

When extending the system:

- Prefer adding new functionality in the most specific module (e.g., new comparison logic in `compare/`).
- Keep the R interface thin and declarative, with heavy lifting in C++.
- Add tests for new behavior in both R (`tests/testthat/`) and, when appropriate, C++ unit tests.
