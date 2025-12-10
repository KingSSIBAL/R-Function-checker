# Package startup message

Displays welcome message when package is loaded via library() or
require().

## Usage

``` r
.onAttach(libname, pkgname)
```

## Arguments

- libname:

  Library name (passed by R)

- pkgname:

  Package name (passed by R)

## Details

Message includes:

- Package version

- Quick start instructions (list_problems, preview_tests, autograder)

- New features in this version

Uses packageStartupMessage() which can be suppressed with:
suppressPackageStartupMessages(library(autograder))
