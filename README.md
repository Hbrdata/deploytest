
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{deploytest}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{deploytest}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
deploytest::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-09-16 16:43:28 CST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ────────────────────────────── deploytest 0.0.0.9000 ────
#> Duration: 31.7s
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: 'pkgload'
#>     All declared Imports should be used.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> deploytest Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/run_app.R: 0.00%
```
