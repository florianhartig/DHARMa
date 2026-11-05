# Extended tests

Tests for specific cases, e.g. from issues, one file per function. They are not part of `R CMD check`; to run them via GitHub Actions, start the workflow "Extended tests" in the Actions tab.

To run them locally from the repository root:

```r
devtools::load_all("DHARMa")
testthat::test_dir("Code/DHARMaTests")
```
