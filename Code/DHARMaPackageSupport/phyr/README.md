# DHARMa support for phyr

Models fitted with `phyr::pglmm()`. Their class is not in DHARMa's list of supported classes (`DHARMa:::getPossibleModels()`), so DHARMa warns and uses its default methods.

Support: **not checked** with the current DHARMa (phyr is not installed on the check machine). The vignette says that DHARMa residuals work with phyr, but that the implementation is not fully tested; open issue #235.

## Former scripts

[archive/](archive/): `phyrTests.R` (binomial `pglmm()` examples with the `oldfield` data, frequentist and Bayesian).
