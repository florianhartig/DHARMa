# DHARMa support for GLMMadaptive

Model class: `MixMod` (`mixed_model()`).

Support: **partial**: supported for 0/1 binomial, poisson, negative binomial and zero-inflated poisson models, with the limits below.

Checked on 2026-11-09 with DHARMa 0.5.2 (master), GLMMadaptive 0.9-7 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/GLMMadaptive/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R) and [testCompatibility.R](../../../DHARMa/tests/testthat/testCompatibility.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| Families: poisson, binomial (0/1), `negative.binomial()`, `zi.poisson()` | supported | checks.R, *Families*; testModelTypes.R, *GLMMadaptive* |
| Gaussian | not available in `mixed_model()` | checks.R, *Gaussian* |
| k/n binomial with `cbind()` | simulations and residuals supported; refit fails (see Known problems) | checks.R, *k/n binomial* |
| Beta-binomial (`beta.binomial()`) | not supported (see Known problems) | checks.R, *k/n binomial* |
| Fitted values | `fitted(type = "mean_subject")`, i.e. without random effects | testCompatibility.R |
| `simulateREs` | conditional (default) and unconditional supported; with `"user-specified"`, `new_RE` is passed to `simulate()`, but `type = "subject_specific"` (vignette table) stops with an error, because it clashes with the argument `type` of `getSimulations()` | checks.R, *Conditional and unconditional simulations*; testCompatibility.R |
| Conditioning on selected REs | not supported; `re.form` gives a warning | checks.R, *Conditional and unconditional simulations* |
| `refit = TRUE` | supported for the families above; refit reproduces the fit (checked for poisson and 0/1 binomial) | checks.R, *Families*, *Refit* |
| Prior weights | weights are per group in GLMMadaptive; DHARMa warns that they are ignored in the simulations | checks.R, *Prior weights*; testModelTypes.R, *GLMMadaptive* |
| Offsets | `offset()` in the formula supported | checks.R, *Offsets* |
| Missing values | residuals for the complete rows | checks.R, *Missing values* |
| Order of residuals | sorted by the grouping factor, not in data order (see Known problems) | checks.R, *Order of residuals* |
| Pearson residuals | not available: `testDispersion(type = "PearsonChisq")` and `testDispersion()` after `refit = TRUE` stop; the default `testDispersion()` is supported | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- Residuals are in the order of the data sorted by the grouping factor (#609). Sort the data by the grouping factor before fitting if you pass predictors to `plotResiduals()`, `testCategorical()`, `testQuantiles()` or a group to `recalculateResiduals()` (as in the vignette).
- `cbind()` response: refit simulations contain only the successes, so refits fail ("y values must be 0 <= y <= 1"; #565, #609).
- The vignette table of `simulateREs = "user-specified"` syntax gives `type = "subject_specific"` for GLMMadaptive; this argument cannot be passed through `simulateResiduals()` (no issue yet). `new_RE = TRUE` alone works.
- Beta-binomial: DHARMa does not recognise the family name "beta binomial" and warns "nKcase - wrong family" (#565).

## Not checked

- `zi.negative.binomial()`, hurdle families, `beta.fam()`, `censored.normal()`, random slopes, `nAGQ` settings, `na.exclude`.

## Former scripts

[archive/](archive/): `GLMMadaptive-PackageSupportChecks.R` (exploration of the GLMMadaptive interface, `update()` behaviour), `glmmAdaptive_examples.R` (binomial, poisson and zero-inflated negative binomial examples; the families are now in *Families*).
