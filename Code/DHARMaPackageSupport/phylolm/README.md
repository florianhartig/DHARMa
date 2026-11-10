# DHARMa support for phylolm

Model classes: `phylolm`, `phyloglm`.

Support: **partial**: supported for gaussian `phylolm()` and binary `phyloglm()`, with the limits below.

Checked on 2026-11-09 with DHARMa 0.5.2 (master), phylolm 2.6.5 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/phylolm/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| `phylolm()` (models `lambda`, `BM`); `phyloglm()` (methods `logistic_MPLE`, `logistic_IG10`) | supported | checks.R, *Model classes*; testModelTypes.R, *phylolm::pylolm/phyloglm* |
| `phyloglm(method = "poisson_GEE")` | not supported, `simulateResiduals()` stops (#478) | checks.R, *poisson_GEE* |
| Simulations | parametric bootstrap of phylolm (`update(boot = nsim, save = TRUE)`), i.e. one model fit per simulation; phylolm warnings from these fits are passed on | checks.R, *Model classes*, *Simulations* |
| `simulateREs` | no effect; the phylogenetic structure is always re-simulated | checks.R, *Simulations* |
| Residual rotation (`rotation = "estimated"`) | supported (runs; uniformity of rotated residuals not checked) | checks.R, *Residual correlation* |
| `refit = TRUE` | supported; refit reproduces the fit when the response is given in tree tip order, as returned by `getObservedResponse()` | checks.R, *Model classes*, *Refit* |
| Order of residuals | order of the tree tips, not of the data (see Known problems) | checks.R, *Order of residuals* |
| Pearson residuals | not available: `testDispersion(type = "PearsonChisq")` stops; the default `testDispersion()` is supported | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- Residuals are in the order of the tree tips (#609). Order predictors as `tree$tip.label` before passing them to `plotResiduals()`, `testCategorical()` etc.
- `poisson_GEE` (#478).
- Simulations and refit use `update()`, which according to a comment in testModelTypes.R finds the tree and the data only in the global environment; models fitted inside functions may therefore fail (not checked).

## Not checked

- Other `phylolm()` models (`OUrandomRoot`, `OUfixedRoot`, `EB`, `kappa`, `delta`), prior weights, missing values, offsets.
- Data with strong phylogenetic signal: in the check data, lambda and alpha were estimated at their bounds (the warnings are in the output of checks.R). For `logistic_IG10`, the future.apply package also warns about random numbers in the bootstrap ("UNRELIABLE VALUE"), which may affect reproducibility.

## Former scripts

[archive/](archive/): `phylolm.R` (`phyloglm()` and `phylolm()` examples, rotation; now in *Model classes* and *Residual correlation*).
