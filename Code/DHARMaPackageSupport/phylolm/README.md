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
| Residual correlation | residuals stay correlated (simulations re-simulate the phylogeny), so the uniformity test rejects too often for correct models. `rotation = m$sigma2 * vcv(tree)` (`model = "BM"`, tip order) fixes this; `rotation = "estimated"` runs, but with the default `n = 250` it is strongly anti-conservative (#489) | checks.R, *Residual correlation*; #489 |
| `refit = TRUE` | supported; refit reproduces the fit when the response is given in tree tip order, as returned by `getObservedResponse()` | checks.R, *Model classes*, *Refit* |
| Order of residuals | order of the tree tips, not of the data (see Known problems) | checks.R, *Order of residuals* |
| Pearson residuals | not available: `testDispersion(type = "PearsonChisq")` stops; the default `testDispersion()` is supported | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- Residuals are in the order of the tree tips (#609). Predictors given as formula are taken in this order automatically (since 0.5.2; package test testTests.R); order vector predictors as `tree$tip.label`.
- `phyloglm`: `plotResiduals(form = ~.)` stops, because phyloglm objects have no model frame.
- `poisson_GEE`, and models fitted inside a function: simulations and refit use `update()`, which fails if the objects named in the model call are not found outside the function (#478).

## Not checked

- Other `phylolm()` models (`OUrandomRoot`, `OUfixedRoot`, `EB`, `kappa`, `delta`), prior weights, missing values, offsets.
- Data with strong phylogenetic signal: in the check data, lambda and alpha were estimated at their bounds (the warnings are in the output of checks.R). For `logistic_IG10`, the future.apply package also warns about random numbers in the bootstrap ("UNRELIABLE VALUE"), which may affect reproducibility.

## Former scripts

[archive/](archive/): `phylolm.R` (`phyloglm()` and `phylolm()` examples, rotation; now in *Model classes* and *Residual correlation*).
