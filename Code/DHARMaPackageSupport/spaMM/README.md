# DHARMa support for spaMM

Model class: `HLfit` (returned by `fitme()`, `HLfit()`, `HLCor()` and `corrHLfit()`).

Support: **full**, with the limits below.

Checked on 2026-11-09 with DHARMa 0.5.2 (master), spaMM 4.7.17 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/spaMM/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R) and [testCompatibility.R](../../../DHARMa/tests/testthat/testCompatibility.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| Families: gaussian, poisson, binomial (0/1, k/n as `cbind()`), `betabin()`, `negbin2()`, Gamma; fitted with `fitme()` or `HLfit()` | supported | checks.R, *Families*; testModelTypes.R, *spaMM::HLfit* |
| k/n binomial as proportion + prior weights | not supported by spaMM (fit stops) | checks.R, *k/n binomial with weights* |
| Spatial models (`Matern()`) | supported, conditional and unconditional; with one observation per location, conditional residuals of correct Gaussian models are underdispersed (#527), and unconditional residuals stay spatially correlated (rotation, #489) | checks.R, *Spatial models*; #527, #489 |
| Fitted values | without random effects (`re.form = NA`) | testCompatibility.R |
| `simulateREs` | conditional (default) and unconditional supported | checks.R, *Conditional and unconditional simulations*; testCompatibility.R |
| Conditioning on selected REs | supported with `simulateREs = "user-specified", re.form = ~(1\|group)`; `re.form` alone stops with an error | checks.R, *Conditioning on selected random effects* |
| `refit = TRUE` | supported, refit reproduces the fit; fails for responses transformed in the formula (see Known problems) | checks.R, *Families*, *Transformed response*, *Refit* |
| Prior weights | gaussian: used in the simulations, no DHARMa warning | checks.R, *Prior weights* |
| Offsets | `offset()` in the formula supported | checks.R, *Offsets* |
| Missing values | residuals for the complete rows | checks.R, *Missing values* |
| Order of residuals | data order | checks.R, *Order of residuals* |
| Pearson residuals | supported, all `testDispersion()` types | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- `simulateResiduals(refit = TRUE)` fails if the response is transformed in the formula, e.g. `log(y + 1)` ("Response, … is not a variable in the 'data'", from `spaMM::update_resp()`; no issue yet). Workaround: store the transformed response as a column of the data.

## Not checked

- Prior weights with non-gaussian families (#7, #175), `offset =`, `na.exclude`.
- Other correlation models (`adjacency()`, `AR1()`, `IMRF()`), non-gaussian random effects (`rand.family`), residual dispersion models (`resid.model`).

## Former scripts

[archive/](archive/): `spaMM-test.R` (examples from the spaMM help: `Loaloa`, `blackcap`, `scotlip` with `adjacency()`, `salamander` with Beta random effects, `wafers` with `resid.model`; not taken over, see *Not checked*).
