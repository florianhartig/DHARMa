# DHARMa support for mgcv

Model classes: `gam`, `bam`. Not supported: `gamm()` (see Known problems); for gamm4, see [lme4](../lme4/README.md).

Support: **full** for families with one linear predictor; **partial** for families with several linear predictors (#600).

Checked on 2026-11-09 with DHARMa 0.5.2 (master), mgcv 1.9-4, mgcViz 0.2.1 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/mgcv/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R) and [testCompatibility.R](../../../DHARMa/tests/testthat/testCompatibility.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| Standard families: gaussian, poisson, Gamma, binomial (0/1, factor, k/n as `cbind()` or proportion + weights); `gam()` and `bam()` | supported, also without mgcViz (#567) | checks.R, *Standard families*; testModelTypes.R, *mgcv::gam*; testCompatibility.R |
| Extended families: `nb`, `ziP`, `tw`, `betar`, `scat` | supported if mgcViz is installed; without mgcViz, simulations stop ("family ... not implemented") | checks.R, *Extended families* |
| Several linear predictors (`gaulss`, `gammals`, `gevlss`, `shash`, `ziplss`, `mvn`) | simulations and residuals run (for `mvn`, residuals wrong); fitted values wrong for `gaulss`, `gammals`, `gevlss`, `shash` (one column per linear predictor); refit fails (see Known problems) | checks.R, *Models with several linear predictors* (gaulss, ziplss); #600 |
| `ocat` | `simulateResiduals()` runs; correctness of the simulations not checked | checks.R, *Models with several linear predictors* |
| `multinom` | not supported, stops with an unclear error (#418, #174) | checks.R, *Models with several linear predictors* |
| Fitted values | without random effect smooths (`bs = "re"`, `"fs"`, #544) | testCompatibility.R |
| `simulateREs` | only conditional; `"unconditional"` gives a warning and simulates conditionally | checks.R, *Conditional and unconditional simulations* |
| `refit = TRUE` | supported for standard and extended families; refit reproduces the fit (checked for gaussian, poisson, k/n binomial) | checks.R, *Standard families*, *Extended families*, *Refit* |
| Prior weights | DHARMa warns that they are ignored in the simulations; `cbind()` response + weights: DHARMa stops (#568) | checks.R, *Prior weights*; testModelTypes.R; testCompatibility.R |
| Offsets | `gam()`: `offset()` and `offset =` supported, same fitted values. `bam()`: `offset()` supported; with `offset =`, DHARMa stops, because mgcv drops the offset from the fitted values (#572). `gaulss`: see Known problems | checks.R, *Offsets*; testCompatibility.R |
| Missing values | `na.omit` (default): residuals for the complete rows; `na.exclude`: fitted values padded with NA, so the dispersion test gives NA and `plot()` and refit fail | checks.R, *Missing values* (na.omit); #88 (na.exclude) |
| Order of residuals | data order | checks.R, *Order of residuals* |
| Pearson residuals | supported, all `testDispersion()` types (DHARMa uses `residuals(type = "scaled.pearson")`) | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- Several linear predictors (#600): `getFitted()` returns one column per linear predictor (e.g. 2n values for `gaulss`), so `testDispersion()`, `plotResiduals()` and `plot()` stop, and `testQuantiles(plot = FALSE)` returns a p-value without error. `refit = TRUE` fails ("need an object with call component"). For `mvn()`, DHARMa only warns about the dimension of the simulations, and the residuals are wrong. With `offset =`, mgcv ignores the offset, DHARMa does not notice.
- `gamm()` and `gamm4()`: `simulateResiduals(model$gam)` fails, because `family()` has no method for these objects (#612); `model$gam` lacks the random effects of `random =`.

## Not checked

- Correctness of `ocat` simulations; other families (`twlss`, `cox.ph`, ...), `bam(discrete = TRUE)`.

## Former scripts

[archive/](archive/): `GAM.R` (simulate.gam, nlme and gamm4 experiments), `GAMproblems.R`, `GAMProblems.Rmd`/`.md` (old k/n binomial simulation format and fitted value problems, fixed in #567, #568, #590).
