# DHARMa support for brms

Model class: `brmsfit`.

Support: **partial** (univariate models), with the limits listed below.

Checked on 2026-11-09 with DHARMa 0.5.2 (master), brms 2.23.0 (rstan backend), rstan 2.32.7, StanHeaders 2.39.1.1 and R 4.6.2 (Windows).
All statements are backed by sections of [checks.R](checks.R) (run from the repository root: `source("Code/DHARMaPackageSupport/brms/checks.R")`; it stops if a statement no longer holds; every model is compiled, about 30 min with 2 cores) or by the package tests ([testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R), test *brms works*, skipped on CRAN; [testGetSimulations.R](../../DHARMaTests/testGetSimulations.R), [testGetPredictorNames.R](../../DHARMaTests/testGetPredictorNames.R)).

## Support

| Feature | Status | Evidence |
|---|---|---|
| Families: gaussian, student, poisson, negbinomial, zero_inflated_poisson, zero_inflated_negbinomial, hurdle_poisson, binomial (`trials()`), bernoulli, Gamma, lognormal, Beta | supported; no false signals for data simulated from the model | checks.R, *Model classes and families*; testModelTypes.R, *brms works* |
| Random slopes, smooth terms `s()`, distributional models (`sigma ~ x`), `ar()` terms | supported | checks.R, *Model structures* |
| Fitted values (`getFitted()`) | median of `posterior_epred(re_formula = NA)`, on the response scale (proportions for k/n binomial) | checks.R, *Fitted values* |
| `simulateREs = "conditional"` (default) / `"unconditional"` | supported. Unconditional simulations draw new REs per group and include the posterior uncertainty of the RE sd, so they vary more than for an ML fit (glmmTMB) of the same model. They stop with a message for grouping variables used elsewhere, e.g. in `ar(gr = )`, and for `gr(..., cov = , by = )` (#574) | checks.R, *Conditional and unconditional simulations*; testGetSimulations.R |
| Observation-level REs | use `simulateREs = "unconditional"`; conditional residuals are not uniform for a correct model | checks.R, *Observation-level random effects* |
| Conditioning on selected REs | `simulateREs = "user-specified", re_formula = ...` (passed to `posterior_predict()`), e.g. `re_formula = ~(1\|group)` | checks.R, *Conditioning on selected random effects* |
| `refit = TRUE` | runs (each refit samples the model again), but `testDispersion()` stops (no Pearson residuals) | checks.R, *Refit* |
| Prior weights (`weights()`) | **ignored in the simulations**, without a warning (see Known problems) | checks.R, *Prior weights* |
| Offsets | `offset()` in the formula works | checks.R, *Offsets* |
| Missing values | rows with NA are removed by brms; residuals for the complete rows | checks.R, *Missing values* |
| Order of residuals | data order | checks.R, *Order of residuals* |
| Predictors in `plotResiduals(form = )` | from the data object named in the call; fails for some calls (see Known problems) | checks.R, *Predictors for plotResiduals(form = )*; testGetPredictorNames.R |
| Pearson residuals, `testDispersion(type = "PearsonChisq")` | not supported, DHARMa stops with a message; the default dispersion test works | checks.R, *Pearson residuals and dispersion tests* |
| Number of simulations `n` | at most the number of posterior draws (see Known problems) | checks.R, *Number of simulations and posterior draws* |
| Ordinal (`cumulative`), `categorical`, multivariate models | not supported; DHARMa stops, but with unclear messages (see Known problems) | checks.R, *Unsupported models* |
| Own simulations via `posterior_predict()` + `createDHARMa()` | works | checks.R, *Posterior predictive simulations with createDHARMa()* |

## Calibration and power compared with glmmTMB

Poisson GLMM with 10 groups, 40 data sets per row, `n = 250` simulations; share of p < 0.05 for the uniformity / dispersion test (checks.R, *Calibration and power compared with glmmTMB*, only run with `runCalibration = TRUE`, about 1 h; values from the run on 2026-11-09).

| Data | n | brms conditional | brms unconditional | glmmTMB conditional | glmmTMB unconditional |
|---|---|---|---|---|---|
| correct model | 30 | 0.03 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| correct model | 100 | 0 / 0 | 0 / 0 | 0 / 0 | 0 / 0 |
| correct model | 400 | 0.03 / 0 | 0.25 / 0 | 0 / 0 | 0.08 / 0 |
| overdispersion 0.5 | 30 | 0 / 0 | 0 / 0 | 0 / 0.08 | 0 / 0 |
| overdispersion 0.5 | 100 | 0.03 / 0.13 | 0.03 / 0 | 0.03 / 0.50 | 0 / 0.03 |

## Known problems

- `n` larger than the number of posterior draws stops with the brms error "Argument 'ndraws' should be between 1 and the maximum number of draws", also with the default `n = 250` for models with fewer draws (#591). Workaround: `n <= ndraws(model)`.
- Prior weights: `posterior_predict()` ignores `weights()`, so DHARMa simulates weighted models with the same variance for all weights, and does not warn (#7).
- `getData()` evaluates the name of the `data` argument in the environment of the formula. It fails for `data = d[d$x > 0, ]` ("object 'd[d$x > 0, ]' not found"), and for a model fitted by a function with an argument `data`, with the formula passed as an argument, it returns `utils::data` (this also happens for `glm()`). `plotResiduals(form = )` then stops (#611). Workaround: fit the model with a data frame that has its own name, or pass the predictor as a vector.
- Ordinal, categorical and multivariate models stop with "argument is not a matrix" or "argument is of length zero" instead of a message that the model type is not supported; ordinal models are not mentioned as unsupported in the documentation (#612).
- Unconditional simulations: for n = 400, the uniformity test gave p < 0.05 for 25% of the correct models (glmmTMB: 8%); see the calibration table.
- The dispersion test detects overdispersion less often than for glmmTMB (n = 100: 13% vs. 50%), probably because the simulations include the posterior uncertainty of the parameters.
- Every `simulateResiduals()` call warns "DHARMa: only simple brms models are supported ...", also for supported models.

## Not checked

- `mm()` terms and non-gaussian random effects (unconditional simulations stop for them according to the code, compatibility.R, `getSimulations.brmsfit`), censored responses (`cens()`), `mi()` and `me()` terms, `gp()`, non-linear formulas (`nl = TRUE`), other families (e.g. `zero_one_inflated_beta`, `hurdle_negbinomial`).
- The cmdstanr backend (not installed).
- Installation: on the Windows machine used here, rstan 2.32.7 adds `-std=c++1y` to the compiler flags (`rstan:::PKG_CPPFLAGS_env_fun()`), which overrides the C++17 standard that StanHeaders 2.39 needs, and Stan models did not compile; for the checks, the flag was removed in the R session.

## Former scripts

[archive/](archive/): `brms.R` (`posterior_predict()` + `createDHARMa()` for the epilepsy data and a Poisson GLMM; now in *Posterior predictive simulations with createDHARMa()*).
