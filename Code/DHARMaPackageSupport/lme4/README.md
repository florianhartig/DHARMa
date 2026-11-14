# DHARMa support for lme4

Model classes: `lmerMod`, `glmerMod` (including `glmer.nb()`), `lmerModLmerTest` (lmerTest); gamm4 via its `$mer` element.

Support: **full**, with the limits below.

Checked on 2026-11-14 with DHARMa 0.5.2 (master), lme4 2.1-0, lmerTest 3.2-1, gamm4 0.3-0 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/lme4/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R) and [testCompatibility.R](../../../DHARMa/tests/testthat/testCompatibility.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| `lmer()`, lmerTest `lmer()` | supported | checks.R, *Model classes and families*; testModelTypes.R, *lme4::lmer* |
| `glmer()`: poisson, Gamma, binomial (0/1, factor, k/n as `cbind()` or proportion + weights) | supported | checks.R, *Model classes and families*; testModelTypes.R, *lme4::glmer* |
| `glmer.nb()` | supported; no false dispersion signal for data simulated from the model | checks.R, *Model classes and families*, *Negative binomial (glmer.nb)* |
| Logical response (e.g. `y > 5`) | supported; `getObservedResponse()` returns a logical vector | checks.R, *Logical response* |
| gamm4 | supported via `simulateResiduals(model$mer)` | checks.R, *gamm4* |
| Fitted values | without random effects (`re.form = ~0`) | testCompatibility.R |
| `simulateREs` | conditional (default) and unconditional supported | checks.R, *Conditional and unconditional simulations*; testCompatibility.R |
| Conditioning on selected REs | supported with `simulateREs = "user-specified", re.form = ~(1\|group)`; `re.form` alone stops with an error | checks.R, *Conditioning on selected random effects* |
| `refit = TRUE` | supported, refit reproduces the fit | checks.R, *Model classes and families*, *Refit* |
| Prior weights | ignored in the simulations (lme4 ignores them), DHARMa warns (not for weights that are all 1, e.g. from gamm4); `cbind()` response + weights: DHARMa stops (#568) | checks.R, *Prior weights*; testCompatibility.R |
| Offsets | `offset()` and `offset =` supported, same fitted values; refit reproduces the fit | checks.R, *Offsets* |
| Missing values | `na.omit` (default): residuals for the complete rows, refit supported; `na.exclude`: fails (see Known problems) | checks.R, *Missing values* |
| Order of residuals | data order | checks.R, *Order of residuals* |
| Pearson residuals | supported, all `testDispersion()` types | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- gamm4 itself warns "modified RE names for gamm4 prediction".
- `na.action = na.exclude`: `simulateResiduals()` stops; for `glmer()` with an error from `simulate.merMod` ("missing value where TRUE/FALSE needed"), for `lmer()` with "Simulations from your fitted model produce NA values" (#88, #134). Workaround: fit with `na.omit`.

## Not checked

- Other families (e.g. `inverse.gaussian`), `nAGQ > 1`, random slopes.

## Former scripts

[archive/](archive/): `glmer.nb.R` (dispersion tests for `glmer.nb`, #47, #48; now in *Negative binomial (glmer.nb)*), `lme4problems.R` (`glmer.nb()` with a `plm.dim` data set and a Tweedie attempt; not taken over).
