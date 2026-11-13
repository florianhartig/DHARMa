# DHARMa support for lm, glm and glm.nb

Model classes: `lm`, `glm` (stats), `negbin` (`MASS::glm.nb()`).

Support: **full**, with the limits below.

Checked on 2026-11-09 with DHARMa 0.5.2 (master), MASS 7.3-66 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/stats/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R) and [testCompatibility.R](../../../DHARMa/tests/testthat/testCompatibility.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| `lm()`; `glm()` with gaussian, poisson, Gamma, binomial (0/1, factor, k/n as `cbind()` or proportion + weights); `glm.nb()` | supported | checks.R, *Model classes and families*; testModelTypes.R, *stats::lm*, *stats::glm*, *MASS::glm.nb* |
| Logical response (e.g. `y > 5`) | supported; `getObservedResponse()` returns a logical vector | checks.R, *Logical response* |
| `inverse.gaussian` | without the package SuppDists, `simulateResiduals()` stops (`stats::simulate` needs it); with SuppDists not checked | checks.R, *inverse.gaussian* |
| `quasipoisson`, `quasibinomial` | not supported (no likelihood to simulate from), `simulateResiduals()` stops | checks.R, *Quasi families* |
| `simulateREs` | no effect (no random effects) | checks.R, *Conditional and unconditional simulations* |
| `refit = TRUE` | supported, refit reproduces the fit (also for transformed responses, models without `data` and with `subset`, #571, #599); `glm.nb()` refits may warn "iteration limit reached" (MASS) | checks.R, *Model classes and families*, *Refit*; testCompatibility.R |
| Prior weights | `lm()` and gaussian `glm()`: used in the simulations, no warning. Other `glm()` families: ignored, warning from `stats::simulate` ("ignoring prior weights"). `glm.nb()`: ignored, DHARMa warns. `cbind()` response + weights: DHARMa stops (#568) | checks.R, *Prior weights*; testCompatibility.R |
| Offsets | `offset()` and `offset =` supported, same fitted values; refit reproduces the fit | checks.R, *Offsets* |
| Missing values | `na.omit` (default): residuals for the complete rows, refit supported; `na.exclude`: fails (see Known problems) | checks.R, *Missing values* |
| Order of residuals | data order | checks.R, *Order of residuals* |
| Pearson residuals | supported, all `testDispersion()` types | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- `na.action = na.exclude`: `simulateResiduals()` stops with "Simulations from your fitted model produce NA values" (#88, #134). Workaround: fit with `na.omit`.

## Not checked

- `quasi()`, Tweedie (`statmod::tweedie`, #379), non-default links.
