# DHARMa support for glmmTMB

Model class: `glmmTMB`.

Support: **full**, with the limits below.

Checked on 2026-11-09 with DHARMa 0.5.2 (master), glmmTMB 1.1.15.2 and R 4.6.2. Evidence: sections of [checks.R](checks.R) (run `source("Code/DHARMaPackageSupport/glmmTMB/checks.R")` from the repository root) and the package tests [testModelTypes.R](../../../DHARMa/tests/testthat/testModelTypes.R) and [testCompatibility.R](../../../DHARMa/tests/testthat/testCompatibility.R).

## Support

| Feature | Status | Evidence |
|---|---|---|
| Families: gaussian, poisson, binomial (0/1, factor, k/n as `cbind()` or proportion + weights), betabinomial (both k/n forms), nbinom1, nbinom2, genpois, beta, Gamma, tweedie | supported | checks.R, *Families*; testModelTypes.R, *glmmTMB* |
| Zero-inflation (`ziformula`), hurdle (`truncated_poisson`), dispersion model (`dispformula`) | supported | checks.R, *Families* |
| Fitted values | without random effects; for zero-inflated models the expected response including the zero-inflation | checks.R, *Fitted values*; testCompatibility.R |
| `simulateREs` | conditional (default) and unconditional supported; DHARMa restores the simulation settings of the model afterwards (#538) | checks.R, *Conditional and unconditional simulations*; testCompatibility.R |
| Conditioning on selected REs | not supported: with `simulateREs = "user-specified"`, `simulate.glmmTMB` accepts only `re.form = NULL`; `NA`, `~0` and `~(1\|group)` stop with an error | checks.R, *Conditioning on selected random effects* |
| Covariance structures | conditional simulations supported for `ar1()` and `ou()`; for `exp()`, DHARMa falls back to unconditional simulations with a message. With one observation per time step, conditional residuals of correct Gaussian models are underdispersed; use `simulateREs = "unconditional"`, with a rotation to test autocorrelation (#527) | checks.R, *Covariance structures*; #527 |
| `refit = TRUE` | supported, refit reproduces the fit; refits of some families (betabinomial, nbinom, genpois, tweedie) may raise convergence warnings of glmmTMB | checks.R, *Families*, *Refit* |
| Prior weights | DHARMa warns that they are ignored in the simulations; `cbind()` response + weights: DHARMa stops (#568) | checks.R, *Prior weights*; testCompatibility.R |
| Offsets | `offset()` and `offset =` supported, same fitted values; refit reproduces the fit | checks.R, *Offsets* |
| Missing values | `na.omit` (default): residuals for the complete rows, refit supported; `na.exclude`: residuals for the complete rows, but fitted values padded with NA, so the dispersion test gives NA and `plot()` and refit fail (see Known problems) | checks.R, *Missing values*; #88 |
| Order of residuals | data order | checks.R, *Order of residuals* |
| Pearson residuals | supported, all `testDispersion()` types, also for zero-inflated models | checks.R, *Pearson residuals and dispersion tests* |

## Known problems

- `na.action = na.exclude`: `getFitted()` returns values padded with NA (one per data row). `testDispersion()` then returns `NA` with many warnings, `plot()` stops, and `refit = TRUE` stops with "wrong dimension of simulations" (#88, #134). Workaround: fit with `na.omit`.
## Not checked

- Families `compois`, `ordbeta`, `skewnormal`, `lognormal` and others not listed above.
- Covariance structures other than `ar1()`, `ou()` and `exp()` (e.g. `gau()`, `mat()`, `us()`, `rr()`, `propto()`).

## Former scripts

[archive/](archive/): `glmmTMB.R`, `glmmTMB.Rmd`/`glmmTMB.md` (Salamander examples, Tweedie, beta-binomial and beta families, old refit and factor response problems; the families are now in *Families*), `glmmTMB-AR1.R` (AR1 examples across packages; AR1 is now in *Covariance structures*).
