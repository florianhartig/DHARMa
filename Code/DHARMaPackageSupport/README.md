# Package support

What DHARMa supports for each regression package, and the checks behind it. Each package folder has a `README.md` with the support and its limits, and a script (`checks.R`) whose sections back the statements in the README (some statements cite a GitHub issue instead). The scripts stop with an error if a statement no longer holds, so they can be re-run after changes to DHARMa or to the packages.

The vignette (section "Supported packages and frameworks") and the package tests ([testModelTypes.R](../../DHARMa/tests/testthat/testModelTypes.R), [testCompatibility.R](../../DHARMa/tests/testthat/testCompatibility.R)) remain the user-facing documentation and the automated tests; these pages give the details and the evidence (see #446).

## Overview

| Package | Model classes | Support | Main limits |
|---|---|---|---|
| [stats, MASS](stats/README.md) | `lm`, `glm`, `negbin` | full | no quasi families |
| [lme4](lme4/README.md) | `lmerMod`, `glmerMod`, `lmerModLmerTest`; gamm4 via `$mer` | full | `na.exclude` fails (#88, #134) |
| [glmmTMB](glmmTMB/README.md) | `glmmTMB` | full | no conditioning on selected REs; conditional simulations fall back to unconditional for `exp()` |
| [mgcv](mgcv/README.md) | `gam`, `bam` | full (one linear predictor), partial (several, #600) | only conditional simulations; extended families need mgcViz; several linear predictors (#600); `multinom`, `gamm()$gam` and `gamm4()$gam` not supported (#418, #612) |
| [spaMM](spaMM/README.md) | `HLfit` | full | no k/n binomial via weights |
| [GLMMadaptive](GLMMadaptive/README.md) | `MixMod` | partial | residuals sorted by group (#609); `cbind()` refit fails (#565); beta-binomial simulations wrong (GLMMadaptive, #565); no Pearson residuals |
| [phylolm](phylolm/README.md) | `phylolm`, `phyloglm` | partial | residuals in tree tip order (#609); `poisson_GEE` and models fitted in functions fail (#478); phyloglm: formula predictors fail (#609); no Pearson residuals |
| [brms](brms/README.md) | `brmsfit` | partial (univariate models) | n ≤ posterior draws (#591); `weights()` ignored (#7); `getData()` fails for data expressions and wrappers (#611); unsupported model types with unclear errors (#612) |
| [phyr](phyr/README.md) | | not checked | #235 |
| [gamlss](gamlss/README.md), [VGAM](vgam/README.md) | | not supported | #319, #322 |
| [sjSDM](sjSDM/README.md) | | not supported | |

Problems that affect several packages:

- `integerResponse` is `FALSE` for many integer-valued families (names per package in #620, e.g. glmmTMB `nbinom12`, mgcv `ziP`, spaMM `negbin2`, GLMMadaptive and brms negative binomial), so `testOutliers()` uses the binomial instead of the bootstrap test for models with up to 500 observations, and refits are not randomized for integers (#620).
- Models fitted in a function with an argument named `data` or `df` (formula passed as an argument): `getData()` returns a function, so formula predictors and refits fail (#611).
- Refits fail if NA rows were removed and the offset, the weights or a predictor are vectors outside the data (#601).
- Conditional simulations (the default) give underdispersed residuals for correct models if each value of a latent field is informed by about one observation, e.g. `ar1()` with one observation per time step or spatial fields with one observation per location (strongly for Gaussian, less for Poisson models); the same mechanism applies to observation-level REs (not checked) (#527).
- `rotation = "estimated"` is strongly anti-conservative unless n is much larger than the number of observations (or groups, after `recalculateResiduals(group = )`); with 200 observations, the rejection rate of the uniformity test was 1.00 for the default `n = 250` and 0.02 for `n = 1000` (#489).
- `na.action = na.exclude` stops or gives unusable results (fitted values padded with NA) for all classes checked in #88 (lm, glm, glm.nb, lmer, glmer, glmmTMB, gam).

## Running the checks

From the repository root, with the packages of the respective folder installed:

```r
source("Code/DHARMaPackageSupport/lme4/checks.R")
```

Each script sources [supportChecks.R](supportChecks.R), which loads the current DHARMa (`devtools::load_all("DHARMa")`) and contains the shared check functions (`checkInterface()`, `refitReproduces()`, `simulationVariance()`, `inDataOrder()`, ...). The scripts run on one core in 5 seconds (stats) to about 9 minutes (glmmTMB).

## Conventions

- One folder per package; sections in `checks.R` are RStudio sections (`# Title ----`), and README statements cite them as "checks.R, *Title*". Statements already covered by the package tests cite the test instead of repeating it.
- Each section ends with `stopifnot()` for its statements. Known bugs are asserted as they currently behave (with the issue number, if any), so the script stops when they are fixed.
- Statements that are not backed by a check cite a GitHub issue or issue comment with a reproducible example; otherwise they are listed under "Not checked".
- Former scripts are kept unchanged in `archive/` subfolders (and in [archive/](archive/) for files that were not package-specific).
