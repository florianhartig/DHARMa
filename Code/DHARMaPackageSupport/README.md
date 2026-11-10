# Package support

What DHARMa supports for each regression package, and the checks behind it. Each package folder has a `README.md` with the support and its limits, and a script (`checks.R`) whose sections back the statements in the README. The scripts stop with an error if a statement no longer holds, so they can be re-run after changes to DHARMa or to the packages.

The vignette (section "Supported packages and frameworks") and the package tests ([testModelTypes.R](../../DHARMa/tests/testthat/testModelTypes.R), [testCompatibility.R](../../DHARMa/tests/testthat/testCompatibility.R)) remain the user-facing documentation and the automated tests; these pages give the details and the evidence (see #446).

## Overview

| Package | Model classes | Support | Main limits |
|---|---|---|---|
| [stats, MASS](stats/README.md) | `lm`, `glm`, `negbin` | full | no quasi families; `na.exclude` fails (#88) |
| [lme4](lme4/README.md) | `lmerMod`, `glmerMod`, `lmerModLmerTest`; gamm4 via `$mer` | full | lmerTest: refit fails; gamm4: spurious weights warning; `na.exclude` fails (#88) |
| [glmmTMB](glmmTMB/README.md) | `glmmTMB` | full | no conditioning on selected REs; conditional simulations fall back to unconditional for `exp()`; `na.exclude`: refit fails (#88) |
| [mgcv](mgcv/README.md) | `gam`, `bam` | full (one linear predictor), partial (several) | only conditional simulations; extended families need mgcViz; `gaulss` fitted values wrong; refit fails for `gaulss`/`ziplss`; `multinom` and `gamm()` not supported |
| [spaMM](spaMM/README.md) | `HLfit` | full | refit fails for transformed responses; no k/n binomial via weights |
| [GLMMadaptive](GLMMadaptive/README.md) | `MixMod` | partial | residuals sorted by group (#609); `cbind()` refit and beta-binomial (#565); no Pearson residuals |
| [phylolm](phylolm/README.md) | `phylolm`, `phyloglm` | partial | residuals in tree tip order (#609); `poisson_GEE` (#478); no Pearson residuals |
| [brms](brms/README.md) | `brmsfit` | partial (univariate models) | n ≤ posterior draws (#591); `weights()` ignored (#7); `getData()` fails for data expressions and wrappers; unsupported model types with unclear errors |
| [phyr](phyr/README.md) | | not checked | #235 |
| [gamlss](gamlss/README.md), [VGAM](vgam/README.md) | | not supported | #319, #322 |
| [sjSDM](sjSDM/README.md) | | not supported | |

## Running the checks

From the repository root, with the packages of the respective folder installed:

```r
source("Code/DHARMaPackageSupport/lme4/checks.R")
```

Each script sources [supportChecks.R](supportChecks.R), which loads the current DHARMa (`devtools::load_all("DHARMa")`) and contains the shared check functions (`checkInterface()`, `refitReproduces()`, `simulationVariance()`, `inDataOrder()`, ...). The scripts run on one core in 5 seconds (stats) to about 9 minutes (glmmTMB).

## Conventions

- One folder per package; sections in `checks.R` are RStudio sections (`# Title ----`), and README statements cite them as "checks.R, *Title*". Statements already covered by the package tests cite the test instead of repeating it.
- Each section ends with `stopifnot()` for its statements. Known bugs are asserted as they currently behave (with the issue number, if any), so the script stops when they are fixed.
- Statements that are not backed by a check are listed under "Not checked".
- Former scripts are kept unchanged in `archive/` subfolders (and in [archive/](archive/) for files that were not package-specific).
