# User-perspective tests

The unit tests (`DHARMa/tests`) and the extended tests (`Code/DHARMaTests`) check that functions return what we expect. They do not show whether the package works well for a user: whether typical workflows run smoothly, whether p-values are plausible, and whether warnings and errors tell the user what to do. The scripts in this folder try the package the way users do. Run them by hand before each release (and after larger changes), look at the output and the plots, and judge the results. They are not part of `R CMD check` or of a GitHub workflow.

## How to run

From the repository root, one script at a time:

```r
source("Code/DHARMaUserTests/01_standardWorkflows.R")
```

The scripts load the package source with `devtools::load_all("DHARMa")`. Plots (PNG) and a log (`*_log.csv`) are written to a temporary folder, or to the folder in the environment variable `DHARMA_USERTEST_OUT`. Each script takes less than a minute. They need devtools, lme4, glmmTMB and mgcv; the last part of `04_advanced.R` also needs GLMMadaptive.

| Script | User | Content |
|---|---|---|
| `01_standardWorkflows.R` | first-time user following the vignette | lm, glm (Poisson, Bernoulli, k/n binomial), glmer, glmmTMB, gam with the standard tests and plots; deliberately misspecified models |
| `02_groupingAutocorrelation.R` | field ecologist | repeated measures per site and year, `recalculateResiduals()`, temporal and spatial autocorrelation |
| `03_userMistakes.R` | careless user | wrong-length or NA predictors, missing arguments, model instead of DHARMa object, very small data |
| `04_advanced.R` | advanced user | `refit = TRUE` (incl. calibration for k/n binomial models), offsets, `simulateREs`, `createDHARMa()`, models fitted inside a function, data order of GLMMadaptive |

## What to check

Every call is wrapped in `chk()`, which records whether it ran, its p-values, warnings, messages and errors. The argument `expect` documents the expected outcome:

- `"ok"`: the call should run without error, and for a correctly specified model the p-values should not be systematically small (single p-values below 0.05 occur by chance). A comment marks the few cases where a known pitfall hides a problem.
- `"detect"`: the model is misspecified on purpose, and the test should give p < 0.05.
- `"error"`: the input is wrong, and DHARMa should stop with a message that explains the problem.

Beyond the expected outcome, check for each call:

- Does it behave as described in the help page and the vignette?
- Are warnings and messages necessary and understandable, or do internal messages (e.g. from qgam) reach the user?
- Do error messages name the actual problem and suggest a solution?
- Are the plots (titles, axis labels, legends) readable?

When the package changes, add a scenario here for each new feature or fixed problem that users can run into.

## Reporting

Summarise the results in a table (call, expected, observed). List problems with a minimal reproducible example and the observed and expected behaviour, split into: bug, confusing message, documentation mismatch, and note whether a problem already existed in the last CRAN version.

## Running the tests with an AI agent

The tests can also be run by an AI coding agent in the role of the user in the table, with the resource limits in AGENTS.md (one R process at a time, no brms or other compiled models). It reports in the format above, marks what it could not verify, and puts additional calls in its own copy of the scripts.
