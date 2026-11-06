NOTE: for more news about the package, see https://github.com/florianhartig/DHARMa/releases

# DHARMa 0.5.1


## Bugfixes

* `testDispersion` (type = "DHARMa", refit = FALSE) was biased for binomial models with more than one trial (k/n models, specified via `cbind(k, n - k)` or as proportions with the number of trials given in `weights`): residuals were calculated as observed / simulated number of successes minus the fitted *proportion*. Residuals are now calculated with the fitted values on the scale of the observed response (expected number of successes), provided by the new function `getFittedResponse`. Poisson, Bernoulli (0/1) and all other models are not affected (#557).

* Version 0.5.0 was changing the `glmmTMB` model object "simcode" when using simulateResiduals. We fixed this problem by restoring the original simcode to the model object (#538).

* Fixing bug in predictions via getFitted for `gam` and `bam` models. Predictions are now unconditional on random effects and factor smooth interactions; previously they were conditional (#544).

* Fixed `gam` simulations without mgcViz or with `mgcViz = FALSE` (#567).

* Binomial models with `cbind()` response and additional prior weights now stop with an informative error (#568).

* `bam` models with the `offset` argument now stop with an error if mgcv excludes the offset from the fitted values (#572).

* Simulations of `gam` models with `cbind()` response via mgcViz are now integers (#590).

* Solving duplicated plots in `plotResiduals`: (1) when having quadratic effects written with I(variable) in the formula, and (2) for brms with transformed variables in the formula, e.g. `log(variable)` (#536, #542).

* Small fixes in `plotResiduals(form = ~x | group)` (#573).

* `recalculateResiduals()` now sets `nObs` and `nGroups` to the number of grouped residuals, which fixes `testOutliers()` and `testTemporalAutocorrelation(time = NULL)` for grouped residuals (#603, #605).

* `recalculateResiduals()` now stops if fewer than 2 residuals remain after grouping and selection, and warns if fewer than 10 remain (#604).

* `plotResiduals(form = ...)`: default title no longer reads "DHARMa residual Residual vs. predictor"; `recalculateResiduals()` gives an informative error if not called on a DHARMa object.

* Minor fixes in testSpatialAutocorrelation: DHARMa errors if repeated values for space in distMat, extended help, improved plot titles and legend (#73, #546).

* Revised weights handling: introduced more robust check for prior weights (especially in n/k models) with new helper function `hasWeights` (#230, #540).

* Predictions from `GLMMadaptive` models are now created using `fitted()`, which avoids errors on models with transformations (e.g. offset, log) (#556).

* brms: `getPearsonResiduals()` stops with an informative error; unconditional simulations re-simulate the random effects instead of setting them to zero and stop for unsupported models (#574).

* Fix in behavior of `update()` on models with transformed predictors (e.g. `offset()`). This fixes behavior of `getRefit` and related functions such as `simulateLRT` (#351).

* `getRefit()` now works for transformed responses (e.g. `log(y)`) and glmmTMB `cbind()` responses, and `simulateResiduals(refit = TRUE)` works for glmmTMB proportions with weights and stops with an informative error if all refits fail (#571).

* `getRefit()` now works for models fitted with `subset` given as row numbers or as a logical vector defined outside the data (#599).

* Small output fixes in `testCategorical()`, `testOutliers()` and `print.DHARMa` (#569).

* `runBenchmarks()` no longer warns about NA values when the output contains none (#577).

* `testTemporalAutocorrelation()` no longer fails if AlgDesign is loaded, e.g. via agricolae (#504).

* `plotResiduals(form = ~x | group %in% c(...))` now plots one panel per selected group level instead of a wrong subset of the data; comparison operators other than `==`, such as `!=`, give an error (#596).

* `plotResiduals(absoluteDeviation = TRUE)` now also transforms the residuals for factor predictors, `form = ~x | group` and `~x | group == "level"` (#596).

* `testDispersion()` with `refit = TRUE` now aggregates the observed Pearson residuals like the refitted ones after `recalculateResiduals()`, and warns and returns NA if these are all (close to) zero (#575).

* `testQuantiles()` now gives the same results with and without plot, evaluates transformations such as `~log(x)`, and stops for categorical predictors and formulas with several variables (#576).

* `recalculateResiduals()` now always uses the traditional randomization for residuals with `refit = TRUE`, as `simulateResiduals()` does, so `rotation` is not available for them; `$method` of both functions stores the method used (#589).

* `simulateResiduals(refit = TRUE)` now randomizes the residuals of binomial and beta-binomial models with more than one trial on the scale of the number of successes instead of proportions, which had removed most of the power (#598).

## Minor changes

* `testOutliers()`: argument `plotBoostrap` renamed to `plotBootstrap`, old name deprecated (@BertvanderVeen, #535, #560).

* `plotResidualsAll()` is deprecated, use `plotResiduals()` with `form` instead (#596).

* Plot titles of `simulateLRT()` and `testOutliers()` now explain the line colors (@BertvanderVeen, #535).

* New compatibility function `getFittedResponse`, which returns the fitted values on the scale of the observed response, i.e. the expected number of successes for k/n binomial models (`getFitted` returns proportions). It is used in `testDispersion`.

* Documentation: improved vignette (more explanations for conditional vs unconditional simulations); corrected several errors in help files, examples and vignette (#570).



# DHARMa 0.5.0


## Major changes

* For hierarchical models (GLMMs), we changed the default simulations from the supported model's default (mostly unconditional) to conditional simulations (for most of the supported models and packages). This represents a major change in the calculated scaled residuals and is implemented to ensure higher power in dispersion and other tests. We expect different results for calculated residuals using this or older `DHARMa` versions. For compatibility, you can use the argument `simulateREs  = "user-specified"` to change back to the model's default (older `DHARMa` versions). See package vignette for more details.

* In `plotResiduals`, we increased the threshold for the automatic change from quantile regression lines to the spline: from 2,000 to 10,000 data points. When it happens, a message is displayed to warn users. Also, when `quantreg = F`, the color of the spline was changed to black because there is no test associated with the line (as there is for quantile regression). 

* In `plotResiduals`, the argument `form` has a new functionality. Beyond the syntax `data$predictor`, it allows now to use the formula structure  `~predictor`, `~predictor1+predictor2`, `~.`,  `~predictor|group` and `~predictor|group == "group_level"` to plot the residuals against specific/all predictors and grouping variables/levels. This works for most of the supported model functions. It solves problems with NAs in datasets that were excluded by the model (#407, #425).

* In `testCategorical`, `recalculateResiduals`, `testQuantiles`, `testSpatialAutocorrelation` and `testTemporalAutocorrelation`, additional predictors (`catPred`, `group, sel`, `predictor`, `time`, `x,y`- respectively) can now be specified as a formula (similar to `plotResiduals`). This handles rows with NAs that were excluded by the model automatically.

* `brms` is now supported by DHARMa for simple models, i.e. models that could also be fit using glmmTMB (no multi-response, multinomial or structural equation models).


## Bugfixes

* Fixing error in runBenchmarks.  

* Fixing inconsistency in testQuantiles(), including rankTransform() help function (#465).


## Minor changes

* Using 95% confidence intervals for confidence bands in plots for testQuantiles(). Before, we were using standard errors (~ 68% CI).

* Vectorized randomization of residuals to improve speed (@StaffanBetner, #493).

* Faster rotation of residuals in `getQuantile()` (argument `rotation`), using `forwardsolve()` instead of `solve()` (@StaffanBetner, #492).

* `getRefit()` now passes additional arguments (`...`) to `update()` (`update_resp()` for `spaMM`) also for `lm`/`glm`, `glmmTMB`, `spaMM` and `GLMMadaptive` models (@LuthanV, #515).

* Adding color legend to testSpatialAutocorrelation().

* New function plotResidualsAll() to plot residuals against multiple predictors/all predictors of the model via plotResiduals(). 

* New function getPredictorNames() extracts names of fixed and random effects (predictors) from a model.

* testCategorical() now allows additional arguments to boxplot via ..., mainly to allow for appropriate x-axis labels when running plotResidualsAll(). 

* Improved appearance of user-specified titles via "main" in plotResiduals(). 

 

# DHARMa 0.4.7

## New Features

* Includes support for the package phylolm.
* New function for testing residual phylogenetic autocorrelation testPylogeneticAutocorrelation().

## Major change

* New optional argument 'rotation' in simulateResiduals() for the rotation of the residual space prior to calculating the quantile residuals to account for residual covariance as created by temporal, spatial or phylogenetic autocorrelation. 

## Minor changes

* Residual plots: Option to change the red color for significant results to any color of preference through options(DHARMaSignalColor = "red"). This is a good option for color-blind friendly plots.
* Documentation: improved help files and vignette.


# DHARMa 0.4.6

## New Features

* new function benchmarkRuntime() for checking runtime of functions
* new function simulatedLRT() to generate a simulated likelihood ratio test

## Bugfixes

* fixed issue with parallelization in runBenchmarks
* various minor bugfixed and help improvements


# DHARMa 0.4.5

## Minor changes

* included option to simulate mgcv models using the functions implemented in mgcViz which should improve mgcv compatibility with DHARMa

* Added option to include plot title in plot() #320

## Bugfixes

* fixed issues with plotting, see #313 and #274


# DHARMa 0.4.4

## Major changes

* remodelled tests so that all tested packages can be used conditionally

## Minor changes

* re-introduced glmmTMB to suggests
* phyr moved to enhances
* re-modelled package unit tests
* added RStan, CmdStanR, rjags, BayesianTools to enhances, as they could be used with DHARMa
* moved parallel calculations in runBenchmark to R native parallel functions

## New features

* Added rotation option to all functions that create residuals (simulateResiduals, recalculateResiduals, createDHARMa)

## Documentation

* Added help comments about autocorrelation structures, in particular in simulateResiduals, testSpatialAutocorrelation, testTemporalAutocorrelation
* Added example about the use of rotation in testTemporalAutocorrelation
* Improved help of dispersion test


# DHARMa 0.4.3

## Bugfixes

* Removed glmmTMB completely, see #289


# DHARMa 0.4.2

## Bugfixes

* Moved glmmTMB from suggest to import because this package is used in the vignette, see #289

## Minor changes

* Added hurricane dataset
* Help and vignette updates
* phyr added in suggests


# DHARMa 0.4.1

## Bugfixes

* Force method = traditional for refit = T, as it turns out that the PIT method is not a good idea on the residuals, see #272


# DHARMa 0.4.0

This is actually a bugfix release for 0.3.4, but on reflection I decided that 0.4.0 should have been a minor release, so I pushed the version number up to 0.4.0

## Bugfixes

* bugfix in getResiduals, which had consequences for quantile residual calculations with refit = T


# DHARMa 0.3.4

0.3.4 is a relatively important release with various minor improvements a smaller new features, most noteworthy the support of glmmAdaptive

## New features

* added parametric dispersion test in testDispersion (0.3.3.2)
* support for glmmAdaptive (0.3.3.1)
* new plot for categorical predictors
* new plots for result of runBenchmarks

## Major changes

## Minor changes

* changed test statistics in standard dispersion test to standardized variance, to be more in line with standard dispersion parameters
* defaults for plot function unified
* removed option to provide no x,y / time in the correlation tests
* recalculateResiduals now allows subsetting #246
* better input checking in correlation tests #190

## Bugfixes

* bugfix in runBenchmarks included in https://github.com/florianhartig/DHARMa/pull/247
* bugfix in testQuantiles https://github.com/florianhartig/DHARMa/pull/261
* bugfix in getRandomState https://github.com/florianhartig/DHARMa/issues/254


# DHARMa 0.3.3

## Bugfixes

* bugfix in testOutliers, see https://github.com/florianhartig/DHARMa/issues/197
* bugfix in the ecdf / PIT residual function, see https://github.com/florianhartig/DHARMa/issues/195

# DHARMa 0.3.2

## Bugfixes

* bugfix in testOutliers, see https://github.com/florianhartig/DHARMa/issues/182


# DHARMa 0.3.1

## Major changes

* added PIT quantile calculations based on suggestion in #168. For details see ?getQuantiles

## Bugfixes

* bugfix for passing on parameters to plot.default through plotR.DHARMa and plotResiduals

## Minor changes

* added checks / warnings for models fit with weights


# DHARMa 0.3.0

## New features

* quantile regressions switched to qgam, which also calculates p-values on the quantile estimates
* new testQuantiles function, based on the qgam quantile regressions

## Changes

* syntax change for plotResiduals, see ?plotResiduals
* transformQuantiles is deprecated, functionality included in residuals()
* nearly all functions can now also be called directly with a fitted model (for computational efficiency, however, it is still recommended to calculate the residuals first)
* qqPlot now shows disribution, dispersion and outlier test

## Bugfixes

* bugfix #158 for fitting glmmTMB binomial with proportions


# DHARMa 0.2.7

## New features

* added smooth scatter in plotResiduals https://github.com/florianhartig/DHARMa/commit/da01d8c7a9a74558817e4a73fe826084164cf05d
* glmmTMB now fully supported through new compulsory version 1.0 of glmmTMB, which includes the re.form argument in the simulations required by DHARMa https://github.com/florianhartig/DHARMa/pull/140


# DHARMa 0.2.6

## Bugfixes

* return of plotResiduals set to invisible


# DHARMa 0.2.5

## New features

* transformQuantiles function added to transform uniform DHARMa residuals to normal or other residuals

## Minor changes

* several smaller corrections to help


# DHARMa 0.2.4

## Bugfixes

* corrected issues in the vignette
* small corrections to help


# DHARMa 0.2.3

## Bugfixes

* added missing distributions https://github.com/florianhartig/DHARMa/pull/104
* bugfix in simulate residuals https://github.com/florianhartig/DHARMa/issues/107


# DHARMa 0.2.2

## Bugfixes

* fixes bug in Vignette (header lost)


# DHARMa 0.2.1

## New features

* Outlier highlighting (in plots) and formal outlier test, implemented in https://github.com/florianhartig/DHARMa/pull/99
* Supporting now also models fit with the spaMM package

## Major changes

* Remodelled createDHARMa function * option to directly provide scaled residuals was removed
* Rewrote ecdf function for DHARMa to get fully balanced scale, in the course of https://github.com/florianhartig/DHARMa/pull/99

## Minor changes

* a number of smaller updates, mostly to help files

## Bugfixes

* fixes #82 / Bug in recalculateResiduals


# DHARMa 0.2.0

## New features

* support for glmmTMB https://github.com/florianhartig/DHARMa/issues/16, implemented since https://github.com/florianhartig/DHARMa/releases/tag/v0.1.6.2
* support for grouping of residuals, see https://github.com/florianhartig/DHARMa/issues/22
* residual function for DHARMa

## Major changes

* remodeled benchmarks functions in https://github.com/florianhartig/DHARMa/releases/tag/v0.1.6.3
* remodeled dispersion testsin https://github.com/florianhartig/DHARMa/releases/tag/v0.1.6.4, adresses https://github.com/florianhartig/DHARMa/issues/62

## Minor changes

* changed plot function names in https://github.com/florianhartig/DHARMa/releases/tag/v0.1.6.1

## Bugfixes

* fixed bug with zeroinflation test for k/n binomial data https://github.com/florianhartig/DHARMa/issues/55
* fixed bug with p-value calculation via ecdf https://github.com/florianhartig/DHARMa/issues/55


# DHARMa 0.1.6

## New features

* option to apply rank tranformation of x values in plotResiduals, see https://github.com/florianhartig/DHARMa/issues/44
* option to convert predictor to factor
* random seed is fixed, random state is recorded

## Minor changes

* changed syntax in tests for sptial / temporal autocorrlation * null provides now random. Also, custom distance matrices can be provided to testSpatialAutocorrelation
* slight changes to plot layout

## Bugfixes

* error catching for crashes in plot function https://github.com/florianhartig/DHARMa/issues/42
* bugfix for glmer.nb parametricOverdispersinTest https://github.com/florianhartig/DHARMa/issues/47


# DHARMa 0.1.5

## Minor changes

* fixes a bug in version 0.1.4 that occurred when running simulateResiduals with refit = T. Apologies for any inconvenience.


# DHARMa 0.1.4

## Major changes

* new experimental non-parametric dispersion test on simulated residuals. Extended simulations to compare dispersion tests

## Minor changes

* supports for binomial with response coded as factor
* error catching for refit procedure https://github.com/florianhartig/DHARMa/issues/18
* warnings in case the refit procedure fails or produces identical parameter values https://github.com/florianhartig/DHARMa/issues/20


# DHARMa 0.1.3

## Major changes

* includes support for model class 'gam' from package 'mgcv'. Required overwriting the 'fitted' function for gam, see https://github.com/florianhartig/DHARMa/issues/12

## Minor changes

* plotResiduals includes support for factors
* updates to the help


# DHARMa 0.1.2

* This bugfix release fixes an issue with backwards compatibility introduced in the 0.1.1 release, which used the 'startsWith' function that is only available in R base since 3.3.0. In 0.1.2, all occurences of 'startsWith' were replaced with 'grepl', which restores the compatibility with older R versions.


# DHARMa 0.1.1

* including now the negative binomial models from MASS and lme4, as well as the possibility to create synthetic data from the negative binomial family
* includes a createDHARMa function that allows using the plot functions of DHARMa also with externally created simualtions, for example for Bayesian predictive simulations


# DHARMA 0.1.0

* initial release, with support for lm, glm, lme4
