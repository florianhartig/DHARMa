NOTE: for more news about the package, see https://github.com/florianhartig/DHARMa/releases

# DHARMa 0.4.8




## Bugfixes

* fixing error in runBenchmarks.  
* fixing inconsistency in testQuantiles() #465. Including rankTransform() help function.  


## Minor changes

* Using 95% confidence intervals for confidence bands in plots for testQuantiles(). Before, we were using standard errors (~ 68% CI).



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
