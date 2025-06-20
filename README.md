[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/DHARMa)](https://cran.r-project.org/package=DHARMa)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg)](https://cran.r-project.org/)
[![](https://cranlogs.r-pkg.org/badges/DHARMa)](https://cran.r-project.org/package=DHARMa)

# DHARMa - Residual Diagnostics for HierARchical Models

The 'DHARMa' package uses a simulation-based approach to create  readily interpretable scaled (quantile) residuals for fitted generalized linear (mixed) models. Currently supported are linear and generalized linear (mixed) models from 'lme4' (classes 'lmerMod', 'glmerMod'), 'glmmTMB', 'GLMMadaptive' and 'spaMM'; phylogenetic linear models from 'phylolm' (classes 'phylolm' and 'phyloglm'); generalized additive models ('gam' from 'mgcv'); 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. Moreover, externally created simulations, e.g. posterior predictive simulations from Bayesian software such as 'JAGS', 'STAN', or 'BUGS' can be processed as well. The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial, temporal and phylogenetic autocorrelation.


## Installing DHARMa

### From CRAN 

DHARMa is on [CRAN](https://cran.r-project.org/web/packages/DHARMa/index.html), and for most users, installing from CRAN will be the best option. To install the latest CRAN release, just run 

```{r}
install.packages("DHARMa")
```

To get an overview about its functionality once the package is installed, run

```{r}
library(DHARMa)
?DHARMa
vignette("DHARMa", package="DHARMa")
```
The vignette, which can also be read online [here](https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html), provides many exampless about how to use the package function for the supported regression models. To cite the package, run 

```{r}
citation("DHARMa")
```

To fit a model (from any package supported by DHARMa), run 


```{r}
testData = createData(sampleSize = 200, family = poisson())
m1 <- glm(observedResponse ~ Environment1, 
                     family = "poisson", data = testData)

res <- simulateResiduals(m1, plot = T)
```

and read to help of ?simulateResiduals and the vignette to understand what you can do with the object `res`. If you want to ask questions about DHARMa, or report a bug, please use the [DHARMa GitHub issue page](https://github.com/florianhartig/DHARMa/issues). 

### Development release 

New features in DHARMa will typically on GitHub 1-2 months before they are on CRAN. If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
dependencies = T, build_vignettes = T)
```
Below the status of the automatic tests via GitHub Actions

[![R-CMD-check](https://github.com/florianhartig/DHARMa/actions?query=branch%3Amaster/R-CMD-check/badge.svg)](https://github.com/florianhartig/DHARMa/actions?query=branch%3Amaster)



### Development branches / older releases

To install a specific (older) release, or a particular branch, decide for the version number that you want to install in [https://github.com/florianhartig/DHARMa/releases](https://github.com/florianhartig/DHARMa/releases) (version numbering corresponds to CRAN, but there may be smaller releases that were not pushed to CRAN), or branch and run 

```{r}
devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
ref = "v0.0.2.1", dependencies = T, build_vignettes = T)
```
with the appropriate version number / branch as argument to ref. 

# Contributing to DHARMa

Contributions to DHARMa are very welcome! There are several ways in which you can contribute:

* A simple but nevertheless important way to contribute is to suggest problems / new features in DHARMa, and post them in our [issue tracker](https://github.com/florianhartig/DHARMa/issues). A good issue should at least have a clear reproducible example. If possible, it could also already contain an analysis of the problem, and / or ideas for a fix. Likewise, feel free to comment on issues existing issues, e.g. by adding examples or suggesting solutions. 

* If you want to propose a solution an existing problem, for simple things (typos, etc.), the easiest would be to just create a PR that I can directly merge. For more complicated changes, however, I would suggest that it is more effective to first discuss the approach at the thread of the issue.

When working on these issues, note that there is extensive code for tests / development purposes outsite the core package in the folde ./code/ on GH. You may find useful information there, and in case you have code intended for development to contribute, you may also create a PR intended for this section.

Also, there are a few technical hints about DHARMA development on the [DHARMa GH wiki](https://github.com/florianhartig/DHARMa/wiki). 

# Code of conduct

The development of DHARMA and all its surrounding activities is based on the values of scientific integrity, free software and knowledge, and mutual respect, indepdent of background or world view. 

# Acknowledgements

A question by Catalina Gutiérrez Chacón provided me with the motivation write the first version of DHARMa. Thanks for useful suggestions to improve DHARMa by Jochen Fründ, Tomer J. Czaczkes, Luis Cayuela Delgado, Alexandre Courtiol, Jim Thorson, Lukas Lohse, jmniehaus, justintimm and many other people that made comments on GitHub, Crossvalidated or via email. 



