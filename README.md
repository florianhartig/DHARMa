# DHARMa - Residual Diagnostics for HierARchical Models

The DHARMa package creates readily interpretable residuals for generalized linear (mixed) models that are standardized to values between 0 and 1. This is achieved by a simulation-based approach, similar to the Bayesian p-value or the parametric bootstrap: 1) simulate new data from the fitted model 2) from this simulated data, calculate the cummulative density function  3) residual is the value of the empirical density function at the value of the observed data. More on the appraoch

* In the DHARMa vignette (see below)
* A blog post [here](https://theoreticalecology.wordpress.com/2016/08/28/dharma-an-r-package-for-residual-diagnostics-of-glmms/)
* A poster for the 2016 GFÖ conference [here](https://florianhartig.files.wordpress.com/2016/09/dharma.pdf)

## Getting DHARMa

### From CRAN 

DHARMa is on [CRAN](https://cran.r-project.org/web/packages/DHARMa/index.html). So, to install the latest major release, just run 

```{r}
install.packages("DHARMa")
```

To get an overview about its functionality once the package is installed, run

```{r}
library(DHARMa)
?DHARMa
vignette("DHARMa", package="DHARMa")
```

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa", dependencies = T)
```
Below the status of the automatic Travis CI tests on the master branch (if this doesn load see [here](https://travis-ci.org/florianhartig/DHARMa))

[![Build Status](https://travis-ci.org/florianhartig/DHARMa.svg?branch=master)](https://travis-ci.org/florianhartig/DHARMa)

### Older releases

To install a specific (older) release, decide for the version number that you want to install in [https://github.com/florianhartig/DHARMa/releases](https://github.com/florianhartig/DHARMa/releases) (version numbering corresponds to CRAN, but there may be smaller releases that were not pushed to CRAN) and run 

```{r}
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa", ref = "v0.0.2.1")
```
with the appropriate version number. 
