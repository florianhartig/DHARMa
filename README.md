# DHARMa - Residual Diagnostics for HierARchical Models

The DHARMa package creates readily interpretable residuals for generalized linear (mixed) models that are standardized to values between 0 and 1. This is achieved by a simulation-based approach, similar to the Bayesian p-value or the parametric bootstrap: 1) simulate new data from the fitted model 2) from this simulated data, calculate the cummulative density function  3) residual is the value of the empirical density function at the value of the observed data.


To install the latest (development) version in this repository, run

```{r}
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa", dependencies = T)
```

To install the latest or previous releases, decide for the version number that you want to install in [https://github.com/florianhartig/DHARMa/releases](https://github.com/florianhartig/DHARMa/releases) and run 

```{r}
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa", ref = "v0.0.2.1")
```

with the appropriate version number. Once the package is installed, run

```{r}
library(DHARMa)
?DHARMa
vignette("DHARMa", package="DHARMa")
```

