# DHARMa - Residual Diagnostics for HierARchical Models

The DHARMa package creates readily interpretable residuals for generalized linear (mixed) models that are standardized to values between 0 and 1. This is achieved by a simulation-based approach, similar to the Bayesian p-value or the parametric bootstrap: 1) simulate new data from the fitted model 2) from this simulated data, calculate the cummulative density function  3) residual is the value of the empirical density function at the value of the observed data.

To install the lates development release, run 

```{r}

library(devtools)
install_url("https://dl.dropboxusercontent.com/s/xlvjf0vpslukl29/DHARMa.tar.gz", dependencies = T)
library(DHARMa)
?DHARMa
vignette("DHARMa", package="DHARMa")

```

