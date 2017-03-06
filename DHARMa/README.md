# DHARMa - Residual Diagnostics for HierARchical Models

The DHARMa package creates readily interpretable residuals for generalized linear (mixed) models that are standardized to values between 0 and 1. This is achieved by a simulation-based approach, similar to the Bayesian p-value or the parametric bootstrap: 1) simulate new data from the fitted model 2) from this simulated data, calculate the cummulative density function  3) residual is the value of the empirical density function at the value of the observed data.

The package includes various functions that deal with issues such as 

* Misfit 
* Overdispersion
* Zero-inflation
* Residual temporal autocorrelation
* Residual spatial autocorrelation

To get more information, install the package and run

```{r}
library(DHARMa)
?DHARMa
vignette("DHARMa", package="DHARMa")
```

# Acknowledgements

Thanks to comments / suggestions from

Jochen Fründ
Tomer J. Czaczkes
Luis Cayuela Delgado