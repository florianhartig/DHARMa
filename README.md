# DHARMa - Residual Diagnostics for HierARchical Models

The DHARMa package creates readily interpretable residuals for generalized linear (mixed) models that are standardized to values between 0 and 1. This is achieved by a simulation-based approach, similar to the Bayesian p-value or the parametric bootstrap: 1) simulate new data from the fitted model 2) from this simulated data, calculate the cummulative density function  3) residual is the value of the empirical density function at the value of the observed data. More on the appraoch

* In the DHARMa vignette (see below)
* A blog post [here](https://theoreticalecology.wordpress.com/2016/08/28/dharma-an-r-package-for-residual-diagnostics-of-glmms/)
* A poster for the 2016 GFÖ conference [here](https://florianhartig.files.wordpress.com/2016/09/dharma.pdf)

## Getting DHARMa

DHARMa is on CRAN. So, to install the latest major release, just run 

```{r}
install.packages("DHARMa")
```

Once the package is installed, run

```{r}
library(DHARMa)
?DHARMa
vignette("DHARMa", package="DHARMa")
```

To get an overview about its functionality. If you want to install a newer (development) version from this repository, run

```{r}
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa", dependencies = T)
```

To install a specific release, decide for the version number that you want to install in [https://github.com/florianhartig/DHARMa/releases](https://github.com/florianhartig/DHARMa/releases) and run 

```{r}
devtools::install_github(repo = "DHARMa", username = "florianhartig", subdir = "DHARMa", ref = "v0.0.2.1")
```

with the appropriate version number. 
