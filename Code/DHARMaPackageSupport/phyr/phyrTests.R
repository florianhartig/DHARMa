# install.packages("phyr")
library(phyr)
library(ape)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data("oldfield")

data = oldfield$data %>%
  dplyr::filter(disturbance == 1)


mod_disturbed <- phyr::pglmm(pres ~ (1 | sp__) , 
                             data = oldfield$data %>%
                               dplyr::filter(disturbance == 1), 
                             cov_ranef = list(sp = oldfield$phy),
                             family = "binomial")

res <- simulateResiduals(mod_disturbed)

plot(res)

length(res$observedResponse)
length(res$fittedPredictedResponse)
length(res$scaledResiduals)

res$simulatedResponse
