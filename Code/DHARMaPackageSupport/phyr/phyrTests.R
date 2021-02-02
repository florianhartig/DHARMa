install.packages("phyr")
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

mod_disturbed <- phyr::pglmm(pres ~ (1 | sp__) + (1 | sp__@site) + 
                               (1 | site), 
                             data = oldfield$data %>%
                               dplyr::filter(disturbance == 1), 
                             cov_ranef = list(sp = oldfield$phy),
                             family = "binomial")
