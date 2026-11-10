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

plot(res) # works

length(res$observedResponse)
length(res$fittedPredictedResponse)
length(res$scaledResiduals)

res$simulatedResponse





library(phyr)
library(ape)
library(dplyr)
library(DHARMa)
data("oldfield") #contained within the phyr package

mod_bayes <- phyr::pglmm(pres ~ disturbance + (1 | sp__) + (1 | site) + 
                           (disturbance | sp__) + (1 | sp__@site), 
                         data = oldfield$data, 
                         cov_ranef = list(sp = oldfield$phy),
                         family = "binomial",
                         bayes = TRUE,
                         prior = "pc.prior.auto")

simulate(mod_bayes)

resids <- DHARMa::simulateResiduals(mod_bayes, plot = FALSE)





