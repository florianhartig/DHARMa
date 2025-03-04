# ISSUE 38

# Check different seeds for Pearson residuals with refit=T (Pearson Param Boot that doesn't depend on the quantile residuals) and compare it with KS test (that depends on quantile residuals), for increasing number of simulations

library(DHARMa)
library(tidyverse)

# varying parameters
nSims <- c(100,250,1000,10000)

#same data

testData <- createData(overdispersion = 0,
                       sampleSize = 100,
                       intercept = 0,
                       numGroups = 10,
                       randomEffectVariance = 0,
                       family = poisson())
#same model
fittedModel <-glm(observedResponse  ~ 
                    Environment1, data = testData, family = poisson()) 

out <- list()

for (k in 1:100) {

    out.sim <- data.frame(nSims=rep(NA,length(nSims)), 
                          DHA.p.val=rep(NA,length(nSims)),
                          KS.p.val=rep(NA,length(nSims)))
    for (i in 1:length(nSims)) {

      out.sim$nSims[i] = nSims[i]
      # DHARMa refit residuals -> bootstrapped Pearson
      res <- simulateResiduals(fittedModel, refit=F, seed = k, n = nSims[i])
      out.sim$DHA.p.val[i] <- testDispersion(res, plot = F, type = "DHARMa")$p.value
      out.sim$KS.p.val[i] <- testUniformity(res, plot=F)$p.value
    }
    out[[k]] <- out.sim  
}

res <- bind_rows(out, .id="seed") %>%
  pivot_longer(3:4, names_to = "test", values_to = "p.val")

ggplot(res, aes(x=as.factor(nSims), y=p.val, col=as.factor(nSims))) + geom_boxplot()+
  ggbeeswarm::geom_beeswarm(alpha=0.4)  + ylim(0,1) +
  facet_grid(~test)

