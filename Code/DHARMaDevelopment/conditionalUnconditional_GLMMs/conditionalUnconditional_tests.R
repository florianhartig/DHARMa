#### Benchmark tests for conditional x unconditional residuals ####
# Melina Leite
# dec 2024

library(lme4)
library(DHARMa)

# comparing power 

# parameters
# 1) RE variances: 0.5, 1, 2
# 2) Ngroups: 4, 10, 20

# same sample size; 100

ngroups <- c(4,10,20)
REvariances <- c(0.5,1,2,3)


## function varying dispersion 
# function to varying sampleSize
calculateStatistics <- function(control = 1){
  # data
  testData <- DHARMa::createData(sampleSize = 100, numGroups = 10,
                                 randomEffectVariance = control,
                                 family = poisson())
  # model
  fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group), 
                             data = testData, family = poisson())
  #results
  out <- list()
  
  # residuals
  resUN <- simulateResiduals(fittedModel, re.form=NA)
  resCO <- simulateResiduals(fittedModel, re.form=NULL)
  
  # ks test
  out$KS.uncond <- testUniformity(resUN, plot = F)$p.value
  out$KS.cond   <- testUniformity(resCO, plot = F)$p.value
  
  #dispersion DHARMA
  out$DISP.uncond <- testDispersion(resUN, plot = F)$p.value
  out$DISP.cond   <- testDispersion(resCO, plot = F)$p.value
  
  # outliers
  out$OUT.uncond <- testOutliers(resUN, plot = F)$p.value
  out$OUT.cond   <- testOutliers(resCO, plot = F)$p.value
  
  return(unlist(out))
}
#calculateStatistics()

####################
##### Running #####
###################

 

output <- list()

    
for (k in ngroups){ # Varying intercepts
      
      out <- runBenchmarks(calculateStatistics, controlValues = REvariances,
                            ngroups = ngroups,
                           nRep=1000, parallel = 7)
      output[[length(output) + 1]] <- out
      
}


save(output, file="benchmarks_poisson.Rdata")