
library(DHARMa)
library(MASS)
library(lme4)
library(mgcv)

# Test data 

testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)

# Fit 

fittedModelGLM <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)

fittedModelGAM <- gam(cbind(observedResponse1,observedResponse0) ~ s(Environment1) ,family = "binomial", data = testData)


out = simulate(fittedModelGLM, nsim = 10)
out2 = simulate(fittedModelGAM, nsim = 10)

# although the class of both objects is df, they have different dimensions

dim(out)
# [1] 200  10
dim(out2)
# [1]  0 10

out2 = data.frame(out2)

# and they do behave differently if used with as.matrix. 


x = out2

# a hack because for some reason simulateGAM returns a different Format
convertGam <- function(x){
  nSim = ncol(x)
  nObs = nrow(x[[1]])
  
  df = list()
  
  for(i in 1:nSim){
    rownames(x[[i]]) <- as.character(1:200)
  }
  df= as.data.frame(df)

  return(df)
}


