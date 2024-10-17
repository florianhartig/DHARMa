## ----global_options, include=FALSE--------------------------------------------
knitr::opts_chunk$set(fig.width=8.5, fig.height=5.5, fig.align='center', warning=FALSE, message=FALSE)

## ----echo = F, message = F----------------------------------------------------
library(DHARMa)
set.seed(123)

## ----eval = F-----------------------------------------------------------------
#  library(rjags)
#  library(BayesianTools)
#  
#  set.seed(123)
#  
#  dat <- DHARMa::createData(200, overdispersion = 0.2)
#  
#  Data = as.list(dat)
#  Data$nobs = nrow(dat)
#  Data$nGroups = length(unique(dat$group))
#  
#  modelCode = "model{
#  
#    for(i in 1:nobs){
#      observedResponse[i] ~ dpois(lambda[i])  # poisson error distribution
#      lambda[i] <- exp(eta[i]) # inverse link function
#      eta[i] <- intercept + env*Environment1[i]  # linear predictor
#    }
#  
#    intercept ~ dnorm(0,0.0001)
#    env ~ dnorm(0,0.0001)
#  
#    # Posterior predictive simulations
#    for (i in 1:nobs) {
#      observedResponseSim[i]~dpois(lambda[i])
#    }
#  
#  }"
#  
#  jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
#  para.names <- c("intercept","env", "lambda", "observedResponseSim")
#  Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)
#  
#  x = BayesianTools::getSample(Samples)
#  
#  colnames(x) # problem: all the variables are in one array - this is better in STAN, where this is a list - have to extract the right columns by hand
#  posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
#  posteriorPredSim = x[,203:402] # these are the simulations
#  
#  sim = createDHARMa(simulatedResponse = t(posteriorPredSim), observedResponse = dat$observedResponse, fittedPredictedResponse = apply(posteriorPredDistr, 2, median), integerResponse = T)
#  plot(sim)

## ----eval=F-------------------------------------------------------------------
#    # Posterior predictive simulations
#    for (i in 1:nobs) {
#      observedResponseSim[i]~dpois(lambda[i])
#    }

## ----eval = F-----------------------------------------------------------------
#    for(i in 1:nobs){
#      observedResponse[i] ~ dpois(lambda[i])  # poisson error distribution
#      lambda[i] <- exp(eta[i]) # inverse link function
#      eta[i] <- intercept + env*Environment1[i] + RE[group[i]] # linear predictor
#    }
#  
#    for(j in 1:nGroups){
#     RE[j] ~ dnorm(0,tauRE)
#    }

## ----eval=F-------------------------------------------------------------------
#    for(j in 1:nGroups){
#     RESim[j] ~ dnorm(0,tauRE)
#    }
#  
#    for (i in 1:nobs) {
#      observedResponseSim[i] ~ dpois(lambdaSim[i])
#      lambdaSim[i] <- exp(etaSim[i])
#      etaSim[i] <- intercept + env*Environment1[i] + RESim[group[i]]
#    }

