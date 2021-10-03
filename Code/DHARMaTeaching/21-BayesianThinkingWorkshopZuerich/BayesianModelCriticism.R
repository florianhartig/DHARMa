library(DHARMa)
library(lme4)
library(rjags) # note - requires to also install Jags via https://sourceforge.net/projects/mcmc-jags/files/
library(BayesianTools)

###### Prior predictive checks ######

# Purpose of prior predictive checks is to find out if the chosen prior
# creates predictions that we would classify as reasonable given our 
# intentions, e.g. when we specified an uninformative prior, we would usually  
# expect that the model predictions have no strong preference for a particular
# value.

# Extreme example: strongly nonlinear model
model = function(x) exp(-abs(x)) + rnorm(1,sd = 0.05)

# wide uniform prior leads to a prior predictive distribution that is centered
# at zero
priorDraws = rnorm(5000, sd = 20)
priorPredictiveSims = model(priorDraws)
hist(priorPredictiveSims, breaks = 50)

# an exponential prior, which concentrates mass around 0 (transformed by the model to large values) is arguably far more "neutral" prior predictive distribution
priorDraws = rexp(5000) * sample(c(-1,1), 5000, T)
priorPredictiveSims = model(priorDraws)
hist(priorPredictiveSims, breaks = 50)

# Essentially, what we see here is a principle that is extended in Jeffrey's prior https://en.wikipedia.org/wiki/Jeffreys_prior

# If you want to do the checks in Jags, just leave the data vector empty (see example below)

# There are two further checks we could do now, both based on re-fitting the simulated data. 

# 1) Calibration check of the MCMC sampler, see e.g. https://www.rdocumentation.org/packages/BayesianTools/versions/0.1.7/topics/calibrationTest
# 2) Check of bias in parameter estimates (i.e. plot true / estimated)

# We will skip this here because it cost too much runtime, but If you want an example: both checks are performed in https://www.nature.com/articles/s41559-019-0908-0

###### Posterior predictive checks ######

# After the prior predictive checks, which mostly as about whether we have a 
# sensibly specified prior, we now turn our attention to the posterior 
# predictive checks, which are checks on the model assumptions (= likelihood)

set.seed(123)

# creating some test data following a poisson distribution
# you can check the data.frame to see the predictors
dat <- DHARMa::createData(200, overdispersion = 0.0)

# I will first show you how the residual checks work for frequentists

# fit poisson glm
fit <- glm(observedResponse ~ Environment1, data = dat, family = "poisson")

# residual checks with DHARMa
res <- DHARMa::simulateResiduals(fit, plot = T) 

# we see overdispersion, add RE to the model
fit <- glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")
res <- simulateResiduals(fit, plot = T) 

# now it looks fine

# how to access residuals in DHARMa
residuals(res) # residual values
plotResiduals(res, dat$Environment1) # plot against predictor

# DHARMa implements a sizable number of additional tests, 
# check the help for details. Here some examples
testZeroInflation(res)
testSpatialAutocorrelation(res, x = dat$x, y = dat$y)
testGeneric(res, mean) # tests a generic summary statistics.

# now the Bayesian way - I am using Jags, same in STAN, but cold also directly use DHARMa with brms, tighter coupling is on our agenda https://github.com/florianhartig/DHARMa/issues/33

library(rjags)
Data = as.list(dat)
Data$nobs = nrow(dat)
Data$nGroups = length(unique(dat$group))

# Possion model without RE
modelCode = "model{

  for(i in 1:nobs){
    observedResponse[i] ~ dpois(lambda[i])  # poisson error distribution
    lambda[i] <- exp(eta[i]) # inverse link function
    eta[i] <- intercept + env*Environment1[i]  # linear predictor
  }
  
  intercept ~ dnorm(0,0.0001)
  env ~ dnorm(0,0.0001)

  # Posterior predictive simulations 
  for (i in 1:nobs) {
    observedResponsePred[i]~dpois(lambda[i])
    lambda2[i] <- exp(intercept + env*Environment1[i])
  }

}"

jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
para.names <- c("intercept","env", "lambda2", "observedResponsePred")
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)

colnames(x) # problem: all the variables are in one array - this is better in STAN, where this is a list - have to extract the right columns by hand
posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
posteriorPredSim = x[,203:402] # these are the simulations 

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), 
                   observedResponse = dat$observedResponse, 
                   fittedPredictedResponse = apply(posteriorPredDistr, 2, median), 
                   integerResponse = T)
plot(sim)

# Task: check if there is a difference between using conditional or unconditional
# predictions for fittedPredictedResponse (by observing lambda vs. lambda2)

# Possion model with RE 
modelCode = "model{

  for(i in 1:nobs){
    observedResponse[i] ~ dpois(lambda[i])  # poisson error distribution
    lambda[i] <- exp(eta[i]) # inverse link function
    eta[i] <- intercept + env*Environment1[i] + RE[group[i]] # linear predictor
  }
  
  for(j in 1:nGroups){
   RE[j] ~ dnorm(0,tauRE)  
  }

  intercept ~ dnorm(0,0.0001)
  env ~ dnorm(0,0.0001)
  tauRE ~ dgamma(0.001,0.001)

  # Posterior predictive simulations, conditional 
  # recipe: re-simulate the top part of the likelihood
  for (i in 1:nobs) {
    observedResponsePred[i]~dpois(lambda[i])
  }
  
  # Posterior predictive simulations, unconditional 
  # recipe: just copy the likelihood and replace 
  # everything that is stochastic
  for(i in 1:nobs){
    observedResponsePred2[i] ~ dpois(lambda2[i])  # poisson error distribution
    lambda2[i] <- exp(eta2[i]) # inverse link function
    eta2[i] <- intercept + env*Environment1[i] + RE2[group[i]] # linear predictor
    lambda3[i] <- exp(intercept + env*Environment1[i])
  }
  
  for(j in 1:nGroups){
   RE2[j] ~ dnorm(0,tauRE)  
  }
  
}"

jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
para.names <- c("intercept","env", "lambda2", "observedResponsePred2")
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)

colnames(x) # problem: all the variables are in one array - this is better in STAN, where this is a list - have to extract the right columns by hand
posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
posteriorPredSim = x[,203:402] # these are the simulations 

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), 
                   observedResponse = dat$observedResponse, 
                   fittedPredictedResponse = apply(posteriorPredDistr, 2, median), 
                   integerResponse = T)
plot(sim)

# Task: check if there is a difference between using conditional or unconditional
# predictions and/or simulations for simulatedResponse and fittedPredictedResponse (by observing lambda vs. lambda2, and observedResponsePred vs. observedResponsePred2, respectively)

# If there is time, here are two additional tasks:

# Optional task 1: try to check one of the models that you have used in the
# introductory course, or use DHARMa to check a GLMM from your research!

# Optional task 2: # In the DHARMa vignette, there is an example fitting GLMMs to the dataset glmmTMB::Owls. Try to implement the same GLMMS in Jags and check the residuals!




