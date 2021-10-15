library(DHARMa)
library(lme4)
library(rjags) # note - requires to also install Jags via https://sourceforge.net/projects/mcmc-jags/files/
library(BayesianTools)

set.seed(123)

# creating some test data following a poisson distribution
# you can check the data.frame to see the predictors
# true values: intercept = 0, slope = 1
dat <- DHARMa::createData(200, overdispersion = 0.0)

# I will first slowly show you how the residual checks work for frequentists

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

# Checking Bayesian models models

# now the Bayesian way - I am using Jags here, would be the same in STAN, but cold also directly use DHARMa with brms, tighter coupling is on our agenda https://github.com/florianhartig/DHARMa/issues/33
# special notes on Bayesian checks with DHARMa in 

library(rjags)

# Poisson GLM without RE
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
    observedResponseSim[i]~dpois(lambda[i])
  }

}"

# data preparation
Data = as.list(dat)
Data$nobs = nrow(dat)
Data$nGroups = length(unique(dat$group))

# for prior predictive distribution, create copy of the same data
# but with data column set to NA
Data2 = Data
Data2$observedResponse = rep(NA, Data$nobs)

# run the Jags model with Data2 (NAs for observations -> prior predictive)
jagsModel <- jags.model(file= textConnection(modelCode), data=Data2, n.chains = 3)
para.names <- c("intercept","env", "lambda")
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)
marginalPlot(x, which = 1:2) # sampled prior parameters
priorPredDistr = x[,3:202] # prior predictive distribution
hist(priorPredDistr, breaks = 100)
hist(log(priorPredDistr), breaks = 100)

# We see that the prior predictive distribution is massively centered at 0
# Still, half of the predictions are > 1. The reason for this is the log link
# in the Poisson GLM. 
# having wide priors on regression estimates is still the standard 
# choice for these models, but a warning that if you would have
# very weak data (priors dominate), these priors would favor posterior 
# predictions close to 0, which could be a problem in practical applications

# Suggested reading: Jeffrey's prior https://en.wikipedia.org/wiki/Jeffreys_prior
# Bedrick, E. J., Christensen, R., & Johnson, W. (1996). A new perspective on priors for generalized linear models. Journal of the American Statistical Association, 91(436), 1450-1460.

# There are two further checks we could do now, both based on re-fitting the simulated data. 

# 1) Calibration check of the MCMC sampler, see e.g. https://www.rdocumentation.org/packages/BayesianTools/versions/0.1.7/topics/calibrationTest
# 2) Check of bias in parameter estimates (i.e. plot true / estimated)

# We will skip this here because it cost too much runtime, but If you want an example: both checks are performed in https://www.nature.com/articles/s41559-019-0908-0

# After the prior predictive checks, which mostly as about whether we have a 
# sensibly specified prior, we now turn our attention to the posterior 
# predictive checks, which are checks on the model assumptions (= likelihood)

# Now using the real data to fit the model

jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
para.names <- c("intercept","env", "lambda", "observedResponseSim" )
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)

marginalPlot(x, which = 1:2) # fitted regression parameters
posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
posteriorPredSim = x[,203:402] # these are the simulations 

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), 
                   observedResponse = dat$observedResponse, 
                   fittedPredictedResponse = apply(posteriorPredDistr, 2, median), 
                   integerResponse = T)
plot(sim)

# Fit is not very nice, so let's add a random effect

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

  # Posterior predictive simulations
  for(i in 1:nobs){
    
    # conditional simulations, use lambda from the model
    observedResponsePred[i]~dpois(lambda[i])
  
    # unconditional simulations, use new lambda with new RE
    observedResponsePred2[i] ~ dpois(lambda2[i])  # poisson error distribution
    lambda2[i] <- exp(eta2[i]) 
    eta2[i] <- intercept + env*Environment1[i] + RE2[group[i]] 
    # unconditional predictions (conditional predictions use normal lambda)
    lambda3[i] <- exp(intercept + env*Environment1[i])
  }
  
  # new RE for unconditional simulations
  for(j in 1:nGroups){
   RE2[j] ~ dnorm(0,tauRE)  
  }
  
}"

jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
para.names <- c("intercept","env", "lambda2", "observedResponsePred2")
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)

marginalPlot(x, which = 1:2) # fitted regression parameters
posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
posteriorPredSim = x[,203:402] # these are the simulations 

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), 
                   observedResponse = dat$observedResponse, 
                   fittedPredictedResponse = apply(posteriorPredDistr, 2, median), 
                   integerResponse = T)
plot(sim)

# Task: check if there is a difference between using conditional or unconditional
# predictions and/or simulations for simulatedResponse and fittedPredictedResponse (by observing lambda vs. lambda3, and observedResponsePred vs. observedResponsePred2, respectively)

# If there is time, here are two additional tasks:

# Optional task 1: try to check one of the models that you have used in the
# introductory course, or use DHARMa to check a GLMM from your research!

# Optional task 2: # In the DHARMa vignette, there is an example fitting GLMMs to the dataset glmmTMB::Owls. Try to implement the same GLMMS in Jags and check the residuals!




