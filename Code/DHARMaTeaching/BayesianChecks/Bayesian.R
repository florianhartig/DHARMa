library(DHARMa)
library(lme4)
library(rjags) # note - requires to also install Jags via https://sourceforge.net/projects/mcmc-jags/files/
library(BayesianTools)
library(brms)

###### Prior predictive checks ######

# Extreme example: strongly nonlinear model
model = function(x) exp(-abs(x)) + rnorm(1,sd = 0.05)

# wide uniform prior leads to a very "biased" prior predictive distribution 
priorDraws = rnorm(5000, sd = 20)
priorPredictiveSims = model(priorDraws)
hist(priorPredictiveSims, breaks = 50)

# an exponential prior, which concentrates mass around 0 (transformed by the model to large values) is arguably far more "neutral"
priorDraws = rexp(5000) * sample(c(-1,1), 5000, T)
priorPredictiveSims = model(priorDraws)
hist(priorPredictiveSims, breaks = 50)

# Essentially, what we see here is a principle that is extended in Jeffrey's prior https://en.wikipedia.org/wiki/Jeffreys_prior

# There are two further checks we can do now, both based on re-fitting the simulated data. 

# 1) Calibration check of the MCMC sampler, see e.g. https://www.rdocumentation.org/packages/BayesianTools/versions/0.1.7/topics/calibrationTest
# 2) Check of bias in parameter estimates (i.e. plot true / estimated)

# If you want an example: both checks are performed in https://www.nature.com/articles/s41559-019-0908-0

###### Posterior predictive checks ######

set.seed(123)

dat <- createData(200)

plot(observedResponse ~ Environment1, data = dat)
plot(observedResponse ~ Environment1, data = dat, pch = as.numeric(dat$group))

fit <- glm(observedResponse ~ Environment1, data = dat, family = "poisson")
res <- simulateResiduals(fit, plot = T) # see that there is a model error, add RE

fit <- glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")
res <- simulateResiduals(fit, plot = T) # now it looks fine

# other tests
testZeroInflation(res)
testSpatialAutocorrelation(res, x = dat$x, y = dat$y)
testTemporalAutocorrelation(res, time = dat$time) # time is not random, correlated with RE 
testGeneric(res, mean)

# more complicated example using glmmTMB::Owls in the vignette, could be a nice exercise to do this Bayesian

# now the Bayesian way - I am using Jags, same in STAN, but cold also directly use DHARMa with brms, tighter coupling is on our agenda https://github.com/florianhartig/DHARMa/issues/33

library(rjags)

Data = as.list(dat)
Data$nobs = nrow(dat)
Data$nGroups = length(unique(dat$group))

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
  }

}"

jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
para.names <- c("intercept","env", "lambda", "observedResponsePred")
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)

colnames(x) # problem: all the variables are in one array - this is better in STAN, where this is a list - have to extract the right columns by hand
posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
posteriorPredSim = x[,203:402] # these are the simulations 

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), observedResponse = dat$observedResponse, fittedPredictedResponse = apply(posteriorPredDistr, 2, median), integerResponse = T)
plot(sim)



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
  for (i in 1:nobs) {
    observedResponsePred[i]~dpois(lambda[i])
  }

}"

jagsModel <- jags.model(file= textConnection(modelCode), data=Data, n.chains = 3)
para.names <- c("intercept","env", "tauRE", "lambda", "observedResponsePred")
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 5000)

x = BayesianTools::getSample(Samples)

colnames(x) # problem: all the variables are in one array - this is better in STAN, where this is a list - have to extract the right columns by hand
posteriorPredDistr = x[,3:202] # this is the uncertainty of the mean prediction (lambda)
posteriorPredSim = x[,203:402] # these are the simulations 

sim = createDHARMa(simulatedResponse = t(posteriorPredSim), observedResponse = dat$observedResponse, fittedPredictedResponse = apply(posteriorPredDistr, 2, median), integerResponse = T)
plot(sim)





