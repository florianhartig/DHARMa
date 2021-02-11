

###### Prior predictive checks ######

# Strongly nonlinear model
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


###### Posterior predictive checks ######