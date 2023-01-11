testData = createData(sampleSize = 40, family = gaussian(), 
                      randomEffectVariance = 0)
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
testTemporalAutocorrelation(res, time =  testData$time)

# If you have several observations per time step, e.g. 
# because you have several locations, you will have to 
# aggregate

timeSeries1 = createData(sampleSize = 40, family = gaussian(), 
                         randomEffectVariance = 0)
timeSeries1$location = 1
timeSeries2 = createData(sampleSize = 40, family = gaussian(), 
                         randomEffectVariance = 0)
timeSeries2$location = 2
testData = rbind(timeSeries1, timeSeries2)

fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Will not work because several residuals per time
# testTemporalAutocorrelation(res, time = testData$time)

# aggregating residuals by time
res = recalculateResiduals(res, group = testData$time)
testTemporalAutocorrelation(res, time = unique(testData$time))

# testing only subgroup location 1, could do same with loc 2
res = recalculateResiduals(res, sel = testData$location == 1)
testTemporalAutocorrelation(res, time = unique(testData$time))

# example to demonstrate problems with strong temporal correlations and
# how to possibly remove them by rotating residuals
# note that if your model allows to condition on estimated REs, this may 
# be preferable!

\dontrun{

  set.seed(123)
  
  # Gaussian error 
  
  # Create AR data with 5 observations per time point
  n <- 100                                              
  x <- MASS::mvrnorm(mu = rep(0,n),
                     Sigma = .9 ^ as.matrix(dist(1:n)) )   
  y <- rep(x, each = 5) + 0.2 * rnorm(5*n)                       
  times <- factor(rep(1:n, each = 5), levels=1:n)
  levels(times)
  group <- factor(rep(1,n*5))
  dat0 <- data.frame(y,times,group)
  
  # fit model
  model = glmmTMB(y ~ ar1(times + 0 | group), data=dat0)
  
  # Standard residuals show spurious problems because of autocorrelation
  res <- simulateResiduals(model)
  plot(res)

  # grouped according to times, unrotated shows autocorrelation
  res2 <- recalculateResiduals(res, group=dat0$times) 
  testTemporalAutocorrelation(res2, time = 1:length(res2$scaledResiduals))
  
  # extract estimated AR1 covariance
  cov <- VarCorr(model)
  cov <- cov$cond$group # extract covariance matrix of REs

  # grouped according to times, rotated with estimated Cov - how all fine!
  res3 <- recalculateResiduals(res, group=dat0$times, rotation=cov) 
  plot(res3)
  testTemporalAutocorrelation(res3, time = 1:length(res2$scaledResiduals))

  # Poisson error 
  # note that for GLMMs, covariances will be estimated at the scale of the 
  # linear predictor, while residual covariance will be at the responses scale
  # and thus further distorted by the link. Thus, for GLMMs with a nonlinear
  # link, there will be no exact rotation for a given covariance structure
  
  set.seed(123)
  
  # Create AR data with 5 observations per time point
  n <- 100                                              
  x <- MASS::mvrnorm(mu = rep(0,n),
                     Sigma = .9 ^ as.matrix(dist(1:n)) )   
  y <- rpois(n = n*5, lambda = exp(rep(x, each = 5)))                     
  times <- factor(rep(1:n, each = 5), levels=1:n)
  levels(times)
  group <- factor(rep(1,n*5))
  dat0 <- data.frame(y,times,group)
  
  # fit model
  model = glmmTMB(y ~ ar1(times + 0 | group), data=dat0, family = poisson)
  
  res <- simulateResiduals(model)

  # grouped according to times, unrotated
  res2 <- recalculateResiduals(res, group=dat0$times) 
  testTemporalAutocorrelation(res2, time = 1:length(res2$scaledResiduals))
  
  # grouped according to times, rotated with estimated Cov - problems remain
  cov <- VarCorr(model)
  cov <- cov$cond$group # extract covariance matrix of REs
  res3 <- recalculateResiduals(res, group=dat0$times, rotation=cov) 
  testTemporalAutocorrelation(res3, time = 1:length(res2$scaledResiduals))

  # grouped according to times, rotated with covariance estimated from residual 
  # simulations at the response scale
  res4 <- recalculateResiduals(res, group=dat0$times, rotation="estimated") 
  testTemporalAutocorrelation(res4, time = 1:length(res2$scaledResiduals))

}
