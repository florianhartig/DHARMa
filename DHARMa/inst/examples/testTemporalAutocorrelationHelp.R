testData = createData(sampleSize = 40, family = gaussian(),
                      randomEffectVariance = 0)
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
# specify time as formula (recommended)
testTemporalAutocorrelation(res, time =  ~time)

# or as variable in your environment
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
res = recalculateResiduals(res, group = ~time)
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

  # fit model / would be similar for nlme::gls and similar models
  model = glmmTMB(y ~ ar1(times + 0 | group), data=dat0)

  # Note that standard residuals still show problems because of autocorrelation
  res <- simulateResiduals(model)
  plot(res)

  # The reason is that most (if not all) autoregressive models treat the
  # autocorrelated error as random, i.e. the autocorrelated error structure
  # is not used for making predictions. If you then make predictions based
  # on the fixed effects and calculate residuals, the autocorrelation in the
  # residuals remains. We can see this if we again calculate the auto-
  # correlation test

  res2 <- recalculateResiduals(res, group=dat0$times)
  testTemporalAutocorrelation(res2, time = 1:length(res2$scaledResiduals))

  # so, how can we check then if the current model correctly removed the
  # autocorrelation?

  # Option 1: rotate the residuals in the direction of the autocorrelation
  # to make the independent. Note that this only works perfectly for gls
  # type models as nonlinear link function make the residuals covariance
  # different from a multivariate normal distribution

  # this can be either done by extracting estimated AR1 covariance
  cov <- VarCorr(model)
  cov <- cov$cond$group # extract covariance matrix of REs

  # grouped according to times, rotated with estimated Cov - how all fine!
  res3 <- recalculateResiduals(res, group=dat0$times, rotation=cov)
  plot(res3)
  testTemporalAutocorrelation(res3, time = 1:length(res2$scaledResiduals))

  # alternatively, you can let DHARMa estimate the covariance from the
  # simulations

  res4 <- recalculateResiduals(res, group=dat0$times, rotation="estimated")
  plot(res4)
  testTemporalAutocorrelation(res3, time = 1:length(res2$scaledResiduals))

  # Alternatively, in glmmTMB, we can condition on the estimated correlated
  # residuals. Unfortunately, in this case, we will have to do simulations by
  # hand as glmmTMB does not allow to simulate conditional on a fitted
  # correlation structure

  # re.form = NULL creates predictions conditional on the fitted temporally
  # autocorreated REs
  pred = predict(model, re.form = NULL)

  # now we simulate data, conditional on the autocorrelation part, with the
  # uncorrelated residual error
  simulations = sapply(1:250, function(i) rnorm(length(pred),
                                                pred,
                                                summary(model)$sigma))
  res5 = createDHARMa(simulations, dat0$y, pred)
  plot(res5)

  res5b <- recalculateResiduals(res5, group=dat0$times)
  testTemporalAutocorrelation(res5b, time = 1:length(res5b$scaledResiduals))

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
