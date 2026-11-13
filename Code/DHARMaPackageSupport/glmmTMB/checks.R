# DHARMa support for glmmTMB: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/glmmTMB/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
library(glmmTMB)
set.seed(123)
fixTMB = function(m) fixef(m)$cond

dat = createData(sampleSize = 200, randomEffectVariance = 1, family = poisson())
datOD = createData(sampleSize = 200, randomEffectVariance = 1, overdispersion = 1, family = poisson())
datZI = createData(sampleSize = 200, randomEffectVariance = 1, pZeroInflation = 0.3, family = poisson())
datBin = createData(sampleSize = 200, randomEffectVariance = 1, family = binomial(), binomialTrials = 10)
datBin$prop = datBin$observedResponse1 / 10
datBinOD = createData(sampleSize = 200, randomEffectVariance = 1, overdispersion = 1, family = binomial(), binomialTrials = 10)
datBinOD$prop = datBinOD$observedResponse1 / 10
datGauss = createData(sampleSize = 200, randomEffectVariance = 1, family = gaussian())
dat$pos = datBin$prop * 0.98 + 0.01 # proportions in (0, 1)
datOD$cont = datOD$observedResponse + rexp(200) # positive continuous

# Families ----

# all fits converge without warnings (overdispersed data for the overdispersed families)
models = noWarnings(list(
  gaussian = glmmTMB(observedResponse ~ Environment1 + (1|group), data = datGauss),
  poisson = glmmTMB(observedResponse ~ Environment1 + (1|group), family = poisson, data = dat),
  bernoulli = glmmTMB(as.numeric(observedResponse1 > 5) ~ Environment1 + (1|group), family = binomial, data = datBin),
  bernoulliFactor = glmmTMB(factor(observedResponse1 > 5) ~ Environment1 + (1|group), family = binomial, data = datBin),
  binomialCbind = glmmTMB(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), family = binomial, data = datBin),
  binomialWeights = glmmTMB(prop ~ Environment1 + (1|group), family = binomial, weights = rep(10, 200), data = datBin),
  betabinomialCbind = glmmTMB(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), family = betabinomial, data = datBinOD),
  betabinomialWeights = glmmTMB(prop ~ Environment1 + (1|group), family = betabinomial, weights = rep(10, 200), data = datBinOD),
  nbinom1 = glmmTMB(observedResponse ~ Environment1 + (1|group), family = nbinom1, data = datOD),
  nbinom2 = glmmTMB(observedResponse ~ Environment1 + (1|group), family = nbinom2, data = datOD),
  zeroInflated = glmmTMB(observedResponse ~ Environment1 + (1|group), ziformula = ~1, family = poisson, data = datZI),
  hurdle = glmmTMB(observedResponse ~ Environment1 + (1|group), ziformula = ~1, family = truncated_poisson, data = datZI),
  genpois = glmmTMB(observedResponse ~ Environment1 + (1|group), family = genpois, data = datOD),
  beta = glmmTMB(pos ~ Environment1 + (1|group), family = beta_family, data = dat),
  Gamma = glmmTMB(cont ~ Environment1 + (1|group), family = Gamma(link = "log"), data = datOD),
  tweedie = glmmTMB(cont ~ Environment1 + (1|group), family = tweedie, data = datOD),
  dispformula = glmmTMB(observedResponse ~ Environment1 + (1|group), dispformula = ~Environment1, data = datGauss)
))
interface = sapply(models, checkInterface)
print(t(interface))
stopifnot(interfaceOk(interface))

# Fitted values ----

# for zero-inflated models, getFitted() returns the expected response including the
# zero-inflation (without random effects; for other models see testCompatibility.R)
m = models$zeroInflated
stopifnot(all.equal(getFitted(m), unname(predict(m, re.form = ~0, type = "conditional")) * (1 - unname(predict(m, type = "zprob")))))

# Conditional and unconditional simulations ----

# default is conditional (conditional simulations vary less than unconditional
# ones: testCompatibility.R)
m = models$poisson
stopifnot(defaultIsConditional(m))
# DHARMa restores the simulation settings of the model object (#538):
# simulate() on the model is unconditional before and after simulateResiduals()
v = simulationVariance(m)
before = mean(apply(simulate(m, nsim = 200), 1, var))
invisible(simulateResiduals(m, n = 10))
after = mean(apply(simulate(m, nsim = 200), 1, var))
print(c(before = before, after = after, v))
stopifnot(abs(after - before) < abs(after - v["conditional"]))

# Conditioning on selected random effects ----

# not available: with simulateREs = "user-specified", simulate.glmmTMB accepts
# only re.form = NULL (default); other values stop with an error
m2 = glmmTMB(observedResponse ~ Environment1 + (1|group) + (1|ID), family = poisson, data = dat)
o = sapply(list(group = ~(1|group), NA_ = NA, zero = ~0), function(f)
  outcome(getSimulations(m2, nsim = 2, simulateREs = "user-specified", re.form = f)))
print(o)
stopifnot(startsWith(o, "error"))
stopifnot(outcome(getSimulations(m2, nsim = 2, simulateREs = "user-specified", re.form = NULL)) == "ok")

# Covariance structures ----

# conditional simulations: DHARMa falls back to unconditional simulations with a
# message for some covariance structures
datTS = data.frame(t = factor(1:200), g = factor(rep(1, 200)), x = runif(200),
                   xc = runif(200), yc = runif(200))
datTS$y = as.numeric(arima.sim(list(ar = 0.7), 200)) + datTS$x
datTS$pos = numFactor(datTS$xc, datTS$yc)
cov = list(
  ar1 = glmmTMB(y ~ x + ar1(t + 0 | g), data = datTS),
  ou = glmmTMB(y ~ x + ou(numFactor(as.numeric(t)) + 0 | g), data = datTS),
  exp = glmmTMB(y ~ x + exp(pos + 0 | g), data = datTS)
)
o = sapply(cov, function(m) outcome(getSimulations(m, nsim = 2)))
print(o)
stopifnot(o[c("ar1", "ou")] == "ok", is_outcome(o["exp"], "message", "fall back to unconditional"))

# Refit ----

stopifnot(sapply(models[c("gaussian", "poisson", "binomialCbind", "betabinomialCbind", "zeroInflated")],
                 refitReproduces, fixef = fixTMB))

# Prior weights ----

# DHARMa warns that weights are ignored in the simulations (cbind() response
# with weights: DHARMa stops, #568, testCompatibility.R)
w = runif(200, 1, 2)
stopifnot(is_outcome(outcome(simulateResiduals(glmmTMB(observedResponse ~ Environment1 + (1|group), weights = w, family = poisson, data = dat))), "warning", "Model was fit with prior weights"))
stopifnot(is_outcome(outcome(simulateResiduals(glmmTMB(observedResponse ~ Environment1 + (1|group), weights = w, data = datGauss))), "warning", "Model was fit with prior weights"))

# Offsets ----

dat$area = runif(200, 1, 3)
mOff1 = glmmTMB(observedResponse ~ Environment1 + offset(log(area)) + (1|group), family = poisson, data = dat)
mOff2 = glmmTMB(observedResponse ~ Environment1 + (1|group), offset = log(area), family = poisson, data = dat)
stopifnot(all.equal(getFitted(mOff1), getFitted(mOff2)))
stopifnot(interfaceOk(checkInterface(mOff1)), interfaceOk(checkInterface(mOff2)))
stopifnot(refitReproduces(mOff1, fixef = fixTMB), refitReproduces(mOff2, fixef = fixTMB))

# Missing values ----

# na.omit (default): residuals for the complete rows; refit works
datNA = dat
datNA$Environment1[1:10] = NA
mNA = glmmTMB(observedResponse ~ Environment1 + (1|group), family = poisson, data = datNA)
mNA2 = update(mNA, na.action = na.exclude)
stopifnot(interfaceOk(checkInterface(mNA)), length(simulateResiduals(mNA)$scaledResiduals) == 190)
# na.exclude: residuals for the complete rows, but getFitted() is padded with NA
# and refit fails (#88, #134)
interface = checkInterface(mNA2, nRefit = 5)
print(interface)
stopifnot(interface[c("observedResponse", "simulations", "simulateResiduals")] == "ok",
          length(simulateResiduals(mNA2)$scaledResiduals) == 190, length(getFitted(mNA2)) == 200,
          is_outcome(interface["simulateResidualsRefit"], "error", "wrong dimension"))

# Order of residuals ----

stopifnot(inDataOrder(models$poisson, dat$observedResponse))

# Pearson residuals and dispersion tests ----

o = sapply(models[c("poisson", "zeroInflated")], dispersionTests)
print(o)
stopifnot(o[c("default", "refit"), ] == "ok", !startsWith(o["PearsonChisq", ], "error"))
