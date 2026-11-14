# DHARMa support for spaMM: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/spaMM/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
library(spaMM)
spaMM.options(nb_cores = 1)
set.seed(123)

dat = createData(sampleSize = 200, randomEffectVariance = 1, family = poisson())
datBin = createData(sampleSize = 200, randomEffectVariance = 1, family = binomial(), binomialTrials = 10)
datBin$prop = datBin$observedResponse1 / 10
datGauss = createData(sampleSize = 200, randomEffectVariance = 1, family = gaussian())
datBin$y01 = as.numeric(datBin$observedResponse1 > 5)
dat$cont = dat$observedResponse + 1

# Families ----

models = list(
  gaussian = fitme(observedResponse ~ Environment1 + (1|group), data = datGauss),
  poisson = fitme(observedResponse ~ Environment1 + (1|group), family = poisson, data = dat),
  bernoulli = fitme(y01 ~ Environment1 + (1|group), family = binomial, data = datBin),
  binomialCbind = fitme(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), family = binomial, data = datBin),
  betabinomial = fitme(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), family = betabin(), data = datBin),
  negbin = fitme(observedResponse ~ Environment1 + (1|group), family = negbin2(), data = dat),
  Gamma = fitme(cont ~ Environment1 + (1|group), family = Gamma(log), data = dat),
  HLfit = HLfit(observedResponse ~ Environment1 + (1|group), family = poisson, data = dat)
)
interface = sapply(models, checkInterface)
print(t(interface))
stopifnot(sapply(models, function(m) class(m)[1]) %in% DHARMa:::getPossibleModels())
stopifnot(interfaceOk(interface))

# Transformed response ----

# spaMM::update_resp() fails for transformed responses; DHARMa then refits via update()
m = fitme(log(observedResponse + 1) ~ Environment1 + (1|group), data = dat)
interface = checkInterface(m, nRefit = 5)
print(interface)
stopifnot(interface == "ok")

# k/n binomial with weights ----

# not possible in spaMM: prior weights are not the number of trials
o = outcome(fitme(prop ~ Environment1 + (1|group), family = binomial, prior.weights = rep(10, 200), data = datBin))
print(o)
stopifnot(startsWith(o, "error"))

# Spatial models ----

dat$x = runif(200); dat$y = runif(200)
mSpatial = fitme(observedResponse ~ Environment1 + Matern(1|x + y), family = poisson, data = dat, fixed = list(nu = 0.5))
interface = checkInterface(mSpatial, nRefit = 5)
print(interface)
stopifnot(interfaceOk(interface))
v = simulationVariance(mSpatial, nsim = 100)
print(v)
stopifnot(v["conditional"] < v["unconditional"])

# Conditional and unconditional simulations ----

# default is conditional (fitted values without REs and conditional simulations
# varying less than unconditional ones: testCompatibility.R)
stopifnot(defaultIsConditional(models$poisson))

# Conditioning on selected random effects ----

datOD = createData(sampleSize = 200, randomEffectVariance = 1, overdispersion = 1, family = poisson())
m2 = fitme(observedResponse ~ Environment1 + (1|group) + (1|ID), family = poisson, data = datOD)
stopifnot(is_outcome(outcome(simulateResiduals(m2, re.form = ~(1|group))), "error", "user-specified"))
stopifnot(outcome(simulateResiduals(m2, simulateREs = "user-specified", re.form = ~(1|group))) == "ok")
v = sapply(list(NA, ~(1|group), NULL), function(f)
  mean(apply(log1p(getSimulations(m2, nsim = 200, simulateREs = "user-specified", re.form = f)), 1, var)))
print(v)
stopifnot(v[1] > v[2], v[2] > v[3])

# Refit ----

# refitting to the observed response reproduces the fixed effects
stopifnot(sapply(models[c("gaussian", "poisson", "binomialCbind")], function(m)
  refitReproduces(m, newresp = response(m), fixef = fixef)))

# Prior weights ----

# no DHARMa warning; for gaussian models, spaMM uses the weights in the simulations
w = rep(c(1, 100), 100)
mw = fitme(observedResponse ~ Environment1 + (1|group), prior.weights = w, data = datGauss)
stopifnot(outcome(simulateResiduals(mw)) == "ok")
v = tapply(apply(getSimulations(mw, nsim = 500), 1, var), w, mean)
print(v)
stopifnot(v["1"] > 10 * v["100"])

# Offsets ----

dat$area = runif(200, 1, 3)
mOff1 = fitme(observedResponse ~ Environment1 + offset(log(area)) + (1|group), family = poisson, data = dat)
stopifnot(interfaceOk(checkInterface(mOff1)))
stopifnot(all.equal(getFitted(mOff1), predict(mOff1, re.form = NA)[, 1]))

# Missing values ----

datNA = dat
datNA$Environment1[1:10] = NA
mNA = fitme(observedResponse ~ Environment1 + (1|group), family = poisson, data = datNA)
stopifnot(interfaceOk(checkInterface(mNA)), length(simulateResiduals(mNA)$scaledResiduals) == 190)

# Order of residuals ----

stopifnot(inDataOrder(models$poisson, dat$observedResponse))

# Pearson residuals and dispersion tests ----

o = dispersionTests(models$poisson)
print(o)
stopifnot(o[c("default", "refit")] == "ok", !startsWith(o["PearsonChisq"], "error"))
