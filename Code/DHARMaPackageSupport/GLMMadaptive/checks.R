# DHARMa support for GLMMadaptive: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/GLMMadaptive/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
library(GLMMadaptive)
set.seed(123)

dat = createData(sampleSize = 200, randomEffectVariance = 1, family = poisson())
datZI = createData(sampleSize = 200, randomEffectVariance = 1, pZeroInflation = 0.3, family = poisson())
datBin = createData(sampleSize = 200, randomEffectVariance = 1, family = binomial(), binomialTrials = 10)
datOD = createData(sampleSize = 200, randomEffectVariance = 1, overdispersion = 1, family = poisson())

# Families ----

models = list(
  poisson = mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, family = poisson(), data = dat),
  bernoulli = mixed_model(as.numeric(observedResponse1 > 5) ~ Environment1, random = ~ 1 | group, family = binomial(), data = datBin),
  negbin = mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, family = negative.binomial(), data = datOD),
  zeroInflated = mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, zi_fixed = ~ 1, family = zi.poisson(), data = datZI)
)
interface = sapply(models, checkInterface)
print(t(interface))
stopifnot(sapply(models, function(m) class(m)[1]) %in% DHARMa:::getPossibleModels())
stopifnot(interfaceOk(interface))

# k/n binomial ----

# cbind() response: simulations and residuals work; refit fails
mCbind = mixed_model(cbind(observedResponse1, observedResponse0) ~ Environment1, random = ~ 1 | group, family = binomial(), data = datBin)
interface = checkInterface(mCbind, nRefit = 5)
stopifnot(interface[names(interface) != "simulateResidualsRefit"] == "ok",
          is_outcome(interface["simulateResidualsRefit"], "error", "0 <= y <= 1"))
# beta-binomial: getObservedResponse() does not recognise the family name
# "beta binomial" (#565)
set.seed(1)
datBB = createData(sampleSize = 200, randomEffectVariance = 0.5, overdispersion = 0.5, family = binomial(), binomialTrials = 10)
mBB = mixed_model(cbind(observedResponse1, observedResponse0) ~ Environment1, random = ~ 1 | group, family = beta.binomial(), data = datBB)
o = outcome(simulateResiduals(mBB))
print(o)
stopifnot(is_outcome(o, "warning", "nKcase - wrong family"))

# Gaussian ----

# mixed_model() has no gaussian family
stopifnot(startsWith(outcome(mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, family = gaussian(), data = dat)), "error"))

# Conditional and unconditional simulations ----

# default is conditional (fitted values without REs and conditional simulations
# varying less than unconditional ones: testCompatibility.R)
m = models$poisson
stopifnot(defaultIsConditional(m))
# simulateREs = "user-specified": new_RE = TRUE is passed to simulate() and re-simulates
# the REs; type = "subject_specific" (vignette table) cannot be passed, because it
# clashes with the argument type of getSimulations()
v = sapply(c(FALSE, TRUE), function(newRE) mean(apply(getSimulations(m, nsim = 200,
  simulateREs = "user-specified", new_RE = newRE), 1, var)))
print(v)
stopifnot(v[1] < v[2])
stopifnot(is_outcome(outcome(simulateResiduals(m, simulateREs = "user-specified", type = "subject_specific")), "error", "arg"))
# re.form is not available: DHARMa warns
stopifnot(is_outcome(outcome(getSimulations(m, nsim = 2, simulateREs = "user-specified", re.form = NA)), "warning", "re.form"))

# Refit ----

stopifnot(sapply(models[c("poisson", "bernoulli")], refitReproduces, fixef = fixef, tolerance = 1e-3))

# Prior weights ----

# weights are per group in GLMMadaptive; DHARMa warns
w = runif(nlevels(dat$group), 1, 2)
mw = mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, family = poisson(), weights = w, data = dat)
stopifnot(is_outcome(outcome(simulateResiduals(mw)), "warning", "Model was fit with prior weights"))

# Offsets ----

dat$area = runif(200, 1, 3)
mOff = mixed_model(observedResponse ~ Environment1 + offset(log(area)), random = ~ 1 | group, family = poisson(), data = dat)
interface = checkInterface(mOff)
print(interface)
stopifnot(interfaceOk(interface))

# Missing values ----

datNA = dat
datNA$Environment1[1:10] = NA
mNA = mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, family = poisson(), data = datNA)
interface = checkInterface(mNA)
print(interface)
stopifnot(interfaceOk(interface))
stopifnot(length(simulateResiduals(mNA)$scaledResiduals) == 190)

# Order of residuals ----

# mixed_model() sorts the data by group; residuals are in this sorted order (#609)
datShuffled = dat[sample(200), ]
mShuffled = mixed_model(observedResponse ~ Environment1, random = ~ 1 | group, family = poisson(), data = datShuffled)
stopifnot(!inDataOrder(mShuffled, datShuffled$observedResponse),
          inDataOrder(mShuffled, datShuffled$observedResponse[order(datShuffled$group)]))

# Pearson residuals and dispersion tests ----

# Pearson residuals are not available; testDispersion() works with the default type
res = simulateResiduals(m)
o = c(default = outcome(testDispersion(res, plot = FALSE)),
      PearsonChisq = outcome(testDispersion(res, type = "PearsonChisq", plot = FALSE)),
      refit = outcome(testDispersion(simulateResiduals(m, refit = TRUE, n = 20), plot = FALSE)))
print(o)
stopifnot(o["default"] == "ok", is_outcome(o["PearsonChisq"], "error", "Pearson residuals"),
          is_outcome(o["refit"], "error", "Pearson residuals"))
