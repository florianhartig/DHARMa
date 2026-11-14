# DHARMa support for lme4: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/lme4/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
library(lme4)
set.seed(123)

dat = createData(sampleSize = 200, randomEffectVariance = 1, family = poisson())
datBin = createData(sampleSize = 200, randomEffectVariance = 1, family = binomial(), binomialTrials = 10)
datBin$prop = datBin$observedResponse1 / 10
datGauss = createData(sampleSize = 200, randomEffectVariance = 1, family = gaussian())

# Model classes and families ----

models = list(
  lmer = lmer(observedResponse ~ Environment1 + (1|group), data = datGauss),
  lmerTest = lmerTest::lmer(observedResponse ~ Environment1 + (1|group), data = datGauss),
  poisson = glmer(observedResponse ~ Environment1 + (1|group), family = poisson, data = dat),
  bernoulli = glmer(as.numeric(observedResponse1 > 5) ~ Environment1 + (1|group), family = binomial, data = datBin),
  bernoulliFactor = glmer(factor(observedResponse1 > 5) ~ Environment1 + (1|group), family = binomial, data = datBin),
  binomialCbind = glmer(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), family = binomial, data = datBin),
  binomialWeights = glmer(prop ~ Environment1 + (1|group), family = binomial, weights = rep(10, 200), data = datBin),
  negbin = glmer.nb(observedResponse ~ Environment1 + (1|group), data = dat),
  Gamma = glmer(observedResponse + 1 ~ Environment1 + (1|group), family = Gamma(link = "log"), data = dat)
)
interface = sapply(models, checkInterface)
print(t(interface))
stopifnot(sapply(models, function(m) class(m)[1]) %in% DHARMa:::getPossibleModels())
stopifnot(interface == "ok")

# Logical response ----

# a logical response (e.g. y > 5) works, but getObservedResponse() returns a logical vector
m = glmer(observedResponse1 > 5 ~ Environment1 + (1|group), family = binomial, data = datBin)
stopifnot(is.logical(getObservedResponse(m)), checkInterface(m)[-1] == "ok")

# gamm4 ----

# gamm4 returns a list; its element mer is a merMod object
m = gamm4::gamm4(observedResponse ~ s(Environment1), random = ~(1|group), family = poisson, data = dat)
interface = checkInterface(m$mer, nRefit = 5)
print(interface)
stopifnot(inherits(m$mer, "glmerMod"), interface[c("observedResponse", "fitted", "fittedResponse")] == "ok")
# gamm4 passes weights of 1 to glmer(); these are not treated as prior weights
stopifnot(all(model.frame(m$mer)[["(weights)"]] == 1),
          !grepl("prior weights", interface))

# Conditional and unconditional simulations ----

# default is conditional (fitted values without REs and conditional simulations
# varying less than unconditional ones: testCompatibility.R)
stopifnot(defaultIsConditional(models$poisson))

# Conditioning on selected random effects ----

# re.form is passed to simulate.merMod, but only with simulateREs = "user-specified"
datOD = createData(sampleSize = 200, randomEffectVariance = 1, overdispersion = 1, family = poisson())
m2 = glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = poisson, data = datOD)
stopifnot(is_outcome(outcome(simulateResiduals(m2, re.form = ~(1|group))), "error", "user-specified"))
stopifnot(outcome(simulateResiduals(m2, simulateREs = "user-specified", re.form = ~(1|group))) == "ok")
v = sapply(list(NA, ~(1|group), NULL), function(f)
  mean(apply(log1p(getSimulations(m2, nsim = 200, simulateREs = "user-specified", re.form = f)), 1, var)))
print(v)
stopifnot(v[1] > v[2], v[2] > v[3])

# Refit ----

# refit = TRUE works (see Model classes and families) and refitting to the
# observed response reproduces the fixed effects
stopifnot(sapply(models[c("lmer", "poisson", "binomialCbind", "negbin")], refitReproduces))

# Prior weights ----

# DHARMa warns that weights are ignored in the simulations
w = runif(200, 1, 2)
stopifnot(is_outcome(outcome(simulateResiduals(lmer(observedResponse ~ Environment1 + (1|group), weights = w, data = datGauss))), "warning", "prior weights"))
stopifnot(is_outcome(outcome(simulateResiduals(glmer(observedResponse ~ Environment1 + (1|group), weights = w, family = poisson, data = dat))), "warning", "prior weights"))
# lme4 does indeed ignore them: the simulated variance does not depend on the weights
mw = suppressWarnings(lmer(observedResponse ~ Environment1 + (1|group), weights = rep(c(1, 100), 100), data = datGauss))
v = apply(suppressWarnings(getSimulations(mw, nsim = 500)), 1, var)
print(tapply(v, rep(c(1, 100), 100), mean))
stopifnot(abs(diff(tapply(v, rep(c(1, 100), 100), mean))) < 0.2 * mean(v))

# Offsets ----

# offset() in the formula and the offset argument give the same fitted values and residuals
dat$area = runif(200, 1, 3)
mOff1 = glmer(observedResponse ~ Environment1 + offset(log(area)) + (1|group), family = poisson, data = dat)
mOff2 = glmer(observedResponse ~ Environment1 + (1|group), offset = log(area), family = poisson, data = dat)
stopifnot(all.equal(getFitted(mOff1), getFitted(mOff2)))
stopifnot(interfaceOk(checkInterface(mOff1)), interfaceOk(checkInterface(mOff2)))
stopifnot(refitReproduces(mOff1), refitReproduces(mOff2))

# Missing values ----

# with na.omit (default), residuals are calculated for the complete rows only; refit works
datNA = dat
datNA$Environment1[1:10] = NA
mNA = glmer(observedResponse ~ Environment1 + (1|group), family = poisson, data = datNA)
stopifnot(interfaceOk(checkInterface(mNA)), length(simulateResiduals(mNA)$scaledResiduals) == 190)
# with na.exclude, simulate.merMod fails (#88, #134)
mNA2 = update(mNA, na.action = na.exclude)
o = outcome(simulateResiduals(mNA2))
print(o)
stopifnot(is_outcome(o, "error", "TRUE/FALSE"))

# Order of residuals ----

# residuals are in the order of the data
stopifnot(inDataOrder(models$poisson, dat$observedResponse))

# Pearson residuals and dispersion tests ----

o = dispersionTests(models$poisson)
print(o)
stopifnot(o[c("default", "refit")] == "ok", !startsWith(o["PearsonChisq"], "error"))

# Negative binomial (glmer.nb) ----

# no false dispersion signal for data simulated from the fitted glmer.nb model
# (former lme4/glmer.nb.R, issues #47, #48)
dat$nbResponse = getSimulations(models$negbin, nsim = 1)[, 1]
mNB = glmer.nb(nbResponse ~ Environment1 + (1|group), data = dat)
res = simulateResiduals(mNB)
p = c(testUniformity(res, plot = FALSE)$p.value, testDispersion(res, plot = FALSE)$p.value)
print(p)
stopifnot(p > 0.01)
