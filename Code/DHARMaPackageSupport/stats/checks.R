# DHARMa support for lm, glm (stats) and glm.nb (MASS): checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/stats/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
set.seed(123)

dat = createData(sampleSize = 200, randomEffectVariance = 0, family = poisson())
datBin = createData(sampleSize = 200, randomEffectVariance = 0, family = binomial(), binomialTrials = 10)
datBin$prop = datBin$observedResponse1 / 10
datGauss = createData(sampleSize = 200, randomEffectVariance = 0, family = gaussian())

# Model classes and families ----

models = list(
  lm = lm(observedResponse ~ Environment1, data = datGauss),
  gaussian = glm(observedResponse ~ Environment1, data = datGauss),
  poisson = glm(observedResponse ~ Environment1, family = poisson, data = dat),
  bernoulli = glm(as.numeric(observedResponse1 > 5) ~ Environment1, family = binomial, data = datBin),
  bernoulliFactor = glm(factor(observedResponse1 > 5) ~ Environment1, family = binomial, data = datBin),
  binomialCbind = glm(cbind(observedResponse1, observedResponse0) ~ Environment1, family = binomial, data = datBin),
  binomialWeights = glm(prop ~ Environment1, family = binomial, weights = rep(10, 200), data = datBin),
  Gamma = glm(observedResponse + 1 ~ Environment1, family = Gamma(link = "log"), data = dat),
  negbin = MASS::glm.nb(observedResponse ~ Environment1, data = dat)
)
interface = sapply(models, checkInterface)
print(t(interface))
stopifnot(sapply(models, function(m) class(m)[1]) %in% DHARMa:::getPossibleModels())
stopifnot(interfaceOk(interface))

# Logical response ----

# a logical response (e.g. y > 5) works, but getObservedResponse() returns a logical vector
m = glm(observedResponse1 > 5 ~ Environment1, family = binomial, data = datBin)
stopifnot(is.logical(getObservedResponse(m)), checkInterface(m)[-1] == "ok")

# inverse.gaussian ----

# stats::simulate needs the package SuppDists for this family
m = glm(observedResponse + 1 ~ Environment1, family = inverse.gaussian(link = "log"), data = dat)
o = outcome(simulateResiduals(m))
print(o)
stopifnot(if (requireNamespace("SuppDists", quietly = TRUE)) o == "ok" else is_outcome(o, "error", "SuppDists"))

# Quasi families ----

# quasi families have no likelihood to simulate from: simulateResiduals() stops
o = sapply(c("quasipoisson", "quasibinomial"), function(f)
  outcome(simulateResiduals(glm(observedResponse1 / 10 ~ Environment1, family = f, data = datBin))))
print(o)
stopifnot(startsWith(o, "error"))

# Conditional and unconditional simulations ----

# without random effects, simulateREs has no effect
set.seed(1); s1 = simulateResiduals(models$poisson, n = 50, simulateREs = "conditional")
set.seed(1); s2 = simulateResiduals(models$poisson, n = 50, simulateREs = "unconditional")
stopifnot(identical(s1$simulatedResponse, s2$simulatedResponse))

# Refit ----

# refitting to the observed response reproduces the coefficients (transformed
# responses, models without data and with subset: testCompatibility.R, #571, #599)
stopifnot(sapply(models[c("lm", "poisson", "binomialCbind", "negbin")], refitReproduces))

# Prior weights ----

# lm and gaussian glm: simulate() uses the weights, no warning
w = rep(c(1, 100), 100)
mw = lm(observedResponse ~ Environment1, weights = w, data = datGauss)
stopifnot(outcome(simulateResiduals(mw)) == "ok")
v = tapply(apply(getSimulations(mw, nsim = 500), 1, var), w, mean)
print(v)
stopifnot(v["1"] > 10 * v["100"])
# poisson glm: stats::simulate warns that it ignores the weights
o = outcome(simulateResiduals(glm(observedResponse ~ Environment1, weights = w, family = poisson, data = dat)))
print(o)
stopifnot(is_outcome(o, "warning", "ignoring prior weights"))
# glm.nb: DHARMa warns
stopifnot(is_outcome(outcome(simulateResiduals(MASS::glm.nb(observedResponse ~ Environment1, weights = w, data = dat))), "warning", "Model was fit with prior weights"))

# Offsets ----

# offset() in the formula and the offset argument give the same fitted values; refit works with both
dat$area = runif(200, 1, 3)
mOff1 = glm(observedResponse ~ Environment1 + offset(log(area)), family = poisson, data = dat)
mOff2 = glm(observedResponse ~ Environment1, offset = log(area), family = poisson, data = dat)
stopifnot(all.equal(getFitted(mOff1), getFitted(mOff2)))
stopifnot(interfaceOk(checkInterface(mOff1)), interfaceOk(checkInterface(mOff2)))
stopifnot(refitReproduces(mOff1), refitReproduces(mOff2))

# Missing values ----

# with na.omit (default), residuals are calculated for the complete rows only; refit works
datNA = dat
datNA$Environment1[1:10] = NA
mNA = glm(observedResponse ~ Environment1, family = poisson, data = datNA)
stopifnot(interfaceOk(checkInterface(mNA)), length(simulateResiduals(mNA)$scaledResiduals) == 190)
# with na.exclude, the simulations contain NA values and DHARMa stops (#88, #134)
mNA2 = update(mNA, na.action = na.exclude)
o = outcome(simulateResiduals(mNA2))
print(o)
stopifnot(is_outcome(o, "error", "produce NA values"))

# Order of residuals ----

stopifnot(inDataOrder(models$poisson, dat$observedResponse))

# Pearson residuals and dispersion tests ----

o = dispersionTests(models$poisson)
print(o)
stopifnot(o == "ok")
