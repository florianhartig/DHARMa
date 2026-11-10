# DHARMa support for mgcv: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/mgcv/checks.R")
# Each section ends with stopifnot() for the statements in the README.

source("Code/DHARMaPackageSupport/supportChecks.R")
library(mgcv)
suppressMessages(loadNamespace("mgcViz")) # avoid its startup message inside the checks
set.seed(123)

dat = createData(sampleSize = 200, randomEffectVariance = 1, family = poisson())
datZI = createData(sampleSize = 200, randomEffectVariance = 0, pZeroInflation = 0.3, family = poisson())
datBin = createData(sampleSize = 200, randomEffectVariance = 1, family = binomial(), binomialTrials = 10)
datBin$prop = datBin$observedResponse1 / 10
datGauss = createData(sampleSize = 200, randomEffectVariance = 1, family = gaussian())
dat$pos = datBin$prop * 0.98 + 0.01
dat$cont = dat$observedResponse + rexp(200)

# Standard families ----

models = list(
  gaussian = gam(observedResponse ~ s(Environment1), data = datGauss),
  poisson = gam(observedResponse ~ s(Environment1) + s(group, bs = "re"), family = poisson, data = dat),
  bernoulli = gam(as.numeric(observedResponse1 > 5) ~ s(Environment1), family = binomial, data = datBin),
  bernoulliFactor = gam(factor(observedResponse1 > 5) ~ s(Environment1), family = binomial, data = datBin),
  binomialCbind = gam(cbind(observedResponse1, observedResponse0) ~ s(Environment1), family = binomial, data = datBin),
  binomialWeights = gam(prop ~ s(Environment1), family = binomial, weights = rep(10, 200), data = datBin),
  Gamma = gam(cont ~ s(Environment1), family = Gamma(link = "log"), data = dat),
  bam = bam(observedResponse ~ s(Environment1) + s(group, bs = "re"), family = poisson, data = dat)
)
interface = sapply(models, checkInterface)
print(t(interface))
stopifnot(sapply(models, function(m) class(m)[1]) %in% DHARMa:::getPossibleModels())
stopifnot(interfaceOk(interface))

# Extended families ----

# simulations for extended families need mgcViz (simulate.gam)
ext = list(
  nb = gam(observedResponse ~ s(Environment1), family = nb, data = dat),
  ziP = gam(observedResponse ~ s(Environment1), family = ziP, data = datZI),
  tw = gam(cont ~ s(Environment1), family = tw, data = dat),
  betar = gam(pos ~ s(Environment1), family = betar, data = dat),
  scat = gam(observedResponse ~ s(Environment1), family = scat, data = datGauss)
)
interface = sapply(ext, checkInterface)
print(t(interface))
stopifnot(requireNamespace("mgcViz", quietly = TRUE), interfaceOk(interface))
# without mgcViz: stats::simulate fails for extended families
o = sapply(ext, function(m) outcome(getSimulations(m, nsim = 2, mgcViz = FALSE)))
print(o)
stopifnot(startsWith(o, "error"))
# (standard families work without mgcViz: #567, testCompatibility.R)

# Models with several linear predictors ----

lss = list(
  gaulss = gam(list(observedResponse ~ s(Environment1), ~ s(Environment1)), family = gaulss, data = datGauss),
  ziplss = gam(list(observedResponse ~ s(Environment1), ~ 1), family = ziplss, data = datZI)
)
interface = sapply(lss, checkInterface)
print(t(interface))
# simulations and residuals work
stopifnot(interface[c("observedResponse", "simulations", "simulationsRefit", "simulateResiduals"), ] == "ok")
# known problems: for gaulss, getFitted() returns the predictions of both linear
# predictors (2n values), so tests and plots that use the fitted values fail;
# refit fails for both families
stopifnot(sapply(lss, function(m) length(getFitted(m))) == c(400, 200),
          is_outcome(outcome(testDispersion(simulateResiduals(lss$gaulss), plot = FALSE)), "error"),
          is_outcome(outcome(plotResiduals(simulateResiduals(lss$gaulss))), "error"),
          startsWith(interface["simulateResidualsRefit", ], "error"))
# multinom: simulateResiduals() stops with an unclear error (#418); ocat: runs
# (correctness of the simulations not checked)
dat$cat = cut(dat$observedResponse, c(-1, 0, 2, Inf), labels = FALSE) - 1
o = c(multinom = outcome(simulateResiduals(gam(list(cat ~ s(Environment1), ~ s(Environment1)), family = multinom(K = 2), data = dat))),
      ocat = outcome(simulateResiduals(gam(cat + 1 ~ s(Environment1), family = ocat(R = 3), data = dat))))
print(o)
stopifnot(is_outcome(o["multinom"], "error", "deviance"), o["ocat"] == "ok")

# Conditional and unconditional simulations ----

# (getFitted() excludes random effect smooths, #544: testCompatibility.R)
m = models$poisson
# only conditional simulations; "unconditional" gives a warning and is conditional
o = outcome(getSimulations(m, nsim = 2, simulateREs = "unconditional"))
print(o)
stopifnot(is_outcome(o, "warning", "not available for gam"))
v = simulationVariance(m)
print(v)
stopifnot(abs(diff(v)) < 0.1 * v[1])

# Refit ----

stopifnot(sapply(models[c("gaussian", "poisson", "binomialCbind")], refitReproduces, fixef = coef, tolerance = 1e-3))

# Prior weights ----

w = runif(200, 1, 2)
stopifnot(is_outcome(outcome(simulateResiduals(gam(observedResponse ~ s(Environment1), weights = w, family = poisson, data = dat))), "warning", "Model was fit with prior weights"))

# Offsets ----

# gam: offset() in the formula and the offset argument work (same fitted values for
# gam, DHARMa stops for bam with offset = : #572, testCompatibility.R)
dat$area = runif(200, 1, 3)
mOff1 = gam(observedResponse ~ s(Environment1) + offset(log(area)), family = poisson, data = dat)
mOff2 = gam(observedResponse ~ s(Environment1), offset = log(area), family = poisson, data = dat)
stopifnot(interfaceOk(checkInterface(mOff1)), interfaceOk(checkInterface(mOff2)))
# gaulss with offset argument: ignored by mgcv, DHARMa does not notice (#600)
mOffLss = gam(list(observedResponse ~ s(Environment1), ~ 1), offset = rep(5, 200), family = gaulss, data = datGauss)
mLss = gam(list(observedResponse ~ s(Environment1), ~ 1), family = gaulss, data = datGauss)
stopifnot(all.equal(getFitted(mOffLss), getFitted(mLss)))

# Missing values ----

datNA = dat
datNA$Environment1[1:10] = NA
mNA = gam(observedResponse ~ s(Environment1), family = poisson, data = datNA)
stopifnot(interfaceOk(checkInterface(mNA)), length(simulateResiduals(mNA)$scaledResiduals) == 190)

# Order of residuals ----

stopifnot(inDataOrder(models$poisson, dat$observedResponse))

# Pearson residuals and dispersion tests ----

# DHARMa uses residuals(type = "scaled.pearson")
o = dispersionTests(models$poisson)
print(o)
stopifnot(o[c("default", "refit")] == "ok", !startsWith(o["PearsonChisq"], "error"))
