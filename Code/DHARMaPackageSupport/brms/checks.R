# DHARMa support for brms: checks behind the statements in README.md
# Run from the repository root: source("Code/DHARMaPackageSupport/brms/checks.R")
# Each section ends with stopifnot() for the statements in the README.
# Every brm() call compiles a Stan model (1-2 min each).

source("Code/DHARMaPackageSupport/supportChecks.R")
library(brms)
options(mc.cores = 2)
runCalibration = FALSE  # TRUE runs the section "Calibration and power compared with glmmTMB" (about 1 h)
set.seed(123)

fit = function(...) brm(..., chains = 2, iter = 1000, refresh = 0, seed = 1)
# every simulateResiduals() call for brms models warns "DHARMa: only simple brms
# models are supported ..."; this warning is not counted as a problem below
okOrSimpleWarning = function(x) x == "ok" | grepl("only simple brms models", x, fixed = TRUE)
simRes = function(m, ...) suppressWarnings(simulateResiduals(m, n = 250, ...))
residualTests = function(so) c(uniformity = testUniformity(so, plot = FALSE)$p.value,
                               dispersion = testDispersion(so, plot = FALSE)$p.value,
                               outliers = testOutliers(so, plot = FALSE)$p.value)

n = 300; ng = 15
d = data.frame(group = factor(rep(1:ng, each = n / ng)), x = runif(n, -1, 1), area = runif(n, 1, 5),
               time = rep(1:(n / ng), ng), size = sample(1:20, n, TRUE), obs = factor(1:n))
re = rnorm(ng)[d$group]
eta = 0.3 + 0.5 * d$x + re
d$yGauss = 1 + d$x + re + rnorm(n, 0, 0.5)
d$yPois = rpois(n, exp(eta))
d$yNB = rnbinom(n, mu = exp(eta), size = 2)
d$yZIP = ifelse(runif(n) < 0.3, 0, rpois(n, exp(eta)))
d$yHurdle = ifelse(runif(n) < 0.3, 0, qpois(runif(n, dpois(0, exp(eta)), 1), exp(eta)))  # zero-truncated Poisson
d$yBinom = rbinom(n, d$size, plogis(eta - 0.3))
d$yBern = rbinom(n, 1, plogis(eta))
d$yGamma = rgamma(n, shape = 3, rate = 3 / exp(eta))
d$yLognormal = rlnorm(n, eta, 0.5)
d$yBeta = rbeta(n, plogis(eta - 0.3) * 10, (1 - plogis(eta - 0.3)) * 10)
d$yStudent = 1 + d$x + re + 0.5 * rt(n, df = 4)

# Model classes and families ----

# data simulated from each family; checkInterface() without refit (see Refit),
# and the residual tests for the correct model
models = list(
  gaussian = fit(yGauss ~ x + (1|group), data = d),
  student = fit(yStudent ~ x + (1|group), family = student(), data = d),
  poisson = fit(yPois ~ x + (1|group), family = poisson(), data = d),
  negbinomial = fit(yNB ~ x + (1|group), family = negbinomial(), data = d),
  zero_inflated_poisson = fit(yZIP ~ x + (1|group), family = zero_inflated_poisson(), data = d),
  zero_inflated_negbinomial = fit(yNB ~ x + (1|group), family = zero_inflated_negbinomial(), data = d),
  hurdle_poisson = fit(yHurdle ~ x + (1|group), family = hurdle_poisson(), data = d),
  binomial = fit(yBinom | trials(size) ~ x + (1|group), family = binomial(), data = d),
  bernoulli = fit(yBern ~ x + (1|group), family = bernoulli(), data = d),
  Gamma = fit(yGamma ~ x + (1|group), family = Gamma(link = "log"), data = d),
  lognormal = fit(yLognormal ~ x + (1|group), family = lognormal(), data = d),
  Beta = fit(yBeta ~ x + (1|group), family = Beta(), data = d)
)
interface = sapply(models, checkInterface, refit = FALSE)
print(t(interface))
tests = sapply(models, function(m) residualTests(simRes(m)))
print(round(t(tests), 3))
stopifnot(okOrSimpleWarning(interface), tests > 0.001)

# Model structures ----

# random slopes, smooth terms, distributional models and ar() terms run
structures = list(
  randomSlope = fit(yGauss ~ x + (x|group), data = d),
  smooth = fit(yGauss ~ s(x), data = d),
  distributional = fit(bf(yGauss ~ x, sigma ~ x), data = d),
  ar = fit(yGauss ~ x + ar(time = time, gr = group) + (1|group), data = d)
)
interface = sapply(structures, checkInterface, refit = FALSE)
print(t(interface))
stopifnot(okOrSimpleWarning(interface))

# Fitted values ----

# getFitted(): median of the posterior of the expected value without random
# effects (re_formula = NA), on the response scale (proportions for k/n binomial)
f = getFitted(models$poisson)
stopifnot(isTRUE(all.equal(f, apply(posterior_epred(models$poisson, re_formula = NA), 2, median))))
stopifnot(getFitted(models$binomial) <= 1)

# Conditional and unconditional simulations ----

# default conditional: posterior_predict() with the fitted REs; unconditional:
# new REs drawn for each group with the sd of the posterior draw (#574), which
# includes the posterior uncertainty of the RE sd, so the simulations vary more
# than unconditional simulations of an ML fit (glmmTMB) of the same model
v = simulationVariance(models$poisson)
vTMB = simulationVariance(glmmTMB::glmmTMB(yPois ~ x + (1|group), family = poisson, data = d))
print(rbind(brms = v, glmmTMB = vTMB))
stopifnot(v["conditional"] < v["unconditional"], v["unconditional"] > vTMB["unconditional"])
# unconditional simulations stop with a message for models whose grouping
# variable is also used elsewhere, e.g. in ar(gr = ) (#574)
stopifnot(is_outcome(outcome(getSimulations(structures$ar, simulateREs = "unconditional")), "error", "Use simulateREs = \"conditional\""))

# Observation-level random effects ----

# conditional simulations of a model with an observation-level RE are fitted to
# the data and give non-uniform residuals for a correct model; use
# simulateREs = "unconditional" for such models (as for the other packages)
d$yOD = rpois(n, exp(eta + rnorm(n, 0, 0.7)))
mOD = fit(yOD ~ x + (1|group) + (1|obs), family = poisson(), data = d)
pCond = residualTests(simRes(mOD))
pUncond = residualTests(simRes(mOD, simulateREs = "unconditional"))
print(rbind(conditional = pCond, unconditional = pUncond))
stopifnot(pCond["uniformity"] < 0.001, pUncond > 0.01)

# Conditioning on selected random effects ----

# simulateREs = "user-specified" passes re_formula to posterior_predict(): no REs
# (NA, REs set to 0, not re-simulated), the group RE only, or all REs (NULL).
# Conditioning on more REs brings the mean simulations closer to the observations.
cors = sapply(list(none = NA, group = ~(1|group), all = NULL), function(f)
  cor(rowMeans(suppressWarnings(getSimulations(mOD, nsim = 200, simulateREs = "user-specified", re_formula = f))), d$yOD))
print(cors)
stopifnot(cors["none"] < cors["group"], cors["group"] < cors["all"])

# Refit ----

# refit = TRUE runs (each refit samples the model again), but testDispersion()
# needs Pearson residuals, which are not available
r = suppressWarnings(simulateResiduals(models$poisson, refit = TRUE, n = 3))
stopifnot(length(r$scaledResiduals) == n, is_outcome(outcome(testDispersion(r, plot = FALSE)), "error", "Pearson residuals are not supported"))

# Prior weights ----

# known problem (#7): posterior_predict() does not use weights(), so the
# simulations of a weighted gaussian model have the wrong variance, and DHARMa
# does not warn. The model implies sd = sigma / sqrt(weight), i.e. a ratio of
# about 1.6 between observations with weight < 2 and > 4; the simulations have the
# same sd for all weights.
d$yWeighted = 1 + d$x + rnorm(n, 0, 1 / sqrt(d$area))
m = fit(yWeighted | weights(area) ~ x, data = d)
w = character()
r = withCallingHandlers(simulateResiduals(m, n = 250), warning = function(x) {w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning")})
s = apply(r$simulatedResponse, 1, sd)
print(c(sdLowWeight = mean(s[d$area < 2]), sdHighWeight = mean(s[d$area > 4])))
stopifnot(mean(s[d$area < 2]) / mean(s[d$area > 4]) < 1.1, !any(grepl("weights", w)))

# Offsets ----

# offset() in the formula is used in the simulations
d$yOffset = rpois(n, exp(eta + log(d$area)))
m = fit(yOffset ~ x + offset(log(area)) + (1|group), family = poisson(), data = d)
stopifnot(residualTests(simRes(m)) > 0.01)

# Missing values ----

# brms removes rows with NA; residuals are for the complete rows
d$xNA = d$x; d$xNA[1:20] = NA
m = fit(yGauss ~ xNA + (1|group), data = d)
stopifnot(length(simRes(m)$scaledResiduals) == n - 20)

# Order of residuals ----

stopifnot(inDataOrder(models$poisson, d$yPois))

# Predictors for plotResiduals(form = ) ----

# known problem (no issue yet): getData() evaluates the name of the data argument
# in the environment of the formula. It fails if the data argument was an
# expression, and finds another object if the model was fitted by a function with
# an argument named data (here utils::data; this also happens for glm());
# plotResiduals(form = ) then stops.
dataSubset = suppressMessages(brm(yGauss ~ x, data = d[d$x > 0, ], empty = TRUE))
stopifnot(is_outcome(outcome(getData(dataSubset)), "error", "not found"))
fitModel = function(formula, data) brm(formula, data = data, empty = TRUE)
stopifnot(is.function(getData(suppressMessages(fitModel(yGauss ~ x, d)))))

# Pearson residuals and dispersion tests ----

# Pearson residuals are not supported, so testDispersion(type = "PearsonChisq") stops;
# the default simulation-based dispersion test works
r = simRes(models$poisson)
stopifnot(is_outcome(outcome(getPearsonResiduals(models$poisson)), "error", "not supported"),
          is_outcome(outcome(testDispersion(r, type = "PearsonChisq", plot = FALSE)), "error", "not supported"),
          outcome(testDispersion(r, plot = FALSE)) == "ok")

# Number of simulations and posterior draws ----

# known problem (#591): n larger than the number of posterior draws stops with a
# brms error, also for the default n = 250 if the model has fewer draws
m = brm(yPois ~ x, family = poisson(), data = d, chains = 1, iter = 200, refresh = 0, seed = 1)
stopifnot(ndraws(m) == 100,
          is_outcome(outcome(simRes(m)), "error", "maximum number of draws"),
          okOrSimpleWarning(outcome(simulateResiduals(m, n = 100))))

# Unsupported models ----

# multivariate and multinomial models are not supported (warning in
# simulateResiduals() and vignette; ordinal models are not mentioned there);
# simulateResiduals() stops, but with messages that do not name the problem
# (no issue yet; general support for multinomial / ordinal responses: #174)
d$yOrd = cut(eta + rlogis(n), c(-Inf, -0.5, 0.5, 1.5, Inf), labels = FALSE)
d$yCat = factor(sample(letters[1:3], n, TRUE))
d$yGauss2 = d$yGauss + rnorm(n)
unsupported = list(
  cumulative = fit(yOrd ~ x, family = cumulative(), data = d),
  categorical = fit(yCat ~ x, family = categorical(), data = d),
  multivariate = fit(bf(mvbind(yGauss, yGauss2) ~ x) + set_rescor(TRUE), data = d)
)
o = sapply(unsupported, function(m) outcome(simRes(m)))
print(o)
stopifnot(is_outcome(o["cumulative"], "error", "argument is not a matrix"),
          is_outcome(o["categorical"], "error", "argument is not a matrix"),
          is_outcome(o["multivariate"], "error", "argument is of length zero"))

# Posterior predictive simulations with createDHARMa() ----

# the alternative route for models DHARMa does not support (former brms.R):
# simulations from posterior_predict() read in with createDHARMa()
preds = t(posterior_predict(models$poisson, ndraws = 250))
r = createDHARMa(simulatedResponse = preds, observedResponse = d$yPois,
                 fittedPredictedResponse = apply(preds, 1, median), integerResponse = TRUE)
stopifnot(residualTests(r) > 0.01)

# Calibration and power compared with glmmTMB ----

# share of p < 0.05 of the uniformity and dispersion tests for correct models
# (calibration) and for overdispersion (power), brms vs. glmmTMB (ML), Poisson GLMM
# with 10 groups, 40 data sets each (vignette "DHARMa for Bayesians")
if (runCalibration) {
  base = brm(observedResponse ~ Environment1 + (1|group), data = createData(100, family = poisson(), numGroups = 10),
             family = poisson(), chains = 4, iter = 1000, refresh = 0, seed = 1)
  study = function(n, od, reps = 40) {
    P = t(sapply(1:reps, function(i) {
      set.seed(10000 * od + 100 * n + i); dd = createData(n, family = poisson(), numGroups = 10, overdispersion = od)
      fb = suppressMessages(update(base, newdata = dd, refresh = 0, seed = i, recompile = FALSE))
      ft = glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = dd, family = poisson)
      suppressWarnings(c(brms_cond = residualTests(simulateResiduals(fb, n = 250))[1:2],
                         brms_uncond = residualTests(simulateResiduals(fb, n = 250, simulateREs = "unconditional"))[1:2],
                         TMB_cond = residualTests(simulateResiduals(ft, n = 250))[1:2],
                         TMB_uncond = residualTests(simulateResiduals(ft, n = 250, simulateREs = "unconditional"))[1:2]))
    }))
    colMeans(P < 0.05)
  }
  calibration = sapply(c(n30 = 30, n100 = 100, n400 = 400), study, od = 0)
  power = sapply(c(n30 = 30, n100 = 100), study, od = 0.5)
  print(round(calibration, 3)); print(round(power, 3))
  # correct models: at most 1 of 40 p-values below 0.05 for conditional simulations,
  # but false positives of the uniformity test for unconditional simulations and n = 400
  stopifnot(calibration[grep("cond.", rownames(calibration), fixed = TRUE)[c(1:2, 5:6)], ] <= 0.025,
            calibration["brms_uncond.uniformity", "n400"] > 0.1)
  # power of the dispersion test is lower for brms than for glmmTMB
  stopifnot(power["brms_cond.dispersion", "n100"] < power["TMB_cond.dispersion", "n100"])
}
