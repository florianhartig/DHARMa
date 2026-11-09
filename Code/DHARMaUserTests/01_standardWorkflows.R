# User-perspective test 1: standard workflows of a first-time user (vignette),
# correctly specified models (expect "ok") and deliberate misspecifications (expect "detect")
source("Code/DHARMaUserTests/helper.R")
library(lme4); library(glmmTMB); library(mgcv)
set.seed(1)
F1 = c(~Environment1, ~group, ~Environment1 | group, ~Environment1 | group %in% c("1", "2"), ~.)

# lm, glm Poisson, Bernoulli
d = createData(sampleSize = 200, family = gaussian(), numGroups = 5, randomEffectVariance = 0)
battery("lm", simulateResiduals(lm(observedResponse ~ Environment1, data = d)), fac = d$group, form = F1)
d = createData(sampleSize = 200, family = poisson(), numGroups = 5, randomEffectVariance = 0)
battery("pois", simulateResiduals(glm(observedResponse ~ Environment1, family = poisson, data = d)), fac = d$group, form = F1)
d = createData(sampleSize = 300, family = binomial(), numGroups = 5, randomEffectVariance = 0)
battery("bin01", simulateResiduals(glm(observedResponse ~ Environment1, family = binomial, data = d)), fac = d$group)

# binomial k/n via cbind() and via proportion + weights: residuals should be identical
d = createData(sampleSize = 200, family = binomial(), binomialTrials = 10, numGroups = 5, randomEffectVariance = 0)
d$prop = d$observedResponse1 / 10
so1 = simulateResiduals(glm(cbind(observedResponse1, observedResponse0) ~ Environment1, family = binomial, data = d))
so2 = simulateResiduals(glm(prop ~ Environment1, family = binomial, weights = rep(10, nrow(d)), data = d))
battery("binKN", so1, fac = d$group)
chk("binKN cbind and proportion + weights give the same residuals", stopifnot(isTRUE(all.equal(residuals(so1), residuals(so2)))))
chk("binKN cbind() with prior weights", simulateResiduals(glm(cbind(observedResponse1, observedResponse0) ~ Environment1, family = binomial, weights = rep(2, nrow(d)), data = d)), expect = "error")

# mixed models and gam
d = createData(sampleSize = 250, family = poisson())
battery("glmer", simulateResiduals(glmer(observedResponse ~ Environment1 + (1|group), family = poisson, data = d)), fac = d$group, form = F1[1:3])
battery("glmmTMB", simulateResiduals(glmmTMB(observedResponse ~ Environment1 + (1|group), family = poisson, data = d)), fac = d$group, form = F1[1:3])
dg = data.frame(x = runif(300, -2, 2)); dg$y = rpois(300, exp(1 + sin(2 * dg$x)))
battery("gam", simulateResiduals(gam(y ~ s(x), family = poisson, data = dg)), form = c(~x))

# deliberate misspecifications: the respective test should detect them (p < 0.05)
d = createData(sampleSize = 200, family = poisson(), overdispersion = 1, numGroups = 5, randomEffectVariance = 0)
chk("overdispersion, Poisson glm", testDispersion(simulateResiduals(glm(observedResponse ~ Environment1, family = poisson, data = d)), plot = FALSE), expect = "detect")
d = createData(sampleSize = 300, intercept = 2, family = poisson(), pZeroInflation = 0.3)
chk("zero-inflation, Poisson glmmTMB", testZeroInflation(simulateResiduals(glmmTMB(observedResponse ~ Environment1 + (1|group), family = poisson, data = d)), plot = FALSE), expect = "detect")
d = createData(sampleSize = 500, family = poisson(), quadraticFixedEffects = -1, numGroups = 5, randomEffectVariance = 0)
chk("missing quadratic term", testQuantiles(simulateResiduals(glm(observedResponse ~ Environment1, family = poisson, data = d)), predictor = d$Environment1, plot = FALSE), expect = "detect")
chk("nonlinear effect fitted as linear", testQuantiles(simulateResiduals(glm(y ~ x, family = poisson, data = dg)), predictor = dg$x, plot = FALSE), expect = "detect")
d = createData(sampleSize = 250, family = binomial(), binomialTrials = 20, overdispersion = 1)
chk("k/n overdispersion, binomial glmer", testDispersion(simulateResiduals(glmer(cbind(observedResponse1, observedResponse0) ~ Environment1 + (1|group), family = binomial, data = d)), plot = FALSE), expect = "detect")
dh = data.frame(x = runif(300)); dh$y = 1 + dh$x + rnorm(300, sd = exp(2 * dh$x))
chk("heteroscedasticity, lm", testQuantiles(simulateResiduals(lm(y ~ x, data = dh)), predictor = dh$x, plot = FALSE), expect = "detect")

saveLog("01_standardWorkflows")
