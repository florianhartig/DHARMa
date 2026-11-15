# User-perspective test 4: advanced use - refit = TRUE (incl. k/n binomial calibration),
# offsets, simulateREs options, createDHARMa(), runBenchmarks(), models fitted inside a function,
# data changed after fitting, further model types and families, data order of GLMMadaptive.
# No brms (compiles Stan).
source("Code/DHARMaUserTests/helper.R")
library(lme4); library(glmmTMB)
set.seed(5)

# k/n binomial with refit = TRUE: residuals of a correct model should be uniform,
# overdispersion should be detected (cf. #598)
n = 200; size = sample(1:25, n, TRUE); x = runif(n)
k = rbinom(n, size, plogis(-0.5 + x))
kOD = rbinom(n, size, plogis(-0.5 + x + rnorm(n)))
dk = data.frame(k, kOD, size, x, g = factor(sample(1:10, n, TRUE)))
for(cl in c("glm", "glmer", "glmmTMB")){
  fit = function(resp) switch(cl,
    glm = glm(as.formula(paste0("cbind(", resp, ", size - ", resp, ") ~ x")), family = binomial, data = dk),
    glmer = glmer(as.formula(paste0("cbind(", resp, ", size - ", resp, ") ~ x + (1|g)")), family = binomial, data = dk),
    glmmTMB = glmmTMB(as.formula(paste0("cbind(", resp, ", size - ", resp, ") ~ x + (1|g)")), family = binomial, data = dk))
  chk(paste(cl, "k/n refit, correct model, uniformity"), testUniformity(simulateResiduals(fit("k"), refit = TRUE, n = 50), plot = FALSE))
  chk(paste(cl, "k/n refit, overdispersion"), testDispersion(simulateResiduals(fit("kOD"), refit = TRUE, n = 50), plot = FALSE), expect = "detect")
}

# offsets in the formula and as argument should give the same residuals; a missing offset should be detected
d = createData(sampleSize = 200, family = poisson(), numGroups = 10)
d$area = runif(200, 1, 5); d$y2 = rpois(200, exp(log(d$area) + 0.5 * d$Environment1))
so1 = simulateResiduals(glm(y2 ~ Environment1 + offset(log(area)), family = poisson, data = d), seed = 1)
so2 = simulateResiduals(glm(y2 ~ Environment1, offset = log(area), family = poisson, data = d), seed = 1)
chk("offset in formula and as argument give the same residuals", stopifnot(isTRUE(all.equal(residuals(so1), residuals(so2)))))
chk("missing offset", testDispersion(simulateResiduals(glm(y2 ~ Environment1, family = poisson, data = d)), plot = FALSE), expect = "detect")

# simulateREs options and refit for mixed models
m = glmer(observedResponse ~ Environment1 + (1|group), family = poisson, data = d)
for(s in c("conditional", "unconditional")) chk(paste("glmer simulateREs =", s), testUniformity(simulateResiduals(m, simulateREs = s), plot = FALSE))
chk("glmer user-specified, re.form = NA", testUniformity(simulateResiduals(m, simulateREs = "user-specified", re.form = NA), plot = FALSE))
sor = chk("glmer refit = TRUE", simulateResiduals(m, refit = TRUE, n = 50))
chk("glmer refit = TRUE, grouped dispersion", testDispersion(recalculateResiduals(sor, group = d$group), plot = FALSE))

# createDHARMa() with own simulations
sims = replicate(250, rpois(200, exp(log(d$area) + 0.5 * d$Environment1)))
sob = createDHARMa(sims, d$y2, integerResponse = TRUE)
chk("createDHARMa, integer response", testUniformity(sob, plot = FALSE))
chk("createDHARMa, plotResiduals with vector predictor", plotResiduals(sob, form = d$Environment1))
chk("createDHARMa, grouped", testDispersion(recalculateResiduals(sob, group = d$group), plot = FALSE))
chk("createDHARMa, transposed simulations", createDHARMa(t(sims), d$y2, integerResponse = TRUE), expect = "error")
# known problem #628: ?runBenchmarks allows a function that returns one value, but this fails
chk("runBenchmarks, one statistic", runBenchmarks(function() testDispersion(sob, plot = FALSE)$p.value, nRep = 3))

# model fitted inside a function with local data: refit and formula predictors should work
f = function(){
  dl = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  ml = glm(observedResponse ~ Environment1, family = poisson, data = dl)
  sl = simulateResiduals(ml, refit = TRUE, n = 20)
  plotResiduals(sl, form = ~Environment1)
}
chk("model fitted inside a function", f())

# data changed after fitting (#611): refits stop if used rows or values changed, other changes are harmless
dr = d; mr = glm(y2 ~ Environment1 + offset(log(area)), family = poisson, data = dr)
dr = d[order(d$Environment1), ]; dr$newColumn = 1
chk("rows sorted and column added after fit, refit", simulateResiduals(mr, refit = TRUE, n = 10))
dr$Environment1[1] = 99
chk("value changed after fit, refit", simulateResiduals(mr, refit = TRUE, n = 10), expect = "error")
ms = glm(y2 ~ scale(Environment1), family = poisson, data = d, subset = Environment1 > 0)
chk("scale() with subset, refit (no false stop)", simulateResiduals(ms, refit = TRUE, n = 10))

# refit and prior weights fixes (#626)
if(requireNamespace("lmerTest", quietly = TRUE)) chk("lmerTest refit", testUniformity(simulateResiduals(lmerTest::lmer(Environment1 ~ area + (1|group), data = d), refit = TRUE, n = 20), plot = FALSE))
if(requireNamespace("gamm4", quietly = TRUE)) chk("gamm4 $mer without prior-weights warning", testUniformity(simulateResiduals(gamm4::gamm4(y2 ~ s(Environment1), random = ~(1|group), family = poisson, data = d)$mer), plot = FALSE))
if(requireNamespace("spaMM", quietly = TRUE)) chk("spaMM log(y + 1) response, refit", testUniformity(simulateResiduals(spaMM::fitme(log(y2 + 1) ~ Environment1, data = d), refit = TRUE, n = 20), plot = FALSE))

# further glmmTMB families for zero-inflated and overdispersed data: zero-inflated, hurdle and betabinomial models
# should pass, the Poisson and binomial models should be detected
dz = data.frame(x = runif(300, -1, 1), g = factor(sample(1:15, 300, TRUE)), size = sample(5:20, 300, TRUE))
mu = exp(0.5 + dz$x + rnorm(15, 0, 0.5)[dz$g])
dz$y = ifelse(runif(300) < 0.3, 0, rnbinom(300, mu = mu, size = 2))
dz$k = rbinom(300, dz$size, rbeta(300, 4 * plogis(dz$x), 4 * (1 - plogis(dz$x))))
chk("glmmTMB zero-inflated nbinom2", testZeroInflation(simulateResiduals(glmmTMB(y ~ x + (1|g), ziformula = ~1, family = nbinom2, data = dz)), plot = FALSE))
chk("glmmTMB hurdle (truncated_nbinom2)", testUniformity(simulateResiduals(glmmTMB(y ~ x + (1|g), ziformula = ~1, family = truncated_nbinom2, data = dz)), plot = FALSE))
chk("glmmTMB Poisson for zero-inflated data", testZeroInflation(simulateResiduals(glmmTMB(y ~ x + (1|g), family = poisson, data = dz)), plot = FALSE), expect = "detect")
chk("glmmTMB betabinomial", testDispersion(simulateResiduals(glmmTMB(cbind(k, size - k) ~ x, family = betabinomial, data = dz)), plot = FALSE))
chk("glmmTMB binomial for betabinomial data", testDispersion(simulateResiduals(glmmTMB(cbind(k, size - k) ~ x, family = binomial, data = dz)), plot = FALSE), expect = "detect")

# GLMMadaptive sorts the data by group: predictors must be in that order (see vignette)
if(requireNamespace("GLMMadaptive", quietly = TRUE)){
  g = factor(sample(letters[1:10], 300, TRUE)); x = runif(300)
  dga = data.frame(y = rpois(300, exp(0.5 + 2 * x^2 + rnorm(10)[g])), x, g)
  fitga = function(dat) GLMMadaptive::mixed_model(y ~ x, random = ~ 1 | g, data = dat, family = poisson())
  # pitfall: misaligned predictor, so the missing quadratic term is not detected
  chk("GLMMadaptive, data not sorted by group", testQuantiles(simulateResiduals(fitga(dga)), predictor = dga$x, plot = FALSE))
  dgs = dga[order(dga$g), ]
  chk("GLMMadaptive, data sorted by group", testQuantiles(simulateResiduals(fitga(dgs)), predictor = dgs$x, plot = FALSE), expect = "detect")
} else cat("GLMMadaptive not installed, skipped\n")

saveLog("04_advanced")
