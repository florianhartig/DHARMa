# User-perspective test 4: advanced use - refit = TRUE (incl. k/n binomial calibration),
# offsets, simulateREs options, createDHARMa(), models fitted inside a function,
# data order of GLMMadaptive. No brms (compiles Stan).
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
chk("createDHARMa, integer response", testUniformity(createDHARMa(sims, d$y2, integerResponse = TRUE), plot = FALSE))

# model fitted inside a function with local data: refit and formula predictors should work
f = function(){
  dl = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  ml = glm(observedResponse ~ Environment1, family = poisson, data = dl)
  sl = simulateResiduals(ml, refit = TRUE, n = 20)
  plotResiduals(sl, form = ~Environment1)
}
chk("model fitted inside a function", f())

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
