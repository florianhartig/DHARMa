# #598: refit = TRUE, quantile method PIT vs traditional, and jitter scale for k/n binomial models
# Simulations (glm only) behind #598; part 1 also shows why refit = TRUE uses "traditional" (#272)
# Runtime with the settings below: part 1 ~10 min, part 2 ~5 min
# Results below were obtained with the development version 0.5.1 (master, Nov 2026)

library(DHARMa)

N = 100      # observations per dataset
nSim = 100   # simulations / refits per dataset
reps = 300   # datasets per scenario

ksRej = function(u) ks.test(u, "punif")$p.value < 0.05
dec = function(u) round(table(cut(u, seq(0, 1, 0.1), include.lowest = TRUE)) / length(u), 3)


######## Part 1: calibration (H0) and power of PIT vs traditional for refit = TRUE ########

makeData = function(scen){
  x = runif(N, -1, 1)
  d = data.frame(x = x)
  switch(scen,
    pois     = {d$y = rpois(N, exp(1 + x));            glm(y ~ x, poisson, d)},
    poisLow  = {d$y = rpois(N, exp(-1.5 + x));         glm(y ~ x, poisson, d)},
    bern     = {d$y = rbinom(N, 1, plogis(x));         glm(y ~ x, binomial, d)},
    binom10  = {d$k = rbinom(N, 10, plogis(x)); d$nk = 10 - d$k
                glm(cbind(k, nk) ~ x, binomial, d)},
    gauss    = {d$y = rnorm(N, x);                     glm(y ~ x, gaussian, d)},
    # misspecified: overdispersed counts fitted as Poisson
    nbAsPois = {d$y = MASS::rnegbin(N, exp(1 + x), theta = 2); glm(y ~ x, poisson, d)},
    # misspecified: missing quadratic term
    quadPois = {d$y = rpois(N, exp(1 + x + 1.5 * x^2)); glm(y ~ x, poisson, d)}
  )
}

set.seed(1)
for(scen in c("pois", "poisLow", "bern", "binom10", "gauss", "nbAsPois", "quadPois")){
  rej = matrix(NA, reps, 4, dimnames = list(NULL, c("noRefit_PIT", "refit_PIT", "refit_trad",
                                                     "refit_tradCountScale")))
  pooled = list(PIT = NULL, trad = NULL, tradCS = NULL, noRefit = NULL)
  for(r in 1:reps){
    fit = makeData(scen)
    s0 = simulateResiduals(fit, n = nSim, seed = FALSE)
    s1 = suppressWarnings(simulateResiduals(fit, n = nSim, refit = TRUE, seed = FALSE))
    w = if(scen == "binom10") 10 else 1   # lattice spacing of response-scale residuals is 1/w
    q = function(m, scale = 1) suppressMessages(getQuantile(s1$refittedResiduals * scale,
          s1$fittedResiduals * scale, integerResponse = s1$integerResponse, method = m))
    uP = q("PIT"); uT = q("traditional"); uC = q("traditional", w)
    rej[r, ] = sapply(list(s0$scaledResiduals, uP, uT, uC), ksRej)
    pooled$PIT = c(pooled$PIT, uP); pooled$trad = c(pooled$trad, uT)
    pooled$tradCS = c(pooled$tradCS, uC); pooled$noRefit = c(pooled$noRefit, s0$scaledResiduals)
  }
  cat("\n=====", scen, ": KS rejection rate\n"); print(round(colMeans(rej), 3))
  print(t(sapply(pooled, dec)))
}

# Results (KS rejection rate; noRefit_PIT / refit_PIT / refit_trad / refit_tradCountScale):
# pois      0.000 0.003 0.003 0.003
# poisLow   0.040 1.000 0.023 0.007
# bern      0.000 0.820 0.003 0.000
# binom10   0.000 0.000 0.030 0.003
# gauss     0.003 0.000 0.000 0.000
# nbAsPois  0.737 0.903 0.750 0.807  (refit_trad and refit_tradCountScale are the same method here, difference = MC noise)
# quadPois  0.507 0.560 0.507 0.503
# Pooled PIT residuals with refit: bern peaks at deciles 0.2-0.3 and 0.7-0.8 (0.232, 0.230), poisLow at 0.3-0.4 (0.381).
# Reason: refitted residuals y* - mu* are not exactly tied, so PIT does not randomize; within a block of equal y,
# the order is set by mu* vs mu, and the observed mu is the centre of the bootstrap distribution -> mid-p values.


######## Part 2a: power for k/n binomial, jitter on proportion vs count scale ########

set.seed(2)
for(scen in c("binom10_H0", "binom10_overdisp", "binom10_quad")){
  rej = matrix(NA, reps, 3, dimnames = list(NULL, c("noRefit_PIT", "refit_trad_propScale", "refit_trad_countScale")))
  for(r in 1:reps){
    x = runif(N, -1, 1)
    p = switch(scen, binom10_H0 = plogis(x),
               binom10_overdisp = rbeta(N, 2 * plogis(x) / 0.5, 2 * (1 - plogis(x)) / 0.5), # beta-binomial
               binom10_quad = plogis(x + 1.5 * x^2 - 0.5))
    d = data.frame(x = x, k = rbinom(N, 10, p)); d$nk = 10 - d$k
    fit = glm(cbind(k, nk) ~ x, binomial, d)
    s0 = simulateResiduals(fit, n = nSim, seed = FALSE)
    s1 = suppressWarnings(simulateResiduals(fit, n = nSim, refit = TRUE, seed = FALSE))
    uC = suppressMessages(getQuantile(s1$refittedResiduals * 10, s1$fittedResiduals * 10,
                                      integerResponse = TRUE, method = "traditional"))
    rej[r, ] = c(ksRej(s0$scaledResiduals), ksRej(s1$scaledResiduals), ksRej(uC))
  }
  cat("\n=====", scen, ": KS rejection rate\n"); print(round(colMeans(rej), 3))
}

# Results (noRefit_PIT / refit_trad_propScale (current) / refit_trad_countScale):
# binom10_H0        0.000 0.037 0.000
# binom10_overdisp  0.977 0.063 0.963
# binom10_quad      0.023 0.037 0.007  (too weak to say anything)


######## Part 2b: grouped refit residuals (recalculateResiduals), PIT vs traditional (#589) ########
## run after part 2a in the same session (results below were obtained this way)

for(scen in c("bern", "poisLow")){
  rej = matrix(NA, reps, 3, dimnames = list(NULL, c("noRefit_grouped", "refit_grouped_PIT", "refit_grouped_trad")))
  pooled = list(PIT = NULL, trad = NULL)
  for(r in 1:reps){
    x = runif(N, -1, 1); g = rep(1:20, each = N / 20)
    y = if(scen == "bern") rbinom(N, 1, plogis(x)) else rpois(N, exp(-1.5 + x))
    fit = glm(y ~ x, if(scen == "bern") binomial else poisson, data.frame(x, y))
    s0 = simulateResiduals(fit, n = nSim, seed = FALSE)
    s1 = suppressWarnings(simulateResiduals(fit, n = nSim, refit = TRUE, seed = FALSE))
    gP = suppressMessages(recalculateResiduals(s1, group = g, seed = FALSE, method = "PIT"))$scaledResiduals
    gT = suppressMessages(recalculateResiduals(s1, group = g, seed = FALSE, method = "traditional"))$scaledResiduals
    g0 = recalculateResiduals(s0, group = g, seed = FALSE)$scaledResiduals
    rej[r, ] = c(ksRej(g0), ksRej(gP), ksRej(gT))
    pooled$PIT = c(pooled$PIT, gP); pooled$trad = c(pooled$trad, gT)
  }
  cat("\n=====", scen, "grouped (20 groups of 5): KS rejection rate\n"); print(round(colMeans(rej), 3))
  print(t(sapply(pooled, dec)))
}

# Results (noRefit_grouped / refit_grouped_PIT / refit_grouped_trad), obtained on master (before PR #584):
# bern     0.00 0.00 0.00
# poisLow  0.00 0.01 0.00
# Pooled poisLow PIT residuals are not uniform (0.157 in decile 0.1-0.2, 0.068 in 0.2-0.3), traditional ~0.10 everywhere.
# With PR #584, recalculateResiduals() forces "traditional" for refit = TRUE, so method = "PIT" has no effect there.
