# ISSUE 185: calibration of testQuantiles() and design of the quantile test in PR #654
#
# Simulations behind the design decisions described in the comments on #185 of
# November 2026. The sections are independent:
#
#   0. degree of the polynomial basis: fixed vs. adaptive
#   1. power of the qgam p-values vs. the score test, size-adjusted
#   2. degree 0 of the score test, conditional and unconditional GLMM residuals
#   3. mechanism: conditional GLMM residuals are concentrated around 0.5
#   4. score test details: curve colours under H0, nRep, residuals on the grid k/nSim
#   5. test versions (degrees 0-10, degrees >= 1, permutation null) and colour rules
#
# Sections 1, 2 and 5 save their results as 185_*.rds in the working directory
# (not committed); the other sections only print. Sections 0, 1, 2 and 5 set a
# seed per scenario or replicate, so their results do not depend on the number
# of cores; sections 3 and 4 set seeds sequentially.
#
# Versions (set the library paths below; NULL = default library):
#   section 0:        DHARMa development version at commit 9ac0daa (libDev)
#   section 1:        DHARMa 0.5.1, qgam p-values in testQuantiles() (libOld)
#   sections 2, 3, 5: DHARMa with the residuals of PR #658 (libNew)
# The score test functions are copies of quantileScoreTest() from the first
# version of PR #654 (commit 456daee), extended by the variants compared, so
# they do not need #654. Run times on 2-8 cores: about 15 minutes to 2.5 hours
# per section. Run in November 2026 with R 4.6.2, lme4 2.1.0 and qgam 2.0.0
# (Windows 11).

library(parallel)
libDev = NULL   # DHARMa at 9ac0daa
libOld = NULL   # DHARMa 0.5.1
libNew = NULL   # DHARMa with PR #658
nCores = 4

# ---- Score test variants ----------------------------------------------------

# joint (PR #654, degrees 0..10), degree 0 only, degrees >= 1 only, and the
# per-quantile p-values of the joint version; null from uniform residuals
scoreVariants <- function(res, pred, quantiles = c(.25, .5, .75), maxDegree = 10, nRep = 1000, seed = 123){
  n = length(res); m = length(quantiles)
  degree = max(0, min(maxDegree, length(unique(pred)) - 1, n - 2))
  while(degree > 0 && inherits(try(stats::poly(pred, degree), silent = TRUE), "try-error")) degree = degree - 1
  basis = qr.Q(qr(cbind(rep(1, n), if(degree > 0) stats::poly(pred, degree))))
  k = ncol(basis)
  covariance = outer(quantiles, quantiles, pmin) - outer(quantiles, quantiles)
  covInverse = solve(covariance)
  scores = function(u) crossprod(basis, outer(u, quantiles, "<=") - rep(quantiles, each = n))
  stat = function(S){
    q = rowSums((S %*% covInverse) * S) # per-degree joint chi2 contribution (m df each)
    joint = cumsum(q)
    single = matrix(apply(S^2, 2, cumsum), nrow = k) / rep(diag(covariance), each = k)
    hi = if(k > 1) min(stats::pchisq(cumsum(q[-1]), (1:(k - 1)) * m, lower.tail = FALSE)) else 1
    c(joint = min(stats::pchisq(joint, (1:k) * m, lower.tail = FALSE)),
      d0 = stats::pchisq(q[1], m, lower.tail = FALSE),
      d1plus = hi,
      apply(matrix(stats::pchisq(single, 1:k, lower.tail = FALSE), nrow = k), 2, min))
  }
  obs = stat(scores(res))
  set.seed(seed)
  sim = replicate(nRep, stat(scores(stats::runif(n))))
  (rowSums(sim <= obs) + 1) / (nRep + 1)
}

# joint and per-quantile p-values of three versions:
#   all = PR #654 (degrees 0..10, uniform null)
#   d1  = degrees 1..10, uniform null
#   pm  = degrees 1..10, permutation null with empirical covariance of the indicators
# restores the random state of the caller
scoreBoth <- function(res, pred, quantiles = c(.25, .5, .75), maxDegree = 10, nRep = 1000){
  old = .Random.seed; on.exit(assign(".Random.seed", old, envir = .GlobalEnv))
  n = length(res); m = length(quantiles)
  degree = max(0, min(maxDegree, length(unique(pred)) - 1, n - 2))
  while(degree > 0 && inherits(try(stats::poly(pred, degree), silent = TRUE), "try-error")) degree = degree - 1
  basis = qr.Q(qr(cbind(rep(1, n), if(degree > 0) stats::poly(pred, degree)))); k = ncol(basis)
  one = function(Sx, cv){ kk = nrow(Sx); ci = solve(cv)
    joint = cumsum(rowSums((Sx %*% ci) * Sx))
    single = matrix(apply(Sx^2, 2, cumsum), nrow = kk) / rep(diag(cv), each = kk)
    c(min(pchisq(joint, (1:kk) * m, lower.tail = FALSE)), apply(matrix(pchisq(single, 1:kk, lower.tail = FALSE), nrow = kk), 2, min)) }
  cvU = outer(quantiles, quantiles, pmin) - outer(quantiles, quantiles)
  I = outer(res, quantiles, "<="); ph = colMeans(I); cvE = crossprod(I) / n - outer(ph, ph)
  sc = function(u, center) crossprod(basis, outer(u, quantiles, "<=") - rep(center, each = n))
  statU = function(u){ Sc = sc(u, quantiles); c(one(Sc, cvU), one(Sc[-1, , drop = FALSE], cvU)) }
  statP = function(u){ Sc = sc(u, ph); one(Sc[-1, , drop = FALSE], cvE) }
  obs = c(statU(res), statP(res))
  set.seed(123); simU = replicate(nRep, statU(runif(n)))
  set.seed(123); simP = replicate(nRep, statP(sample(res)))
  p = (rowSums(rbind(simU, simP) <= obs) + 1) / (nRep + 1)
  names(p) = c("allJ", "all25", "all50", "all75", "d1J", "d125", "d150", "d175", "pmJ", "pm25", "pm50", "pm75"); p
}

# run fun(i) for all rows of grid on nCores workers with the given DHARMa library
runGrid <- function(grid, fun, lib, export = character(0)){
  cl = makeCluster(nCores); on.exit(stopCluster(cl))
  clusterExport(cl, c("grid", "lib", "scoreVariants", "scoreBoth", export), envir = environment())
  invisible(clusterEvalQ(cl, {library(DHARMa, lib.loc = lib); NULL}))
  out = parLapplyLB(cl, seq_len(nrow(grid)), fun)
  cbind(grid, as.data.frame(do.call(rbind, out)))
}

# rejection rate at the 5 % quantile of the p-values under H0 (size-adjusted power)
sizeAdjusted <- function(pH0, pH1) mean(pH1 <= quantile(pH0, 0.05))

# ---- 0. Degree of the polynomial basis: fixed vs. adaptive --------------------
# Run with DHARMa at commit 9ac0daa (residuals before PR #658).
# fixed degree d (joint statistic over degrees 0..d, Monte Carlo null) vs.
# adaptive (min-p over the cumulative degrees 0..10)

qcand = function(res, pred, quantiles = c(0.25, 0.5, 0.75), degree = 3, nMC = 1000){
  n = length(res); r = rank(pred)
  d = min(degree, length(unique(r)) - 1, n - 2)
  Q = qr.Q(qr(if(d > 0) cbind(1, poly(r, d)) else matrix(1, n, 1)))
  Sig = outer(quantiles, quantiles, pmin) - outer(quantiles, quantiles); SigInv = solve(Sig)
  m = length(quantiles); k = ncol(Q)
  stat = function(u){ S = crossprod(Q, outer(u, quantiles, "<=") - rep(quantiles, each = n))
    c(colSums(S^2) / diag(Sig), sum((S %*% SigInv) * S)) }
  obs = stat(res)
  if(nMC > 0 && n <= 5000){ sim = replicate(nMC, stat(runif(n))); p = (rowSums(sim >= obs) + 1)/(nMC + 1) }
  else p = pchisq(obs, c(rep(k, m), k*m), lower.tail = FALSE)
  c(p[m + 1], p[1:m])
}
qadapt = function(res, pred, quantiles = c(0.25, 0.5, 0.75), dmax = 10, nMC = 1000){
  n = length(res); r = rank(pred)
  d = max(0, min(dmax, length(unique(r)) - 1, n - 2))
  Q = qr.Q(qr(if(d > 0) cbind(1, poly(r, d)) else matrix(1, n, 1)))
  Sig = outer(quantiles, quantiles, pmin) - outer(quantiles, quantiles); SigInv = solve(Sig)
  m = length(quantiles); k = ncol(Q)
  stat = function(u){ S = crossprod(Q, outer(u, quantiles, "<=") - rep(quantiles, each = n))
    joint = cumsum(rowSums((S %*% SigInv) * S))
    per = apply(S^2, 2, cumsum) / rep(diag(Sig), each = k); if(k == 1) per = matrix(per, 1)
    c(min(pchisq(joint, (1:k)*m, lower.tail = FALSE)), apply(pchisq(per, 1:k, lower.tail = FALSE), 2, min)) }
  obs = stat(res); sim = replicate(nMC, stat(runif(n)))
  (rowSums(sim <= obs) + 1)/(nMC + 1)
}
gen0 = function(scen){
  switch(scen,
   pois200 = { x = runif(200, -1, 1); y = rpois(200, exp(1 + x)); glm(y ~ x, family = poisson) },
   glmer = { g = gl(10, 20); x = runif(200, -1, 1); y = rpois(200, exp(0.5 + x + rnorm(10, 0, 0.5)[g])); lme4::glmer(y ~ x + (1|g), family = poisson) },
   H_quad50 = { x = runif(50, -1, 1); y = rpois(50, exp(1 + x + x^2)); glm(y ~ x, family = poisson) },
   H_het100 = { x = runif(100, -1, 1); y = x + rnorm(100, 0, exp(0.6*x)); lm(y ~ x) },
   H_sin100 = { x = runif(100, -1.5, 1.5); y = rpois(100, exp(1 + x + 0.5*sin(4*x))); glm(y ~ x, family = poisson) },
   H_sin500 = { x = runif(500, -1.5, 1.5); y = rpois(500, exp(1 + 0.5*x + 0.3*sin(6*x))); glm(y ~ x, family = poisson) })
}
scen0 = c(H_quad50 = 200, H_sin100 = 200, H_het100 = 200, H_sin500 = 200, pois200 = 500, glmer = 300)
cl = makeCluster(nCores); clusterExport(cl, c("qcand", "qadapt", "gen0", "libDev", "scen0"))
r0 = parLapply(cl, names(scen0), function(scen){
  library(DHARMa, lib.loc = libDev)
  set.seed(185)
  out = t(replicate(scen0[[scen]], {
    res = simulateResiduals(gen0(scen), n = 250, seed = FALSE)
    pr = res$fittedPredictedResponse
    c(d3 = qcand(res$scaledResiduals, pr, degree = 3)[1], d6 = qcand(res$scaledResiduals, pr, degree = 6)[1],
      adaptive = qadapt(res$scaledResiduals, pr)[1])
  }))
  colMeans(out < 0.05)
})
stopCluster(cl)
names(r0) = names(scen0)
print(round(setNames(as.data.frame(do.call(rbind, r0)), c("d3", "d6", "adaptive")), 3))

# ---- 1. Power of qgam p-values (DHARMa 0.5.1) vs. score test -----------------

makeData1 = function(scen, n){
  x = runif(n, -1, 1)
  if(scen == "lm") y = x + rnorm(n)
  if(scen == "lmQuad") y = x + 1.2 * x^2 + rnorm(n)
  if(scen == "lmHetero") y = x + rnorm(n, sd = exp(0.7 * x))
  if(scen == "pois") y = rpois(n, exp(0.5 + x))
  if(scen == "poisQuad") y = rpois(n, exp(0.5 + x + 0.8 * x^2))
  if(scen == "poisOD") y = rpois(n, exp(0.5 + x + rnorm(n, sd = 0.5)))
  fit = if(grepl("^pois", scen)) glm(y ~ x, family = poisson) else lm(y ~ x)
  simulateResiduals(fit, seed = FALSE)
}
grid = rbind(expand.grid(scen = c("lm", "lmQuad", "lmHetero", "pois", "poisQuad"), n = 100, rep = 1:300, stringsAsFactors = FALSE),
             expand.grid(scen = c("pois", "poisOD"), n = 200, rep = 1:300, stringsAsFactors = FALSE))
grid$seed = 8e5 + seq_len(nrow(grid))
r1 = runGrid(grid, function(i){
  stopifnot(packageVersion("DHARMa") == "0.5.1")
  set.seed(grid$seed[i]); sim = makeData1(grid$scen[i], grid$n[i])
  qg = suppressWarnings(suppressMessages(testQuantiles(sim, plot = FALSE)$p.value))
  pred = DHARMa:::ensurePredictor(sim, NULL); pred = rank(pred, ties.method = "average"); pred = pred / max(pred)
  c(qgam = qg, scoreBoth(sim$scaledResiduals, pred), dispersion = testDispersion(sim, plot = FALSE)$p.value)
}, libOld, "makeData1")
saveRDS(r1, "185_power.rds")

for(h in list(c("lm", "lmQuad", 100), c("lm", "lmHetero", 100), c("pois", "poisQuad", 100), c("pois", "poisOD", 200))){
  h0 = r1[r1$scen == h[1] & r1$n == h[3], ]; h1 = r1[r1$scen == h[2] & r1$n == h[3], ]
  v = c("qgam", "allJ", "pmJ", "dispersion")
  cat(h[2], "n =", h[3], "\n  raw:          ", round(100 * colMeans(h1[v] <= 0.05), 1),
      "\n  size-adjusted:", round(100 * sapply(v, function(x) sizeAdjusted(h0[[x]], h1[[x]])), 1),
      "\n  H0:           ", round(100 * colMeans(h0[v] <= 0.05), 1), "\n")
}

# ---- 2. Degree 0, conditional and unconditional GLMM residuals ---------------

makeData2 = function(scen, n, gs){
  x = runif(n, -1, 1)
  if(scen == "poisH0"){ y = rpois(n, exp(0.5 + x)); sim = simulateResiduals(glm(y ~ x, family = poisson), seed = FALSE)
  } else if(scen == "poisOD"){ y = rpois(n, exp(0.5 + x + rnorm(n, sd = 0.5))); sim = simulateResiduals(glm(y ~ x, family = poisson), seed = FALSE)
  } else if(scen == "lmH0"){ y = x + rnorm(n); sim = simulateResiduals(lm(y ~ x), seed = FALSE)
  } else if(scen == "lmQuad"){ y = x + 1.5 * x^2 + rnorm(n); sim = simulateResiduals(lm(y ~ x), seed = FALSE)
  } else if(scen == "lmHet"){ y = x + rnorm(n, sd = exp(0.5 * x)); sim = simulateResiduals(lm(y ~ x), seed = FALSE)
  } else { # glmmCond / glmmUncond, gs = observations per group
    g = factor(rep(1:(n %/% gs), length.out = n))
    y = rpois(n, exp(0.5 + x + rnorm(nlevels(g))[g]))
    fit = suppressMessages(suppressWarnings(lme4::glmer(y ~ x + (1|g), family = poisson)))
    sim = simulateResiduals(fit, simulateREs = if(scen == "glmmCond") "conditional" else "unconditional", seed = FALSE)
  }
  pred = DHARMa:::rankTransform(DHARMa:::ensurePredictor(sim, NULL))
  list(sim = sim, res = sim$scaledResiduals, pred = pred)
}
grid = rbind(
  expand.grid(scen = c("poisH0", "poisOD", "lmH0", "lmHet"), n = 200, gs = NA, rep = 1:400, stringsAsFactors = FALSE),
  expand.grid(scen = c("lmH0", "lmQuad"), n = 100, gs = NA, rep = 1:400, stringsAsFactors = FALSE),
  expand.grid(scen = "glmmCond", n = c(500, 2000), gs = c(5, 10, 50), rep = 1:200, stringsAsFactors = FALSE),
  expand.grid(scen = "glmmUncond", n = 1000, gs = c(10, 50), rep = 1:150, stringsAsFactors = FALSE))
grid$seed = 7e5 + seq_len(nrow(grid))
grid = grid[order(-grid$n), ]
r2 = runGrid(grid, function(i){
  t = grid[i, ]; set.seed(t$seed)
  d = makeData2(t$scen, t$n, t$gs)
  sv = scoreVariants(d$res, d$pred)
  ks = testUniformity(d$sim, plot = FALSE)$p.value
  disp = testDispersion(d$sim, plot = FALSE)$p.value
  c(sv, ks = ks, disp = disp, share25 = mean(d$res <= 0.25), share75 = mean(d$res > 0.75))
}, libNew, "makeData2")
saveRDS(r2, "185_degree0.rds")

r2$gs[is.na(r2$gs)] = 0
a = aggregate(cbind(joint = joint <= .05, d0 = d0 <= .05, d1plus = d1plus <= .05, ks = ks <= .05, disp = disp <= .05,
                    sh25 = share25, sh75 = share75) ~ scen + n + gs, data = r2, FUN = mean)
a$reps = aggregate(joint ~ scen + n + gs, r2, length)$joint
print(a, digits = 3)
for(h in list(c("poisH0", "poisOD", 200), c("lmH0", "lmHet", 200), c("lmH0", "lmQuad", 100))){
  h0 = r2[r2$scen == h[1] & r2$n == h[3], ]; h1 = r2[r2$scen == h[2], ]
  cat(h[2], "size-adjusted:", sapply(c("joint", "d0", "d1plus", "ks", "disp"), function(v) round(sizeAdjusted(h0[[v]], h1[[v]]), 3)), "\n")
}

# ---- 3. Mechanism: conditional GLMM residuals --------------------------------

library(DHARMa, lib.loc = libNew)
set.seed(3); n = 20000; gs = 5
x = runif(n, -1, 1); g = factor(rep(1:(n %/% gs), length.out = n)); b = rnorm(nlevels(g))[g]
y = rpois(n, exp(0.5 + x + b))
f = suppressMessages(lme4::glmer(y ~ x + (1|g), family = poisson))
s = simulateResiduals(f, seed = FALSE); r = s$scaledResiduals
cat("Poisson GLMM gs5: share<=.25", mean(r <= .25), " >.75", mean(r > .75), " sd(qnorm)", sd(qnorm(r)), "\n")
mu = fitted(f); bin = cut(mu, quantile(mu, 0:5/5), include.lowest = TRUE)
print(round(tapply(r <= .25, bin, mean), 3)); print(round(tapply(r > .75, bin, mean), 3))
gt = tapply(y, g, sum)[g]; cat("groups with all zeros: share<=.25", mean(r[gt == 0] <= .25), ">.75", mean(r[gt == 0] > .75), "n", sum(gt == 0), "\n")
cat("others: share<=.25", mean(r[gt > 0] <= .25), ">.75", mean(r[gt > 0] > .75), "\n")
# simulation with the true REs (no estimation)
lam = exp(0.5 + x + b); simT = matrix(rpois(n * 250, lam), n)
rT = createDHARMa(simT, y, integerResponse = TRUE, seed = FALSE)$scaledResiduals
cat("true REs: share<=.25", mean(rT <= .25), ">.75", mean(rT > .75), "\n")
# Gaussian LMM, same structure
yl = 0.5 + x + b + rnorm(n); fl = lme4::lmer(yl ~ x + (1|g))
rl = simulateResiduals(fl, seed = FALSE)$scaledResiduals
cat("LMM gs5: share<=.25", mean(rl <= .25), ">.75", mean(rl > .75), " sd(qnorm)", sd(qnorm(rl)), " sigma", sigma(fl), "\n")
# testDispersion() with conditional simulations
set.seed(4)
for(gs in c(5, 50)) {
  o = t(replicate(60, {
    x = runif(2000, -1, 1); g = factor(rep(1:(2000 %/% gs), length.out = 2000))
    y = rpois(2000, exp(0.5 + x + rnorm(nlevels(g))[g]))
    f = suppressMessages(lme4::glmer(y ~ x + (1|g), family = poisson))
    s = simulateResiduals(f, seed = FALSE)
    td = testDispersion(s, plot = FALSE)
    tl = testDispersion(s, plot = FALSE, alternative = "less")
    c(ratio = unname(td$statistic), p2 = td$p.value, pless = tl$p.value)
  }))
  cat("gs", gs, "mean ratio", mean(o[,1]), "range", range(o[,1]), "rej two-sided", mean(o[,2] <= .05), "rej less", mean(o[,3] <= .05), "\n")
}

# ---- 4. Score test details: colours under H0, nRep, grid residuals ------------

# statistic of the joint version (PR #654) for a fixed predictor
setup = function(pred, quantiles = c(.25,.5,.75), maxDegree = 10){
  n = length(pred); m = length(quantiles)
  degree = max(0, min(maxDegree, length(unique(pred)) - 1, n - 2))
  basis = qr.Q(qr(cbind(rep(1, n), if(degree > 0) stats::poly(pred, degree)))); k = ncol(basis)
  cv = outer(quantiles, quantiles, pmin) - outer(quantiles, quantiles); ci = solve(cv)
  function(u){ S = crossprod(basis, outer(u, quantiles, "<=") - rep(quantiles, each = n))
    joint = cumsum(rowSums((S %*% ci) * S))
    single = matrix(apply(S^2, 2, cumsum), nrow = k) / rep(diag(cv), each = k)
    c(min(pchisq(joint, (1:k) * m, lower.tail = FALSE)), apply(matrix(pchisq(single, 1:k, lower.tail = FALSE), nrow = k), 2, min)) }
}
pv = function(obs, ref) (rowSums(ref <= obs) + 1) / (ncol(ref) + 1)
# curve colours vs joint test, iid uniform residuals, n = 200 (reference nRep 1000, seed 123 as in the PR)
set.seed(99); pr = runif(200); st = setup(pr); set.seed(123); ref = replicate(1000, st(runif(200)))
set.seed(11); C = t(replicate(5000, pv(st(runif(200)), ref)))
cat("n=200: joint sig", mean(C[,1] <= .05), " any single sig", mean(apply(C[,2:4] <= .05, 1, any)),
    " single sig & joint n.s.", mean(apply(C[,2:4] <= .05, 1, any) & C[,1] > .05),
    " any Holm sig", mean(apply(C[,2:4], 1, function(p) any(p.adjust(p, "holm") <= .05))), "\n")
# realized joint size for the fixed seed-123 reference, nRep 1000 vs 5000
for(n in c(30, 100, 200, 1000)){ set.seed(99); pr = runif(n); st = setup(pr)
  set.seed(7); fresh = replicate(20000, st(runif(n))[1])
  for(nRep in c(1000, 5000)){ set.seed(123); ref = replicate(nRep, st(runif(n))[1])
    cat("n", n, "nRep", nRep, "size", mean(sapply(fresh, function(o) (sum(ref <= o) + 1)/(nRep + 1)) <= .05), "\n") } }
# residuals on the grid k/nSim (before PR #658, true model), n = 2000
set.seed(99); pr = runif(2000); st = setup(pr); set.seed(123); ref = replicate(1000, st(runif(2000)))
for(nSim in c(50, 250)){ set.seed(13); E = replicate(500, pv(st(sample(0:nSim, 2000, TRUE)/nSim), ref)[1])
  cat("grid nSim", nSim, "rejection", mean(E <= .05), "\n") }

# ---- 5. Test versions and curve colour rules ----------------------------------

makeData5 = function(scen, n){
  x = runif(n, -1, 1)
  if(scen == "lm") y = x + rnorm(n)
  if(scen == "pois") y = rpois(n, exp(0.5 + x))
  if(scen == "bern") y = rbinom(n, 1, plogis(x))
  if(scen == "hetero") y = x + rnorm(n, sd = exp(0.7 * x))           # 0.25 and 0.75 move apart along x
  if(scen == "upperTail"){ z = rnorm(n); y = x + ifelse(z > 0, z * (1 + 1.5 * (x + 1)), z) }  # upper tail widens along x
  if(scen == "quad") y = x + 1.2 * x^2 + rnorm(n)                    # location shift along x (all quantiles)
  if(scen == "poisOD") y = rpois(n, exp(0.5 + x + rnorm(n, sd = 0.5)))
  if(scen == "glmm5"){ g = factor(rep(1:(n %/% 5), length.out = n)); y = rpois(n, exp(0.5 + x + rnorm(nlevels(g))[g]))
    sim = simulateResiduals(suppressMessages(lme4::glmer(y ~ x + (1|g), family = poisson)), seed = FALSE)
    return(list(res = sim$scaledResiduals, pred = DHARMa:::rankTransform(DHARMa:::ensurePredictor(sim, NULL)))) }
  fit = if(scen %in% c("pois", "poisOD")) glm(y ~ x, family = poisson) else if(scen == "bern") glm(y ~ x, family = binomial) else lm(y ~ x)
  sim = simulateResiduals(fit, seed = FALSE)
  list(res = sim$scaledResiduals, pred = DHARMa:::rankTransform(DHARMa:::ensurePredictor(sim, NULL)))
}
grid = rbind(expand.grid(scen = c("lm", "pois", "bern"), n = c(100, 500), rep = 1:400, stringsAsFactors = FALSE),
             expand.grid(scen = c("hetero", "upperTail", "quad", "poisOD"), n = c(100, 500), rep = 1:300, stringsAsFactors = FALSE),
             expand.grid(scen = "glmm5", n = 2000, rep = 1:150, stringsAsFactors = FALSE))
grid$seed = 9e5 + seq_len(nrow(grid))
r5 = runGrid(grid, function(i){
  set.seed(grid$seed[i]); d = makeData5(grid$scen[i], grid$n[i]); scoreBoth(d$res, d$pred)
}, libNew, "makeData5")
saveRDS(r5, "185_colours.rds")

# rejection rates (%) per version: joint test; any / q25 / q50 / q75 red and
# disagreement between title (joint test) and curves for the rules
# raw (per-curve p), gate (red only if the joint test is significant), holm
a = .05
tab = do.call(rbind, lapply(split(r5, list(r5$scen, r5$n), drop = TRUE), function(d){
  out = c(scen = d$scen[1], n = d$n[1], reps = nrow(d))
  for(v in c("all", "d1", "pm")){
    J = d[[paste0(v, "J")]]; P = as.matrix(d[, paste0(v, c("25", "50", "75"))])
    raw = P <= a; gate = raw & (J <= a); holm = t(apply(P, 1, p.adjust, "holm")) <= a
    f = function(M, lab) setNames(c(mean(apply(M, 1, any)), colMeans(M), mean(apply(M, 1, any) != (J <= a))),
                                  paste0(v, ".", lab, c(".any", ".q25", ".q50", ".q75", ".disagree")))
    out = c(out, setNames(mean(J <= a), paste0(v, ".joint")), f(raw, "raw"), f(gate, "gate"), f(holm, "holm"))
  }
  out }))
tab = as.data.frame(tab); num = setdiff(names(tab), "scen"); tab[num] = lapply(tab[num], as.numeric)
tab[grep("[.]", names(tab))] = round(100 * tab[grep("[.]", names(tab))], 1)
for(v in c("all", "d1", "pm")){ cat("\n==", v, "\n"); print(tab[, c("scen", "n", grep(paste0("^", v, "[.](joint|raw|gate|holm)"), names(tab), value = TRUE))], row.names = FALSE) }
