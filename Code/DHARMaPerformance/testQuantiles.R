# Calibration of testQuantiles() under H0 (see issue #185)
#
# testQuantiles() fits qgam quantile regressions of the scaled residuals on the
# (rank-transformed) predicted values and combines the p-values of the intercept
# and the spline of each quantile with Benjamini & Hochberg. This script
# simulates data from correctly specified models and records how often the test
# rejects at alpha = 0.05, depending on the sample size, which component
# (intercept or spline) rejects, and how often qgam reports convergence problems.
#
# Scenarios (x ~ U(0, 1) unless noted):
#   known:           no model fitted, simulations from the true normal model
#   knownPoisson:    no model fitted, simulations from the true Poisson model
#                    (randomized PIT residuals)
#   lm:              lm, y ~ N(1 + x, 1)
#   poisson:         glm, y ~ Pois(exp(1 + x)), x ~ U(-1, 1)
#   binomial:        glm, y ~ Bern(plogis(x)), x ~ N(0, 1)
#   binomialExtreme: glm, design of the #185 comment of 2026-11-08 with many
#                    fitted probabilities close to 0 or 1
#
# The simulations take several hours on 2 cores. Results are cached in
# testQuantiles.rds and figures written to testQuantiles_*.png in the working
# directory; both are not committed. Each replicate uses its own seed, so the
# results do not depend on the number of cores.

library(DHARMa)
library(parallel)

nCores = 2          # each core runs one R process with one thread
nRep = 300          # replicates per scenario and sample size
nSim = 250          # simulations per residual calculation
sampleSizes = c(10, 20, 50, 100, 200, 500)
scenarios = c("known", "knownPoisson", "lm", "poisson", "binomial", "binomialExtreme")
resultFile = "testQuantiles.rds"

# Simulate data for one scenario and return the DHARMa object
simulateScenario <- function(scenario, n, nSim){
  switch(scenario,
    known = {
      x = runif(n)
      createDHARMa(simulatedResponse = matrix(rnorm(n * nSim, x), nrow = n),
                   observedResponse = rnorm(n, x),
                   fittedPredictedResponse = x, seed = FALSE)
    },
    knownPoisson = {
      x = runif(n); lambda = exp(1 + x)
      createDHARMa(simulatedResponse = matrix(rpois(n * nSim, lambda), nrow = n),
                   observedResponse = rpois(n, lambda),
                   fittedPredictedResponse = lambda, integerResponse = TRUE, seed = FALSE)
    },
    lm = {
      x = runif(n); y = rnorm(n, 1 + x)
      simulateResiduals(lm(y ~ x), n = nSim, seed = FALSE)
    },
    poisson = {
      x = runif(n, -1, 1); y = rpois(n, exp(1 + x))
      simulateResiduals(glm(y ~ x, family = poisson), n = nSim, seed = FALSE)
    },
    binomial = {
      x = rnorm(n); y = rbinom(n, 1, plogis(x))
      simulateResiduals(glm(y ~ x, family = binomial), n = nSim, seed = FALSE)
    },
    binomialExtreme = {
      x1 = rnorm(n, 1); x2 = rbinom(n, 1, 0.7)
      y = rbinom(n, 1, plogis(-5 + 2 * x1 + x2 + 3 * x1 * x2))
      fit = suppressWarnings(glm(y ~ x1 * x2, family = binomial))
      simulateResiduals(fit, n = nSim, seed = FALSE)
    })
}

# One replicate: p-values of testQuantiles() and of its components
runReplicate <- function(task){
  set.seed(task$seed)
  res = simulateScenario(task$scenario, task$n, nSim)
  nMessages = 0
  out = withCallingHandlers(
    testQuantiles(res, plot = FALSE),
    message = function(m){ nMessages <<- nMessages + 1; invokeRestart("muffleMessage") },
    warning = function(w){ nMessages <<- nMessages + 1; invokeRestart("muffleWarning") })
  comp = sapply(1:3, function(i){
    fit = out$qgamFits[i]
    if(length(fit) == 0 || is.null(fit[[1]])) return(c(NA, NA))
    s = summary(fit[[1]])
    c(s$p.table[1, 4], s$s.table[1, 4])
  })
  data.frame(task, p = out$p.value,
             int25 = comp[1, 1], int50 = comp[1, 2], int75 = comp[1, 3],
             spl25 = comp[2, 1], spl50 = comp[2, 2], spl75 = comp[2, 3],
             nMessages = nMessages,
             uniformity = testUniformity(res, plot = FALSE)$p.value)
}

if(!file.exists(resultFile)){
  tasks = expand.grid(rep = 1:nRep, n = sampleSizes, scenario = scenarios,
                      stringsAsFactors = FALSE)
  tasks$seed = seq_len(nrow(tasks))
  cl = makeCluster(nCores)
  clusterEvalQ(cl, {Sys.setenv(OMP_NUM_THREADS = 1); library(DHARMa)})
  clusterExport(cl, c("simulateScenario", "nSim"))
  results = parLapplyLB(cl, split(tasks, seq_len(nrow(tasks))), runReplicate)
  stopCluster(cl)
  results = do.call(rbind, results)
  attr(results, "info") = list(DHARMa = packageVersion("DHARMa"),
                               qgam = packageVersion("qgam"),
                               date = Sys.Date(), nSim = nSim)
  saveRDS(results, resultFile)
}

results = readRDS(resultFile)

# Rejection rates per scenario and sample size
rate = function(p) mean(p < 0.05, na.rm = TRUE)
rates = aggregate(cbind(p, uniformity, int25, int50, int75, spl25, spl50, spl75) ~ scenario + n,
                  data = results, FUN = rate, na.action = na.pass)
rates$failed = aggregate(is.na(p) ~ scenario + n, data = results, FUN = mean)[, 3]
# share of replicates without failure in which qgam emitted a warning or message
rates$messages = aggregate(nMessages > 0 ~ scenario + n, data = results[!is.na(results$p), ], FUN = mean)[, 3]
print(rates[order(rates$scenario, rates$n), ], digits = 2)

cols = setNames(c("grey40", "black", "#E69F00", "#56B4E9", "#009E73", "#D55E00"), scenarios)
band = qbinom(c(0.025, 0.975), nRep, 0.05) / nRep # 95% range of the rejection rate if calibrated

plotRates <- function(column, main, ylim = c(0, max(rates[, column], na.rm = TRUE))){
  plot(NULL, xlim = range(sampleSizes), ylim = ylim, log = "x", las = 1,
       xlab = "Sample size n", ylab = "Rejection rate at alpha = 0.05", main = main)
  rect(min(sampleSizes) / 2, band[1], max(sampleSizes) * 2, band[2], col = "grey90", border = NA)
  abline(h = 0.05, lty = 2)
  for(s in scenarios){
    r = rates[rates$scenario == s, ]
    lines(r$n, r[, column], type = "b", pch = 19, col = cols[s])
  }
}

# Figure 1: rejection rate of the combined p-value
png("testQuantiles_typeI.png", width = 1600, height = 1000, res = 200)
par(mar = c(4, 4, 2, 1))
plotRates("p", "testQuantiles() under H0")
legend("topright", legend = scenarios, col = cols, pch = 19, lty = 1, bty = "n", cex = 0.8)
dev.off()

# Figure 2: distribution of the combined p-value (uniform if calibrated)
png("testQuantiles_pvalues.png", width = 2400, height = 900, res = 200)
par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
for(n in intersect(c(20, 100, 500), sampleSizes)){
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), las = 1, main = paste("n =", n),
       xlab = "p-value", ylab = "Empirical CDF")
  abline(0, 1, lty = 2)
  for(s in scenarios){
    p = results$p[results$scenario == s & results$n == n]
    lines(ecdf(p), col = cols[s], do.points = FALSE, verticals = TRUE)
  }
  if(n == 20) legend("bottomright", legend = scenarios, col = cols, lty = 1, bty = "n", cex = 0.8)
}
dev.off()

# Figure 3: which component rejects (unadjusted p-values; intercept: mean over the
# 0.25 and 0.75 quantiles, see the printed table for the 0.5 quantile)
rates$intercept = rowMeans(rates[, c("int25", "int75")])
rates$spline = rowMeans(rates[, c("spl25", "spl50", "spl75")])
png("testQuantiles_components.png", width = 2400, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
yl = c(0, max(rates[, c("intercept", "spline")], na.rm = TRUE))
plotRates("intercept", "Intercept test, quantiles 0.25 and 0.75 (unadjusted)", yl)
legend("topright", legend = scenarios, col = cols, pch = 19, lty = 1, bty = "n", cex = 0.8)
plotRates("spline", "Spline test, all quantiles (unadjusted)", yl)
dev.off()
