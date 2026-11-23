# Calibration of testOutliers() under H0: binomial test vs bootstrap (#653)
#
# For each scenario, data are simulated from the model that also generates the
# simulations (known parameters), or, for refit = TRUE, from a correctly specified
# fitted model. Reported are the rejection rates at alpha = 0.05 of
#   - the binomial test (type = "binomial"),
#   - the bootstrap (type = "bootstrap"), which compares each of nBoot simulations
#     with the other nSim - 1 simulations (DHARMa >= 0.5.2),
#   - the bootstrap of DHARMa <= 0.5.1, which resampled nSim simulations with
#     replacement (re-implemented below, applied to the current residuals),
# and the mean outlier frequency against its expectation 2 / (nSim + 1).
#
# Run from the package folder of DHARMa >= 0.5.2 (branch i653-pitCalibration or later):
#   Rscript ../Code/DHARMaPerformance/outlierTests/outlierTests.R
# Results: outlierTests.csv (committed), outlierTests.rds (raw p-values, not committed).
# Run time about 30-60 min with 6 cores.
# Results in the csv: DHARMa 0.5.2 (branch i653-pitCalibration, PR #658), R 4.6.2; seeds are
# set per replicate in the script.

library(parallel)
nCores = 6
nRep = as.numeric(Sys.getenv("NREP", 300))
nBoot = 50
outDir = "../Code/DHARMaPerformance/outlierTests"

scenarios = expand.grid(response = c("normal", "poisson", "bernoulli", "halfInteger"),
                        method = c("PIT", "traditional"), refit = FALSE,
                        n = 500, nSim = 50, stringsAsFactors = FALSE)
scenarios = rbind(scenarios,
                  data.frame(response = c("normal", "poisson"), method = "traditional",
                             refit = TRUE, n = 200, nSim = 50))

simulateScenario = function(s, seed){
  set.seed(seed)
  n = s$n; nSim = s$nSim

  # bootstrap of DHARMa <= 0.5.1: nSim simulations resampled with replacement
  oldBootstrap = function(d){
    simResp = if(d$refit) d$refittedResiduals else d$simulatedResponse
    freq = sapply(1:nBoot, function(i){
      sel = sample((1:nSim)[-i], size = nSim, replace = TRUE)
      res = getQuantile(simResp[, sel], simResp[, i], integerResponse = d$integerResponse, method = d$method)
      mean(DHARMa:::isOutlier(res, nSim, "both"))
    })
    observed = mean(DHARMa:::isOutlier(d$scaledResiduals, nSim, "both"))
    c(p = DHARMa:::getP(freq, observed, "two.sided"), expected = mean(freq))
  }

  if(s$refit){
    x = runif(n, -1, 1)
    if(s$response == "poisson"){
      y = rpois(n, exp(1 + x)); m = glm(y ~ x, family = poisson)
    } else {
      y = 1 + x + rnorm(n); m = lm(y ~ x)
    }
    d = simulateResiduals(m, n = nSim, refit = TRUE, seed = FALSE)
  } else {
    r = switch(s$response,
               normal = function(k) rnorm(k),
               poisson = function(k) rpois(k, 3),
               bernoulli = function(k) rbinom(k, 1, 0.3),
               halfInteger = function(k) rpois(k, 3) / 2) # non-integer values with ties
    d = createDHARMa(matrix(r(n * nSim), n), r(n), fittedPredictedResponse = rep(1, n),
                     integerResponse = s$response %in% c("poisson", "bernoulli"),
                     method = s$method, seed = FALSE)
  }
  binom = testOutliers(d, type = "binomial", plot = FALSE)
  boot = testOutliers(d, type = "bootstrap", nBoot = nBoot, plot = FALSE)
  bootExpected = as.numeric(sub(".*expected: ([^ ]+) .*", "\\1", names(boot$estimate)))
  old = oldBootstrap(d)
  c(pBinomial = binom$p.value, pBootstrap = boot$p.value, pBootstrapOld = unname(old["p"]),
    outlierFrequency = unname(binom$estimate), expectedBootstrap = bootExpected,
    expectedBootstrapOld = unname(old["expected"]),
    defaultIsBootstrap = testOutliers(d, plot = FALSE, nBoot = 2)$method == "DHARMa bootstrapped outlier test")
}

tasks = expand.grid(scenario = seq_len(nrow(scenarios)), rep = seq_len(nRep))
cl = makeCluster(nCores)
pkgDir = getwd()
clusterExport(cl, c("scenarios", "tasks", "simulateScenario", "nBoot", "pkgDir"))
invisible(clusterEvalQ(cl, pkgload::load_all(pkgDir, quiet = TRUE)))
raw = parLapply(cl, seq_len(nrow(tasks)), function(k){
  s = scenarios[tasks$scenario[k], ]
  suppressWarnings(suppressMessages(simulateScenario(s, seed = 1000 * tasks$scenario[k] + tasks$rep[k])))
})
stopCluster(cl)
raw = cbind(tasks, do.call(rbind, raw))
saveRDS(raw, file.path(outDir, "outlierTests.rds"))

summ = do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i){
  x = raw[raw$scenario == i, ]
  data.frame(scenarios[i, ], nRep = nrow(x),
             rejBinomial = mean(x$pBinomial < 0.05),
             rejBootstrap = mean(x$pBootstrap < 0.05),
             rejBootstrapOld = mean(x$pBootstrapOld < 0.05),
             outlierFrequency = mean(x$outlierFrequency),
             expected = 2 / (scenarios$nSim[i] + 1),
             expectedBootstrap = mean(x$expectedBootstrap),
             expectedBootstrapOld = mean(x$expectedBootstrapOld),
             defaultType = ifelse(x$defaultIsBootstrap[1] == 1, "bootstrap", "binomial"))
}))
summ[sapply(summ, is.numeric)] = lapply(summ[sapply(summ, is.numeric)], signif, 3)
write.csv(summ, file.path(outDir, "outlierTests.csv"), row.names = FALSE)
cat("DHARMa", read.dcf(file.path(pkgDir, "DESCRIPTION"))[, "Version"], "loaded from", pkgDir, "|", R.version.string, "\n")
print(summ)
