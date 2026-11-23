# Calibration of PIT residuals under H0: DHARMa <= 0.5.1 vs DHARMa >= 0.5.2 (#653)
#
# Data and simulations come from the same distribution (known parameters), with many
# observations, so that small deviations from uniformity become visible. Compared are
#   - old: PIT residuals of DHARMa 0.3.3 - 0.5.1, k / nSim with ties randomized between
#     k / nSim and (k + t) / nSim (re-implemented below),
#   - new: randomized PIT residuals (k + U * (t + 1)) / (nSim + 1) of DHARMa >= 0.5.2,
#     as calculated by getQuantile(),
# with k the number of simulations below and t the number tied with the observation.
# Reported are the rejection rates at alpha = 0.05 of the KS test for uniformity and of
# the binomial outlier test (outliers: residuals < 1/(nSim + 1) or > nSim/(nSim + 1)),
# and the mean outlier frequency against its expectation 2 / (nSim + 1).
#
# Run from the package folder of DHARMa >= 0.5.2 (branch i653-pitCalibration or later):
#   Rscript ../Code/DHARMaPerformance/pitResiduals/pitResiduals.R
# Results: pitResiduals.csv (committed), pitResiduals.rds (raw results, not committed).
# Run time about 10-20 min with 6 cores.
# Results in the csv: DHARMa 0.5.2 (branch i653-pitCalibration, PR #658), R 4.6.2; seeds are
# set per replicate in the script.

library(parallel)
nCores = 6
nRep = as.numeric(Sys.getenv("NREP", 300))
n = 8000
outDir = "../Code/DHARMaPerformance/pitResiduals"

oldPIT = function(sims, obs){
  lower = rowMeans(sims < obs); upper = rowMeans(sims <= obs)
  ifelse(lower != upper, runif(length(obs), lower, upper), lower)
}

simulateScenario = function(t){
  set.seed(t$seed)
  if(t$response == "normal"){ sims = matrix(rnorm(n * t$nSim), n); obs = rnorm(n) }
  else { sims = matrix(rpois(n * t$nSim, 3), n); obs = rpois(n, 3) }
  res = list(old = oldPIT(sims, obs), new = getQuantile(sims, obs, integerResponse = FALSE))
  do.call(rbind, lapply(names(res), function(m){
    r = res[[m]]
    outliers = sum(DHARMa:::isOutlier(r, t$nSim))
    data.frame(t, residuals = m,
               pKS = suppressWarnings(ks.test(r, punif)$p.value),
               pBinomial = binom.test(outliers, n, p = 2 / (t$nSim + 1))$p.value,
               outlierFrequency = outliers / n)
  }))
}

tasks = expand.grid(rep = seq_len(nRep), nSim = c(50, 250), response = c("normal", "poisson"),
                    stringsAsFactors = FALSE)
tasks$seed = 1e5 * (tasks$response == "poisson") + 1000 * tasks$nSim + tasks$rep
pkgDir = getwd()
cl = makeCluster(nCores)
clusterExport(cl, c("oldPIT", "simulateScenario", "n", "pkgDir"))
invisible(clusterEvalQ(cl, pkgload::load_all(pkgDir, quiet = TRUE)))
raw = do.call(rbind, parLapply(cl, split(tasks, seq_len(nrow(tasks))), simulateScenario))
stopCluster(cl)
saveRDS(raw, file.path(outDir, "pitResiduals.rds"))

summ = aggregate(cbind(rejKS = pKS < 0.05, rejBinomial = pBinomial < 0.05, outlierFrequency) ~
                   residuals + response + nSim, data = raw, FUN = mean)
summ$expected = 2 / (summ$nSim + 1)
summ$nRep = nRep
summ$n = n
summ[sapply(summ, is.numeric)] = lapply(summ[sapply(summ, is.numeric)], signif, 3)
write.csv(summ, file.path(outDir, "pitResiduals.csv"), row.names = FALSE)
cat("DHARMa", read.dcf(file.path(pkgDir, "DESCRIPTION"))[, "Version"], "loaded from", pkgDir, "|", R.version.string, "\n")
print(summ)
