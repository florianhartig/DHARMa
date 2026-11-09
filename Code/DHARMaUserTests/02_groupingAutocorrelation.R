# User-perspective test 2: recalculateResiduals() and temporal / spatial autocorrelation,
# with repeated measures per site and year, as in typical field data
source("Code/DHARMaUserTests/helper.R")
set.seed(3)

# one observation per time step / location
d = createData(sampleSize = 100, family = poisson(), numGroups = 1, randomEffectVariance = 0)
so = simulateResiduals(glm(observedResponse ~ Environment1, family = poisson, data = d))
chk("temporal, vector time", testTemporalAutocorrelation(so, time = d$time), png = "TAC")
chk("temporal, formula time", testTemporalAutocorrelation(so, time = ~time, plot = FALSE))
chk("spatial, vectors", testSpatialAutocorrelation(so, x = d$x, y = d$y), png = "SAC")
chk("spatial, formulas", testSpatialAutocorrelation(so, x = ~x, y = ~y, plot = FALSE))
chk("spatial, distMat", testSpatialAutocorrelation(so, distMat = as.matrix(dist(cbind(d$x, d$y))), plot = FALSE))
chk("spatial, x formula and y vector", testSpatialAutocorrelation(so, x = ~x, y = d$y, plot = FALSE), expect = "error")

# AR(1) residual pattern should be detected
da = data.frame(t = 1:200, x = rnorm(200)); e = as.numeric(arima.sim(list(ar = 0.7), 200)) * 0.7
da$y = rpois(200, exp(1 + 0.5 * da$x + e))
chk("temporal AR(0.7)", testTemporalAutocorrelation(simulateResiduals(glm(y ~ x, family = poisson, data = da)), time = ~t, plot = FALSE), expect = "detect")

# 20 sites x 10 years, spatially structured site effect and AR(1) year effect, shuffled rows
nS = 20; nT = 10
sites = data.frame(site = factor(1:nS), sx = runif(nS), sy = runif(nS))
dd = merge(expand.grid(site = factor(1:nS), year = 1:nT), sites)
dd$env = rnorm(nrow(dd))
yearEff = as.numeric(arima.sim(list(ar = 0.8), nT)) * 0.5
siteEff = 1.5 * sin(5 * sites$sx)
dd$y = rpois(nrow(dd), exp(1 + 0.5 * dd$env + yearEff[dd$year] + siteEff[as.integer(dd$site)]))
dd = dd[sample(nrow(dd)), ]
sog = simulateResiduals(glm(y ~ env, family = poisson, data = dd))

chk("ungrouped, duplicated times", testTemporalAutocorrelation(sog, time = dd$year, plot = FALSE), expect = "error")
soY = chk("group = year (vector)", recalculateResiduals(sog, group = dd$year))
chk("group = ~year (formula)", recalculateResiduals(sog, group = ~year))
chk("grouped by year, time = sort(unique(year))", testTemporalAutocorrelation(soY, time = sort(unique(dd$year)), plot = FALSE), expect = "detect")
# known problem #359: formula is not aggregated; this call should run once #359 is fixed
chk("grouped by year, time = ~year", testTemporalAutocorrelation(soY, time = ~year, plot = FALSE), expect = "error")
soS = recalculateResiduals(sog, group = dd$site)
xs = aggregate(dd$sx, list(dd$site), mean)$x; ys = aggregate(dd$sy, list(dd$site), mean)$x
chk("grouped by site, aggregated coordinates", testSpatialAutocorrelation(soS, x = xs, y = ys, plot = FALSE), expect = "detect")
chk("sel = ~ year > 5", recalculateResiduals(sog, sel = ~ year > 5))
chk("group + sel", recalculateResiduals(sog, group = ~site, sel = ~ year > 5))
chk("group of wrong length", recalculateResiduals(sog, group = dd$site[-1]), expect = "error")
chk("sel leaving 1 residual", recalculateResiduals(sog, sel = 1), expect = "error")

saveLog("02_groupingAutocorrelation")
