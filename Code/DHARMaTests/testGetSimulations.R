test_that("gam simulations: bam offsets and several linear predictors (#572)", {
  set.seed(1)
  n = 200
  testData = data.frame(x = runif(n), a = runif(n, 1, 3))
  testData$y = rpois(n, testData$a * exp(1 + testData$x))
  # no false positives of the bam offset check
  expect_no_error(simulateResiduals(mgcv::bam(y ~ s(x) + offset(log(a)), family = poisson, data = testData), n = 10))
  expect_no_error(simulateResiduals(mgcv::bam(y ~ s(x), offset = log(a), family = poisson, data = testData, discrete = TRUE), n = 10))
  expect_no_error(simulateResiduals(mgcv::gam(list(y ~ s(x), ~ 1), family = mgcv::gaulss(), data = testData), n = 10))
})


test_that("gam simulations without mgcViz for factor responses (#567)", {
  set.seed(1)
  testData = createData(sampleSize = 200, family = binomial(), binomialTrials = 20, randomEffectVariance = 0)
  testData$f = factor(testData$observedResponse1 > 10)
  mf = mgcv::gam(f ~ Environment1, family = binomial, data = testData)
  expect_equal(dim(getSimulations(mf, nsim = 2, mgcViz = FALSE)), c(200, 2))
  # the fallback must not use mgcViz's simulate.gam once the mgcViz namespace is loaded
  skip_if_not_installed("mgcViz")
  loadNamespace("mgcViz")
  m = mgcv::gam(cbind(observedResponse1, observedResponse0) ~ Environment1, family = binomial, data = testData)
  simulationOutput = simulateResiduals(m, n = 100, mgcViz = FALSE)
  expect_equal(mean(simulationOutput$simulatedResponse), mean(simulationOutput$observedResponse), tolerance = 0.05)
})


test_that("binomial models with cbind() response and prior weights (#568)", {
  set.seed(2)
  testData = data.frame(x = rnorm(100), N = sample(20:30, 100, TRUE), g = factor(sample(1:10, 100, TRUE)), w = 2, w1 = 1)
  testData$k = rbinom(100, testData$N, plogis(testData$x))
  expect_error(simulateResiduals(lme4::glmer(cbind(k, N - k) ~ x + (1|g), family = binomial, weights = w, data = testData)), "additional prior weights")
  expect_error(simulateResiduals(glmmTMB::glmmTMB(cbind(k, N - k) ~ x, family = binomial, weights = w, data = testData)), "additional prior weights")
  expect_error(simulateResiduals(mgcv::gam(cbind(k, N - k) ~ s(x), family = binomial, weights = w, data = testData)), "additional prior weights")
  # weights of 1 are fine
  expect_no_warning(s <- simulateResiduals(mgcv::gam(cbind(k, N - k) ~ s(x), family = binomial, weights = w1, data = testData), n = 10))
  expect_equal(mean(s$simulatedResponse), mean(s$observedResponse), tolerance = 0.1)
  # glmmTMB refit simulations: cbind() response with weights of 1, one simulation per column
  m = glmmTMB::glmmTMB(cbind(k, N - k) ~ x, weights = w1, family = binomial, data = testData)
  expect_equal(length(getSimulations(m, nsim = 3, type = "refit")), 3)
})
