test_that("glmmTMB refit simulations for proportions with weights (#571)", {
  set.seed(1)
  testData = data.frame(x = runif(60), N = sample(5:15, 60, TRUE))
  testData$p = rbinom(60, testData$N, plogis(testData$x)) / testData$N
  m = glmmTMB::glmmTMB(p ~ x, weights = N, family = binomial, data = testData)
  sims = getSimulations(m, nsim = 3, type = "refit")
  expect_equal(ncol(sims), 3)
  # proportions of the number of trials
  expect_equal(as.matrix(sims) * testData$N, round(as.matrix(sims) * testData$N))
})
