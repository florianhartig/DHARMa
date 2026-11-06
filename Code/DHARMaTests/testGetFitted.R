test_that("getFitted for gam with offsets, ziP and ocat (#572)", {
  set.seed(1)
  n = 200
  testData = data.frame(x = runif(n), a = runif(n, 1, 3), g = factor(sample(1:10, n, TRUE)))
  testData$y = rpois(n, testData$a * exp(1 + testData$x + rnorm(10, sd = 0.5)[testData$g]))
  # without REs, getFitted equals fitted
  m1 = mgcv::gam(y ~ s(x), offset = log(a), family = poisson, data = testData)
  expect_equal(unname(getFitted(m1)), unname(fitted(m1)))
  # gamm()$gam has no family() method
  mg = mgcv::gamm(y ~ s(x), random = list(g = ~1), family = poisson, data = testData)$gam
  expect_equal(unname(getFitted(mg)), unname(fitted(mg)))
  # ziP: offset as argument or in the formula, expected response as predict()
  m7 = mgcv::gam(y ~ s(x) + s(g, bs = "re"), offset = log(a), family = mgcv::ziP(), data = testData)
  m8 = mgcv::gam(y ~ s(x) + s(g, bs = "re") + offset(log(a)), family = mgcv::ziP(), data = testData)
  expect_equal(getFitted(m7), getFitted(m8), tolerance = 1e-4)
  expect_equal(unname(getFitted(m8)), as.vector(predict(m8, type = "response", exclude = "s(g)")))
  # ocat: one value per observation
  testData$yo = as.integer(cut(testData$x + rnorm(n, sd = 0.2), 3))
  expect_length(getFitted(mgcv::gam(yo ~ s(x), family = mgcv::ocat(R = 3), data = testData)), n)
})
