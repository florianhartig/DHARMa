test_that("getRefit reproduces the fit for further models and responses (#571)", {
  set.seed(1)
  n = 60
  testData = data.frame(x = runif(n), N = sample(5:15, n, TRUE))
  testData$k = rbinom(n, testData$N, plogis(testData$x))
  refitCoef = function(m, fix = coef) unlist(fix(getRefit(m, model.response(model.frame(m)))))
  # data with a single column
  m = glm(z ~ 1, family = poisson, data = data.frame(z = rpois(n, 3)))
  expect_equal(refitCoef(m), coef(m))
  testData$g = factor(sample(1:8, n, TRUE))
  m = GLMMadaptive::mixed_model(cbind(k, N - k) ~ x, random = ~1|g, data = testData, family = binomial())
  expect_equal(refitCoef(m, GLMMadaptive::fixef), GLMMadaptive::fixef(m))
  # formulas with '.'
  m = GLMMadaptive::mixed_model(k ~ . - g, random = ~1|g, data = testData[, c("k", "x", "g")], family = poisson())
  expect_equal(refitCoef(m, GLMMadaptive::fixef), GLMMadaptive::fixef(m))
})
