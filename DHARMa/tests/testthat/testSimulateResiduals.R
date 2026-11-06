
test_that("Rotation of residuals works", {

  testData = createData(family = gaussian())
  fittedModel <- lm(observedResponse ~ Environment1 , data = testData)

  expect_no_error(res1 <- simulateResiduals(fittedModel))
  expect_no_error(res2 <- simulateResiduals(fittedModel,
                                    rotation = diag(x = rep(1,100))))
  expect_equal(res1$scaledResiduals, res2$scaledResiduals)

  expect_no_error(res3 <- recalculateResiduals(res1, group = testData$group))
  expect_no_error(res4 <- recalculateResiduals(res2, group = testData$group,
                              rotation = diag(x = rep(1,10))))
  expect_equal(res3$scaledResiduals, res4$scaledResiduals)

})


test_that("Conditional, unconditional and user-specified simulations work for lme4", {

  testData = createData(family = gaussian())
  fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group), data = testData)

  expect_no_error(res1 <- simulateResiduals(fittedModel, simulateREs = "conditional"))
  expect_no_error(res2 <- simulateResiduals(fittedModel, simulateREs = "unconditional"))
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified"))
  expect_no_error(res4 <- simulateResiduals(fittedModel, simulateREs = "user-specified", re.form = ~(1|group)))

  expect_equal(residuals(res1), residuals(res4))
  expect_equal(residuals(res2), residuals(res3))

})


test_that("Conditional, unconditional and user-specified simulations work for spaMM", {

  testData = createData(family = gaussian())
  fittedModel <- spaMM::HLfit(observedResponse ~ Environment1 + (1|group), data = testData)

  expect_no_error(res1 <- simulateResiduals(fittedModel, simulateREs = "conditional"))
  expect_no_error(res2 <- simulateResiduals(fittedModel, simulateREs = "unconditional"))
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified"))
  expect_no_error(res4 <- simulateResiduals(fittedModel, simulateREs = "user-specified", re.form = ~(1|group)))

  expect_equal(residuals(res1), residuals(res4))
  expect_equal(residuals(res2), residuals(res3))

})


test_that("Conditional, unconditional and user-specified simulations work for glmmTMB", {

  testData = createData(family = gaussian())
  fittedModel <- glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group), data = testData)

  expect_no_error(res1 <- simulateResiduals(fittedModel, simulateREs = "conditional"))
  expect_no_error(res2 <- simulateResiduals(fittedModel, simulateREs = "unconditional"))
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified"))
  expect_error(res4 <- simulateResiduals(fittedModel, simulateREs = "user-specified", re.form = ~(1|group)))

  expect_equal(residuals(res2), residuals(res3))

})


test_that("Conditional, unconditional and user-specified simulations work for GLMMadaptive", {

  testData = createData(family = binomial())
  fittedModel <- GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~1|group, data = testData, family = binomial)

  expect_no_error(res1 <- simulateResiduals(fittedModel, simulateREs = "conditional"))
  expect_no_error(res2 <- simulateResiduals(fittedModel, simulateREs = "unconditional"))
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified"))

  expect_equal(residuals(res1), residuals(res3))
  expect_error(expect_equal(residuals(res2), residuals(res3)))
  expect_error(expect_equal(residuals(res1), residuals(res2)))

})


test_that("Simulations work for mgcv::gam", {

  testData = createData(family = gaussian())
  fittedModel <- mgcv::gam(observedResponse ~ s(Environment1) + s(group, bs = "re"), data = testData)

  expect_no_error(res1 <- simulateResiduals(fittedModel, simulateREs = "conditional"))
  expect_warning(res2 <- simulateResiduals(fittedModel, simulateREs = "unconditional"))
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified"))

})


test_that("refit = TRUE: k/n binomial residuals are on the scale of the number of successes (#598)", {

  set.seed(1)
  testData = data.frame(x = runif(50), N = sample(5:15, 50, replace = TRUE))
  testData$k = rbinom(50, testData$N, 0.5)
  fittedModel <- glm(cbind(k, N - k) ~ x, family = binomial, data = testData)
  res = simulateResiduals(fittedModel, n = 10, refit = TRUE)
  expect_equal(res$fittedResiduals, residuals(fittedModel, type = "response") * testData$N, ignore_attr = TRUE)

  testData$p = testData$k / testData$N
  fittedModel2 <- glm(p ~ x, family = binomial, weights = N, data = testData)
  res2 = simulateResiduals(fittedModel2, n = 10, refit = TRUE)
  expect_equal(res2$fittedResiduals, res$fittedResiduals)
})


test_that("simulateResiduals and recalculateResiduals: matched method, refit = TRUE with method and rotation (#589)", {
  testData = createData(sampleSize = 50, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 10)
  expect_equal(simulationOutput$method, "PIT")
  expect_equal(recalculateResiduals(simulationOutput, group = testData$group)$method, "PIT")
  expect_message(simulationOutput <- simulateResiduals(fittedModel, n = 5, refit = TRUE, method = "PIT"), "traditional")
  expect_error(simulateResiduals(fittedModel, refit = TRUE, rotation = "estimated"), "refit = TRUE")
  grouped = recalculateResiduals(simulationOutput, group = testData$group, method = "PIT")
  expect_equal(grouped$method, "traditional")
  expect_equal(grouped$scaledResiduals, recalculateResiduals(simulationOutput, group = testData$group, method = "traditional")$scaledResiduals)
  expect_error(recalculateResiduals(simulationOutput, group = testData$group, rotation = "estimated"), "rotation")
})
