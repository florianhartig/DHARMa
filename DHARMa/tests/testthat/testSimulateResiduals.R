
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
  expect_true(res2$integerResponse)
})


test_that("integerResponse is detected from the simulations; beta-binomial names count as binomial (#620)", {

  testData = createData(sampleSize = 100, family = poisson())
  fittedModel = glmmTMB::glmmTMB(observedResponse ~ Environment1, family = glmmTMB::nbinom12, data = testData)
  expect_true(simulateResiduals(fittedModel, n = 10)$integerResponse)
  expect_false(simulateResiduals(fittedModel, n = 10, integerResponse = FALSE)$integerResponse)
  fittedModel = glm(observedResponse ~ Environment1, family = Gamma(link = "log"), data = testData[testData$observedResponse > 0, ])
  expect_false(simulateResiduals(fittedModel, n = 10)$integerResponse)

  testData = createData(sampleSize = 100, family = binomial(), binomialTrials = 10)
  fittedModel = GLMMadaptive::mixed_model(cbind(observedResponse1, observedResponse0) ~ Environment1, random = ~ 1 | group, data = testData, family = GLMMadaptive::beta.binomial())
  expect_no_warning(simulateResiduals(fittedModel, n = 10))
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

test_that("seed: same residuals in fresh sessions and with extra random draws (#630, #496)", {
  fittedModel = glm(carb ~ wt, family = poisson, data = mtcars)

  # no random state, as in a fresh R session (#630)
  if(exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
  res = residuals(simulateResiduals(fittedModel, n = 50))
  expect_false(exists(".Random.seed", envir = globalenv()))
  runif(1)
  expect_equal(residuals(simulateResiduals(fittedModel, n = 50)), res)

  # extra random draws after the simulations and refits do not change the residuals (#496)
  resRefit = residuals(simulateResiduals(fittedModel, n = 10, refit = TRUE))
  getSimulationsOrig = getSimulations
  getRefitOrig = getRefit
  local_mocked_bindings(getSimulations = function(...) {x = getSimulationsOrig(...); runif(100); x},
                        getRefit = function(...) {x = getRefitOrig(...); runif(3); x})
  expect_equal(residuals(simulateResiduals(fittedModel, n = 50)), res)
  expect_equal(residuals(simulateResiduals(fittedModel, n = 10, refit = TRUE)), resRefit)
})
