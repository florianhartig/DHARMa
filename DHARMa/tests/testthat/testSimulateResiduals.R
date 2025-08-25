
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

  expect_equal(residuals(res2), residuals(res3))
  expect_error(expect_equal(residuals(res1), residuals(res2)))

})


test_that("Simulations work for mgcv::gam", {

  testData = createData(family = gaussian())
  fittedModel <- mgcv::gam(observedResponse ~ s(Environment1) + s(group, bs = "re"), data = testData)

  expect_no_error(res1 <- simulateResiduals(fittedModel, simulateREs = "conditional"))
  expect_warning(res2 <- simulateResiduals(fittedModel, simulateREs = "unconditional"))
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified"))

})
