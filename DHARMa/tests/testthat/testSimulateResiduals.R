
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


test_that("simulateResiduals works for lme4 models with poly() and scale() attributes (issue #516)", {

  # scale() returns a 1-column matrix, not a plain vector. When poly() is used
  # on such a matrix in prediction mode (poly.default() with coefs != NULL),
  # outer(x, 0:degree, "^") produces a 3-D array instead of a 2-D matrix,
  # causing subscript-out-of-bounds errors inside predict.merMod(). The fix
  # is to (a) strip scale attributes in getData.merMod/getData.default, and
  # (b) add getFitted.merMod that uses the stored model matrix directly.

  set.seed(123)
  testData = createData(sampleSize = 100, fixedEffects = c(1, 2), family = poisson())
  testData$cat = rep(c("cat1", "cat2"), each = 50)
  testData$env1_s = scale(testData$Environment1)  # returns a matrix, not a vector

  fittedModel = lme4::glmer(
    observedResponse ~ Environment2 + poly(env1_s, 2) * cat +
      (1 + Environment1 | group),
    data = testData,
    family = poisson()
  )

  expect_no_error(res <- simulateResiduals(fittedModel))
  expect_no_error(plot(res))
  expect_no_error(plotResiduals(res, form = ~ Environment1))

})
