
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
  expect_no_error(res3 <- simulateResiduals(fittedModel, simulateREs = "user-specified", re.form = ~(1|group)))

})
