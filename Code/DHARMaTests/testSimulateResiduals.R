test_that("simulateResiduals with refit = TRUE stops if all refits fail (#571)", {
  testData = createData(sampleSize = 50, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  fittedModel$call$data = quote(notExisting)
  expect_error(simulateResiduals(fittedModel, n = 3, refit = TRUE), "all refits failed")
})
