test_that("testDispersion with refit = TRUE: selection and zero aggregated residuals (#575)", {
  set.seed(123)
  testData = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 20, refit = TRUE)
  # selection with sel: observed residuals subset in the same way
  selected = recalculateResiduals(simulationOutput, sel = testData$Environment1 > 0)
  x = testDispersion(selected, plot = FALSE)
  observed = sum(residuals(fittedModel, type = "pearson")[testData$Environment1 > 0]^2)
  expect_equal(unname(x$statistic), observed / mean(colSums(selected$refittedPearsonResiduals^2)))
  # grouping variable as only predictor: aggregated residuals are zero, the test stops
  fittedModel = glm(observedResponse ~ group, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 5, refit = TRUE)
  expect_error(testDispersion(recalculateResiduals(simulationOutput, group = testData$group), plot = FALSE), "close to\\) zero")
})
