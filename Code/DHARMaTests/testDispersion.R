test_that("testDispersion with refit = TRUE: selection and zero aggregated residuals (#575)", {
  set.seed(123)
  testData = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 20, refit = TRUE)
  # selection with sel (formula with an object from its environment): observed residuals subset in the same way
  threshold = 0
  selected = recalculateResiduals(simulationOutput, sel = ~Environment1 > threshold)
  x = testDispersion(selected, plot = FALSE)
  observed = sum(residuals(fittedModel, type = "pearson")[testData$Environment1 > 0]^2)
  expect_equal(unname(x$statistic), observed / mean(colSums(selected$refittedPearsonResiduals^2)))
  # grouping variable as only predictor: aggregated residuals are zero, warning and NA; plots still work
  fittedModel = glm(observedResponse ~ group, family = poisson, data = testData)
  grouped = recalculateResiduals(simulateResiduals(fittedModel, n = 5, refit = TRUE), group = testData$group)
  expect_warning(x <- testDispersion(grouped, plot = FALSE), "close to\\) zero")
  expect_true(is.na(x$p.value))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_warning(plot(grouped), "close to\\) zero")
  expect_warning(testDispersion(grouped), "close to\\) zero")
  expect_warning(testResiduals(grouped, plot = FALSE), "close to\\) zero")
})
