test_that("testQuantiles with categorical, ordered and formula predictors (#576)", {
  set.seed(123)
  testData = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 50)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  # categorical predictors stop, for plot = TRUE and FALSE
  for(plot in c(FALSE, TRUE)){
    expect_error(testQuantiles(simulationOutput, predictor = testData$group, plot = plot), "testCategorical")
    expect_error(testQuantiles(simulationOutput, predictor = ~group, plot = plot), "testCategorical")
    expect_error(testQuantiles(simulationOutput, predictor = testData$Environment1 > 0, plot = plot), "testCategorical")
  }
  # ordered factors: same quantile regression with and without plot
  pred = factor(round(testData$Environment1), ordered = TRUE)
  expect_equal(testQuantiles(simulationOutput, predictor = pred, plot = FALSE)$p.value,
               testQuantiles(simulationOutput, predictor = pred, plot = TRUE)$p.value)
  # formulas: transformations are evaluated, objects from the formula environment are used, unknown variables stop
  expect_equal(testQuantiles(simulationOutput, predictor = ~log(Environment1 + 3), rank = FALSE, plot = FALSE)$p.value,
               testQuantiles(simulationOutput, predictor = log(testData$Environment1 + 3), rank = FALSE, plot = FALSE)$p.value)
  k = 3
  expect_equal(testQuantiles(simulationOutput, predictor = ~log(Environment1 + k), rank = FALSE, plot = FALSE)$p.value,
               testQuantiles(simulationOutput, predictor = log(testData$Environment1 + 3), rank = FALSE, plot = FALSE)$p.value)
  expect_error(testQuantiles(simulationOutput, predictor = ~unknownVariable), "exactly one variable")
})
