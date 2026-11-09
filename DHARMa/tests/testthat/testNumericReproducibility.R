
test_that("residuals are numerically reproducible", {
  
  set.seed(123)
  
  library(lme4)
  
  testData = createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
  fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                       family = "poisson", data = testData)
  
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  expect_snapshot_value(residuals(simulationOutput), style = "deparse", tolerance = 0.01,
                        cran = TRUE)
})
