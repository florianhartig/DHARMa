
doClassFunctions <- function(simulationOutput){
  print(simulationOutput)
  expect_true(class(residuals(simulationOutput)) == "numeric")
}

doPlots <- function(simulationOutput, testData){
  plot(simulationOutput, quantreg = T, rank = F, asFactor = T)
  plot(simulationOutput, quantreg = F, rank = F)

  plot(simulationOutput, quantreg = T, rank = T)
  plot(simulationOutput, quantreg = F, rank = T)

  # qq plot
  plotQQunif(simulationOutput = simulationOutput)

  # residual vs. X plots, various options
  plotResiduals(pred = simulationOutput)
  plotResiduals(pred = simulationOutput$fittedPredictedResponse, residuals = simulationOutput$scaledResiduals)
  hist(simulationOutput)
}

doTests <- function(simulationOutput, testData){
  testUniformity(simulationOutput = simulationOutput)
  testZeroInflation(simulationOutput = simulationOutput)
  testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time)
  testTemporalAutocorrelation(simulationOutput = simulationOutput)

  testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x, y = testData$y)
}

# currently not testing the following because of warning
#testOverdispersion(simulationOutput)
#testOverdispersion(simulationOutput, alternative = "both", plot = T)


# simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T, n = 10) # n=10 is very low, set higher for serious tests
#
# print(simulationOutput2)
# plot(simulationOutput2, quantreg = F)
#
# testOverdispersion(simulationOutput2)
# testOverdispersion(simulationOutput2, alternative = "both", plot = T)
# testOverdispersionParametric(fittedModel)



test_that("Plots work",
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 ,family = "binomial", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            doClassFunctions(simulationOutput)
            doPlots(simulationOutput, testData)


            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 2, numGroups = 4, family = gaussian())
            fittedModel <- glm(observedResponse ~ group , data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            doClassFunctions(simulationOutput)
            doPlots(simulationOutput, testData)
            plotResiduals(testData$group, simulationOutput$scaledResiduals)
          }
)
