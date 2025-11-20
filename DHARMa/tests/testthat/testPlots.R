
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
  plotResiduals(simulationOutput)
  plotResiduals(simulationOutput, rank = T, quantreg = F)
  plotResiduals(simulationOutput, quantiles = 0.5)
  plotResiduals(simulationOutput, quantiles = c(0.3, 0.6))
  plotResiduals(simulationOutput$scaledResiduals, form =
                  simulationOutput$fittedPredictedResponse)

  # hist
  hist(simulationOutput)
}

doTests <- function(simulationOutput, testData){
  testUniformity(simulationOutput = simulationOutput)
  testZeroInflation(simulationOutput = simulationOutput)
  testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time)
  testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x,
                             y = testData$y)
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
            testData = createData(sampleSize = 100, overdispersion = 0,
                                  randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 ,
                               family = "binomial", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            doClassFunctions(simulationOutput)
            doPlots(simulationOutput, testData)
            doTests(simulationOutput, testData)


            testData = createData(sampleSize = 100, overdispersion = 0,
                                  randomEffectVariance = 2, numGroups = 4,
                                  family = gaussian())
            fittedModel <- glm(observedResponse ~ group , data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            doClassFunctions(simulationOutput)
            doPlots(simulationOutput, testData)
            plotResiduals(simulationOutput, testData$group)
          }
)

test_that("different form arguments in plotResiduals work",
          {
            testData = createData(100, hasNA = TRUE, randomEffectVariance = 2, numGroups = 10)
            fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (Environment1|group),
                               family = "poisson", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)

            expect_no_error(plotResiduals(simulationOutput, form = ~Environment1))
            expect_no_error(plotResiduals(simulationOutput, form = ~Environment1|group))
            expect_no_error(plotResiduals(simulationOutput, form = ~x))
            expect_error(plotResiduals(simulationOutput, form = testData$Environment1))
          }
)
