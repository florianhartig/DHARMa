
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
            expect_no_error(plotResiduals(simulationOutput, form = ~Environment1|group=="1"))
            expect_no_error(plotResiduals(simulationOutput, form = ~x))
            expect_no_error(plotResiduals(simulationOutput, form = ~.))
            expect_error(plotResiduals(simulationOutput, form = testData$Environment1))

            # one panel per group level (issue #573)
            n = 0
            setHook("plot.new", function() n <<- n + 1)
            on.exit(setHook("plot.new", NULL, "replace"))
            plotResiduals(simulationOutput, form = ~Environment1|group, quantreg = FALSE)
            expect_equal(n, 10)

            # one panel per selected group level (issue #596)
            n = 0
            plotResiduals(simulationOutput, form = ~Environment1|group %in% c("1", "3"), quantreg = FALSE)
            expect_equal(n, 2)
            expect_error(plotResiduals(simulationOutput, form = ~Environment1|group %in% c("1", "99")), "find specified group level")
            expect_error(plotResiduals(simulationOutput, form = ~Environment1|group != "1"), "group levels")
            expect_message(plotResidualsAll(simulationOutput, predictorNames = "Environment1", quantreg = FALSE), "deprecated")

            # absoluteDeviation also for a selected group level (#597)
            set.seed(1)
            testData = createData(200, numGroups = 2, randomEffectVariance = 0)
            simulationOutput <- simulateResiduals(glm(observedResponse ~ Environment1, family = "poisson", data = testData))
            expect_false(plotResiduals(simulationOutput, form = ~Environment1|group == "1")$p.value ==
                           plotResiduals(simulationOutput, form = ~Environment1|group == "1", absoluteDeviation = TRUE)$p.value)
          }
)


