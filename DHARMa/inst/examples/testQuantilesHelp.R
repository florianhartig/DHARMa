testData = createData(sampleSize = 200, overdispersion = 0.0, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# run the quantile test
x = testQuantiles(simulationOutput)
x # the test shows a combined p-value, corrected for multiple testing

\dontrun{
# accessing results of the test
x$pvals # pvalues for the individual quantiles
x$qgamFits # access the fitted quantile regression
summary(x$qgamFits[[1]]) # summary of the first fitted quantile

# possible to test user-defined quantiles
testQuantiles(simulationOutput, quantiles = c(0.7))

#  example with missing environmental predictor
fittedModel <- glm(observedResponse ~ 1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# specified as formula (recommended)
testQuantiles(simulationOutput, predictor = ~Environment1)

# or as variable in your environment
testQuantiles(simulationOutput, predictor = testData$Environment1)

plot(simulationOutput)
plotResiduals(simulationOutput)
}

