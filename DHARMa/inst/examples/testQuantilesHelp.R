testData = createData(sampleSize = 200, overdispersion = 0.0, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# the plot function runs 4 tests

x = testQuantiles(simulationOutput)
x # the test shows a combined p-value, corrected for multiple testing
x$pvals # pvalues for the individual quantiles
x$qgamFits # access the fitted quantile regression 
summary(x$qgamFits[[1]]) # summary of the first fitted quantile

# possible to test parcticular quantiles
testQuantiles(simulationOutput, quantiles = c(0.7))

#  model with missing environmental predictor
fittedModel <- glm(observedResponse ~ 1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

testQuantiles(simulationOutput, predictor = testData$Environment1)

# the test is automatically performed in 
\dontrun{
plot(simulationOutput)
plotResiduals(simulationOutput)
}

