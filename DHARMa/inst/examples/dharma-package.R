# simulating a dataset
testData = createData(sampleSize = 200, family = poisson(),
                      fixedEffects = c(1,1),
                      randomEffectVariance = 1, numGroups = 10)
# fitting glm model
fittedModel <- glm(observedResponse ~ Environment1 + Environment2,
                   family = "poisson", data = testData)

# simulating residuals
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

#plotting residuals
plot(simulationOutput, quantreg = FALSE)