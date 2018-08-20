

testData = createData(sampleSize = 200, family = poisson(), numGroups = 40, 
                      randomEffectVariance = 2)

#len = sum(testData$group == 1)
#testData$group[testData$group == 1] = sample(c(1,21),20, replace = T)

fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)

simulationOutput = simulateResiduals(fittedModel)
plot(simulationOutput)
dim(simulationOutput$simulatedResponse)

simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput)


testData = createData(sampleSize = 200, family = gaussian())
fittedModel <- glm(observedResponse ~ Environment1, family = "gaussian", data = testData)
simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)


