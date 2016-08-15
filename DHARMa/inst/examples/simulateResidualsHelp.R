testData = createData(sampleSize = nData, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)
