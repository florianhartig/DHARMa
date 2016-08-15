testData = createData(sampleSize = 50, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)

# plot directly
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = T)

# create simulations with refitting
simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 10, refit = T, plot = T)
