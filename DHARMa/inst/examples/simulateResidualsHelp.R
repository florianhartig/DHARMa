library(lme4)

testData = createData(sampleSize = 200, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = testData,
                     control=glmerControl(optCtrl=list(maxfun=20000) ))

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plotSimulatedResiduals(simulationOutput = simulationOutput)

# plot directly
simulationOutput <- simulateResiduals(fittedModel = fittedModel, plot = TRUE)

# create simulations with refitting, n=10 is too low, set higher when using this

simulationOutput <- simulateResiduals(fittedModel = fittedModel, 
                                      n = 10, refit = TRUE, plot = TRUE)
