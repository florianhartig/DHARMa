library(lme4)

testData = createData(sampleSize = 200, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = testData,
                     control=glmerControl(optCtrl=list(maxfun=20000) ))

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# plot residuals, quantreg = T is better but costs more time
plot(simulationOutput, quantreg = FALSE)

# calculating summaries per group
simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput, quantreg = FALSE)

# create simulations with refitting, n=5 is very low, set higher when using this
simulationOutput <- simulateResiduals(fittedModel = fittedModel, 
                                      n = 10, refit = TRUE)
plot(simulationOutput, quantreg = FALSE)

# grouping per random effect group works as above
simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput, quantreg = FALSE)

