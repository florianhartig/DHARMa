# This was related to https://github.com/florianhartig/DHARMa/issues/47 and https://github.com/florianhartig/DHARMa/issues/48


library(lme4)

testData = createData(sampleSize = 300, overdispersion = 2, randomEffectVariance = 0, family = poisson())

##################################################
# fitted poisson - clear overdispersion
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = 
                       "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput, rank = T)

testDispersion(simulationOutput)

simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T)
plot(simulationOutput2)

testDispersion(simulationOutput2)

testDispersion(fittedModel)


##################################################
# fitted glmer.nb
fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput, rank = T)

# still doesn't fit 100%, but this is expected, as the data wasn't create with a neg binom distribution

testOverdispersion(simulationOutput)

# sometimes significant, apparently because of misfit, because see below 
simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T)
testOverdispersion(simulationOutput2)

# doesn't work
testOverdispersionParametric(fittedModel)


##### true glmer.nb response

testData$response2 = simulationOutput$simulatedResponse[,1]
fittedModel <- glmer.nb(response2 ~ Environment1 + (1|group) , data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput, rank = T)

testOverdispersion(simulationOutput)
simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T)
testOverdispersion(simulationOutput2)

testOverdispersionParametric(fittedModel)

