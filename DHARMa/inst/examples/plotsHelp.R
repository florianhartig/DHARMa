testData = createData(sampleSize = 200, family = poisson(),
                      fixedEffects = c(1,1),
                      randomEffectVariance = 1, numGroups = 10)
testData$Environment2[1] = NA
fittedModel <- glm(observedResponse ~ Environment1 + Environment2,
                   family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

######### main plotting function #############

# for all functions, quantreg = T will be more
# informative, but slower

plot(simulationOutput, quantreg = FALSE)

#############  Distribution  ######################

plotQQunif(simulationOutput = simulationOutput,
           testDispersion = FALSE,
           testUniformity = FALSE,
           testOutliers = FALSE)

hist(simulationOutput )




