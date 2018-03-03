testData = createData(sampleSize = 200, family = poisson(), 
                      randomEffectVariance = 0, numGroups = 5)
fittedModel <- glm(observedResponse ~ Environment1, 
                   family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

######### main plotting function #############

plot(simulationOutput = simulationOutput)

#############  qq plot  ######################

plotQQunif(simulationOutput = simulationOutput)

#############  residual plots  ###############

# a single residual vs. predicte plot, various options
plotResiduals(simulationOutput, rank = T)
plotResiduals(simulationOutput, quantreg = F)

# residual vs predictors
plotResiduals(pred = testData$Environment1, residuals = simulationOutput$scaledResiduals, quantreg = F)
plotResiduals(pred = testData$time, residuals = simulationOutput$scaledResiduals, quantreg = T)

# factor vs. non factor
plotResiduals(pred = testData$group, residuals = simulationOutput$scaledResiduals, quantreg = F, asFactor = F)
plotResiduals(pred = testData$group, residuals = simulationOutput$scaledResiduals, quantreg = F, asFactor = T)

