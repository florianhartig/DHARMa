testData = createData(sampleSize = 200, family = poisson(),
                      fixedEffects = c(1,2),
                      randomEffectVariance = 1, numGroups = 10)
testData$Environment2[1] = NA
testData$categorPred = as.factor(rep(1:10, times = 20))
# simulationOutput <- simulateResiduals(glm(observedResponse ~ Environment1 + Environment2, family = "poisson", data = testData))


fittedModel = lme4::glmer(observedResponse ~ Environment1 + Environment2 + categorPred +  (1|group),
                          family = "poisson", data = testData)

simulationOutput = simulateResiduals(fittedModel)

# plotResiduals
plotResiduals(simulationOutput, quantreg  = NULL)
plotResiduals(simulationOutput, form = testData$Environment1) # expect error
plotResiduals(simulationOutput, form = testData$Environment1[-1])
plotResiduals(simulationOutput, form = ~Environment1)
plotResiduals(simulationOutput, form = ~categorPred|group == 2)
plotResiduals(simulationOutput, form = ~categorPred)
plotResiduals(simulationOutput, form = ~.)
plotResiduals(simulationOutput, main = "MY TITLE")

# testCategorical
testCategorical(simulationOutput, catPred = ~group)
testCategorical(simulationOutput, catPred = testData$group) # expect error
testCategorical(simulationOutput, testData$group[-1])

# recalculateResiduals
recalculateResiduals(simulationOutput, group = testData$group) # expect error
recalculateResiduals(simulationOutput, group = ~group)
recalculateResiduals(simulationOutput, group = testData$group[-1])

# with sel
recalculateResiduals(simulationOutput, sel = testData$group == 1, group = ~group) # expect error (because sel selects first 10 rows, but there are only 9 rows fitted because of NA)
recalculateResiduals(simulationOutput, sel = (testData$group == 1)[-1], group = ~group)

# if we set Environment1 for all of group = 1 to NA (issue #507)
testData$Environment1[1:20] = NA
fittedModel = lme4::glmer(observedResponse ~ Environment1 + Environment2 + categorPred +  (1|group),
                          family = "poisson", data = testData)
simulationOutput = simulateResiduals(fittedModel)

recalculateResiduals(simulationOutput, sel = testData$group == 1) # expect error
recalculateResiduals(simulationOutput, sel = ~group == 1) # expect error because there is no group 1
recalculateResiduals(simulationOutput, sel = ~group == 2)
recalculateResiduals(simulationOutput, sel = ~Environment1<0.5)
recalculateResiduals(simulationOutput, sel = ~Environment1<0.5, group = ~group)
recalculateResiduals(simulationOutput, sel = 1:20) # expect warning


# testTemporal
testTemporalAutocorrelation(simulationOutput, time = testData$time) # expect error
testTemporalAutocorrelation(simulationOutput, time = testData$time[-1])
testTemporalAutocorrelation(simulationOutput, time = ~time)

# testSpatial
testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y) # expect error
testSpatialAutocorrelation(simulationOutput, x = testData$x[-1], y = testData$y[-1])
testSpatialAutocorrelation(simulationOutput, x = ~x, y = ~y)

dM = as.matrix(dist(cbind(testData$x, testData$y)))
testSpatialAutocorrelation(simulationOutput, x = ~x, y = ~y, distMat = dM) # expect error

dM = as.matrix(dist(cbind(testData$x[-1], testData$y[-1])))
testSpatialAutocorrelation(simulationOutput, x = ~x, y = ~y, distMat = dM) # expect message but no error!

# testQuantiles
testQuantiles(simulationOutput)
testQuantiles(simulationOutput, predictor = testData$Environment1) # expect error
testQuantiles(simulationOutput, predictor = testData$Environment1[-1])
testQuantiles(simulationOutput, predictor = ~Environment1)





