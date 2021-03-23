testData = createData(sampleSize = 200, family = poisson(), 
                      randomEffectVariance = 1, numGroups = 10)
fittedModel <- glm(observedResponse ~ Environment1, 
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

#############  residual plots  ###############

# rank transformation, using a simulationOutput
plotResiduals(simulationOutput, rank = TRUE, quantreg = FALSE)

# smooth scatter plot - usually used for large datasets, default for n > 10000
plotResiduals(simulationOutput, rank = TRUE, quantreg = FALSE, smoothScatter = TRUE)

# residual vs predictors, using explicit values for pred, residual 
plotResiduals(simulationOutput, form = testData$Environment1, 
              quantreg = FALSE)

# if pred is a factor, or if asFactor = T, will produce a boxplot
plotResiduals(simulationOutput, form = testData$group)

# All these options can also be provided to the main plotting function

# If you want to plot summaries per group, use
simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput, quantreg = FALSE) 
# we see one residual point per RE


