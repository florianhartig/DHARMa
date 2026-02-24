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

#############  residual plots  ###############

# Default in DHARMa is to show predictions rank transformed
# if you want plots based on raw predictions, use rank = F
plotResiduals(simulationOutput, rank = FALSE, quantreg = FALSE)

# smooth scatter plot - default for large datasets with n > 10000
plotResiduals(simulationOutput, rank = TRUE, quantreg = FALSE, smoothScatter = TRUE)

# It is very advisable to plot the residual against all predictors
# the following syntax uses the predictor values from the fitted model
plotResiduals(simulationOutput, form = ~ Environment1,
              quantreg = FALSE)


# plot against all predictors
plotResiduals(simulationOutput, form = ~.,
              quantreg = FALSE)

# if pred is a factor, or if asFactor = TRUE, will produce a boxplot
plotResiduals(simulationOutput, form = ~group)

# plot residuals against a predictor for a specific group level, here group 1
plotResiduals(simulationOutput, form = ~Environment1|group == "1")

# or in a grid for multiple group levels, e.g. groups 1 to 4
par(mfrow= c(2,2))
for(g in unique(testData$group)[1:4]) {
  plotResiduals(simulationOutput, form = ~Environment1|group == g, xlab = paste("group", g))
}
par(mfrow= c(1,1))




# alternatively, you can plot against a variable from the global environment
# in this case, the model function automatically removed one row of observations
# because there was an NA in Environment2. When using the variable Environment1
# from the global environment, we have to remove this observation as well
plotResiduals(simulationOutput,
              form = testData$Environment1[complete.cases(testData)],
              quantreg = FALSE)



# to diagnose overdispersion and heteroskedasticity it can be useful to
# display residuals as absolute deviation from the expected mean 0.5
plotResiduals(simulationOutput, absoluteDeviation = TRUE, quantreg = FALSE)

# All these options can also be provided to the main plotting function

# If you want to plot summaries per group, use
simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput, quantreg = FALSE)
# we see one residual point per RE


