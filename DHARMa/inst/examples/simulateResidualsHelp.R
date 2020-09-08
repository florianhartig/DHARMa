library(lme4)

testData = createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# standard plot 
plot(simulationOutput)

# one of the possible test, for other options see ?testResiduals / vignette
testDispersion(simulationOutput)

# the calculated residuals can be accessed via 
residuals(simulationOutput)

# transform residuals to other pdf, see ?residuals.DHARMa for details
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))

# calculating residuals per group
simulationOutput = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput, quantreg = FALSE)



