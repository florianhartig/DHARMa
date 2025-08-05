library(lme4)

testData = createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group),
                     family = "poisson", data = testData)

# simulate residuals (default behavior, conditional on the fitted random effects)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# simulate residuals unconditional on the fitted random effects (REs are re-simulated)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, simulateREs = "unconditional")

# simulate residuals user-specified using lme4 syntax (e.g. conditional only on a specific RE)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, simulateREs = "user-specified", re.form = ~(1|group))

# standard plot
plot(simulationOutput)

# one of the possible test, for other options see ?testResiduals / vignette
testDispersion(simulationOutput)

# the calculated residuals can be accessed via
residuals(simulationOutput)

# transform residuals to other pdf, see ?residuals.DHARMa for details
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))

# get residuals that are outside the simulation envelope
outliers(simulationOutput)

# calculating aggregated residuals per group
simulationOutput2 = recalculateResiduals(simulationOutput, group = testData$group)
plot(simulationOutput2, quantreg = FALSE)

# calculating residuals only for subset of the data
simulationOutput3 = recalculateResiduals(simulationOutput, sel = testData$group == 1 )
plot(simulationOutput3, quantreg = FALSE)
