library(lme4)

testData = createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group),
                     family = "poisson", data = testData)

# simulate residuals (default behavior, conditional on the fitted random effects)
simulationOutput1 <- simulateResiduals(fittedModel = fittedModel)
plot(simulationOutput1)

# simulate residuals unconditional on the fitted random effects (REs are re-simulated)
simulationOutput2 <- simulateResiduals(fittedModel = fittedModel,
                                       simulateREs = "unconditional")
plot(simulationOutput2)

# simulate residuals user-specified using lme4 syntax (e.g. conditional only on a specific RE)
simulationOutput3 <- simulateResiduals(fittedModel = fittedModel,
                                       simulateREs = "user-specified",
                                       re.form = ~(1|group))
plot(simulationOutput3)

# one of the possible test, for other options see ?testResiduals / vignette
testDispersion(simulationOutput1)

# the calculated residuals can be accessed via
residuals(simulationOutput1)

# transform residuals to other pdf, see ?residuals.DHARMa for details
residuals(simulationOutput1, quantileFunction = qnorm, outlierValues = c(-7,7))

# get residuals that are outside the simulation envelope
outliers(simulationOutput1)

# calculating aggregated residuals per group
# group here specified as formula (recommended)
# but you can also use a variable from your environment, e.g. testData$group
simulationOutput4 = recalculateResiduals(simulationOutput1, group = ~group)
plot(simulationOutput4, quantreg = FALSE)
# we see one residual point per RE

# with group as variable in your environment
# but then you need to exclude NAs by hand
simulationOutput5 = recalculateResiduals(simulationOutput1, group = testData$group)

# calculating residuals only for subset of the data
simulationOutput6 = recalculateResiduals(simulationOutput1, sel = testData$group == 1)
plot(simulationOutput6, quantreg = FALSE)

# or with sel as formula
simulationOutput7 = recalculateResiduals(simulationOutput1, sel = ~Environment1<0.5)

# as condition based on a variable in your environment
simulationOutput8 = recalculateResiduals(simulationOutput1, sel = testData$Environment1<0.5)

# or combine group and sel
simulationOutput9 = recalculateResiduals(simulationOutput1, group = ~group, sel = ~Environment1<0.5)

# or as subset of rows
# (note: order of rows is based on residuals, not on original dataframe)
# so be careful with NAs!
simulationOutput10 = recalculateResiduals(simulationOutput1, sel = 1:20)
