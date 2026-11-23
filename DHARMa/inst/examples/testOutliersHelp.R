set.seed(123)

testData = createData(sampleSize = 200, overdispersion = 1, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# default outlier test (with plot)
testOutliers(simulationOutput)

# the default test is binomial, except for some residuals with method = "traditional", see help
testOutliers(simulationOutput, type = "binomial") 
testOutliers(simulationOutput, type = "bootstrap") 

# note that default is to test outliers at BOTH margins for both an excess AND a lack
# of outliers. In the case above, the test reported an excess of outliers (you)
# can see this because expected frequency < observed. Moreover, if we plot the residuals

plotResiduals(simulationOutput, quantreg = FALSE)

# we see that we mostly have an excess of outliers at the upper margin. 
# Let's see what would have happened if we would just have checked the lower margin
# (lower margin means residuals < 1/(nSim + 1), i.e. lower tail of the simulation
# envelope)

testOutliers(simulationOutput, margin = "lower", plot = FALSE)

# OK, now the frequency of outliers is 0, so we have too few, but this is n.s. against
# the expectation

# just for completeness, what would have happened if we would have checked both
# margins, but just for a lack of outliers (sign of underdispersion)

testOutliers(simulationOutput, alternative = "less", plot = FALSE)


