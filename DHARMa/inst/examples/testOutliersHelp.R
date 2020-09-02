set.seed(123)

testData = createData(sampleSize = 200, overdispersion = 1, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# default outlier test (with plot)
testOutliers(simulationOutput)

# note that default is to test outliers at both margins for both an excess and a lack
# of outliers. Here we see that we mostly have an excess of outliers at the upper
# margin. You see that it is an excess because the frequency of outliers is 0.055,
# while expected is 0.008

# Let's see what would have happened if we would just have checked the lower margin
testOutliers(simulationOutput, margin = "lower", plot = FALSE)

# OK, now the frequency of outliers is 0, so we have too few, but this is n.s. against
# the expectation

# just for completeness, what would have happened if we would have checked both
# margins, but just for a lack of outliers (i.e. underdispersion)

testOutliers(simulationOutput, alternative = "less", plot = FALSE)
