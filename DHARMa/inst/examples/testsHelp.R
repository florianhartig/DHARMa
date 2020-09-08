testData = createData(sampleSize = 100, overdispersion = 0.5, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# the plot function runs 4 tests
# i) KS test i) Dispersion test iii) Outlier test iv) quantile test
plot(simulationOutput, quantreg = TRUE)

# testResiduals tests distribution, dispersion and outliers
# testResiduals(simulationOutput)

####### Individual tests #######

# KS test for correct distribution of residuals
testUniformity(simulationOutput)

# Dispersion test
testDispersion(simulationOutput) # tests under and overdispersion
testDispersion(simulationOutput, alternative = "less") # only underdispersion

# if model is refitted, a different test will be called
simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = TRUE, seed = 12)
testDispersion(simulationOutput2)

# often useful to test dispersion per group (e.g. binomial data, see vignette)
simulationOutput3 = recalculateResiduals(simulationOutput, group = testData$group)
testDispersion(simulationOutput3)

# Outlier test (number of observations outside simulation envelope)
# Should set type = "boostrap" for exact values
testOutliers(simulationOutput, type = "binomial") 

# testing zero inflation
testZeroInflation(simulationOutput)

# testing generic summaries
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes) # 1-inflation
testGeneric(simulationOutput, summary = countOnes, alternative = "less") # 1-deficit

means <- function(x) mean(x) # testing if mean prediction fits
testGeneric(simulationOutput, summary = means) 

spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = spread) 




