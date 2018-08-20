testData = createData(sampleSize = 40, family = gaussian())
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
testSpatialAutocorrelation(res, x =  testData$x, y = testData$y)

# If x and y is not provided, random values will be created
testSpatialAutocorrelation(res)

# Alternatively, one can provide a distance matrix
dM = as.matrix(dist(cbind(testData$x, testData$y)))
testSpatialAutocorrelation(res, distMat = dM)

# carefull with clustered data and conditional / unconditional simulations
# this originates from https://github.com/florianhartig/DHARMa/issues/81

# Assume our data is divided into clusters, and we use a RE to take out cluster effects

clusters = 100
subsamples = 10
size = clusters * subsamples

testData = createData(sampleSize = size, family = gaussian(), numGroups = clusters )
testData$x  = rnorm(clusters)[testData$group] + rnorm(size, sd = 0.01)
testData$y  = rnorm(clusters)[testData$group] + rnorm(size, sd = 0.01)

library(lme4)
fittedModel <- lmer(observedResponse ~ Environment1 + (1|group), data = testData)

# DHARMa default is to re-simulted REs - this means spatial pattern remains
# because residuals are still clustered

res = simulateResiduals(fittedModel)
testSpatialAutocorrelation(res, x =  testData$x, y = testData$y)

# However, it should disappear if you just calculate an aggregate residuals per cluster
# Because at least how the data are simualted, cluster are spatially independent

res2 = recalculateResiduals(res, group = testData$group)
testSpatialAutocorrelation(res2, 
                           x =  aggregate(testData$x, list(testData$group), mean)$x, 
                           y = aggregate(testData$y, list(testData$group), mean)$x)

# For lme4, possible to simulated residuals conditional on fitted REs (re.form)
# This takes out most of the RSA - a remainder is probably due the shrinkage
# of the REs

res = simulateResiduals(fittedModel, re.form = NULL)
testSpatialAutocorrelation(res, x =  testData$x, y = testData$y)



