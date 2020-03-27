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

# if there are multiple observations with the same x values,
# create first ar group with unique values for each location
# then aggregate the residuals per location, and calculate
# spatial autocorreation on the new group

res2 = recalculateResiduals(res, group = testData$group)
testSpatialAutocorrelation(res)


# carefull when using REs to account for spatially clustered (but not grouped)
# data. this originates from https://github.com/florianhartig/DHARMa/issues/81


# Assume our data is divided into clusters, where observations are close together
# but not at te same point, and we suspect that observations in clusters are
# autocorrelated

clusters = 100
subsamples = 10
size = clusters * subsamples

testData = createData(sampleSize = size, family = gaussian(), numGroups = clusters )
testData$x  = rnorm(clusters)[testData$group] + rnorm(size, sd = 0.01)
testData$y  = rnorm(clusters)[testData$group] + rnorm(size, sd = 0.01)

# It's a good idea to use a RE to take out the cluster effects. This accounts
# for the autocorrelation within clusters 

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

# For lme4, it's also possible to simulated residuals conditional on fitted 
# REs (re.form). Conditional on the fitted REs (i.e. accounting for the clusters)
# the residuals should now be indepdendent. The remaining RSA we see here is 
# probably due to the RE shrinkage

res = simulateResiduals(fittedModel, re.form = NULL)
testSpatialAutocorrelation(res, x =  testData$x, y = testData$y)