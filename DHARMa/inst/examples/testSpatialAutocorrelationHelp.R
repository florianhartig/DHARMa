testData = createData(sampleSize = 40, family = gaussian())
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
# specify x,y as formulas (recommended)
testSpatialAutocorrelation(res, x = ~x, y = ~y)

# or as variables in your environment
testSpatialAutocorrelation(res, x =  testData$x, y = testData$y)

# Alternatively, one can provide a distance matrix
dM = as.matrix(dist(cbind(testData$x, testData$y)))
testSpatialAutocorrelation(res, distMat = dM)

# You could add a spatial variogram via
# library(gstat)
# dat = data.frame(res = residuals(res), x = testData$x, y = testData$y)
# coordinates(dat) = ~x+y
# vario = variogram(res~1, data = dat, alpha=c(0,45,90,135))
# plot(vario, ylim = c(-1,1))

# if there are multiple observations with the same x values,
# create first ar group with unique values for each location
# then aggregate the residuals per location, and calculate
# spatial autocorrelation on the new group

# modifying x, y, so that we have the same location per group
# just for completeness
testData$x = as.numeric(testData$group)
testData$y = as.numeric(testData$group)

# calculating x, y positions per group
groupLocations = aggregate(testData[, 6:7], list(testData$group), mean)

# calculating residuals per group
res2 = recalculateResiduals(res, group = ~group)

# running the spatial test on grouped residuals
testSpatialAutocorrelation(res2, groupLocations$x, groupLocations$y)

# careful when using REs to account for spatially clustered (but not grouped)
# data. this originates from https://github.com/florianhartig/DHARMa/issues/81

# Assume our data is divided into clusters, where observations are close together
# but not at the same point, and we suspect that observations in clusters are
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

# Re-simulating REs (unconditional) means spatial pattern remains
# because residuals are still clustered

res = simulateResiduals(fittedModel, simulateREs = "unconditional")
testSpatialAutocorrelation(res, x = ~x, y = ~y)

# However, it should disappear if you just calculate an aggregate residuals per cluster
# Because at least how the data are simulated, cluster are spatially independent

res2 = recalculateResiduals(res, group = ~group)
testSpatialAutocorrelation(res2,
                           x =  aggregate(testData$x, list(testData$group), mean)$x,
                           y = aggregate(testData$y, list(testData$group), mean)$x)

# For lme4 models, it's also possible to simulate residuals conditional on fitted
# REs (DHARMa default). Conditional on the fitted REs (i.e. accounting for the clusters)
# the residuals should now be independent. The remaining RSA we see here is
# probably due to the RE shrinkage

res = simulateResiduals(fittedModel, simulateREs = "conditional")
testSpatialAutocorrelation(res, x = ~x, y = ~y)


