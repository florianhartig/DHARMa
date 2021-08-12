testData = createData(sampleSize = 40, family = gaussian(), 
                      randomEffectVariance = 0)
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
testTemporalAutocorrelation(res, time =  testData$time)

# If you have several observations per time step, e.g. 
# because you have several locations, you will have to 
# aggregate

timeSeries1 = createData(sampleSize = 40, family = gaussian(), 
                         randomEffectVariance = 0)
timeSeries1$location = 1
timeSeries2 = createData(sampleSize = 40, family = gaussian(), 
                         randomEffectVariance = 0)
timeSeries2$location = 2
testData = rbind(timeSeries1, timeSeries2)

fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Will not work because several residuals per time
# testTemporalAutocorrelation(res, time = testData$time)

# aggregating residuals by time
res = recalculateResiduals(res, group = testData$time)
testTemporalAutocorrelation(res, time = unique(testData$time))

# testing only subgroup location 1, could do same with loc 2
res = recalculateResiduals(res, sel = testData$location == 1)
testTemporalAutocorrelation(res, time = unique(testData$time))

# example to demonstrate problems with strong temporal correlations and
# how to possibly remove them by rotating residuals

\dontrun{

set.seed(1)
C <- exp(-as.matrix(dist(seq(0,50,by=.1))))
obs <- as.numeric(mvtnorm::rmvnorm(1,sigma=C))

opar <- par(mfrow = c(1,2))
image(C, main = "Specified autocorrelation (covariance)")
plot(obs, type = "l", main = "Time series")
par(opar)

# calculate standard DHARMa residuals

## simulations from the model:
x = replicate(1000, as.numeric(mvtnorm::rmvnorm(1,sigma=C)))

res <- createDHARMa(x, obs, integerResponse = F)
testUniformity(res)
testTemporalAutocorrelation(res, time = 1:length(res$scaledResiduals))

# calculated rotated DHARMa residuals to remove temporal correlation
# this only works if the autocorrelation is homogeneous / stationary
res <- createDHARMa(x, obs, integerResponse = F, rotation = C)
testUniformity(res)
testTemporalAutocorrelation(res, time = 1:length(res$scaledResiduals))

# the same, but with a covariance based on simulations
res <- createDHARMa(x, obs, integerResponse = F, rotation = "approximated")
testUniformity(res)
testTemporalAutocorrelation(res, time = 1:length(res$scaledResiduals))


}
