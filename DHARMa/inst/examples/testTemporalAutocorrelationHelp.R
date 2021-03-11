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

