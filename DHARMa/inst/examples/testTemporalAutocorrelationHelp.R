testData = createData(sampleSize = 40, family = gaussian())
fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# Standard use
testTemporalAutocorrelation(res, time =  testData$time)

# If no time is provided, random values will be created
testTemporalAutocorrelation(res)

# If you have several observations per time step

timeSeries1 = createData(sampleSize = 40, family = gaussian())
timeSeries1$location = 1
timeSeries2 = createData(sampleSize = 40, family = gaussian())
timeSeries2$location = 2
testData = rbind(timeSeries1, timeSeries2)

fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# for this, you cannot do testTemporalAutocorrelation(res, time = testData$time)
# because here we would have observations with the same time, i.e. 
# zero difference in time. We have two options a) aggregate observations
# b) calculate / test per subset. Testing per subset might also be useful
# if you have several locations, regardless of whether the times are 
# identical, because you would expect the autocorrelation structure to be 
# independent per location

# testing grouped residuals 

res = recalculateResiduals(res, group = testData$time)
testTemporalAutocorrelation(res, time = unique(testData$time))

# plotting and testing per subgroup

# extract subgroup
testData$Residuals = res$scaledResiduals
temp = testData[testData$location == 1,]

# plots and tests
plot(Residuals ~ time, data = temp)
lmtest::dwtest(temp$Residuals ~ 1, order.by = temp$time)

