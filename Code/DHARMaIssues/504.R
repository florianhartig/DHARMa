# problem with the package AlDesign that overrides the default model.matrix.default from stats and it is used by lmtest::dwtest() in testTemporalAutocorrelation


library(DHARMa)

timeSeries1 = createData(sampleSize = 40, family = gaussian(),
                         randomEffectVariance = 0)
timeSeries1$location = 1
timeSeries2 = createData(sampleSize = 40, family = gaussian(),
                         randomEffectVariance = 0)
timeSeries2$location = 2
testData = rbind(timeSeries1, timeSeries2)

fittedModel <- lm(observedResponse ~ Environment1, data = testData)
res = simulateResiduals(fittedModel)

# aggregating residuals by time
res = recalculateResiduals(res, group = testData$time)
testTemporalAutocorrelation(res, time = unique(testData$time))


library(agricolae)
testTemporalAutocorrelation(res, time = as.numeric(unique(testData$time)))


# solution

assignInNamespace("model.matrix.formula", stats::model.matrix.default, ns = "AlgDesign" )

testTemporalAutocorrelation(res, time = as.numeric(unique(testData$time)))

# it doesn't work with detaching agricolae!
