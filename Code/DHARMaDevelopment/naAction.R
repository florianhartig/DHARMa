library(DHARMa)
testData = createData(sampleSize = 300, family = gaussian())
testData$observedResponse[4] = NA
rownames(testData) = NULL

fittedModel <- lm(observedResponse ~ Environment1, data = testData)



res = simulateResiduals(fittedModel)

# This will not work
res2 = recalculateResiduals(res, group = testData$group)

# This won't work either
plotResiduals(res$scaledResiduals, testData$Environment1)

# in principle, we see the NAs that have been removed in the row names of 
rownames(model.frame(fittedModel))

# could use this to check in DHARMa and select accordingly