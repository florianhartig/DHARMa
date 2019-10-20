
set.seed(1)
testData = createData(sampleSize = 200, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = testData)
res <- simulateResiduals(fittedModel = fittedModel)
plot(res$fittedPredictedResponse, transformQuantiles(res))

