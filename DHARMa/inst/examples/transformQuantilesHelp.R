
set.seed(1)
testData = createData(sampleSize = 200, family = poisson())
fittedModel <- glm(observedResponse ~ Environment1, 
                     family = "poisson", data = testData)
res <- simulateResiduals(fittedModel = fittedModel)
plot(res$fittedPredictedResponse, transformQuantiles(res))

