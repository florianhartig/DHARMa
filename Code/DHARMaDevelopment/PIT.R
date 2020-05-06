library(lme4)

testData = createData(sampleSize = 200, family = binomial())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group),
                     family = "binomial", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel, method = "PIT", plot = T)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, method = "traditional", plot = T)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, method = "df", plot = T)



