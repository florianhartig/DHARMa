set.seed(123)

library(lme4)

testData = createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

referenceData = list()
referenceData$mod1$info = "lme4 Poisson model"
referenceData$mod1$model = fittedModel
referenceData$mod1$residuals = residuals(simulationOutput)


save(referenceData, file = "referenceData.RData")