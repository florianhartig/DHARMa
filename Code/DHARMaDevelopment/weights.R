testData = createData(sampleSize = 200, overdispersion = 0, family = gaussian())

fit <- lmer(observedResponse ~ Environment1 + (1|group), weights = 1:200, 
                    data = testData)

model.frame(fit)


simulationOutput <- simulateResiduals(fittedModel = fittedModel)
