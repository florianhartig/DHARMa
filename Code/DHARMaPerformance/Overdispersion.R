library(sjstats) 
library(DHARMa)
library(lme4)
library(performance)

testData = createData(sampleSize = 1000, overdispersion = 0.5)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                    data = testData, family = poisson)
summary(fittedModel)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), 
                     data = testData, family = poisson)
summary(fittedModel)


simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 2000)

testDispersion(simulationOutput)
performance::check_overdispersion(fittedModel)


simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 2000, refit = T)
testDispersion(simulationOutput)