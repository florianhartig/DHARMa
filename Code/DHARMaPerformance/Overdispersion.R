library(sjstats) 
library(DHARMa)
library(lme4)
library(performance)

testData = createData(sampleSize = 1000, overdispersion = 1)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                    data = testData, family = poisson)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

testDispersion(simulationOutput)
performance::check_overdispersion(fittedModel)
