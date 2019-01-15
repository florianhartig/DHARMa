library(lme4)
library(DHARMa)

# have to run these chunks several time to get a bit of a feeling for the variance

testData = createData(sampleSize = 10000, overdispersion = 0.0, randomEffectVariance = 1, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
summary(fittedModel)
res = simulateResiduals(fittedModel)
plot(res)

testData = createData(sampleSize = 10000, overdispersion = 0.0, randomEffectVariance = 1, family = gaussian())
fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData)
summary(fittedModel)
res = simulateResiduals(fittedModel)
plot(res)

# without REML

testData = createData(sampleSize = 10000, overdispersion = 0.0, randomEffectVariance = 1, family = gaussian())
fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData, REML = F)
summary(fittedModel)
res = simulateResiduals(fittedModel)
plot(res)
