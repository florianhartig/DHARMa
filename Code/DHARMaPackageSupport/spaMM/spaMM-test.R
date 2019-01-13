library(spaMM)

data("Loaloa") ## from spaMM package
fittedModel <- corrHLfit(cbind(npos,ntot-npos)~maxNDVI1+seNDVI
                    +Matern(1|longitude+latitude),
                    data=Loaloa[1:30,],family=binomial(),
                    init.corrHLfit=list(Nugget=0.1),ranFix=list(nu=0.5))

testModel(fittedModel)



DHARMa:::getFixedEffects(fittedModel)

res = simulateResiduals(fittedModel)
plot(res)

testData = createData(sampleSize = 10000, overdispersion = 0.0, randomEffectVariance = 1, family = poisson())

fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
summary(fittedModel)
res = simulateResiduals(fittedModel, re.form = ~0)
plot(res)



testData = createData(sampleSize = 200, overdispersion = 0.0, randomEffectVariance = 1, family = poisson())
fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
res = simulateResiduals(fittedModel, refit = T)
plot(res)



testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
fittedModel <- HLfit(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)

testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial())
fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "binomial",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)
testResiduals(res)



