library(DHARMa)

dat = createData(replicates = 1, sampleSize = 300, intercept = 0,
                 fixedEffects = 1, quadraticFixedEffects = 2, 
                 randomEffectVariance = 0, family = poisson())

fit = glm(observedResponse ~ Environment1 , data = dat, family = poisson)
res = simulateResiduals(fit)
plot(res)
