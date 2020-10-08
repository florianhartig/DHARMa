# 

n = 10

mean((replicate(1000000, runif(1) < min(runif(n)))))

1/11


# Original test of the problem

testData = createData(sampleSize = 5000, fixedEffects = 1, family = binomial(), randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ 1, family = "binomial", data = testData)

res <- simulateResiduals(fittedModel = fittedModel)
testOutliers(res)


# additional tests


testData = createData(sampleSize = 50000, fixedEffects = 1, family = binomial(), randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ 1, family = "binomial", data = testData)

res <- simulateResiduals(fittedModel = fittedModel)
testOutliers(res, type = "binomial")
testOutliers(res)

testData = createData(sampleSize = 50000, fixedEffects = 1, family = poisson(), randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
res <- simulateResiduals(fittedModel = fittedModel)
testOutliers(res, type = "binomial")
testOutliers(res)