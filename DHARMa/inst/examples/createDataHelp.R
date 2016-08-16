testData = createData(sampleSize = 500, intercept = 2, fixedEffects = c(1), 
  overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3), 
  randomEffectVariance = 0)

par(mfrow = c(1,2))
plot(testData$Environment1, testData$observedResponse)
hist(testData$observedResponse)

# with zero-inflation

testData = createData(sampleSize = 500, intercept = 2, fixedEffects = c(1), 
  overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3), 
  randomEffectVariance = 0, pZeroInflation = 0.6)

par(mfrow = c(1,2))
plot(testData$Environment1, testData$observedResponse)
hist(testData$observedResponse)

