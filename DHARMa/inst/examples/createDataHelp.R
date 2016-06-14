testData = createData(sampleSize = 500, intercept = 1, fixedEffects = c(1), overdispersion = 0, family = poisson(), quadraticFixedEffects = c(-3))

plot(testData$Environment1, testData$observedResponse)
