returnStatistics <- function(control = 0){
  testData = createData(sampleSize = 20, family = poisson(), overdispersion = control, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c(testUniformity(res, plot = F)$p.value, testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

# testing a single return
returnStatistics()

# running benchmark
out = runBenchmarks(returnStatistics, nRep = 20)

# running benchmark parallel
out = runBenchmarks(returnStatistics, nRep = 500, parallel = TRUE)

# include control values
out = runBenchmarks(returnStatistics, controlValues = c(0,0.5,1), nRep = 20)



