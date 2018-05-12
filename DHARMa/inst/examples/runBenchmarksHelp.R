returnP <- function(){
  testData = createData(sampleSize = 20, family = gaussian())
  fittedModel <- lm(observedResponse ~ Environment1, data = testData)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c(testUniformity(res)$p.value, res$scaledResiduals)
  return(out)
}

# testing a single return
returnP()

# running benchmark
out = runBenchmarks(returnP, nRep = 20)

# running benchmark parallel
out = runBenchmarks(returnP, nRep = 500, parallel = T)


returnP <- function(control = 0){
  testData = createData(sampleSize = 20, family = poisson(), overdispersion = control)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- suppressWarnings(c(testUniformity(res)$p.value, res$scaledResiduals))
  return(out)
}

# testing a single return
returnP(control = 1)

# running benchmark
out = runBenchmarks(returnP, controlValues = c(0,0.5,1), nRep = 20)

# running benchmark parallel
out = runBenchmarks(returnP, controlValues = c(0,0.5,1), nRep = 500, parallel = T)

