returnP <- function(counter=NULL){
  testData = DHARMa::createData(sampleSize = 20, family = gaussian())
  fittedModel <- lm(observedResponse ~ Environment1, data = testData)
  res <- DHARMa::simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c(DHARMa::testUniformity(res, plot = FALSE)$p.value, res$scaledResiduals)
  return(out)
}

# testing a single return
returnP()

# running benchmark
out = runBenchmarks(returnP, nRep = 20)

# running benchmark parallel
out = runBenchmarks(returnP, nRep = 500, parallel = TRUE)


returnP <- function(control = 0){
  testData = DHARMa::createData(sampleSize = 20, family = poisson(), overdispersion = control)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  res <- DHARMa::simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- suppressWarnings(c(DHARMa::testUniformity(res, plot = FALSE)$p.value, res$scaledResiduals))
  return(out)
}

# testing a single return
returnP(control = 1)

# running benchmark
out = runBenchmarks(returnP, controlValues = c(0,0.5,1), nRep = 20)

# running benchmark parallel
out = runBenchmarks(returnP, controlValues = c(0,0.5,1), nRep = 500, parallel = TRUE)

