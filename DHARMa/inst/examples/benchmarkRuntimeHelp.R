
createModel = function(){
  testData = createData(family = poisson(), overdispersion = 1,
                        randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  return(fittedModel)
}

a = function(m){
  testUniformity(m, plot = FALSE)$p.value
}

b = function(m){
  testDispersion(m, plot = FALSE)$p.value
}

evaluationFunctions = list(a,b)

benchmarkRuntime(createModel, evaluationFunctions, 2)
