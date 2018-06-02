
context("createDHARMa")



test_that("Standard creation works", {
  
  testData = createData(sampleSize = 200, family = poisson(), 
                        randomEffectVariance = 0, numGroups = 5)
  fittedModel <- glm(observedResponse ~ Environment1, 
                     family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  
  truth = testData$observedResponse
  pred = simulationOutput$fittedPredictedResponse
  simulatedResponse = simulationOutput$simulatedResponse
    
  sim = createDHARMa(simulatedResponse = simulatedResponse, observedResponse = truth, fittedPredictedResponse = pred,  integerResponse = T)
  plot(sim, quantreg = F)

  sim = createDHARMa(simulatedResponse = simulatedResponse, observedResponse = truth, fittedPredictedResponse = pred,  integerResponse = F)
  plot(sim, quantreg = F)
  
  sim = createDHARMa(scaledResiduals = simulationOutput$scaledResiduals, fittedPredictedResponse = pred,  integerResponse = F)
  plot(sim, quantreg = F)
  
  
})

