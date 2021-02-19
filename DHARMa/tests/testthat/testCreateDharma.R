
context("DHARMa object creations and re-calculations")



test_that("createDHARMa works", {
  
  testData = createData(sampleSize = 200, family = poisson(), 
                        randomEffectVariance = 0, numGroups = 5)
  fittedModel <- glm(observedResponse ~ Environment1, 
                     family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  
  truth = testData$observedResponse
  pred = simulationOutput$fittedPredictedResponse
  simulatedResponse = simulationOutput$simulatedResponse
    
  sim = createDHARMa(simulatedResponse = simulatedResponse, 
                     observedResponse = truth, 
                     fittedPredictedResponse = pred,  
                     integerResponse = T)
  plot(sim, quantreg = F)

  sim = createDHARMa(simulatedResponse = simulatedResponse, 
                     observedResponse = truth, 
                     fittedPredictedResponse = pred,  
                     integerResponse = F)
  plot(sim, quantreg = F)
  
  sim = createDHARMa(simulatedResponse = simulatedResponse, observedResponse = truth)
  plot(sim, quantreg = F)

  
})



test_that("RecalcuateResiduals works", {
  
  # set.seed(123)
  
  testData = createData(sampleSize = 200, family = poisson(), numGroups = 20, 
                        randomEffectVariance = 1)
  
  #len = sum(testData$group == 1)
  #testData$group[testData$group == 1] = sample(c(1,21),20, replace = T)
  
  fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
  
  simulationOutput = simulateResiduals(fittedModel)
  plot(simulationOutput, quantreg = F)
  dim(simulationOutput$simulatedResponse)
  
  ##### only grouping #####
  
  simulationOutput2 = recalculateResiduals(simulationOutput, group = testData$group)
  plot(simulationOutput2, quantreg = F)
  
  ##### only selection ##### 
  
  # simulationOutput2 = recalculateResiduals(simulationOutput, sel = 1) # doesn't work, doesn't have to work!
  
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = 1:2))) == 2)
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = c(1,6,8)))) == 3)
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = testData$group == 1))) == 10)
  
  ##### selection and grouping #####
  
  # only i data point and group, doesn't work
  simulationOutput2 = recalculateResiduals(simulationOutput, group = testData$group, sel = 1)
  
})





