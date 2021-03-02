
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
  simulationOutput2 = recalculateResiduals(simulationOutput, sel = 1:400) 
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = 1:2))) == 2)
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = c(1,6,8)))) == 3)
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = testData$group == 1))) == 10) 
  
  ##### selection and grouping #####
  # selection by different variables
  simulationOutput2 = recalculateResiduals(simulationOutput, sel = testData$time) 
  plot(simulationOutput2, quantreg = F)
  
  
  #selecting = float   -   wrong dimensions (Sel has length 0)
  # testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = 1.0 :20.0))) == 20) 
  # simulationOutput2 = recalculateResiduals(simulationOutput, group = testData$group, sel = 1.0 : 10.0)  # not working, don´t has to work  
  
  
  # testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = 1.5 :10.7))) == 10) # not checking float 
  # simulationOutput2 = recalculateResiduals(simulationOutput, group = testData$group, sel = 1.5:10.6)    # not working, don´t has to work 
  
  
  # selecting = negative Values
  # testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = -10 : 5))) == 194) # not working, don´t has to work 
  testthat::expect_true( length( residuals(recalculateResiduals(simulationOutput, sel = -10 : -5))) == 194)
  simulationOutput3 = recalculateResiduals(simulationOutput, sel =  -10 : -3)
  
  
  # checking NULL functions
  simulationOutput3 = recalculateResiduals(simulationOutput, sel =  NULL)
  # simulationOutput3 = recalculateResiduals(simulationOutput, seed = NULL) not working, don´t has to work
  simulationOutput3 = recalculateResiduals(simulationOutput, seed = 0)
  simulationOutput3 = recalculateResiduals(simulationOutput, group = NULL, sel = NULL)
  simulationOutput3 = recalculateResiduals(simulationOutput, group = NULL, sel = c(1:5,8,9), method = "PIT")
  simulationOutput3 = recalculateResiduals(simulationOutput, group = NULL, sel = c(1:5,8,9), method = "traditional")
  
  
  # checking for 
  simulationOutput4 = recalculateResiduals(simulationOutput, group = 1:simulationOutput$nObs, sel = simulationOutput$nObs/2:simulationOutput$nObs, method = "traditional")
  plot(simulationOutput4)
  simulationOutput4 = recalculateResiduals(simulationOutput, group = 1:simulationOutput$nObs, sel = simulationOutput$nObs/3.5:simulationOutput$nObs, method = "PIT")
  plot(simulationOutput4)
  
  
})



