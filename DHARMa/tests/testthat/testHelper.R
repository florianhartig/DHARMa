library(DHARMa)
library(testthat)

context("Helper tests")

test_that("ensureDHARMa", {
  
  set.seed(123)
  
  testData = createData(sampleSize = 200, overdispersion = 3, pZeroInflation = 0.4, randomEffectVariance = 0)
  
  pred = testData$Environment1
  
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  expect_s3_class(DHARMa:::ensureDHARMa(simulationOutput), "DHARMa")  
  expect_error(DHARMa:::ensureDHARMa(simulationOutput$scaledResiduals), "DHARMa")  
  expect_error(DHARMa:::ensureDHARMa(fittedModel), "DHARMa") 

  expect_s3_class(DHARMa:::ensureDHARMa(simulationOutput, convert = T), "DHARMa")    
  expect_s3_class(DHARMa:::ensureDHARMa(simulationOutput$scaledResiduals, convert = T), "DHARMa")  
  expect_s3_class(DHARMa:::ensureDHARMa(fittedModel, convert = T), "DHARMa") 
  expect_error(DHARMa:::ensureDHARMa(matrix(rnorm(100), nrow = 4), convert = T))
  expect_error(DHARMa:::ensureDHARMa(list(c = 1), convert = T)) 
  
  expect_s3_class(DHARMa:::ensureDHARMa(fittedModel, convert = "Model"), "DHARMa") 
  expect_error(DHARMa:::ensureDHARMa(simulationOutput$scaledResiduals, convert = "Model"))   
  expect_error(DHARMa:::ensureDHARMa(matrix(rnorm(100))), "DHARMa") 
  

  
  DHARMa:::ensurePredictor(simulationOutput, predictor = pred)
  DHARMa:::ensurePredictor(simulationOutput)
  DHARMa:::ensurePredictor(simulationOutput, predictor = testData$observedResponse)  
  expect_error(DHARMa:::ensurePredictor(simulationOutput, predictor = c(1,2,3)))  
  
  
  

  # testResiduals tests distribution, dispersion and outliers
  
  testOutliers(simulationOutput$scaledResiduals)
  
  expect_error(testQuantiles(simulationOutput$scaledResiduals))  

  
  
  
})
