context("DHARMa tests")

# erase? library(stringr)

test_that("overdispersion recognized", {
  
  set.seed(123)
  
  testData = createData(sampleSize = 200, overdispersion = 3, pZeroInflation = 0.4, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  x = testUniformity(simulationOutput, alternative = "greater")
  expect_true(  x$p.value < 0.05)
  x = testOutliers(simulationOutput, alternative = "greater")
  expect_true(  x$p.value < 0.05)
  x = testDispersion(simulationOutput, alternative = "greater")
  expect_true(  x$p.value < 0.05)
  x = testZeroInflation(simulationOutput, alternative = "greater")
  expect_true(  x$p.value < 0.05)  
  
})




test_that("tests work", {
  
  # creating test data
  
  testData = createData(sampleSize = 200, overdispersion = 0.5, pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  ###### Distribution tests #####
  
  testUniformity(simulationOutput)
  testUniformity(simulationOutput, alternative = "less")
  testUniformity(simulationOutput, alternative = "greater")
  
  ###### Dispersion tests #######
  
  testDispersion(simulationOutput)
  testDispersion(simulationOutput, alternative = "less")
  testDispersion(simulationOutput, alternative = "greater")
  testDispersion(simulationOutput, alternative = "two.sided", plot = FALSE)
  
  testOverdispersion(simulationOutput)
  testOverdispersionParametric(simulationOutput)
  
  ###### Both together###########
  
  testResiduals(simulationOutput)
  testSimulatedResiduals(simulationOutput)
  
  ###### zero-inflation ##########
  
  # testing zero inflation
  testZeroInflation(simulationOutput)
  testZeroInflation(simulationOutput, alternative = "less")
  
  # testing generic summaries
  countOnes <- function(x) sum(x == 1)  # testing for number of 1s
  testGeneric(simulationOutput, summary = countOnes) # 1-inflation
  testGeneric(simulationOutput, summary = countOnes, alternative = "less") # 1-deficit
  
  means <- function(x) mean(x) # testing if mean prediction fits
  testGeneric(simulationOutput, summary = means) 
  
  spread <- function(x) sd(x) # testing if mean sd fits
  testGeneric(simulationOutput, summary = spread) 
  
  ##################################################################
  
  # grouped
  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)
  
  ###### Distribution tests #####
  
  testUniformity(simulationOutput)
  testUniformity(simulationOutput, alternative = "less")
  testUniformity(simulationOutput, alternative = "greater")
  
  ###### Dispersion tests #######
  
  testDispersion(simulationOutput)
  testDispersion(simulationOutput, alternative = "less")
  testDispersion(simulationOutput, alternative = "greater")
  testDispersion(simulationOutput, alternative = "two.sided", plot = T)
  
  testOverdispersion(simulationOutput)
  testOverdispersionParametric(simulationOutput)
  
  ###### Both together###########
  
  testResiduals(simulationOutput)
  testSimulatedResiduals(simulationOutput)
  
  ###### zero-inflation ##########
  
  # testing zero inflation
  testZeroInflation(simulationOutput)
  testZeroInflation(simulationOutput, alternative = "less")
  
  # testing generic summaries
  countOnes <- function(x) sum(x == 1)  # testing for number of 1s
  testGeneric(simulationOutput, summary = countOnes) # 1-inflation
  testGeneric(simulationOutput, summary = countOnes, alternative = "less") # 1-deficit
  
  means <- function(x) mean(x) # testing if mean prediction fits
  testGeneric(simulationOutput, summary = means) 
  
  spread <- function(x) sd(x) # testing if mean sd fits
  testGeneric(simulationOutput, summary = spread) 
  
  
  
  ################################################################
  
  ###### Refited ##############
  
  # if model is refitted, a different test will be called
  
  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
  testDispersion(simulationOutput)
  
  
  # Standard use
  testSpatialAutocorrelation(simulationOutput, x =  testData$x, y = testData$y)
  testSpatialAutocorrelation(simulationOutput, x =  testData$x, y = testData$y, alternative = "two.sided")
  
  # If x and y is not provided, random values will be created
  testSpatialAutocorrelation(simulationOutput)
  
  # Alternatively, one can provide a distance matrix
  dM = as.matrix(dist(cbind(testData$x, testData$y)))
  testSpatialAutocorrelation(simulationOutput, distMat = dM)
  
  
  # Standard use
  testTemporalAutocorrelation(simulationOutput, time =  testData$time)
  testTemporalAutocorrelation(simulationOutput, time =  testData$time, alternative = "greater")
  
  # If no time is provided, random values will be created
  testTemporalAutocorrelation(simulationOutput)
  
  ##################################################################
  
  # grouped
  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)
  
  # if model is refitted, a different test will be called
  
  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
  testDispersion(simulationOutput)
  
  
  # Standard use
  testSpatialAutocorrelation(simulationOutput, x =  testData$x, y = testData$y)
  testSpatialAutocorrelation(simulationOutput, x =  testData$x, y = testData$y, alternative = "two.sided")
  
  # If x and y is not provided, random values will be created
  testSpatialAutocorrelation(simulationOutput)
  
  # Alternatively, one can provide a distance matrix
  dM = as.matrix(dist(cbind(testData$x, testData$y)))
  testSpatialAutocorrelation(simulationOutput, distMat = dM)
  
  
  # Standard use
  testTemporalAutocorrelation(simulationOutput, time =  testData$time)
  testTemporalAutocorrelation(simulationOutput, time =  testData$time, alternative = "two.sided")
  
  # If no time is provided, random values will be created
  testTemporalAutocorrelation(simulationOutput)
  
})







