context("DHARMa tests")

# erase? library(stringr)

test_that("overdispersion recognized", {

  set.seed(123)

  testData = createData(sampleSize = 200, overdispersion = 3, pZeroInflation = 0.4, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  x = testUniformity(simulationOutput, plot = F)
  expect_true(  x$p.value < 0.05)
  x = testOutliers(simulationOutput, plot = F)
  expect_true(  x$p.value < 0.05)
  x = testDispersion(simulationOutput, alternative = "greater", plot = F)
  expect_true(  x$p.value < 0.05)
  x = testZeroInflation(simulationOutput, alternative = "greater", plot = F)
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


  ###### Refited ##############

  # if model is refitted, a different test will be called

  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
  testDispersion(simulationOutput)



})



###### Correlation tests #####

test_that("correlation tests work", {
  
  testData = createData(sampleSize = 200, overdispersion = 0.5, pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  # grouped
  simulationOutputGrouped <- recalculateResiduals(simulationOutput, group = testData$group)
    
  ###### testSpatialAutocorrelation #####

  # Standard use
  testSpatialAutocorrelation(simulationOutput, x =  testData$x, y = testData$y)
  testSpatialAutocorrelation(simulationOutput, x =  testData$x, y = testData$y, alternative = "two.sided")
  
  # If x and y is not provided, random values will be created
  expect_error(testSpatialAutocorrelation(simulationOutput))
  
  # Alternatively, one can provide a distance matrix
  dM = as.matrix(dist(cbind(testData$x, testData$y)))
  testSpatialAutocorrelation(simulationOutput, distMat = dM)
  testSpatialAutocorrelation(simulationOutput, distMat = dM , alternative = "two.sided")
  
  # testting when x and y have different length
  #testSpatialAutocorrelation(simulationOutput, x =  testData$x[1:10], y = testData$y[1:9] )      # Error different length  
  #testSpatialAutocorrelation(simulationOutput[1:10], x =  testData$x[1:10], y = testData$y[1:10] ) # causing Error since x and y have equal length but unequal to simulationOutput
  # see Issue #190  'https://github.com/florianhartig/DHARMa/issues/190' 
  
  # testing distance matrix and an extra x or y argument 
  testSpatialAutocorrelation(simulationOutput, distMat = dM, x = testData$x)
  testSpatialAutocorrelation(simulationOutput, distMat = dM, y = testData$y)
  
  
  ###### testTemporalAutocorrelation #####
  
  # Standard use
  testTemporalAutocorrelation(simulationOutput, time =  testData$time)
  testTemporalAutocorrelation(simulationOutput, time =  testData$time, alternative = "greater")
  
  # error if time is forgotten
  expect_error(testTemporalAutocorrelation(simulationOutput))

})
  
# Test Outliers
test_that("testOutliers", {

  testData = createData(sampleSize = 1000, overdispersion = 0, pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  x = testOutliers(simulationOutput, plot = T, alternative = "two.sided")
  x
  testOutliers(simulationOutput, plot = T, margin = "lower")
  testOutliers(simulationOutput, plot = T, alternative = "two.sided", margin = "lower")
  testOutliers(simulationOutput, plot = T, margin = "upper")

})


