
test_that("overdispersion recognized", {

  set.seed(123)

  testData = createData(sampleSize = 200, overdispersion = 3, pZeroInflation = 0.4, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson",
                     data = testData)
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

  testData = createData(sampleSize = 200, overdispersion = 0.5,
                        pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson",
                     data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  ###### Distribution tests #####

  expect_snapshot(testUniformity(simulationOutput, plot = FALSE))
  expect_snapshot(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "less"))
  expect_snapshot(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "greater"))

  ###### Dispersion tests #######

  expect_snapshot(testDispersion(simulationOutput, plot = FALSE))
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE,
                  alternative = "less"))
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE,
                   alternative = "greater"))
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE,
                                 alternative = "two.sided"))

  expect_message(testOverdispersion(simulationOutput))
  expect_message(testOverdispersionParametric(simulationOutput))

  ###### Both together###########

  expect_snapshot(testResiduals(simulationOutput, plot = FALSE))
  expect_message(testSimulatedResiduals(simulationOutput))

  ###### zero-inflation ##########

  # testing zero inflation
  expect_snapshot(testZeroInflation(simulationOutput, plot = FALSE))
  expect_snapshot(testZeroInflation(simulationOutput, plot = FALSE,
                                    alternative = "less"))

  # testing generic summaries

  # testing for number of 1s
  countOnes <- function(x) sum(x == 1)
  expect_snapshot(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE)) # 1-inflation
  expect_snapshot(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE,
                              alternative = "less")) # 1-deficit

  # testing if mean prediction fits
  means <- function(x) mean(x)
  expect_snapshot(testGeneric(simulationOutput, summary = means, plot = FALSE))

  # testing if mean sd fits
  spread <- function(x) sd(x)
  expect_snapshot(testGeneric(simulationOutput, summary = spread, plot = FALSE))

  ##################################################################

  # grouped
  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)

  ###### Distribution tests #####

  expect_snapshot(testUniformity(simulationOutput, plot = FALSE))
  expect_snapshot(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "less"))
  expect_snapshot(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "greater"))

  ###### Dispersion tests #######

  expect_snapshot(testDispersion(simulationOutput, plot = FALSE))
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE,
                  alternative = "less"))
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE,
                alternative = "greater"))
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE,
                               alternative = "two.sided"))

  expect_message(testOverdispersion(simulationOutput))
  expect_message(testOverdispersionParametric(simulationOutput))

  ###### Both together###########

  expect_snapshot(testResiduals(simulationOutput, plot = FALSE))
  expect_message(testSimulatedResiduals(simulationOutput))

  ###### zero-inflation ##########

  # testing zero inflation
  expect_snapshot(testZeroInflation(simulationOutput, plot = FALSE))
  expect_snapshot(testZeroInflation(simulationOutput, plot = FALSE,
                                    alternative = "less"))

  # testing generic summaries

  # testing for number of 1s
  countOnes <- function(x) sum(x == 1)
  expect_snapshot(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE)) # 1-inflation
  expect_snapshot(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE,
                              alternative = "less")) # 1-deficit

  # testing if mean prediction fits
  means <- function(x) mean(x)
  expect_snapshot(testGeneric(simulationOutput, summary = means, plot = FALSE))

  # testing if mean sd fits
  spread <- function(x) sd(x)
  expect_snapshot(testGeneric(simulationOutput, summary = spread, plot = FALSE))


  ###### Refited ##############

  # if model is refitted, a different test will be called

  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
  expect_snapshot(testDispersion(simulationOutput, plot = FALSE))

})



###### Correlation tests #####

test_that("correlation tests work", {

  testData = createData(sampleSize = 200, overdispersion = 0.5,
                        pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson",
                     data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  # grouped
  simulationOutputGrouped <- recalculateResiduals(simulationOutput,
                                                  group = testData$group)

  ###### testSpatialAutocorrelation #####

  # Standard use
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, x =  testData$x,
                                             y = testData$y, plot = FALSE))
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, x =  testData$x,
                                             y = testData$y, plot = FALSE,
                             alternative = "two.sided"))

  # If x and y is not provided, random values will be created
  expect_error(testSpatialAutocorrelation(simulationOutput))

  # Alternatively, one can provide a distance matrix
  dM = as.matrix(dist(cbind(testData$x, testData$y)))
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE))
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE,
                                             alternative = "two.sided"))

  # testting when x and y have different length
  # Error different length
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, plot = FALSE,
                                             x =  testData$x[1:10],
                                          y = testData$y[1:9]),
                  error = TRUE)
  # x and y have equal length but unequal to simulationOutput
  expect_snapshot(testSpatialAutocorrelation(simulationOutput[1:10], plot = FALSE,
                                          x =  testData$x[1:10],
                                          y = testData$y[1:10]),
                  error=TRUE)
  # see Issue #190  'https://github.com/florianhartig/DHARMa/issues/190'

  # testing distance matrix and an extra x or y argument
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE, x = testData$x))
  expect_snapshot(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE, y = testData$y))


  ###### testTemporalAutocorrelation #####

  # Standard use
  expect_snapshot(testTemporalAutocorrelation(simulationOutput, plot = FALSE,
                                              time =  testData$time))
  expect_snapshot(testTemporalAutocorrelation(simulationOutput, plot = FALSE,
                                              time =  testData$time, alternative = "greater"))

  # error if time is forgotten
  expect_error(testTemporalAutocorrelation(simulationOutput))

})


### Test phylogenetic autocorrelation
test_that("test phylogenetic autocorrelation", {

  set.seed(123)
  tre = ape::rcoal(60)
  b0 = 0; b1 = 1;
  x <- runif(length(tre$tip.label), 0,1)
  y <- b0 + b1*x +
    phylolm::rTrait(n = 1, phy = tre, model = "BM",
           parameters = list(ancestral.state = 0, sigma2 = 10))
  dat = data.frame(trait = y, pred = x)

  fit = lm(trait ~ pred, data = dat)
  res = simulateResiduals(fit, plot = F)

  restest <- testPhylogeneticAutocorrelation(res, tree = tre)

  expect_snapshot(restest)
  expect_true(restest$p.value <= 0.05)

  fit2 = phylolm::phylolm(trait ~ pred, data = dat, phy = tre, model = "BM")
  res2 = simulateResiduals(fit2, plot = F, rotation = "estimated")

  restest2 <- testPhylogeneticAutocorrelation(res2, tree = tre)

  expect_snapshot(restest2)
  expect_true(restest2$p.value > 0.05)
})



# Test Outliers
test_that("testOutliers", {
  set.seed(123)
  testData = createData(sampleSize = 1000, overdispersion = 0,
                        pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson",
                     data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  expect_snapshot(testOutliers(simulationOutput, plot = F, margin = "lower"))
  expect_snapshot(testOutliers(simulationOutput, plot = F,
                               alternative = "two.sided", margin = "lower"))
  expect_snapshot(testOutliers(simulationOutput, plot = F,
                                               margin = "upper"))

})


