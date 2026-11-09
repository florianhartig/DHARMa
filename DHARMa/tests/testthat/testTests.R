
# snapshot of statistic and p-value only, robust to changes of the printed output;
# full output via expect_snapshot() once per test with its own DHARMa method text (#595)
expectTest = function(x) {
  expect_snapshot_value(list(statistic = unname(x$statistic), p.value = x$p.value),
                        style = "deparse", tolerance = 1e-3)
}

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

  set.seed(123)
  # creating test data

  testData = createData(sampleSize = 200, overdispersion = 0.5,
                        pZeroInflation = 0, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson",
                     data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  ###### Distribution tests #####

  expectTest(testUniformity(simulationOutput, plot = FALSE))
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "less"))
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "greater"))

  ###### Dispersion tests #######

  expect_snapshot(testDispersion(simulationOutput, plot = FALSE))
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                  alternative = "less"))
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                   alternative = "greater"))
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                                 alternative = "two.sided"))

  expect_message(testOverdispersion(simulationOutput))
  expect_message(testOverdispersionParametric(simulationOutput))

  ###### Both together###########

  out = testResiduals(simulationOutput, plot = FALSE)
  expectTest(out$uniformity)
  expectTest(out$dispersion)
  expectTest(out$outliers)
  expect_message(testSimulatedResiduals(simulationOutput))

  ###### zero-inflation ##########

  # testing zero inflation
  expect_snapshot(testZeroInflation(simulationOutput, plot = FALSE))
  expectTest(testZeroInflation(simulationOutput, plot = FALSE,
                                    alternative = "less"))

  # testing generic summaries

  # testing for number of 1s
  countOnes <- function(x) sum(x == 1)
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE)) # 1-inflation
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE,
                              alternative = "less")) # 1-deficit

  # testing if mean prediction fits
  means <- function(x) mean(x)
  expect_snapshot(testGeneric(simulationOutput, summary = means, plot = FALSE))

  # testing if mean sd fits
  spread <- function(x) sd(x)
  expectTest(testGeneric(simulationOutput, summary = spread, plot = FALSE))

  ##################################################################

  # grouped
  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)

  ###### Distribution tests #####

  expectTest(testUniformity(simulationOutput, plot = FALSE))
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "less"))
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "greater"))

  ###### Dispersion tests #######

  expectTest(testDispersion(simulationOutput, plot = FALSE))
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                  alternative = "less"))
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                alternative = "greater"))
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                               alternative = "two.sided"))

  expect_message(testOverdispersion(simulationOutput))
  expect_message(testOverdispersionParametric(simulationOutput))

  ###### Both together###########

  out = testResiduals(simulationOutput, plot = FALSE)
  expectTest(out$uniformity)
  expectTest(out$dispersion)
  expectTest(out$outliers)
  expect_message(testSimulatedResiduals(simulationOutput))

  ###### zero-inflation ##########

  # testing zero inflation
  expectTest(testZeroInflation(simulationOutput, plot = FALSE))
  expectTest(testZeroInflation(simulationOutput, plot = FALSE,
                                    alternative = "less"))

  # testing generic summaries

  # testing for number of 1s
  countOnes <- function(x) sum(x == 1)
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE)) # 1-inflation
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE,
                              alternative = "less")) # 1-deficit

  # testing if mean prediction fits
  means <- function(x) mean(x)
  expectTest(testGeneric(simulationOutput, summary = means, plot = FALSE))

  # testing if mean sd fits
  spread <- function(x) sd(x)
  expectTest(testGeneric(simulationOutput, summary = spread, plot = FALSE))


  ###### Refited ##############

  # if model is refitted, a different test will be called

  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
  expectTest(testDispersion(simulationOutput, plot = FALSE))

})



###### Correlation tests #####

test_that("correlation tests work", {
  set.seed(123)
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
  expectTest(testSpatialAutocorrelation(simulationOutput, x =  testData$x,
                                             y = testData$y, plot = FALSE,
                             alternative = "two.sided"))

  # If x and y is not provided, random values will be created
  expect_error(testSpatialAutocorrelation(simulationOutput))

  # Alternatively, one can provide a distance matrix
  dM = as.matrix(dist(cbind(testData$x, testData$y)))
  expectTest(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE))
  expectTest(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE,
                                             alternative = "two.sided"))

  # testting when x and y have different length
  # Error different length
  expect_error(testSpatialAutocorrelation(simulationOutput, plot = FALSE,
                                             x =  testData$x[1:10],
                                          y = testData$y[1:9]),
               "Dimensions of x / y coordinates don't match")
  # x and y have equal length but unequal to simulationOutput
  expect_error(testSpatialAutocorrelation(simulationOutput, plot = FALSE,
                                          x =  testData$x[1:10],
                                          y = testData$y[1:10]),
               "Dimensions of x / y coordinates don't match")
  # see Issue #190  'https://github.com/florianhartig/DHARMa/issues/190'

  # testing distance matrix and an extra x or y argument
  expect_message(out <- testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE, x = testData$x),
                 "coordinates will be ignored")
  expectTest(out)
  expect_message(out <- testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE, y = testData$y),
                 "coordinates will be ignored")
  expectTest(out)


  ###### testTemporalAutocorrelation #####

  # Standard use
  expectTest(testTemporalAutocorrelation(simulationOutput, plot = FALSE,
                                              time =  testData$time))
  expectTest(testTemporalAutocorrelation(simulationOutput, plot = FALSE,
                                              time =  testData$time, alternative = "greater"))

  # error if time is forgotten
  expect_error(testTemporalAutocorrelation(simulationOutput))

})


### Test phylogenetic autocorrelation
test_that("test phylogenetic autocorrelation", {

  set.seed(123)
  tre <<- ape::rcoal(60)
  b0 = 0; b1 = 1;
  x <- runif(length(tre$tip.label), 0,1)
  y <- b0 + b1*x +
    phylolm::rTrait(n = 1, phy = tre, model = "BM",
           parameters = list(ancestral.state = 0, sigma2 = 10))
  dat <<- data.frame(trait = y, pred = x)

  fit = lm(trait ~ pred, data = dat)
  res = simulateResiduals(fit, plot = F)

  restest <- testPhylogeneticAutocorrelation(res, tree = tre)

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
  expectTest(testOutliers(simulationOutput, plot = F,
                               alternative = "two.sided", margin = "lower"))
  expectTest(testOutliers(simulationOutput, plot = F,
                                               margin = "upper"))

  x = testOutliers(simulationOutput, type = "bootstrap", nBoot = 5, plot = F)
  expect_equal(attr(x$conf.int, "conf.level"), 0.95)

  # deprecated argument name plotBoostrap, see #560
  expect_warning(testOutliers(simulationOutput, plot = F, type = "bootstrap",
                              nBoot = 10, plotBoostrap = F), "deprecated")
  expect_error(testOutliers(simulationOutput, plot = F, plotBootstrap = F,
                            plotBoostrap = F), "deprecated")
  expect_message(testOutliers(simulationOutput, plot = F, type = "binomial",
                              plotBootstrap = T), "ignored")

})


# Test Categorical
test_that("testCategorical", {
  set.seed(123)
  testData = createData(sampleSize = 200, randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson",
                     data = testData)
  simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 50)

  # plot = FALSE must not call title(), which fails if no plot was started
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(testCategorical(simulationOutput, catPred = testData$group, plot = F))
})


test_that("testDispersion with refit = TRUE after grouping (#575)", {
  set.seed(123)
  testData = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 20, refit = TRUE)
  grouped = recalculateResiduals(simulationOutput, group = testData$group)
  x = testDispersion(grouped, plot = FALSE)
  observed = sum(rowsum(residuals(fittedModel, type = "pearson"), testData$group)^2)
  expect_equal(unname(x$statistic), observed / mean(colSums(grouped$refittedPearsonResiduals^2)))
})


test_that("testQuantiles with categorical, formula and few-valued predictors (#576)", {
  set.seed(123)
  testData = createData(sampleSize = 100, family = poisson(), randomEffectVariance = 0)
  fittedModel = glm(observedResponse ~ Environment1, family = poisson, data = testData)
  simulationOutput = simulateResiduals(fittedModel, n = 50)
  expect_error(testQuantiles(simulationOutput, predictor = testData$group, plot = FALSE), "testCategorical")
  expect_error(testQuantiles(simulationOutput, predictor = ~Environment1 | group), "exactly one variable")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  pred = round(testData$Environment1)
  expect_equal(testQuantiles(simulationOutput, predictor = pred, plot = FALSE)$p.value,
               testQuantiles(simulationOutput, predictor = pred, plot = TRUE)$p.value)
})
