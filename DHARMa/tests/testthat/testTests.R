
# compares statistic and p-value instead of the printed output, not on CRAN (#595)
expectTest = function(x, statistic, p.value) {
  force(x)
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) return(invisible())
  expect_equal(unname(x$statistic), statistic, tolerance = 1e-3)
  expect_equal(x$p.value, p.value, tolerance = 1e-3)
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

  expectTest(testUniformity(simulationOutput, plot = FALSE), 0.0355771, 0.961904)
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "less"), 0.0355771, 0.602726)
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "greater"), 0.0344184, 0.622601)

  ###### Dispersion tests #######

  expectTest(testDispersion(simulationOutput, plot = FALSE), 1.25778, 0.096)
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                  alternative = "less"), 1.25778, 0.952)
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                   alternative = "greater"), 1.25778, 0.048)
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                                 alternative = "two.sided"), 1.25778, 0.096)

  expect_message(testOverdispersion(simulationOutput))
  expect_message(testOverdispersionParametric(simulationOutput))

  ###### Both together###########

  out = testResiduals(simulationOutput, plot = FALSE)
  expectTest(out$uniformity, 0.0355771, 0.961904)
  expectTest(out$dispersion, 1.25778, 0.096)
  expectTest(out$outliers, 1, 0.82)
  expect_message(testSimulatedResiduals(simulationOutput))

  ###### zero-inflation ##########

  # testing zero inflation
  expectTest(testZeroInflation(simulationOutput, plot = FALSE), 1.05652, 0.552)
  expectTest(testZeroInflation(simulationOutput, plot = FALSE,
                                    alternative = "less"), 1.05652, 0.784)

  # testing generic summaries

  # testing for number of 1s
  countOnes <- function(x) sum(x == 1)
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE), 0.974565, 0.808) # 1-inflation
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE,
                              alternative = "less"), 0.974565, 0.404) # 1-deficit

  # testing if mean prediction fits
  means <- function(x) mean(x)
  expectTest(testGeneric(simulationOutput, summary = means, plot = FALSE), 1.00767, 0.944)

  # testing if mean sd fits
  spread <- function(x) sd(x)
  expectTest(testGeneric(simulationOutput, summary = spread, plot = FALSE), 1.0834, 0.208)

  ##################################################################

  # grouped
  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)

  ###### Distribution tests #####

  expectTest(testUniformity(simulationOutput, plot = FALSE), 0.225467, 0.613005)
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "less"), 0.162787, 0.533776)
  expectTest(testUniformity(simulationOutput, plot = FALSE,
                                 alternative = "greater"), 0.225467, 0.315)

  ###### Dispersion tests #######

  expectTest(testDispersion(simulationOutput, plot = FALSE), 2.10913, 0.048)
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                  alternative = "less"), 2.10913, 0.976)
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                alternative = "greater"), 2.10913, 0.024)
  expectTest(testDispersion(simulationOutput, plot = FALSE,
                               alternative = "two.sided"), 2.10913, 0.048)

  expect_message(testOverdispersion(simulationOutput))
  expect_message(testOverdispersionParametric(simulationOutput))

  ###### Both together###########

  out = testResiduals(simulationOutput, plot = FALSE)
  expectTest(out$uniformity, 0.225467, 0.613005)
  expectTest(out$dispersion, 2.10913, 0.048)
  expectTest(out$outliers, 20, 0.1)
  expect_message(testSimulatedResiduals(simulationOutput))

  ###### zero-inflation ##########

  # testing zero inflation
  expectTest(testZeroInflation(simulationOutput, plot = FALSE), NaN, 1)
  expectTest(testZeroInflation(simulationOutput, plot = FALSE,
                                    alternative = "less"), NaN, 1)

  # testing generic summaries

  # testing for number of 1s
  countOnes <- function(x) sum(x == 1)
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE), NaN, 1) # 1-inflation
  expectTest(testGeneric(simulationOutput, summary = countOnes,
                              plot = FALSE,
                              alternative = "less"), NaN, 1) # 1-deficit

  # testing if mean prediction fits
  means <- function(x) mean(x)
  expectTest(testGeneric(simulationOutput, summary = means, plot = FALSE), 1.00767, 0.944)

  # testing if mean sd fits
  spread <- function(x) sd(x)
  expectTest(testGeneric(simulationOutput, summary = spread, plot = FALSE), 1.47252, 0.04)


  ###### Refited ##############

  # if model is refitted, a different test will be called

  simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
  expectTest(testDispersion(simulationOutput, plot = FALSE), 1.12305, 0.248)

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

  moran = c(-0.0166319, -0.00502513, 0.011275)

  # Standard use
  expectTest(testSpatialAutocorrelation(simulationOutput, x =  testData$x,
                                             y = testData$y, plot = FALSE), moran, 0.303277)
  expectTest(testSpatialAutocorrelation(simulationOutput, x =  testData$x,
                                             y = testData$y, plot = FALSE,
                             alternative = "two.sided"), moran, 0.303277)

  # If x and y is not provided, random values will be created
  expect_error(testSpatialAutocorrelation(simulationOutput))

  # Alternatively, one can provide a distance matrix
  dM = as.matrix(dist(cbind(testData$x, testData$y)))
  expectTest(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE), moran, 0.303277)
  expectTest(testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE,
                                             alternative = "two.sided"), moran, 0.303277)

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
  expectTest(out, moran, 0.303277)
  expect_message(out <- testSpatialAutocorrelation(simulationOutput, distMat = dM,
                                             plot = FALSE, y = testData$y),
                 "coordinates will be ignored")
  expectTest(out, moran, 0.303277)


  ###### testTemporalAutocorrelation #####

  # Standard use
  expectTest(testTemporalAutocorrelation(simulationOutput, plot = FALSE,
                                              time =  testData$time), 1.97032, 0.832966)
  expectTest(testTemporalAutocorrelation(simulationOutput, plot = FALSE,
                                              time =  testData$time, alternative = "greater"),
             1.97032, 0.416483)

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

  expectTest(restest2, c(0.0474742, -0.0169492, 0.0882598), 0.465434)
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

  expectTest(testOutliers(simulationOutput, plot = F, margin = "lower"), 4, 0.803659)
  expectTest(testOutliers(simulationOutput, plot = F,
                               alternative = "two.sided", margin = "lower"), 4, 0.803659)
  expectTest(testOutliers(simulationOutput, plot = F,
                                               margin = "upper"), 4, 0.803659)

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
