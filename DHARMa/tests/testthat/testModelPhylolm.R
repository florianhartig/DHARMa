
skip_on_cran()
skip_on_ci()

set.seed(1234)

doPlots = F

# Test functions --------------------------------------------------------------
checkOutput <- function(simulationOutput){

  # print(simulationOutput)

  if(any(simulationOutput$scaledResiduals < 0)) stop()
  if(any(simulationOutput$scaledResiduals > 1)) stop()
  if(any(is.na(simulationOutput$scaledResiduals))) stop()

  if(length(simulationOutput$scaledResiduals) !=
     length(simulationOutput$observedResponse)) stop()
  if(length(simulationOutput$fittedPredictedResponse) != length(simulationOutput$observedResponse)) stop()

}

expectDispersion <- function(x, answer = T){
  res <- simulateResiduals(x)
  if (answer) expect_lt(testDispersion(res, plot = doPlots)$p.value, 0.05)
  else expect_gt(testDispersion(res, plot = doPlots)$p.value, 0.05)
}

# pylolm/phyloglms works -------------------------------------------------------
#
# test_that("phylolm works",
#           {
#             # LM
#             set.seed(123456)
#             tre = ape::rcoal(60)
#             taxa = sort(tre$tip.label)
#             b0 = 0; b1 = 1;
#             x <- phylolm::rTrait(n = 1, phy = tre, model = "BM",
#                                  parameters = list(ancestral.state = 0,
#                                                    sigma2 = 10))
#             y <- b0 + b1*x +
#               phylolm::rTrait(n = 1, phy = tre, model = "lambda",
#                               parameters = list(ancestral.state = 0, sigma2 = 1,
#                                                 lambda = 0.5))
#             testData = data.frame(trait = y[taxa], predictor = x[taxa],
#                                   x = runif(length(y)), y= runif(length(y)))
#             fittedModel = phylolm::phylolm(trait ~ predictor, data = testData,
#                                            phy = tre, model = "lambda")
#
#             t = getObservedResponse(fittedModel)
#             expect_true(is.vector(t))
#             expect_true(is.numeric(t))
#
#             x = getSimulations(fittedModel, 1)
#             expect_true(is.matrix(x))
#             expect_true(ncol(x) == 1)
#
#             x = getSimulations(fittedModel, 2)
#             expect_true(is.numeric(x))
#             expect_true(is.matrix(x))
#             expect_true(ncol(x) == 2)
#
#             x = getSimulations(fittedModel, 1, type = "refit")
#             expect_true(is.data.frame(x))
#
#             x = getSimulations(fittedModel, 2, type = "refit")
#             expect_true(is.data.frame(x))
#
#             fittedModel2 = getRefit(fittedModel,x[[1]])
#             expect_false(any(getFixedEffects(fittedModel) -
#                              getFixedEffects(fittedModel2) > 0.5)) # doesn't work for some models
#
#             simulationOutput <- simulateResiduals(fittedModel = fittedModel,  n = 200)
#
#             checkOutput(simulationOutput)
#
#             if(doPlots) plot(simulationOutput, quantreg = F)
#
#             expect_gt(testOutliers(simulationOutput, plot = doPlots)$p.value, 0.001)
#             expect_gt(testDispersion(simulationOutput, plot = doPlots)$p.value, 0.001)
#             expect_gt(testUniformity(simulationOutput = simulationOutput,
#                                      plot = doPlots)$p.value, 0.001)
#             expect_gt(testZeroInflation(simulationOutput = simulationOutput,
#                                         plot = doPlots)$p.value, 0.001)
#             expect_gt(testTemporalAutocorrelation(simulationOutput = simulationOutput,
#                                                   time = testData$time,
#                                                   plot = doPlots)$p.value, 0.001)
#             expect_gt(testSpatialAutocorrelation(simulationOutput = simulationOutput,
#                                                  x = testData$x, y = testData$y,
#                                                  plot = F)$p.value, 0.001)
#
#             simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)
#             expect_gt(testDispersion(simulationOutput, plot = doPlots)$p.value, 0.001)
#
#             simulationOutput2 <- simulateResiduals(fittedModel = fittedModel,
#                                                    refit = T, n = 100)
#
#             checkOutput(simulationOutput2)
#             if(doPlots) plot(simulationOutput2, quantreg = F)
#
#             # note that the pearson test is biased, therefore have to test greater
#             #expect_gt(testDispersion(simulationOutput2, plot = doPlots, alternative = "greater")$p.value, 0.001)
#             x = testDispersion(simulationOutput2, plot = doPlots)
#
#             simulationOutput3 <- recalculateResiduals(simulationOutput2, group = testData$group)
#             #expect_gt(testDispersion(simulationOutput3, plot = doPlots, alternative = "greater")$p.value, 0.001)
#             x = testDispersion(simulationOutput3, plot = doPlots)
#           })

test_that("phyloglm works",
          {
            #GLM
            set.seed(123456)
            tre = ape::rtree(50)
            x = phylolm::rTrait(n = 1, phy = tre)
            X = cbind(rep(1, 50), x)
            y = phylolm::rbinTrait(n = 1, phy = tre, beta = c(-1,0.5), alpha = 1,
                                   X = X)
            testData = data.frame(trait = y, predictor = x,
                                  x = runif(length(y)), y = runif(length(y)))
            fittedModel = phylolm::phyloglm(trait ~ predictor, btol=20,
                                            phy = tre, data = testData)

            t = getObservedResponse(fittedModel)
            expect_true(is.vector(t))
            expect_true(is.numeric(t))

            x = getSimulations(fittedModel, 1)
            expect_true(is.matrix(x))
            expect_true(ncol(x) == 1)

            x = getSimulations(fittedModel, 2)
            expect_true(is.numeric(x))
            expect_true(is.matrix(x))
            expect_true(ncol(x) == 2)

            x = getSimulations(fittedModel, 1, type = "refit")
            expect_true(is.data.frame(x))

            x = getSimulations(fittedModel, 2, type = "refit")
            expect_true(is.data.frame(x))

            fittedModel2 = getRefit(fittedModel,x[[1]])
            expect_false(any(getFixedEffects(fittedModel) -
                               getFixedEffects(fittedModel2) > 0.5)) # doesn't work for some models

            simulationOutput <- simulateResiduals(fittedModel = fittedModel,  n = 200)

            checkOutput(simulationOutput)

            if(doPlots) plot(simulationOutput, quantreg = F)

            expect_gt(testOutliers(simulationOutput, plot = doPlots)$p.value, 0.001)
            expect_gt(testDispersion(simulationOutput, plot = doPlots)$p.value, 0.001)
            expect_gt(testUniformity(simulationOutput = simulationOutput,
                                     plot = doPlots)$p.value, 0.001)
            expect_gt(testZeroInflation(simulationOutput = simulationOutput,
                                        plot = doPlots)$p.value, 0.001)
            expect_gt(testTemporalAutocorrelation(simulationOutput = simulationOutput,
                                                  time = testData$time,
                                                  plot = doPlots)$p.value, 0.001)
            expect_gt(testSpatialAutocorrelation(simulationOutput = simulationOutput,
                                                 x = testData$x, y = testData$y,
                                                 plot = F)$p.value, 0.001)

            simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)
            expect_gt(testDispersion(simulationOutput, plot = doPlots)$p.value, 0.001)

            simulationOutput2 <- simulateResiduals(fittedModel = fittedModel,
                                                   refit = T, n = 100)

            checkOutput(simulationOutput2)
            if(doPlots) plot(simulationOutput2, quantreg = F)

            # note that the pearson test is biased, therefore have to test greater
            #expect_gt(testDispersion(simulationOutput2, plot = doPlots, alternative = "greater")$p.value, 0.001)
            x = testDispersion(simulationOutput2, plot = doPlots)

            simulationOutput3 <- recalculateResiduals(simulationOutput2, group = testData$group)
            #expect_gt(testDispersion(simulationOutput3, plot = doPlots, alternative = "greater")$p.value, 0.001)
            x = testDispersion(simulationOutput3, plot = doPlots)




          })
