context("Tests DHARMa functions on all implemented model types")

library(testthat)
library(DHARMa)
library(MASS)
library(lme4)
library(mgcv)
library(glmmTMB)
library(spaMM)
set.seed(123)

doPlots = F
# doPlots = T

checkOutput <- function(simulationOutput){

  # print(simulationOutput)

  if(any(simulationOutput$scaledResiduals < 0)) stop()
  if(any(simulationOutput$scaledResiduals > 1)) stop()
  if(any(is.na(simulationOutput$scaledResiduals))) stop()

  if(length(simulationOutput$scaledResiduals) != length(simulationOutput$observedResponse)) stop()
  if(length(simulationOutput$fittedPredictedResponse) != length(simulationOutput$observedResponse)) stop()

}

expectDispersion <- function(x, answer = T){
  res <- simulateResiduals(x)
  if (answer) expect_lt(testDispersion(res, plot = doPlots)$p.value, 0.01)
  else expect_gt(testDispersion(res, plot = doPlots)$p.value, 0.01)
}



runEverything = function(fittedModel, testData, DHARMaData = T){

  t = DHARMa:::getObservedResponse(fittedModel)
  expect_true(is.vector(t))
  expect_true(is.numeric(t))

  x = getSimulations(fittedModel, 2)
  expect_true(is.numeric(x))
  expect_true(is.matrix(x))
  expect_true(ncol(x) == 2)

  x = getSimulations(fittedModel, 1)
  expect_true(is.matrix(x))
  expect_true(ncol(x) == 1)

  x = getSimulations(fittedModel, 1, type = "refit")
  expect_true(is.data.frame(x))

  x = getSimulations(fittedModel, 2, type = "refit")
  expect_true(is.data.frame(x))

  fittedModel2 = getRefit(fittedModel,x[[1]])
  expect_false(any(DHARMa:::getFixedEffects(fittedModel) - DHARMa:::getFixedEffects(fittedModel2) > 0.5))

  simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 200)

  checkOutput(simulationOutput)

  if(doPlots) plot(simulationOutput, quantreg = F)

  expect_gt(testOutliers(simulationOutput, plot = doPlots)$p.value, 0.001)
  expect_gt(testDispersion(simulationOutput, plot = doPlots)$p.value, 0.001)
  expect_gt(testUniformity(simulationOutput = simulationOutput, plot = doPlots)$p.value, 0.001)
  expect_gt(testZeroInflation(simulationOutput = simulationOutput, plot = doPlots)$p.value, 0.001)
  expect_gt(testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time, plot = doPlots)$p.value, 0.001)
  expect_gt(testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x, y = testData$y, plot = F)$p.value, 0.001)

  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)
  expect_gt(testDispersion(simulationOutput, plot = doPlots)$p.value, 0.001)

  simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T, n = 5) # n=10 is very low, set higher for serious tests

  checkOutput(simulationOutput2)

  if(doPlots) plot(simulationOutput2, quantreg = F)

  testDispersion(simulationOutput2, plot = doPlots)

  simulationOutput2 <- recalculateResiduals(simulationOutput2, group = testData$group)
  testDispersion(simulationOutput2, plot = doPlots)

}



# LM ----------------------------------------------------------------------

test_that("lm works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = gaussian())

            fittedModel <- lm(observedResponse ~ Environment1 + Environment2 , data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glm(observedResponse ~ Environment1 , data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- gam(observedResponse ~ s(Environment1) , data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(observedResponse ~ Environment1 + (1|group) , data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , data = testData)
            runEverything(fittedModel, testData)

          }
)


# test_that("lmer gaussian with weights works",
#           {
#             skip_on_cran()
#             testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 1, family = gaussian())
#             fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , weights = runif(100, 0.9,1.1), data = testData)
#             runEverything(fittedModel, testData)
#           }
# )



# Binomial 1/0 ------------------------------------------------------------

test_that("binomial 1/0 works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, randomEffectVariance = 0, family = binomial())

            fittedModel <- glm(observedResponse ~ Environment1  , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- gam(observedResponse ~ s(Environment1) ,family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glm(observedResponse ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "binomial",  data = testData)
            runEverything(fittedModel, testData)
          }
)


# Binomial y/n (factor) ------------------------------------------------------------

test_that("binomial y/n (factor) works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial(), factorResponse = T)

            fittedModel <- glm(observedResponse ~ Environment1 + Environment2 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- gam(observedResponse ~ s(Environment1) ,family = "binomial", data = testData)
            # runEverything(fittedModel, testData)

            fittedModel <- glm(observedResponse ~ Environment1 + Environment2 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "binomial",  data = testData)
            runEverything(fittedModel, testData)
          }
)

# Binomial n/k (Matrix) ------------------------------------------------------------

test_that("glm binomial n/k with matrix works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
            fittedModel <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- gam(cbind(observedResponse1,observedResponse0) ~ s(Environment1) ,family = "binomial", data = testData)

            # gam doesn't work, check
            #runEverything(fittedModel, testData)

            fittedModel <- glmer(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "betabinomial", data = testData)
            runEverything(fittedModel, testData)

            fittedModel <- HLfit(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial",  data = testData)
            runEverything(fittedModel, testData)

          }
)


# Binomial n/k (Weights) ------------------------------------------------------------

test_that("glm binomial n/k with weights works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)

            testData$prop = testData$observedResponse1 / 20

            fittedModel <- glm(prop  ~ Environment1 , family = "binomial", data = testData, weights = rep(20,200))
            runEverything(fittedModel, testData)

            fittedModel <- gam(prop ~ s(Environment1) ,family = "binomial", data = testData, weights = rep(20,200))
            runEverything(fittedModel, testData)

            fittedModel <- glmer(prop ~ Environment1 + (1|group) , family = "binomial", data = testData, weights = rep(20,200))
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(prop ~ Environment1 + (1|group) , family = "binomial", data = testData, weights = rep(20,200))
            runEverything(fittedModel, testData)

            fittedModel <- glmmTMB(prop ~ Environment1 + (1|group) , family = "betabinomial", data = testData, weights = rep(20,200))
            runEverything(fittedModel, testData)


            # spaMM doesn't support binomial k/n via weights
            #fittedModel <- HLfit(prop ~ Environment1 + (1|group) , family = "binomial",  data = testData, prior.weights = rep(20,200))
            #runEverything(fittedModel, testData)

          }
)


# Poisson --------------------------------------------------------


test_that("glm poisson works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 0.000, family = poisson())
            testData2 = createData(sampleSize = 100, overdispersion = 2, randomEffectVariance = 0.000, family = poisson())
            #testData = createData(sampleSize = 200, randomEffectVariance = 1, family = negative.binomial(theta = 1.2, link = "log"))

            fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
            fittedModel2 <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData2)
            expectDispersion(fittedModel2)


            #testData = createData(sampleSize = 200, randomEffectVariance = 1, family = negative.binomial(theta = 1.2, link = "log"))





            fittedModel <- gam(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
            fittedModel2 <- gam(observedResponse ~ Environment1 , family = "poisson", data = testData2)
            expectDispersion(fittedModel2)

            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))
            runEverything(fittedModel, testData)
            fittedModel2 <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData2, control=glmerControl(optCtrl=list(maxfun=20000) ))
            expectDispersion(fittedModel2)

            fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=20000) ))
            runEverything(fittedModel, testData)
            fittedModel2 <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData2, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=20000) ))
            expectDispersion(fittedModel2, F)

            fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
            runEverything(fittedModel, testData)
            fittedModel2 <- glm.nb(observedResponse ~ Environment1, data = testData2)
            expectDispersion(fittedModel2,F)

            fittedModel <- glmmTMB(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
            fittedModel2 <- glmmTMB(observedResponse ~ Environment1 , family = "poisson", data = testData2)
            expectDispersion(fittedModel2)

            fittedModel <- glmmTMB(observedResponse ~ Environment1 + (1|group), zi=~1 , family = nbinom2, data = testData)
            fittedModel2 <- glmmTMB(observedResponse ~ Environment1 + (1|group), zi=~1 , family = nbinom2, data = testData2)
            # does not fully work
            # runEverything(fittedModel, testData)
            expectDispersion(fittedModel2, F)



            fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
            runEverything(fittedModel, testData)
            fittedModel2 <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData2)
            expectDispersion(fittedModel2)
          }
)




# test_that("glmer poisson with weights works",
#           {
#             skip_on_cran()
#             testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
#             fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", weights = runif(200, 0.9,1.1),  data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))
#             runEverything(fittedModel, testData)
#           }
# )


# hasWeights tests --------------------------------------------------------------

test_that("glm poisson weights throws warning",
                    {
                      skip_on_cran()

                      weights = rep(c(1,1.1), each = 100)

                      testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0.5, family = poisson())

                      # lm weights are considered in simulate(), should not throw warning
                      fittedModel <- lm(observedResponse ~ Environment1 , data = testData, weights = weights)
                      simulateResiduals(fittedModel)

                      # glm gaussian still the same
                      fittedModel <- glm(observedResponse ~ Environment1 , data = testData, weights = weights)
                      simulateResiduals(fittedModel)

                      # glm behaves nice, throws a warning that simulate ignores weights for poisson
                      fittedModel <- glm(observedResponse ~ Environment1 , weights = weights, data = testData, family = "poisson")
                      expect_warning(simulateResiduals(fittedModel))

                      # gam also warns
                      fittedModel <- gam(observedResponse ~ Environment1 , weights = weights, data = testData, family = "poisson")
                      expect_warning(simulateResiduals(fittedModel))

                      # lmer does not warn!
                      fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData, weights = weights)
                      expect_warning(simulateResiduals(fittedModel))

                      # glmer warns, OK
                      fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData, weights = weights)
                      expect_warning(simulateResiduals(fittedModel))

                      # glmer.nb warns
                      fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData, weights = weights)
                      expect_warning(simulateResiduals(fittedModel))

                      # glm.nb does not warn, does not seem to simulate according to weights
                      fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData, weights = weights)
                      expect_warning(simulateResiduals(fittedModel))

                      # glmmTMB does not warn, implemented warning in DHARMa
                      fittedModel <- glmmTMB(observedResponse ~ Environment1 , family = "poisson", data = testData, weights = weights)
                      expect_warning(simulateResiduals(fittedModel))

                      # spaMM does not warn, but seems to be simulating with correct (heteroskedastic) variance.
                      # weights cannot be fit with poisson, because spaMM directly interpretes weights as variance
                      expect_error( fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData, prior.weights = weights))
                      # This does not properly work
                      #fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = negbin(),  data = testData, prior.weights = weights)
                      #fittedModel <- fitme(observedResponse ~ Environment1 + (1|group) , family = negbin(),  data = testData, prior.weights = weights)


                      expect_warning(simulateResiduals(fittedModel))
                    }
          )

# isNA tests --------------------------------------------------------------


# test_that("isNA works",
#           {
#             skip_on_cran()
#
#             testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0.5, family = poisson(), hasNA = T)
#
#             fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- gam(observedResponse ~ Environment1 , family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glmmTMB(observedResponse ~ Environment1 , family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
#             expect_true(hasNA(fittedModel))
#
#             # now without NA
#
#             testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0.5, family = poisson(), hasNA = F)
#
#             fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- gam(observedResponse ~ Environment1 , family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glmmTMB(observedResponse ~ Environment1 , family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
#             expect_false(hasNA(fittedModel))
#
#
#           }
# )
#
