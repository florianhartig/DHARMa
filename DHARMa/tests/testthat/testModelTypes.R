
skip_on_cran()
#skip_on_ci()

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

runEverything = function(fittedModel, testData, DHARMaData = T, phy = NULL,
                         expectOverdispersion = F){

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
  # expect_false(any(getFixedEffects(fittedModel) -
  #                  getFixedEffects(fittedModel2) > 0.5)) # doesn't work for some models

  simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 200)

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

}


# testData --------------------------------------------------------------------
testData = list()
testData$lm = createData(sampleSize = 200, fixedEffects = c(1,0),
                         overdispersion = 0, randomEffectVariance = 0,
                         family = gaussian())
testData$lmm = createData(sampleSize = 200,
                          overdispersion = 0, randomEffectVariance = 0.5,
                          family = gaussian())


testData$binomial_10 = createData(sampleSize = 200, randomEffectVariance = 0,
                                  family = binomial())
testData$binomial_yn = createData(sampleSize = 200, fixedEffects = c(1,0),
                                  overdispersion = 0, randomEffectVariance = 0,
                                  family = binomial(), factorResponse = T)
testData$binomial_nk_matrix = createData(sampleSize = 200, overdispersion = 0,
                                         randomEffectVariance = 0, family = binomial(),
                                         binomialTrials = 20)
testData$binomial_nk_weights = createData(sampleSize = 200, overdispersion = 0,
                                          randomEffectVariance = 0, family = binomial(),
                                          binomialTrials = 20)
testData$binomial_nk_weights$prop = testData$binomial_nk_weights$observedResponse1 / 20

testData$binomial_nk_weights2 = createData(sampleSize = 200, overdispersion = 1,
                                           randomEffectVariance = 0, family = binomial(),
                                           binomialTrials = 20)
testData$binomial_nk_weights2$prop = testData$binomial_nk_weights2$observedResponse1 / 20



testData$poisson1 = createData(sampleSize = 500, overdispersion = 0,
                               randomEffectVariance = 0.000, family = poisson())
testData$poisson2 = createData(sampleSize = 200, overdispersion = 2,
                               randomEffectVariance =0.000, family = poisson())
testData$poisson3 = createData(sampleSize = 500, overdispersion = 0.5,
                               randomEffectVariance = 0.000, family = poisson())

testData$poisson_weights = createData(sampleSize = 200, overdispersion = 0.5,
                                      randomEffectVariance = 0.5, family = poisson())
testData$weights = rep(c(1,1.1), each = 100)
testData$poisson_weights$weights = testData$weights

# stats::lm ----------------------------------------------------------------------

test_that("lm works",
          {
            fittedModel <- lm(observedResponse ~ Environment1 + Environment2 ,
                              data = testData$lm)
            runEverything(fittedModel, testData = testData$lm)

            # lm weights are considered in simulate(), should not throw warning
            fittedModel <- lm(observedResponse ~ Environment1,
                              data = testData$lm, weights = testData$weights)
            expect_s3_class(simulateResiduals(fittedModel), "DHARMa")
          }
)

# stats::glm ----------------------------------------------------------------------

test_that("glm works",
          {
            fittedModel <- glm(observedResponse ~ Environment1, data = testData$lm)
            runEverything(fittedModel, testData = testData$lm)

            fittedModel <- glm(observedResponse ~ Environment1,
                               family = "binomial", data = testData$binomial_10)
            runEverything(fittedModel, testData$binomial_10)

            fittedModel <- glm(observedResponse ~ Environment1 + Environment2,
                               family = "binomial", data = testData$binomial_yn)
            runEverything(fittedModel, testData$binomial_yn)

            fittedModel <- glm(cbind(observedResponse1,observedResponse0) ~
                                 Environment1, family = "binomial",
                               data = testData$binomial_nk_matrix)
            runEverything(fittedModel, testData$binomial_nk_matrix)

            fittedModel <- glm(prop ~ Environment1, family = "binomial",
                               data = testData$binomial_nk_weights,
                               weights = rep(20,200))
            runEverything(fittedModel, testData$binomial_nk_weights)

            fittedModel <- glm(observedResponse ~ Environment1,
                               family = "poisson", data = testData$poisson1)
            runEverything(fittedModel, testData$poisson1)

            fittedModel2 <- glm(observedResponse ~ Environment1,
                                family = "poisson", data = testData$poisson2)
            expectDispersion(fittedModel2)

            ### Weights checks

            # glm gaussian still the same
            fittedModel <- glm(observedResponse ~ Environment1,
                               data = testData$lm, weights = testData$weights)
            expect_s3_class(simulateResiduals(fittedModel), "DHARMa")

            # glm behaves nice, throws a warning that simulate ignores weights for poisson
            fittedModel <- glm(observedResponse ~ Environment1,
                               weights = testData$weights,
                               data = testData$poisson2, family = "poisson")
            expect_warning(simulateResiduals(fittedModel))
          }
)

# MASS::glm.nb --------------------------------------------------------------

test_that("glm.nb works",
          {
            fittedModel <- MASS::glm.nb(observedResponse ~ Environment1,
                                        data = testData$poisson3)
            runEverything(fittedModel, testData$poisson3)

            # glm.nb does not warn, does not seem to simulate according to weights
            fittedModel <- MASS::glm.nb(observedResponse ~ Environment1,
                                        data = testData$poisson2,
                                        weights = testData$weights)
            expect_warning(simulateResiduals(fittedModel))
          }
)

# mgcv::gam  --------------------------------------------------------------

test_that("mgcv gam works",
          {

            fittedModel <- mgcv::gam(observedResponse ~ Environment1,
                                     data = testData$lm)
            runEverything(fittedModel, testData$lm)

            fittedModel <- mgcv::gam(observedResponse ~ s(Environment1),
                                     data = testData$lm)
            runEverything(fittedModel, testData$lm)

            fittedModel <- mgcv::gam(observedResponse ~ Environment1,
                                     family = "binomial", data = testData$binomial_10)
            runEverything(fittedModel, testData$binomial_10)

            fittedModel <- mgcv::gam(observedResponse ~ Environment1,
                                     family = "binomial", data = testData$binomial_yn)
            runEverything(fittedModel, testData = testData$binomial_yn)

            fittedModel <- mgcv::gam(cbind(observedResponse1,observedResponse0) ~ Environment1,
                                     family = "binomial",
                                     data = testData$binomial_nk_matrix)
            runEverything(fittedModel, testData = testData$binomial_nk_matrix)

            fittedModel <- mgcv::gam(prop ~ Environment1, family = "binomial",
                                     data = testData$binomial_nk_weights,
                                     weights = rep(20,200))
            runEverything(fittedModel, testData$binomial_nk_weights)

            fittedModel <- mgcv::gam(observedResponse ~ Environment1,
                                     family = "poisson", data = testData$poisson1)
            runEverything(fittedModel, testData$poisson1)

            fittedModel2 <- mgcv::gam(observedResponse ~ Environment1,
                                      family = "poisson", data = testData$poisson2)
            expectDispersion(fittedModel2)

            # mgcv::gam warns about weights
            fittedModel <- mgcv::gam(observedResponse ~ Environment1,
                                     weights = testData$weights,
                                     data = testData$poisson_weights, family = "poisson")
            expect_warning(simulateResiduals(fittedModel))
          }
)



# lme4::lmer  --------------------------------------------------------------

test_that("lme4:lmer works",
          {
            fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group),
                                      data = testData$lmm)
            suppressMessages(runEverything(fittedModel, testData$lmm))

            # lmer warns!
            fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group),
                                      data = testData$lm, weights = testData$weights)
            expect_warning(simulateResiduals(fittedModel))
          }
)

# lme4::glmer  --------------------------------------------------------------

test_that("lme4:glmer works",
          {
            fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group),
                                       family = "binomial",
                                       data = testData$binomial_10)
            suppressMessages(runEverything(fittedModel, testData$binomial_10))

            fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group),
                                       family = "binomial",
                                       data = testData$binomial_yn)
            suppressMessages( runEverything(fittedModel, testData$binomial_yn))

            fittedModel <- lme4::glmer(cbind(observedResponse1,observedResponse0) ~
                                         Environment1 + (1|group),
                                       family = "binomial",
                                       data = testData$binomial_nk_matrix)
            suppressMessages(runEverything(fittedModel, testData$binomial_nk_matrix))

            fittedModel <- lme4::glmer(prop ~ Environment1 + (1|group),
                                       family = "binomial",
                                       data = testData$binomial_nk_weights,
                                       weights = rep(20,200))
            suppressMessages(runEverything(fittedModel, testData$binomial_nk_weights))

            fittedModel <- lme4::glmer(observedResponse ~ Environment1 +
                                         (1|group) + (1|ID),
                                       family = "poisson",
                                       data = testData$poisson1,
                                       control = lme4::glmerControl(optCtrl = list(
                                         maxfun = 20000)))
            suppressMessages(runEverything(fittedModel, testData$poisson1))

            fittedModel2 <- lme4::glmer(observedResponse ~ Environment1 +
                                          (1|group), family = "poisson",
                                        data = testData$poisson2,
                                        control = lme4::glmerControl(optCtrl = list(
                                          maxfun = 20000)))
            expectDispersion(fittedModel2)

            fittedModel <- lme4::glmer.nb(observedResponse ~ Environment1 +
                                            (1|group), data = testData$poisson1,
                                          control = lme4::glmerControl(
                                            optimizer = "bobyqa",
                                            optCtrl = list(maxfun=20000)))
            suppressMessages(runEverything(fittedModel, testData$poisson1))

            # lme4::glmer warns, OK
            fittedModel <- lme4::glmer(observedResponse ~ Environment1 +
                                         (1|group), family = "poisson",
                                       data = testData$poisson_weights,
                                       weights = testData$weights)
            expect_warning(simulateResiduals(fittedModel))

            # lme4::glmer.nb warns
            fittedModel <- lme4::glmer.nb(observedResponse ~ Environment1 +
                                            (1|group),
                                          data = testData$poisson_weights,
                                          weights = testData$weights)
            expect_warning(simulateResiduals(fittedModel))
          }
)



# glmmTMB --------------------------------------------------------------

test_that("glmmTMB works",
          {

            fittedModel <- glmmTMB::glmmTMB(observedResponse ~ Environment1 +
                                              (1|group), data = testData$lmm)
            runEverything(fittedModel, testData$lmm)

            fittedModel <- glmmTMB::glmmTMB(observedResponse ~ Environment1 +
                                              (1|group), family = "binomial",
                                            data = testData$binomial_10)
            runEverything(fittedModel, testData$binomial_10)

            fittedModel <- glmmTMB::glmmTMB(observedResponse ~ Environment1 +
                                              (1|group), family = "binomial",
                                            data = testData$binomial_yn)
            runEverything(fittedModel, testData$binomial_yn)

            fittedModel <- glmmTMB::glmmTMB(cbind(observedResponse1,
                                                  observedResponse0) ~
                                              Environment1 + (1|group),
                                            family = "binomial",
                                            data = testData$binomial_nk_matrix)
            runEverything(fittedModel, testData$binomial_nk_matrix)

            fittedModel <- glmmTMB::glmmTMB(cbind(observedResponse1,
                                                  observedResponse0) ~
                                              Environment1 + (1|group),
                                            family = glmmTMB::betabinomial(),
                                            data = testData$binomial_nk_matrix)
            runEverything(fittedModel, testData$binomial_nk_matrix)

            fittedModel <- glmmTMB::glmmTMB(prop ~ Environment1 + (1|group),
                                            family = "binomial",
                                            data = testData$binomial_nk_weights,
                                            weights = rep(20,200))
            runEverything(fittedModel, testData$binomial_nk_weights)

            fittedModel <- glmmTMB::glmmTMB(prop ~ Environment1 + (1|group),
                                            family = glmmTMB::betabinomial(),
                                            data = testData$binomial_nk_weights2,
                                            weights = rep(20,200))
            runEverything(fittedModel, testData$binomial_nk_weights2)

            # glmmTMB does not warn, implemented warning in DHARMa
            fittedModel <- glmmTMB::glmmTMB(observedResponse ~ Environment1,
                                            family = "poisson",
                                            data = testData$poisson_weights,
                                            weights = testData$weights)
            expect_warning(simulateResiduals(fittedModel))
          }
)


# spaMM::HLfit  --------------------------------------------------------------

test_that("spaMM::HLfit works",
          {

            fittedModel <- spaMM::HLfit(observedResponse ~ Environment1 + (1|group),
                                        data = testData$lm)
            runEverything(fittedModel, testData$lm)

            fittedModel <- spaMM::HLfit(observedResponse ~ Environment1 +
                                          (1|group), family = "binomial",
                                        data = testData$binomial_10)
            runEverything(fittedModel, testData$binomial_10)

            fittedModel <- spaMM::HLfit(observedResponse ~ Environment1 + (1|group),
                                        family = "binomial",
                                        data = testData$binomial_yn)
            runEverything(fittedModel, testData$binomial_yn)

            fittedModel <- spaMM::HLfit(cbind(observedResponse1,observedResponse0) ~
                                          Environment1 + (1|group),
                                        family = "binomial",
                                        data = testData$binomial_nk_matrix)
            runEverything(fittedModel, testData$binomial_nk_matrix)

            # spaMM doesn't support binomial k/n via weights
            expect_error(spaMM::HLfit(prop ~ Environment1 + (1|group),
                                      family = "binomial",
                                      data = testData$binomial_nk_weights,
                                      prior.weights = rep(20,200)))

            fittedModel <- spaMM::HLfit(observedResponse ~ Environment1 + (1|group),
                                        family = "poisson",  data = testData$poisson1)
            runEverything(fittedModel, testData$poisson1)

            fittedModel <- spaMM::HLfit(observedResponse ~ Environment1 + (1|group),
                                        family = "poisson",  data = testData$poisson2)
            expectDispersion(fittedModel)

            # spaMM does not warn, but seems to be simulating with correct (heteroskedastic) variance.
            # weights cannot be fit with poisson, because spaMM directly interpretes weights as variance

            # doesn't throw error / warning, but seems intended, see https://github.com/florianhartig/DHARMa/issues/175

            expect_error(spaMM::HLfit(observedResponse ~ Environment1 + (1|group),
                                      family = negbin(1),
                                      data = testData$poisson_weights,
                                      prior.weights = weights))
            expect_s3_class(simulateResiduals(fittedModel), "DHARMa")
          }
)


# GLMMadaptive  --------------------------------------------------------------

test_that("GLMMadaptive works",
          {

            # GLMMadaptive does not support gaussian
            expect_error(GLMMadaptive::mixed_model(fixed = observedResponse ~
                                                     Environment1,
                                                   random = ~ 1 | group, data = testData$lmm,
                                                   family = gaussian()))

            fittedModel <- GLMMadaptive::mixed_model(fixed = observedResponse ~
                                                       Environment1,
                                                     random = ~ 1 | group,
                                                     data = testData$binomial_10,
                                                     family = binomial())
            runEverything(fittedModel, testData$binomial_10)

            fittedModel <- GLMMadaptive::mixed_model(fixed = observedResponse ~
                                                       Environment1,
                                                     random = ~ 1 | group,
                                                     data = testData$binomial_yn,
                                                     family = binomial())
            runEverything(fittedModel, testData$binomial_yn)

            fittedModel <- GLMMadaptive::mixed_model(fixed =
                                                       cbind(observedResponse1,
                                                             observedResponse0) ~
                                                       Environment1,
                                                     random = ~ 1 | group,
                                                     data = testData$binomial_nk_matrix,
                                                     family = binomial())
            # Does not work yet
            #runEverything(fittedModel, testData)

            # GLMMadaptive doesn't support binomial k/n via weights
            #fittedModel <- GLMMadaptive::mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 | group, data = testData, family = binomial(), weights = rep(20,200))
            # does not yet work
            # runEverything(fittedModel, testData)

            fittedModel <- GLMMadaptive::mixed_model(fixed = observedResponse ~
                                                       Environment1,
                                                     random = ~ 1 | group,
                                                     data = testData$poisson1,
                                                     family = poisson())
            runEverything(fittedModel, testData$poisson1)

            # GLMMadaptive requires weights according to groups
            weights = rep(c(1,1.1), each = 5)
            fittedModel <- GLMMadaptive::mixed_model(fixed = observedResponse ~
                                                       Environment1,
                                                     random = ~ 1 | group,
                                                     data = testData$poisson_weights,
                                                     family = poisson(),
                                                     weights = weights)
            expect_warning(simulateResiduals(fittedModel))
          }
)

rm(testData)


# phylolm::pylolm/phyloglm -----------------------------------------------

# OBS: somehow, the base function update() in getSimulations.phylolm() and
# getRefit.phylolm() searches for the objects in the global env, then the
# test_that wasn't working. A workaround was to put the objects of the tree and
# the dataset in the global env.

test_that("phylolm works",
          {
            # LM
            set.seed(123456)
            tre1 <<- ape::rcoal(60) # global env
            taxa = sort(tre1$tip.label)
            b0 = 0; b1 = 1;
            x <- phylolm::rTrait(n = 1, phy = tre1, model = "BM",
                                 parameters = list(ancestral.state = 0,
                                                   sigma2 = 10))
            y <- b0 + b1*x +
              phylolm::rTrait(n = 1, phy = tre1, model = "lambda",
                              parameters = list(ancestral.state = 0, sigma2 = 1,
                                                lambda = 0.5))
            testData <<- data.frame(trait = y[taxa], predictor = x[taxa],
                                    x = runif(length(y)), y= runif(length(y)))
            fittedModel = phylolm::phylolm(trait ~ predictor, data = testData,
                                           phy = tre1, model = "lambda")

            runEverything(fittedModel, testData = testData, phy = tre1)
          })

test_that("phyloglm works",
          {
            #GLM
            set.seed(123456)
            tre <<- ape::rtree(50) # global env
            x = phylolm::rTrait(n = 1, phy = tre)
            X = cbind(rep(1, 50), x)
            y = phylolm::rbinTrait(n = 1, phy = tre, beta = c(-1,0.5), alpha = 1,
                                   X = X)
            testData <<- data.frame(trait = y, predictor = x,
                                    x = runif(length(y)), y = runif(length(y)))
            fittedModel = phylolm::phyloglm(trait ~ predictor,
                                            phy = tre, data = testData)

            runEverything(fittedModel,  testData = testData, phy = tre)
          })


# isNA works? ------------------------------------------------------------------

# test_that("isNA works",
#           {
#             skip_on_cran()
#
#             testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0.5, family = poisson(), hasNA = T)
#
#             fittedModel <- lm(observedResponse ~ Environment1, data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- gam(observedResponse ~ Environment1, family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- lme4::glmer.nb(observedResponse ~ Environment1 + (1|group), data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- glmmTMB(observedResponse ~ Environment1, family = "poisson", data = testData)
#             expect_true(hasNA(fittedModel))
#
#             fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group), family = "poisson",  data = testData)
#             expect_true(hasNA(fittedModel))
#
#             # now without NA
#
#             testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0.5, family = poisson(), hasNA = F)
#
#             fittedModel <- lm(observedResponse ~ Environment1, data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- gam(observedResponse ~ Environment1, family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- lme4::glmer.nb(observedResponse ~ Environment1 + (1|group), data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- glmmTMB(observedResponse ~ Environment1, family = "poisson", data = testData)
#             expect_false(hasNA(fittedModel))
#
#             fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group), family = "poisson",  data = testData)
#             expect_false(hasNA(fittedModel))
#
#
#           }
# )
#

