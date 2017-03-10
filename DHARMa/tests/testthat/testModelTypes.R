context("Tests DHARMa functions on all implemented model types")

library(DHARMa)
library(MASS)
library(lme4)
library(mgcv)


runEverything = function(fittedModel, testData, DHARMaData = T){
  
  cat("\n\n============= NEW MODEL ====================\n\n")
  
  
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)

  print(simulationOutput)
  plot(simulationOutput, quantreg = F)

  plotSimulatedResiduals(simulationOutput = simulationOutput)
  if(DHARMaData == T) plotResiduals(pred = testData$Environment1, simulationOutput$scaledResiduals, quantreg = F)

  testUniformity(simulationOutput = simulationOutput)
  testZeroInflation(simulationOutput = simulationOutput)
  testTemporalAutocorrelation(simulationOutput = simulationOutput, time = runif(length(simulationOutput$scaledResiduals )))
  testSpatialAutocorrelation(simulationOutput = simulationOutput, x = runif(length(simulationOutput$scaledResiduals )), y =  runif(length(simulationOutput$scaledResiduals )))
  
  # currently not testing the following because of warning
  #testOverdispersion(simulationOutput)
  #testOverdispersion(simulationOutput, alternative = "both", plot = T)


  simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T, n = 10) # n=10 is very low, set higher for serious tests

  print(simulationOutput2)
  plot(simulationOutput2, quantreg = F)

  testOverdispersion(simulationOutput2)
  testOverdispersion(simulationOutput2, alternative = "both", plot = T)
  testOverdispersionParametric(fittedModel)

}


test_that("lm gaussian works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- lm(observedResponse ~ Environment1 + Environment2 , data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm gaussian works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- glm(observedResponse ~ Environment1 , data = testData)
            runEverything(fittedModel, testData)
          }
)

test_that("gam gaussian works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- gam(observedResponse ~ s(Environment1) , data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("lmer gaussian works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 1, family = gaussian())
            fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData)
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


test_that("glm binomial y/n (factor) works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial(), factorResponse = T)
            fittedModel <- glm(observedResponse ~ Environment1 + Environment2 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)

test_that("glm binomial 1/0 works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 + Environment2 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)

test_that("gam binomial 1/0 works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- gam(observedResponse ~ s(Environment1) ,family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm binomial n/k works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
            fittedModel <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("gam binomial n/k works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
            fittedModel <- gam(cbind(observedResponse1,observedResponse0) ~ s(Environment1) ,family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)




test_that("glmer binomial 1/0 works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, overdispersion = 2, family = binomial())
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glmer binomial n/k works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, overdispersion = 2, family = binomial(), binomialTrials = 20)
            fittedModel <- glmer(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm poisson works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("gam poisson works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- gam(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
          }
)



test_that("glmer poisson works",
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))
            runEverything(fittedModel, testData)
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




# Negative binomial models


test_that("glmer.nb works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, randomEffectVariance = 1, family = negative.binomial(theta = 1.2, link = "log"))


            fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))

            runEverything(fittedModel, testData)
          }
)



test_that("glm.nb from MASS works",
          {
            skip_on_cran()

            testData = createData(sampleSize = 200, randomEffectVariance = 1, family = negative.binomial(theta = 1.2, link = "log"))

            fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
            runEverything(fittedModel, testData)
          }
)


