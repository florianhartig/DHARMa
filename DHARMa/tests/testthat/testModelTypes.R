context("Tests DHARMa functions on all implemented model types")

library(testthat)
library(DHARMa)
library(MASS)
library(lme4)
library(mgcv)
library(glmmTMB)
library(spaMM)

checkOutput <- function(simulationOutput){
  
  print(simulationOutput)
  
  if(any(simulationOutput$scaledResiduals < 0)) stop()
  if(any(simulationOutput$scaledResiduals > 1)) stop()
  if(any(is.na(simulationOutput$scaledResiduals))) stop()
  
  if(length(simulationOutput$scaledResiduals) != length(simulationOutput$observedResponse)) stop()
  if(length(simulationOutput$fittedPredictedResponse) != length(simulationOutput$observedResponse)) stop()
  
}

runEverything = function(fittedModel, testData, DHARMaData = T){
  
  cat("\n\n============= NEW MODEL ====================\n\n")
  
  print(class(fittedModel))
  
  t = DHARMa:::getResponse(fittedModel)
  
  x = getSimulations(fittedModel, 2)
  expect_equal(class(x), "data.frame")
  refit(fittedModel, x[[1]])
  
  #class(x[, 1:ncol(x)])
  
  simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 100)
  
  checkOutput(simulationOutput)
  
  testOutliers(simulationOutput)
  testDispersion(simulationOutput)
  expect_gt(testUniformity(simulationOutput = simulationOutput)$p.value, 0.001)
  testZeroInflation(simulationOutput = simulationOutput)
  testTemporalAutocorrelation(simulationOutput = simulationOutput, time = testData$time)
  testSpatialAutocorrelation(simulationOutput = simulationOutput, x = testData$x, y = testData$y)
  
  testDispersion(simulationOutput)
  
  simulationOutput <- recalculateResiduals(simulationOutput, group = testData$group)
  testDispersion(simulationOutput)
  
  simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T, n = 5) # n=10 is very low, set higher for serious tests
  
  checkOutput(simulationOutput2)
  
  plot(simulationOutput2, quantreg = F)
  
  testDispersion(simulationOutput2)
  
  simulationOutput2 <- recalculateResiduals(simulationOutput2, group = testData$group)
  testDispersion(simulationOutput2)
  
}


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



test_that("binomial 1/0 works",
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, fixedEffects = c(1,0), randomEffectVariance = 0, family = binomial())
            
            fittedModel <- glm(observedResponse ~ Environment1 + Environment2 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- gam(observedResponse ~ s(Environment1) ,family = "binomial", data = testData)
            runEverything(fittedModel, testData)
            
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


test_that("binomial y/n (factor) works",
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial(), factorResponse = T)
            
            fittedModel <- glm(observedResponse ~ Environment1 + Environment2 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- gam(observedResponse ~ s(Environment1) ,family = "binomial", data = testData)
            runEverything(fittedModel, testData)
            
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


test_that("glm binomial n/k works",
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
            
            fittedModel <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- gam(cbind(observedResponse1,observedResponse0) ~ s(Environment1) ,family = "binomial", data = testData)
            runEverything(fittedModel, testData)
            
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


test_that("glm poisson works",
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0.001, family = poisson())
            #testData = createData(sampleSize = 200, randomEffectVariance = 1, family = negative.binomial(theta = 1.2, link = "log"))
            
            fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- gam(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))
            runEverything(fittedModel, testData)
            
            fittedModel <- glmer.nb(observedResponse ~ Environment1 + (1|group) , data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))
            runEverything(fittedModel, testData)
            
            fittedModel <- glm.nb(observedResponse ~ Environment1,  data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- glmmTMB(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- glmmTMB(observedResponse ~ Environment1 + (1|group), zi=~1 , family = nbinom2, data = testData)
            runEverything(fittedModel, testData)
            
            fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
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

