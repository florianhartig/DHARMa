context("Tests DHARMa functions on all implemented model types")

library(lme4)

runEverything = function(fittedModel){
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  plotSimulatedResiduals(simulationOutput = simulationOutput)
  testUniformDistribution(simulationOutput = simulationOutput)
  testZeroInflation(simulationOutput = simulationOutput)
  testTemporalAutocorrelation(simulationOutput = simulationOutput, time = runif(length(simulationOutput$scaledResiduals )))
  testSpatialAutocorrelation(simulationOutput = simulationOutput, x = runif(length(simulationOutput$scaledResiduals )), y =  runif(length(simulationOutput$scaledResiduals )))
}



test_that("lm gaussian works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
            runEverything(fittedModel)
          }
)


test_that("glm gaussian works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- glm(observedResponse ~ Environment1 , data = testData)
            runEverything(fittedModel)
          }
)

test_that("lmer gaussian works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData)
            runEverything(fittedModel)
          }
)


test_that("glm binomial 1/0 works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel)
          }
)


test_that("glm binomial n/k works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), bionomialTrials = 20)
            fittedModel <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel)
          }
)



test_that("glmer binomial 1/0 works", 
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, overdispersion = 2, family = binomial())
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel)
          }
)


test_that("glmer binomial n/k works", 
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, overdispersion = 2, family = binomial(), bionomialTrials = 20)
            fittedModel <- glmer(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel)
          }
)


test_that("glm poisson works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel)
          }
)



test_that("glmer poisson works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData)
            runEverything(fittedModel)
          }
)


