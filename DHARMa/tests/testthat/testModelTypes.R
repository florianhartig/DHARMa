context("Tests DHARMa functions on all implemented model types")

library(lme4)


test_that("glmer binomial 1/0 works", 
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 500, overdispersion = 2, family = binomial())
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            plotSimulatedResiduals(simulationOutput = simulationOutput)
            testSimulatedResiduals(simulationOutput = simulationOutput)
          }
)


test_that("glm binomial 1/0 works", 
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 500, overdispersion = 2, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 , family = "binomial", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            plotSimulatedResiduals(simulationOutput = simulationOutput)
            testSimulatedResiduals(simulationOutput = simulationOutput)
          }
)


test_that("lm works on 1/0 data", 
          {
            
            skip_on_cran()
            
            testData = createData(sampleSize = 500, overdispersion = 2, family = binomial())
            fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            plotSimulatedResiduals(simulationOutput = simulationOutput)
            testSimulatedResiduals(simulationOutput = simulationOutput)
          }
)


test_that("lmer works on 1/0 data", 
          {
            
            skip_on_cran()
            testData = createData(sampleSize = 500, overdispersion = 2, family = binomial())
            
            fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData)

            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            plotSimulatedResiduals(simulationOutput = simulationOutput)
            testSimulatedResiduals(simulationOutput = simulationOutput)
            
          }
)


