test_that("Plots work",
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 ,family = "binomial", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            
            
            # standard plots
            
            plot(simulationOutput = simulationOutput, quantreg = T, rank = F)
            plot(simulationOutput = simulationOutput, quantreg = F, rank = F)
            
            plot(simulationOutput = simulationOutput, quantreg = T, rank = T)
            plot(simulationOutput = simulationOutput, quantreg = F, rank = T)
            
            plotSimulatedResiduals(simulationOutput = simulationOutput, quantreg = T, rank = F)
            plotSimulatedResiduals(simulationOutput = simulationOutput, quantreg = F, rank = F)
            
            plotSimulatedResiduals(simulationOutput = simulationOutput, quantreg = T, rank = T)
            plotSimulatedResiduals(simulationOutput = simulationOutput, quantreg = F, rank = T)
            
            # qq plot
            plotQQunif(simulationOutput = simulationOutput)
            
            # residual vs. X plots, various options
            plotResiduals(pred = simulationOutput)
            plotResiduals(pred = simulationOutput$fittedPredictedResponse, residuals = simulationOutput$scaledResiduals)
          }
)