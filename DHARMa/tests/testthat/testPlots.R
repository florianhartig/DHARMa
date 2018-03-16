
doPlots <- function(simulationOutput){
  plot(simulationOutput, quantreg = T, rank = F, asFactor = T)
  plot(simulationOutput, quantreg = F, rank = F)
  
  plot(simulationOutput, quantreg = T, rank = T)
  plot(simulationOutput, quantreg = F, rank = T)
  
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


test_that("Plots work",
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 ,family = "binomial", data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            doPlots(simulationOutput)
            
            
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 2, numGroups = 4, family = gaussian())
            fittedModel <- glm(observedResponse ~ group , data = testData)
            simulationOutput <- simulateResiduals(fittedModel = fittedModel)
            doPlots(simulationOutput)
            plotResiduals(testData$group, simulationOutput$scaledResiduals)
          }
)

