#' creates Poisson data overdispersion and random intercept
plotSimulatedResiduals <- function(simulationOutput){
  par(mfrow = c(1,2))
  
  hist(simulationOutput$scaledResiduals, main = "Distribution of scaled residuals")
  plot(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")
}


#plot(simulationOutput$observedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")

#plot(simulationOutput$observedResponse, simulationOutput$fittedPredictedResponse - simulationOutput$observedResponse)
