#' creates Poisson data overdispersion and random intercept
plotSimulatedResiduals <- function(simulationOutput){
  par(mfrow = c(1,2), oma = c(0,1,2,1))
  
  gap::qqunif(simulationOutput$scaledResiduals,pch=21,bg="blue",bty="n", logscale = F)
  #hist(simulationOutput$scaledResiduals, main = "Distribution of scaled residuals", breaks = 50, freq = F)
  #lines(density(simulationOutput$scaledResiduals, na.rm = T, from = 0, to = 1, kernel = "rectangular", bw = 0.01, cut = 0.01), col = "red")
  
  plot(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")
  
  mtext("DHARMa scaled residual plots", outer = T)
}


#plot(simulationOutput$observedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")

#plot(simulationOutput$observedResponse, simulationOutput$fittedPredictedResponse - simulationOutput$observedResponse)

#plot(cumsum(sort(simulationOutput$scaledResiduals)))

plotConcentionalResiduals <- function(fittedModel){
  par(mfrow = c(1,3), oma = c(0,1,2,1))
  plot(predict(fittedModel), resid(fittedModel, type = "deviance"), main = "Deviance" , ylab = "Residual", xlab = "predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "pearson") , main = "Pearson", ylab = "Residual", xlab = "predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "response") , main = "Raw residuals" , ylab = "Residual", xlab = "predicted")  
  mtext("Conventional residual plots", outer = T)
}

#plotConcentionalResiduals(fittedModel)

