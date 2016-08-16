#' Runs simulation to confirm uniformity of residuals under H0
#' @param dataCreator a function that returns a list with two elements, data and model. See help for details
#' @param nsim number of simulations
#' @param plot should a plot be created
#' @export
#' @example inst/examples/benchmarkUniformityHelp.R
benchmarkUniformity <- function(dataModelCreator,  nSim = 100, plot = T){
  
  nData = nrow(dataModelCreator()$data)  
  
  out = matrix(NA, nrow = nSim, ncol = nData)
  predicted =  matrix(NA, nrow = nSim, ncol = nData)
  outRefit = matrix(NA, nrow = nSim, ncol = nData)
  
  for (i in 1:nSim){
    temp = dataModelCreator()
    
    testData = temp$data
    fittedModel <- temp$model
    simResults = simulateResiduals(fittedModel = fittedModel, refit = F, n=250)
    out[i,] = simResults$scaledResiduals
    predicted[i,] =  simResults$fittedPredictedResponse
    #outRefit[i,] = simulateResiduals(fittedModel = fittedModel, n = 10, refit = T)$scaledResiduals
  }
  
  if(plot == T){
    oldpar <- par(mfrow = c(5,5))
    hist(out, breaks = 50, col = "red", main = paste("mean of", nSim, "simulations"))
    for (i in 1:24) hist(out[i,], breaks = 50, freq = F, main = i)
    par(oldpar)    
  }
}





