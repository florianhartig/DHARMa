#' Uniformity benchmarks
#' 
#' This function runs simulation to confirm uniformity of residuals under H0
#' 
#' @param dataModelCreator a function that returns a list with two elements, data and model. See help for details
#' @param nSim number of simulations
#' @param plot should a plot be created
#' @param ... parameters to pass on to \code{\link{simulateResiduals}}
#' @details This function runs repeated simulations to test of residuals are really uniform if the data-creating process and the model that is used to fit the data are identical
#' @note The benchmark function in DHARMa are intended for development purposes, and for users that want to test / confirm the properties of functions in DHARMa. If you are running an applied data analysis, they are probably of little use. 
#' @export
#' @seealso \code{\link{benchmarkOverdispersion}}
#' @example inst/examples/benchmarkUniformityHelp.R
benchmarkUniformity <- function(dataModelCreator,  nSim = 100, plot = T, ...){
  
  if(class(dataModelCreator)[1] %in% getPossibleModels()) dataModelCreator = generateGenerator(dataModelCreator)
    
  nData = nrow(dataModelCreator()$data)  

  out = matrix(NA, nrow = nSim, ncol = nData)
  predicted =  matrix(NA, nrow = nSim, ncol = nData)
  outRefit = matrix(NA, nrow = nSim, ncol = nData)
  
  for (i in 1:nSim){
    temp = dataModelCreator()
    
    testData = temp$data
    fittedModel <- temp$model
    simResults = simulateResiduals(fittedModel = fittedModel, ...)
    out[i,] = simResults$scaledResiduals
    predicted[i,] =  simResults$fittedPredictedResponse
  }
  
  if(plot == T){
    oldpar <- par(mfrow = c(4,4))
    hist(out, breaks = 50, col = "red", main = paste("mean of", nSim, "simulations"))
    for (i in 1:min(nSim, 15)) hist(out[i,], breaks = 50, freq = F, main = i)
    par(oldpar)    
  }
  
  return(list(residuals = out, predictedResponse = predicted))
  
}

generateGenerator <- function(mod){
  
  out <- function(){
    
    simulations = simulate(mod, nsim = 1)

    newData <-model.frame(mod)  
    
    if(is.vector(simulations[[1]])){
      newData[,1] = simulations[[1]]
    } else {
      # Hack to make the binomial n/k case work
      newData[[1]] = NULL
      newData = cbind(simulations[[1]], newData)
    }
    
    refittedModel = update(mod, data = newData)
    
    list(data = newData, model = refittedModel)
    
  }

  
}



