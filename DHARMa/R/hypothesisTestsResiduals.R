#' Performs automatic tests of the simulated residuals
#' @param simulationOutput a simulation output
#' @export
testSimulatedResiduals <- function(simulationOutput){
 
  pValueUnivariate <- testUnivariateDistribution(simulationOutput$scaledResiduals)
  
  out = list(pValueUnivariate = pValueUnivariate)
  
  return(out)
}
#plotConcentionalResiduals(fittedModel)

#' Tests residuals against a uniform distribution
#' @details Tests against uniform distribution with KS test 
#' @export
testUnivariateDistribution <- function(residuals, print = T){
  
  pValueUnivariate <- suppressWarnings(ks.test(residuals, 'punif'))
  if(print == T) pValueUnivariate
  return(pValueUnivariate)
}



#' Tests for zero-inflation in the residuals
#' @details Tests against uniform distribution with KS test 
#' @export
testZeroInflation <- function(simulationOutput, print = T, plot = T){
  
  countZeros <- function(x) sum( x == 0)
  
  zerosObserved = countZeros(simulationOutput$observedResponse)
  
  zerosExpected = apply(simulationOutput$simulatedResponse, 2, countZeros)
  

  out = c(expected = mean(zerosExpected), sdExpected = sd(zerosExpected), observed = zerosObserved)   

  if(print == T) out
  if(plot == T) {
    hist(zerosExpected, xlim = range(zerosExpected, zerosObserved ))
    abline(v = zerosObserved, lwd= 2, col = "red")
  }
  return(out)
}