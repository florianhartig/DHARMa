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


#' Test for temporal autocorrelation
#' @param simulationOutput the result of a residual simulation
#' @param time the time, in the same order as the data points
#' @note This function is provide as a base recommendation. 
#' @details performs the Durbin-Watson Test and plots the residuals against time
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time, print = T, plot = T){
  
 
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ time)
  
  if(print == T) out
  if(plot == T) {
    plot(simulationOutput$scaledResiduals ~ time)
  }
  return(out)
}


#' Convenience Function to test for spatial autocorrelation
#' @param simulationOutput the result of a residual simulation
#' @param x the x coordinate, in the same order as the data points
#' @param y the x coordinate, in the same order as the data points
#' @note This function is provide as a base recommendation. 
#' @details performs the Durbin-Watson Test against euklidian distance and plots the residuals against time
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x, y, print = T, plot = T){
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ I(sqrt(x^2 + y^2)))
  
  if(print == T) out
  if(plot == T) {
    
    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    
    plot(x,y, col = rgb(col, max = 255) )
  }
  return(out)
}
