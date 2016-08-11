#' Wrapper for the various test functions
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @details Currently, this function calls only the \code{\link{testUniformDistribution}} function. All other tests (see below) have to be called by hand. 
#' @export
#' @note for special problems, in particular overdispersion, a \code{\link{parametricDispersionTest}} will likely be more powerful 
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
testSimulatedResiduals <- function(simulationOutput){
 
  out <- testUniformDistribution(simulationOutput$scaledResiduals)
  
  return(out)
}
#plotConcentionalResiduals(fittedModel)

#' Tests residuals against a uniform distribution
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param print whether to print output
#' @details Tests residuals against a uniform distribution with the KS test 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testUniformDistribution <- function(simulationOutput, print = T){
  
  out <- suppressWarnings(ks.test(simulationOutput, 'punif'))
  if(print == T) out
  return(out)
}



#' Tests for zero-inflation in the residuals
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param print whether to print output
#' @param plot whether to plot output
#' @details shows the expected distribution of zeros against the observed
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testSimulatedResiduals}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
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
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param time the time, in the same order as the data points
#' @param print whether to print output
#' @param plot whether to plot output
#' @note This function is provide as a base recommendation. 
#' @details performs the Durbin-Watson Test and plots the residuals against time
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testZeroInflation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testSpatialAutocorrelation}}
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
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param x the x coordinate, in the same order as the data points
#' @param y the x coordinate, in the same order as the data points
#' @note This function is provide as a base recommendation. 
#' @details performs the Durbin-Watson Test against euklidian distance and plots the residuals against time
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSimulatedResiduals}}
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
