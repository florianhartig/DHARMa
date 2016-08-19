#' Wrapper for the various test functions
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @details Currently, this function calls only the \code{\link{testUniformDistribution}} function. All other tests (see below) have to be called by hand. 
#' @export
#' @note for special problems, in particular overdispersion, a \code{\link{parametricDispersionTest}} will likely be more powerful 
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
testSimulatedResiduals <- function(simulationOutput){
 
  out$pValueUnivariate <- testUniformDistribution(simulationOutput)
  
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
  
  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif'))
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
#' @note It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testZeroInflation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time = NULL, print = T, plot = T){
  
  if(is.null(time)) time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time)
  
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
#' @param print whether to print output
#' @param plot whether to plot output
#' @note It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation
#' @details performs the Durbin-Watson Test against euklidian distance and plots the residuals against time
#' @seealso \code{\link{testUniformDistribution}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSimulatedResiduals}}
#' @import grDevices
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y = NULL, print = T, plot = T){
  
  if(is.null(x)) x = runif(simulationOutput$nObs, -1,1)

  if(is.null(y)) y = runif(simulationOutput$nObs, -1,1)

  
  distMat <- as.matrix(dist(cbind(x, y)))
  
  invDistMat <- 1/distMat
  diag(invDistMat) <- 0
  
  out = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat)
  
  if(print == T) out
  if(plot == T) {
    
    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    
    plot(x,y, col = rgb(col, maxColorValue = 255) )
  }
  return(out)
}
