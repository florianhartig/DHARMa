
#' Test for temporal autocorrelation
#' 
#' This function performs a standard test for temporal autocorrelation on the simulated residuals
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param time the time, in the same order as the data points. If set to "random", random values will be created
#' @param plot whether to plot output
#' @note The sense of being able to run the test with time = NULL (random values) is to test the rate of false positives under the current residual structure (random time corresponds to H0: no spatial autocorrelation), e.g. to check if the test has noninal error rates for particular residual structures (note that Durbin-Watson originally assumes normal residuals, error rates seem correct for uniform residuals, but may not be correct if there are still other residual problems).
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @example inst/examples/testTemporalAutocorrelationHelp.R
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time = NULL , plot = T){
  
  if(is.null(time)) time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time)
  
  if(plot == T) {
    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    plot(simulationOutput$scaledResiduals ~ time, col = rgb(col, maxColorValue = 255))
  }
  return(out)
}


#' Test for spatial autocorrelation
#' 
#' This function performs a standard test for spatial autocorrelation on the simulated residuals
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param x the x coordinate, in the same order as the data points. If not provided, random values will be created
#' @param y the x coordinate, in the same order as the data points. If not provided, random values will be created
#' @param distMat optional distance matrix. If not provided, a distance matrix will be calculated based on x and y. See details for explanation
#' @param plot whether to plot output
#' @details The function performs Moran.I test from the package ape, based on the provided distance matrix of the data points. 
#' 
#' There are several ways to specify this distance. If a distance matrix (distMat) is provided, calculations will be based on this distance matrix, and x,y coordinates will only used for the plotting (if provided)
#' If distMat is not provided, the function will calculate the euclidian distances between x,y coordinates, and test Moran.I based on these distances.
#' 
#' The sense of being able to run the test with x/y = NULL (random values) is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation), e.g. to check if the test has noninal error rates for particular residual structures.
#' 
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @import grDevices
#' @example inst/examples/testSpatialAutocorrelationHelp.R
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y  = NULL, distMat = NULL, plot = T){
  
  if( !is.null(x) & !is.null(distMat) ) warning("coordinates and distMat provided, coordinates will only be used for plotting")
  # if not provided, fill x and y with random numbers (Null model)
  if(is.null(x)) x = runif(simulationOutput$nObs, -1,1) 
  if(is.null(y)) y = runif(simulationOutput$nObs, -1,1)
  
  # if not provided, create distance matrix based on x and y
  if(is.null(distMat)) distMat <- as.matrix(dist(cbind(x, y)))
  
  invDistMat <- 1/distMat
  diag(invDistMat) <- 0
  
  MI = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat)
  
  out = list()
  out$statistic = c(observed = MI$observed, expected = MI$expected, sd = MI$sd)
  out$method = "DHARMa Moran's I test for spatial autocorrelation"
  out$alternative = "Spatial autocorrelation"
  out$p.value = MI$p.value
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"
  
  if(plot == T) {
    
    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    
    plot(x,y, col = rgb(col, maxColorValue = 255) )
  }
  return(out)
}
