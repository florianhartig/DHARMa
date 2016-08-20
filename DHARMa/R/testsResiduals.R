#' Wrapper for the various test functions
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @details Currently, this function calls only the \code{\link{testUniformity}} function. All other tests (see below) have to be called by hand. 
#' @export
#' @note for special problems, in particular overdispersion, a \code{\link{parametricDispersionTest}} will likely be more powerful 
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}
testSimulatedResiduals <- function(simulationOutput){
  
  out$pValueUnivariate <- testUniformity(simulationOutput)
  
  return(out)
}



#' Tests residuals against a uniform distribution
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param print whether to print output
#' @details Tests residuals against a uniform distribution with the KS test 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}
#' @export
testUniformity<- function(simulationOutput, print = T){
  
  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif'))
  if(print == T) out
  return(out)
}


#' Simulated overdipersion test
#' @param model a fitted model object. See details for possible models
#' @param alternative whether to test for overdispersion, underdispersion, or both
#' @details This test compares the approximate deviance (via squared pearson residuals) with the same quantity from a number of simulated models.
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testOverdispersion <- function(simulationOutput, alternative = "overdispersion", print = F, plot = F){
  
  if(is.null(simulationOutput$refittedPearsonResiduals)) stop("Overdispersion test requires a simulation object with refit = T")
  
  observed = sum(residuals(simulationOutput$fittedModel, type = "pearson")^2)
  
  ss = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
  
  p = ecdf(ss)(observed)
  
  if(alternative == "overdispersion") p = 1-p
  if(alternative == "underdispersion") p = p  
  if(alternative == "both") p = min(p, 1-p) * 2     
  
  out = list()
  out$statistic = c(dispersion = observed / mean(ss))
  out$method = "Overdispersion test via comparison to simulation under H0"
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"
  
  if(print == T) out
  if(plot == T) {
    hist(ss, xlim = range(ss, observed ))
    abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}



#' Tests for zero-inflation in the residuals
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param print whether to print output
#' @param plot whether to plot output
#' @details shows the expected distribution of zeros against the observed
#' @seealso \code{\link{testUniformity}}, \code{\link{testSimulatedResiduals}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}
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
#' @param time the time, in the same order as the data points. If set to "random", random values will be created
#' @param print whether to print output
#' @param plot whether to plot output
#' @note It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time , print = T, plot = T){
  
  if(length(time) == 1) if(time == "random") time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time)
  
  if(print == T) out
  if(plot == T) {
    plot(simulationOutput$scaledResiduals ~ time)
  }
  return(out)
}


#' Convenience Function to test for spatial autocorrelation
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param x the x coordinate, in the same order as the data points. If set to "random", random values will be created
#' @param y the x coordinate, in the same order as the data points. If set to "random", random values will be created
#' @param print whether to print output
#' @param plot whether to plot output
#' @note It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation
#' @details performs the Durbin-Watson Test against euklidian distance and plots the residuals against time
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testOverdispersion}}
#' @import grDevices
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x , y , print = T, plot = T){
  
  if(length(x) == 1) if (x == "random") x = runif(simulationOutput$nObs, -1,1) 
  if(length(y) == 1) if (y == "random") y = runif(simulationOutput$nObs, -1,1)

  
  distMat <- as.matrix(dist(cbind(x, y)))
  
  invDistMat <- 1/distMat
  diag(invDistMat) <- 0
  
  MI = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat)
  
  out = list()
  out$statistic = c(observed = MI$observed, expected = MI$expected, sd = MI$sd)
  out$method = "Moran's I"
  out$alternative = "Spatial autocorrelation"
  out$p.value = MI$p.value
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"
  
  if(print == T) out
  if(plot == T) {
    
    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    
    plot(x,y, col = rgb(col, maxColorValue = 255) )
  }
  return(out)
}
