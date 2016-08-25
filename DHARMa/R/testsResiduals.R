#' Residual tests
#' 
#' This is intended as a wrapper for the various test functions. Currently, this function calls only the \code{\link{testUniformity}} function. Other tests (see below) have to be called by hand
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @details Currently, this function calls only the \code{\link{testUniformity}} function. All other tests (see below) have to be called by hand. 
#' @export
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
testSimulatedResiduals <- function(simulationOutput){
  testUniformity(simulationOutput)
}



#' Test for overall uniformity 
#' 
#' This function tests the overall uniformity of the residuals
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @details Tests residuals against a uniform distribution with the KS test 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @export
testUniformity<- function(simulationOutput){
  
  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif'))
  return(out)
}


#' Test for over/underdispersion
#' 
#' This function performs a simulation-based test for over/underdispersion 
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param plot whether to plot output
#' @param alternative whether to test for "overdispersion", "underdispersion", or "both"
#' @details This test compares the approximate deviance (via squared pearson residuals) with the same quantity from a number of simulated models. It is MUCH slower than the parametric alternative \code{\link{testOverdispersionParametric}}, but potentially more exact in situations where one would expect problems with the chi2 test employed in the parametric test
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersionParametric}}
#' @export
testOverdispersion <- function(simulationOutput, alternative = "overdispersion", plot = F){
  
  if(simulationOutput$refit == F) stop("Overdispersion test requires simulated residuals with refit = T")
  
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

  if(plot == T) {
    hist(ss, xlim = range(ss, observed ))
    abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}



#' Tests for zero-inflation 
#' 
#' This function compares the observed number of zeros with the zeros expected from simulations. 
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param plot whether to plot output
#' @param alternative whether to test for 'more', 'less', or 'both' more or less zeros in the observed data
#' @details shows the expected distribution of zeros against the observed
#' @seealso \code{\link{testUniformity}}, \code{\link{testSimulatedResiduals}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @export
testZeroInflation <- function(simulationOutput,  plot = T, alternative = "more"){
  
  countZeros <- function(x) sum( x == 0)
  
  zerosObserved = countZeros(simulationOutput$observedResponse)
  
  zerosExpected = apply(simulationOutput$simulatedResponse, 2, countZeros)
  
  
  p = ecdf(zerosExpected)(zerosObserved)
  
  if(alternative == "more") p = 1-p
  if(alternative == "less") p = p  
  if(alternative == "both") p = min(p, 1-p) * 2     
  
  out = list()
  out$statistic = c(ratioObsExp = zerosObserved / mean(zerosExpected))
  out$method = "Zero-inflation test via comparison to expected zeros with simulation under H0"
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"

  if(plot == T) {
    hist(zerosExpected, xlim = range(zerosExpected, zerosObserved ))
    abline(v = zerosObserved, lwd= 2, col = "red")
  }
  return(out)
}


#' Test for temporal autocorrelation
#' 
#' This function performs a standard test for temporal autocorrelation on the simulated residuals
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param time the time, in the same order as the data points. If set to "random", random values will be created
#' @param plot whether to plot output
#' @note It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time , plot = T){
  
  if(length(time) == 1) if(time == "random") time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time)
  
  if(plot == T) {
    plot(simulationOutput$scaledResiduals ~ time)
  }
  return(out)
}


#' Test for spatial autocorrelation
#' 
#' This function performs a standard test for spatial autocorrelation on the simulated residuals
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param x the x coordinate, in the same order as the data points. If set to "random", random values will be created
#' @param y the x coordinate, in the same order as the data points. If set to "random", random values will be created
#' @param plot whether to plot output
#' @note It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation
#' @details performs the Durbin-Watson Test against euklidian distance and plots the residuals against time
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @import grDevices
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x , y , plot = T){
  
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
  
  if(plot == T) {
    
    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    
    plot(x,y, col = rgb(col, maxColorValue = 255) )
  }
  return(out)
}
