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
#' @param alternative whether to test for "overdispersion", "underdispersion", or "both" (both reduces power)
#' @details The function implements two tests, depending on whether it is applied on a simulation with refit = F, or refit = T. 
#' 
#' If refit = F (not recommended), the function tests if the IQR of the scaled residuals deviate from the null hypothesis of a uniform distribution. Simulations show that this option is not properly calibrated and much less powerful than the parametric alternative \code{\link{testOverdispersionParametric}} and even the simple \code{\link{testUniformity}}, and therefore it's use is not recommended. A warning will be returned if the function is called. 
#' 
#' If refit = T, the function compares the approximate deviance (via squared pearson residuals) with the same quantity from the models refitted with simulated data. It is much slower than the parametric alternative \code{\link{testOverdispersionParametric}}, but simulations show that it is slightly more powerful than the latter, and more powerful than any other non-parametric test in DHARMa, and it doesn't make any parametric assumptions. However, given the computational cost, I would suggest that most users will be satisfied with the parametric overdispersion test. 
#' 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersionParametric}}
#' @export
testOverdispersion <- function(simulationOutput, alternative = "overdispersion", plot = F){
  
  out = list()
  
  if(simulationOutput$refit == F){
    warning("You have called the non-parametric test for overdispersion based on the scaled residuals. Simulations show that this test is less powerful for detecting overdispersion than the default uniform test on the scaled residuals, and a lot less powerful than a parametric overdispersion test, or the non-parametric test on re-simulated residuals. The test you called is only implemented for testing / development purposes, there is no scenario where it would be preferred. See vignette for details.")
    observed = IQR(simulationOutput$scaledResiduals)
    sims = matrix(runif(simulationOutput$nObs * 1000), nrow = 1000)
    ss = apply(sims, 1, IQR)
    out$statistic = c(dispersion = observed / mean(ss))
    out$method = "DHARMa nonparametric overdispersion test via IQR of scaled residuals against IQR expected under uniform"
  } else {
    observed = sum(residuals(simulationOutput$fittedModel, type = "pearson")^2)
    ss = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
    out$statistic = c(dispersion = observed / mean(ss))
    out$method = "DHARMa nonparametric overdispersion test via comparison to simulation under H0 = fitted model"
  }
  
  #stop("Overdispersion test requires simulated residuals with refit = T") 
  
  p = ecdf(ss)(observed)
  
  if(alternative == "overdispersion") p = 1-p
  if(alternative == "underdispersion") p = p  
  if(alternative == "both") p = min(p, 1-p) * 2     
  
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"

  if(plot == T) {
    hist(ss, xlim = range(ss, observed, na.rm = T))
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
  out$method = "DHARMa zero-inflation test via comparison to expected zeros with simulation under H0 = fitted model"
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"

  if(plot == T) {
    hist(zerosExpected, xlim = range(zerosExpected, zerosObserved, na.rm=T ))
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
testTemporalAutocorrelation <- function(simulationOutput, time = NULL , plot = T){
  
  if(is.null(time)) time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
  
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
#' @param x the x coordinate, in the same order as the data points. If not provided, random values will be created
#' @param y the x coordinate, in the same order as the data points. If not provided, random values will be created
#' @param distMat optional distance matrix. If not provided, a distance matrix will be calculated based on x and y. See details for explanation
#' @param plot whether to plot output
#' @details The function performs Moran.I test from the package ape, based on the provided distance matrix of the data points. 
#' 
#' There are several ways to specify this distance. If a distance matrix (distMat) is provided, calculations will be based on this distance matrix, and x,y coordinates will only used for the plotting (if provided)
#' If distMat is not provided, the function will calculate the euclidian distances between x,y coordinates, and test Moran.I based on these distances.
#' 
#'    It is possible to not specify x and y. In this case, random x and y values are created. The sense of this option is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation). This may be useful because it may be that the test doesn't have noninal error rates due to some problem in the residual structure that is different from spatial autocorrelation 
#' 
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSimulatedResiduals}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @import grDevices
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y  = NULL, distMat = NULL, plot = T){
  
  if( !is.null(x) & !is.null(distMat) ) 
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
