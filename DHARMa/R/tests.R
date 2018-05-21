#' DHARMa general residual test
#' 
#' Calls both uniformity and dispersion test
#'
#' This function is a wrapper for the various test functions implemented in DHARMa. Currently, this function calls the \code{\link{testUniformity}} and the \code{\link{testDispersion}} functions. All other tests (see below) have to be called by hand.
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @author Florian Hartig
#' @export
#' @seealso \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
testResiduals <- function(simulationOutput){
  
  oldpar = par(mfrow = c(1,2))
  out = list()
  out$uniformity = testUniformity(simulationOutput)
  out$dispersion = testDispersion(simulationOutput)
  
  par(oldpar)
  print(out)
  return(out)
}

#' Residual tests
#' 
#' @details Deprecated, switch your code to using the \code{\link{testResiduals}} function
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @author Florian Hartig
#' @export
testSimulatedResiduals <- function(simulationOutput){
  message("testSimulatedResiduals is deprecated, switch your code to using the testResiduals function")
  testResiduals(simulationOutput)
}


#' Test for overall uniformity 
#' 
#' This function tests the overall uniformity of the simulated residuals in a DHARMa object
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot if T, plots calls \code{\link{plotQQunif}} as well 
#' @details The function applies a KS test for uniformity on the simulated residuals
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testUniformity<- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = T){
  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  return(out)
}


#' DHARMa dispersion tests
#' 
#' This function performs a simulation-based test for over/underdispersion 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param plot whether to plot output
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. Greate corresponds to overdispersion.   
#' @param ... arguments to pass on to \code{\link{testGeneric}}
#' @details The function implements two tests, depending on whether it is applied on a simulation with refit = F, or refit = T. 
#' 
#' If refit = F (not recommended), the function tests if the IQR of the scaled residuals deviate from the null hypothesis of a uniform distribution. Simulations show that this option is not properly calibrated and much less powerful than the parametric alternative \code{\link{testOverdispersionParametric}} and even the simple \code{\link{testUniformity}}, and therefore it's use is not recommended. A warning will be returned if the function is called. 
#' 
#' If refit = T, the function compares the approximate deviance (via squared pearson residuals) with the same quantity from the models refitted with simulated data. It is much slower than the parametric alternative \code{\link{testOverdispersionParametric}}, but simulations show that it is slightly more powerful than the latter, and more powerful than any other non-parametric test in DHARMa, and it doesn't make any parametric assumptions. However, given the computational cost, I would suggest that most users will be satisfied with the parametric overdispersion test. 
#' 
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @example inst/examples/testsHelp.R
#' @export
testDispersion <- function(simulationOutput, alternative = c("greater", "two.sided", "less"), plot = T, ...){
  
  alternative <- match.arg(alternative)
  
  if(simulationOutput$refit == F){
    spread <- function(x) sd(x - simulationOutput$fittedPredictedResponse) 
    out = testGeneric(simulationOutput, summary = spread, alternative = alternative, methodName = "DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated", plot = plot, ...)
  } else {

    out = list()
    
    observed = tryCatch(sum(residuals(simulationOutput$fittedModel, type = "pearson")^2), error = function(e) {
      message(paste("DHARMa: the requested tests requires pearson residuals, but your model does not implement these calculations. Test will return NA. Error message:", e))
      return(NA)
    })
    if(is.na(observed)) return(NA)
    expected = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
    out$statistic = c(dispersion = observed / mean(expected))
    out$method = "DHARMa nonparametric dispersion test via mean deviance residual fitted vs. simulated-refitted"
    
    p = getP(simulated = expected, observed = observed, alternative = alternative)

    out$alternative = alternative
    out$p.value = p
    out$data.name = deparse(substitute(simulationOutput))
    class(out) = "htest"

    if(plot == T) {
      plotTitle = gsub('(.{1,50})(\\s|$)', '\\1\n', out$method)
      xLabel = paste("Simulated values, red line = fitted model. p-value (",out$alternative, ") = ", out$p.value, sep ="")
      hist(expected, xlim = range(expected, observed, na.rm=T ), col = "lightgrey", main = plotTitle, xlab = xLabel, breaks = 20)
      abline(v = observed, lwd= 2, col = "red")
    }
  }

  return(out)
}

#' Simulated overdisperstion tests
#' 
#' @details Deprecated, switch your code to using the \code{\link{testDispersion}} function
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param ... additional arguments to \code{\link{testDispersion}}
#' @export
testOverdispersion <- function(simulationOutput, ...){
  message("plotSimulatedResiduals is deprecated, switch your code to using the testDispersion function")
  testDispersion(simulationOutput, ...)
}

#' Parametric overdisperstion tests
#' 
#' @details Deprecated, switch your code to using the \code{\link{testDispersion}} function. The function will do nothing, arguments will be ignored, the parametric tests is no longer recommend
#' 
#' @param ... arguments will be ignored, the parametric tests is no longer recommend
#' @export
testOverdispersionParametric <- function(...){
  message("testOverdispersionParametric is deprecated and no longer recommended, see release notes in DHARMA 0.2.0 - switch your code to using the testDispersion function")
  return(0)
}


#' Tests for zero-inflation 
#' 
#' This function compares the observed number of zeros with the zeros expected from simulations. 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param ... further arguments to \code{\link{testGeneric}}
#' @details shows the expected distribution of zeros against the observed
#' @author Florian Hartig
#' @example inst/examples/testsHelp.R
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testZeroInflation <- function(simulationOutput, ...){
  countZeros <- function(x) sum( x == 0)
  testGeneric(simulationOutput = simulationOutput, summary = countZeros, methodName = "DHARMa zero-inflation test via comparison to expected zeros with simulation under H0 = fitted model", ... )
}


#' Generic simulation test of a summary statistic
#' 
#' This function tests if a user-defined summary differs when applied to simulated / observed data. 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param summary a function that can be applied to simulated / observed data. See examples below
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot whether to plot the simulated summary
#' @param methodName name of the test (will be used in plot)
#' 
#' @details This function tests if a user-defined summary differs when applied to simulated / observed data. the function can easily be remodeled to apply summaries on the residuals, by simply defining f = function(x) summary (x - predictions), as done in \code{\link{testDispersion}}
#' 
#' @note The function that you supply is applied on the data as it is represented in your fitted model, which may not always correspond to how you think. This is important in particular when you use k/n binomial data, and want to test for 1-inflation. As an example, if have k/20 observations, and you provide your data via cbind (y, y-20), you have to test for 20-inflation (because this is how the data is represented in the model). However, if you provide data via y/20, and weights = 20, you should test for 1-inflation. In doubt, check how the data is internally represented in model.frame(model), or via simulate(model)
#' 
#' @export
#' @author Florian Hartig
#' @example inst/examples/testsHelp.R
#' 
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
testGeneric <- function(simulationOutput, summary, alternative = c("greater", "two.sided", "less"), plot = T, methodName = "DHARMa generic simulation test"){
  
  alternative <- match.arg(alternative)
  
  observed = summary(simulationOutput$observedResponse)
  
  simulated = apply(simulationOutput$simulatedResponse, 2, summary)
  
  p = getP(simulated = simulated, observed = observed, alternative = alternative)

  out = list()
  out$statistic = c(ratioObsSim = observed / mean(simulated))
  out$method = methodName
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
                           
  class(out) = "htest"
                           
  if(plot == T) {
    plotTitle = gsub('(.{1,50})(\\s|$)', '\\1\n', methodName)
    xLabel = paste("Simulated values, red line = fitted model. p-value (",out$alternative, ") = ", out$p.value, sep ="")
   hist(simulated, xlim = range(simulated, observed, na.rm=T ), col = "lightgrey", main = plotTitle, xlab = xLabel, breaks = 20)
   abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}


#' Test for temporal autocorrelation
#' 
#' This function performs a standard test for temporal autocorrelation on the simulated residuals
#' 
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param time the time, in the same order as the data points. If set to "random", random values will be created
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot whether to plot output
#' @note The sense of being able to run the test with time = NULL (random values) is to test the rate of false positives under the current residual structure (random time corresponds to H0: no spatial autocorrelation), e.g. to check if the test has noninal error rates for particular residual structures (note that Durbin-Watson originally assumes normal residuals, error rates seem correct for uniform residuals, but may not be correct if there are still other residual problems).
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testSpatialAutocorrelation}}
#' @example inst/examples/testTemporalAutocorrelationHelp.R
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time = NULL , alternative = c("greater", "two.sided", "less"), plot = T){
  
  if(is.null(time)) time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time, alternative = alternative)
  
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
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot whether to plot output
#' @details The function performs Moran.I test from the package ape, based on the provided distance matrix of the data points. 
#' 
#' There are several ways to specify this distance. If a distance matrix (distMat) is provided, calculations will be based on this distance matrix, and x,y coordinates will only used for the plotting (if provided)
#' If distMat is not provided, the function will calculate the euclidian distances between x,y coordinates, and test Moran.I based on these distances.
#' 
#' The sense of being able to run the test with x/y = NULL (random values) is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation), e.g. to check if the test has noninal error rates for particular residual structures.
#' 
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}} 
#' @import grDevices
#' @example inst/examples/testSpatialAutocorrelationHelp.R
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y  = NULL, distMat = NULL, alternative = c("greater", "two.sided", "less"), plot = T){
  
  alternative <- match.arg(alternative)
  
  if( !is.null(x) & !is.null(distMat) ) warning("coordinates and distMat provided, coordinates will only be used for plotting")
  # if not provided, fill x and y with random numbers (Null model)
  if(is.null(x)) x = runif(simulationOutput$nObs, -1,1) 
  if(is.null(y)) y = runif(simulationOutput$nObs, -1,1)
  
  # if not provided, create distance matrix based on x and y
  if(is.null(distMat)) distMat <- as.matrix(dist(cbind(x, y)))
  
  invDistMat <- 1/distMat
  diag(invDistMat) <- 0
  
  MI = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat, alternative = alternative)
  
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


getP <- function(simulated, observed, alternative){

  if(alternative == "greater") p = mean(simulated <= observed)
  if(alternative == "less") p = mean(simulated >= observed) 
  if(alternative == "two.sided") p = min(min(mean(simulated <= observed), mean(simulated >= observed) ) * 2,1)    
  
  return(p)
}

