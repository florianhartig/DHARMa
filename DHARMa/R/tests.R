#' DHARMa general residual test
#' 
#' Calls both uniformity and dispersion test
#'
#' This function is a wrapper for the various test functions implemented in DHARMa. Currently, this function calls the \code{\link{testUniformity}} and the \code{\link{testDispersion}} functions. All other tests (see below) have to be called by hand.
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @author Florian Hartig
#' @export
#' @seealso \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
testResiduals <- function(simulationOutput){
  
  oldpar = par(mfrow = c(1,3))
  out = list()
  out$uniformity = testUniformity(simulationOutput)
  out$dispersion = testDispersion(simulationOutput)
  out$outliers = testOutliers(simulationOutput)
  
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
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. See \code{\link[stats]{ks.test}} for details  
#' @param plot if T, plots calls \code{\link{plotQQunif}} as well 
#' @details The function applies a \code{\link[stats]{ks.test}} for uniformity on the simulated residuals.
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testUniformity<- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = T){
  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  return(out)
}


# Experimental

testBivariateUniformity<- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = T){
  #out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  #if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  out = NULL
  return(out)
}


#unif.2017YMi(X, type = c("Q1", "Q2", "Q3"), lower = rep(0, ncol(X)),upper = rep(1, ncol(X)))

#' Test for outliers 
#' 
#' This function tests if the number of observations that are strictly greater / smaller than all simulations are larger than expected 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot if T, the function will create an additional plot
#' @details DHARMa residuals are created by simulating from the fitted model, and comparing the simulated values to the observed data. It can occur that all simulated values are higher or smaller than the observed data, in which case they get the residual value of 0 and 1, respectively. I refer to these values as simulation outliers, or simply outliers. 
#' 
#' Because no data was simulated in the range of the observed value, we actually don't know "how much" these values deviate from the model expecation, so the term "outlier" should be used with a grain of salt - it's not a judgement about the probability of a deviation from an expectation, but denotes that we are outside the simulated range. The number of outliers would usually decrease if the number of DHARMa simulations is increased. 
#'  
#' The probability of an outlier depends on the number of simulations (in fact, it is 1/(nSim +1) for each side), so whether the existence of outliers is a reason for concern depends also on the number of simulations. The expected number of outliers is therefore binomially distributed, and we can calculate a p-value from that
#' 
#' 
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testOutliers <- function(simulationOutput, alternative = c("two.sided", "greater", "less"), plot = T){

  alternative <- match.arg(alternative)
  
  out = list()  
  out$alternative = alternative
  out$method = "DHARMa outlier test based on exact binomial test"

  outLow = sum(simulationOutput$scaledResiduals == 0)
  outHigh = sum(simulationOutput$scaledResiduals == 1)
  trials = simulationOutput$nObs 
  outFreqH0 = 1/(simulationOutput$nSim +1)
  
  out$testlow = binom.test(outLow, trials, p = outFreqH0, alternative = "less")
  out$testhigh = binom.test(outHigh, trials, p = outFreqH0, alternative = "greater")

  out$statistic = c(outLow = outLow, outHigh = outHigh, nobs = trials, freqH0 = outFreqH0)
  
  if(alternative == "greater") p = out$testlow$p.value
  if(alternative == "less") p = out$testhigh$p.value
  if(alternative == "two.sided") p = min(min(out$testlow$p.value, out$testhigh$p.value) * 2,1)    
  
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  class(out) = "htest"  
  
  if(plot == T) {
    hist(simulationOutput)
    
  } 
  return(out)
}


#' DHARMa dispersion tests
#' 
#' This function performs a simulation-based test for over/underdispersion 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param plot whether to plot output
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. Greater corresponds to overdispersion.   
#' @param ... arguments to pass on to \code{\link{testGeneric}}
#' @details The function implements two tests, depending on whether it is applied on a simulation with refit = F, or refit = T. 
#' 
#' If refit = F, the function tests the sd of the data against the sd of the simulated data. 
#' 
#' If refit = T, the function compares the approximate deviance (via squared pearson residuals) with the same quantity from the models refitted with simulated data. Applying this is much slower than the previous alternative, but simulations show that it is slightly more powerful as well. 
#' 
#' However, given the computational cost, I would suggest that most users will be satisfied with the standard dispersion test. 
#' 
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}},  \code{\link{testOutliers}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @example inst/examples/testsHelp.R
#' @export
testDispersion <- function(simulationOutput, alternative = c("two.sided", "greater", "less"), plot = T, ...){
  
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
  message("testOverdispersion is deprecated, switch your code to using the testDispersion function")
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
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
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
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
testGeneric <- function(simulationOutput, summary, alternative = c("two.sided", "greater", "less"), plot = T, methodName = "DHARMa generic simulation test"){
  
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
#' @param time the time, in the same order as the data points. If not provided, random values will be created
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot whether to plot output
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#' 
#' If no time values are provided, random values will be created. The sense of being able to run the test with time = NULL (random values) is to test the rate of false positives under the current residual structure (random time corresponds to H0: no spatial autocorrelation), e.g. to check if the test has noninal error rates for particular residual structures (note that Durbin-Watson originally assumes normal residuals, error rates seem correct for uniform residuals, but may not be correct if there are still other residual problems).
#' 
#' Testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use the recalculateResiduals function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testSpatialAutocorrelation.
#' 
#' @note Important to note for all autocorrelation tests (spatial / temporal): the autocorrelation tests are valid to check for residual autocorrelation in models that don't assume such a correlation (in this case, you can use conditional or unconditional simulations), or if there is remaining residual autocorrelation after accounting for it in a spatial/temporal model (in that case, you have to use conditional simulations), but if checking unconditional simulations from a model with an autocorrelation structure on data that corresponds to this model, they will be significant, even if the model fully accounts for this structure. 
#' 
#' This behavior is not really a bug, but rather originates from the definition of the quantile residuals: quantile residuals are calculated independently per data point, i.e. without consideratin of any correlation structure between data points that may exist in the simulations. As a result, the simulated distributions from a unconditional simulaton will typically not reflect the correlation structure that is present in each single simulation, and the same is true for the subsequently calculated quantile residuals. 
#' 
#' The bottomline here is that spatial / temporal / other autoregressive models should either be tested based on conditional simulations, or (ideally) custom tests should be used that are not based on quantile residuals, but rather compare the correlation structure in the simulated data with the correlation structure in the observed data. 
#' 
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testSpatialAutocorrelation}}
#' @example inst/examples/testTemporalAutocorrelationHelp.R
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time = NULL , alternative = c("two.sided", "greater", "less"), plot = T){
  # actually not sure if this is neccessary for dwtest, but seems better to aggregate
  if(any(duplicated(time))) stop("testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use the recalculateResiduals function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testSpatialAutocorrelation.")
  
  alternative <- match.arg(alternative)
  
  if(is.null(time)){
    time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
    message("DHARMa::testTemporalAutocorrelation - no time argument provided, using random times for each data point")
  } 
  
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
#' @param y the y coordinate, in the same order as the data points. If not provided, random values will be created
#' @param distMat optional distance matrix. If not provided, a distance matrix will be calculated based on x and y. See details for explanation
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis  
#' @param plot whether to plot output
#' @details The function performs Moran.I test from the package ape, based on the provided distance matrix of the data points. 
#' 
#' There are several ways to specify this distance. If a distance matrix (distMat) is provided, calculations will be based on this distance matrix, and x,y coordinates will only used for the plotting (if provided)
#' If distMat is not provided, the function will calculate the euclidian distances between x,y coordinates, and test Moran.I based on these distances.
#' 
#' If no x/y values are provided, random values will be created. The sense of being able to run the test with x/y = NULL (random values) is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation), e.g. to check if the test has nominal error rates for particular residual structures.
#' 
#' Testing for spatial autocorrelation requires unique x,y values - if you have several observations per location, either use the recalculateResiduals function to aggregate residuals per location, or extract the residuals from the fitted object, and plot / test each of them independently for spatially repeated subgroups (a typical scenario would repeated spatial observation, in which case one could plot / test each time step separately for temporal autocorrelation). Note that the latter must be done by hand, outside testSpatialAutocorrelation.
#' 
#' @note Important to note for all autocorrelation tests (spatial / temporal): the autocorrelation tests are valid to check for residual autocorrelation in models that don't assume such a correlation (in this case, you can use conditional or unconditional simulations), or if there is remaining residual autocorrelation after accounting for it in a spatial/temporal model (in that case, you have to use conditional simulations), but if checking unconditional simulations from a model with an autocorrelation structure on data that corresponds to this model, they will be significant, even if the model fully accounts for this structure. 
#' 
#' This behavior is not really a bug, but rather originates from the definition of the quantile residuals: quantile residuals are calculated independently per data point, i.e. without consideratin of any correlation structure between data points that may exist in the simulations. As a result, the simulated distributions from a unconditional simulaton will typically not reflect the correlation structure that is present in each single simulation, and the same is true for the subsequently calculated quantile residuals. 
#' 
#' The bottomline here is that spatial / temporal / other autoregressive models should either be tested based on conditional simulations, or (ideally) custom tests should be used that are not based on quantile residuals, but rather compare the correlation structure in the simulated data with the correlation structure in the observed data. 
#' 
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}} 
#' @import grDevices
#' @example inst/examples/testSpatialAutocorrelationHelp.R
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y  = NULL, distMat = NULL, alternative = c("two.sided", "greater", "less"), plot = T){
  
  if(any(duplicated(cbind(x,y)))) stop("testing for spatial autocorrelation requires unique x,y values - if you have several observations per location, either use the recalculateResiduals function to aggregate residuals per location, or extract the residuals from the fitted object, and plot / test each of them independently for spatially repeated subgroups (a typical scenario would repeated spatial observation, in which case one could plot / test each time step separately for temporal autocorrelation). Note that the latter must be done by hand, outside testSpatialAutocorrelation.")
  
  alternative <- match.arg(alternative)

  if( (!is.null(x) | !is.null(y)) & !is.null(distMat) ) message("both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting")
  # if not provided, fill x and y with random numbers (Null model)
  if(is.null(x)){
    x = runif(simulationOutput$nObs, -1,1) 
    message("DHARMa::testSpatialAutocorrelation - no x coordinates provided, using random values for each data point")
  } 
  
  if(is.null(y)){
    y = runif(simulationOutput$nObs, -1,1)
    message("DHARMa::testSpatialAutocorrelation - no x coordinates provided, using random values for each data point")
  } 
  
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

  if(alternative == "greater") p = mean(simulated >= observed)
  if(alternative == "less") p = mean(simulated <= observed) 
  if(alternative == "two.sided") p = min(min(mean(simulated <= observed), mean(simulated >= observed) ) * 2,1)    
  
  return(p)
}

