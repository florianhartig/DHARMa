#' @title DHARMa - Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models
#' @name DHARMa
#' @docType package
#' @description The DHARMa package uses a simulation-based approach to create readily interpretable scaled residuals from fitted generalized linear mixed models. Currently supported are 'merMod' classes from 'lme4', 'glm' (except quasi-distributions) and 'lm' model classes. The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model mispecification problem, such as over/underdispersion, zero-inflation, and spatial and temporal autocorrelation.
#' @details See index / vignette for details
#' @seealso \code{\link{simulateResiduals}}
#' @examples 
#' vignette("DHARMa", package="DHARMa")
NULL


#' Plot simulated residuals
#' 
#' @param x an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... optional arguments to pass on to \code{\link{plotSimulatedResiduals}}
#' @export
plot.DHARMa <- function(x, ...){
  plotSimulatedResiduals(x, ...)
}

#' Print simulated residuals
#' 
#' @param x an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... optional arguments to pass on to \code{\link{plotSimulatedResiduals}}
#' @export
print.DHARMa <- function(x, ...){
  cat(paste("Object of Class DHARMa with simulated residuals based on", x$nSim, "simulations with refit =", x$refit , ". See ?DHARMa::simulateResiduals for help."), "\n", "\n")
  if (length(x$scaledResiduals) < 20) cat("Scaled residual values:", x$scaledResiduals)
  else {
    cat("Scaled residual values:", x$scaledResiduals[1:20], "...")
  } 
} 


#' Convert simulated residuals to a DHARMa object
#' 
#' @param scaledResiduals optional scaled residuals from a simulation, e.g. Bayesian p-values. If those are not provided, simulated and true observations have to be provided. 
#' @param simulatedResponse matrix of observations simulated from the fitted model - row index for observations and colum index for simulations 
#' @param observedResponse true observations
#' @param fittedPredictedResponse fitted predicted response. Optional, but will be neccessary for some plots. If scaled residuals are Bayesian p-values, using the median posterior prediction as fittedPredictedResponse is recommended. 
#' @details The use of this function is to convert simulated residuals (e.g. from a point estimate, or Bayesian p-values) to a DHARMa object, to make use of the plotting / test functions in DHARMa 
#' @note Either scaled residuals or (simulatedResponse AND observed response) have to be provided 
#' @export
createDHARMa <- function(scaledResiduals = NULL, simulatedResponse = NULL, observedResponse = NULL, fittedPredictedResponse = NULL, integerResponse = F){
  out = list()

  if(is.null(scaledResiduals)) {
    if(!is.matrix(simulatedResponse) & !is.null(observedResponse)) stop("either scaled residuals or simulations and observations have to be provided")
    
    out$nObs = length(observedResponse)
    out$nSim = ncol(simulatedResponse)
      
    scaledResiduals = rep(NA, out$nObs)
    
    for (i in 1:out$nObs){
      
      if(integerResponse == T){
        scaledResiduals[i] <- ecdf(simulatedResponse[i,] + runif(out$nSim, -0.5, 0.5))(observedResponse[i] + runif(1, -0.5, 0.5))           
      }else{
        scaledResiduals[i] <- ecdf(simulatedResponse[i,])(observedResponse[i])
      }
    }
  }
  out$observedResponse = observedResponse
  out$scaledResiduals = scaledResiduals
  out$nObs = length(scaledResiduals)
  if(is.null(fittedPredictedResponse)) fittedPredictedResponse = rep(1, out$nObs) # makes sure that DHARM plots that rely on this vector won't crash
  out$fittedPredictedResponse = fittedPredictedResponse
  class(out) = "DHARMa"
  return(out)
}

