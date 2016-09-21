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
  print(paste("Class DHARMa with simulated residuals based on", x$nSim, "simulations with refit =", x$refit))
  print("see ?DHARMa::simulateResiduals for help")
  print("-----------------------------")
  print(x$scaledResiduals)
} 


#' Convert simulated residuals to a DHARMa object
#' 
#' @param scaledResiduals scaled residuals from a simulation, e.g. Bayesian p-values
#' @param fittedPredictedResponse fitted predicted response. Optional, but will be neccessary for some plots. If scaled residuals are Bayesian p-values, using the median posterior prediction as fittedPredictedResponse is recommended. 
#' @details The use of this function is to convert simulated residuals (e.g. from a point estimate, or Bayesian p-values) to a DHARMa object, to make use of the plotting / test functions in DHARMa 
#' @export
createDHARMa <- function(scaledResiduals, observedResponse = NULL, fittedPredictedResponse = NULL){
  out = list()
  out$scaledResiduals = scaledResiduals
  out$nObs = length(scaledResiduals)
  out$observedResponse = observedResponse
  out$fittedPredictedResponse = fittedPredictedResponse
  class(out) = "DHARMa"
  return(out)
}

