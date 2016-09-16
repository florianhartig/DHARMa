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