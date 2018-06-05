#' @title DHARMa - Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models
#' @name DHARMa
#' @docType package
#' @description The 'DHARMa' package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted generalized linear mixed models. Currently supported are generalized linear mixed models from 'lme4' (classes 'lmerMod', 'glmerMod') and 'glmmTMB', generalized additive models ('gam' from 'mgcv'), 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. Alternatively, externally created simulations, e.g. posterior predictive simulations from Bayesian software such as 'JAGS', 'STAN', or 'BUGS' can be processed as well. The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial and temporal autocorrelation.
#' @details See index / vignette for details
#' @seealso \code{\link{simulateResiduals}}
#' @examples 
#' vignette("DHARMa", package="DHARMa")
NULL


#' Print simulated residuals
#' 
#' @param x an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @export
print.DHARMa <- function(x, ...){
  cat(paste("Object of Class DHARMa with simulated residuals based on", x$nSim, "simulations with refit =", x$refit , ". See ?DHARMa::simulateResiduals for help."), "\n", "\n")
  if (length(x$scaledResiduals) < 20) cat("Scaled residual values:", x$scaledResiduals)
  else {
    cat("Scaled residual values:", x$scaledResiduals[1:20], "...")
  } 
} 

#' Return residuals of a DHARMa simulation
#' 
#' @param object an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @details the function accesses the slot $scaledResiduals in a fitted DHARMa object
#' @export
#' @example inst/examples/simulateResidualsHelp.R
#'
residuals.DHARMa <- function(object, ...){
  return(object$scaledResiduals)
}

#' Convert simulated residuals or posterior predictive simulations to a DHARMa object
#' 
#' @param scaledResiduals optional scaled residuals from a simulation, e.g. Bayesian p-values. If those are not provided, simulated and true observations have to be provided. 
#' @param simulatedResponse matrix of observations simulated from the fitted model - row index for observations and colum index for simulations 
#' @param observedResponse true observations
#' @param fittedPredictedResponse fitted predicted response. Optional, but will be neccessary for some plots. If scaled residuals are Bayesian p-values, using the median posterior prediction as fittedPredictedResponse is recommended. 
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Unlike in \code{\link{simulateResiduals}}, the nature of the data is not automatically detected, so this MUST be set by the user appropriately
#' @details The use of this function is to convert simulated residuals (e.g. from a point estimate, or Bayesian p-values) to a DHARMa object, to make use of the plotting / test functions in DHARMa 
#' @note Either scaled residuals or (simulatedResponse AND observed response) have to be provided 
#' @example inst/examples/createDharmaHelp.R
#' @export
createDHARMa <- function(scaledResiduals = NULL, simulatedResponse = NULL, observedResponse = NULL, fittedPredictedResponse = NULL, integerResponse = F){

  out = list()
  
  if(is.null(scaledResiduals)) {
    if(!is.matrix(simulatedResponse) & !is.null(observedResponse)) stop("either scaled residuals or simulations and observations have to be provided")
    if(ncol(simulatedResponse) < 2) stop("simulatedResponse with less than 2 simulations provided - cannot calculate residuals on that.")
    
    if(ncol(simulatedResponse) < 10) warning("simulatedResponse with less than 10 simulations provided. This rarely makes sense")
    
    out$nObs = length(observedResponse)
    
    if (out$nObs < 3) stop("warning - number of observations < 3 ... this rarely makes sense")
    
    if(! (out$nObs == nrow(simulatedResponse))) stop("dimensions of observedResponse and simulatedResponse do not match")
    
    out$nSim = ncol(simulatedResponse)

    out$scaledResiduals = getQuantile(simulations = simulatedResponse , observed = observedResponse , n = out$nObs, nSim = out$nSim, integerResponse = integerResponse)
    
  } else {
    
    if(is.matrix(scaledResiduals) | is.data.frame(scaledResiduals)) stop("DHARMa::createDHARMa - matrix provided to parmaeter scaledResiduals - if you want to provide simulations, use parameter simulatedResponse")
    
    if(!is.vector(scaledResiduals)) stop("scaledResiduals should be a vector")
    out$scaledResiduals = scaledResiduals
    out$nObs = length(scaledResiduals)
  }
  
  out$observedResponse = observedResponse
  
  # makes sure that DHARM plots that rely on this vector won't crash  
  if(is.null(fittedPredictedResponse)){
    if(! is.null(simulatedResponse)){
      message("No fitted predicted response provided, using the mean of the simulations")
      fittedPredictedResponse = apply(simulatedResponse, 1, mean)         
    }else{
      message("No fitted predicted response provided, DHARMa will not be able to plot residuals against predicted")
      fittedPredictedResponse = as.factor(rep(1, out$nObs))       
    }
  }
  out$fittedPredictedResponse = fittedPredictedResponse       
 
  class(out) = "DHARMa"
  return(out)
}

