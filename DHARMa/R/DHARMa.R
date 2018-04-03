#' @title DHARMa - Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models
#' @name DHARMa
#' @docType package
#' @description The 'DHARMa' package uses a simulation-based approach to create readily interpretable scaled (quantile) residuals for fitted generalized linear mixed models. Currently supported are generalized linear mixed models from 'lme4' (classes 'lmerMod', 'glmerMod') and 'glmmTMB', generalized additive models ('gam' from 'mgcv'), 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. Alternatively, externally created simulations, e.g. posterior predictive simulations from Bayesian software such as 'JAGS', 'STAN', or 'BUGS' can be processed as well. The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial and temporal autocorrelation.
#' @details See index / vignette for details
#' @seealso \code{\link{simulateResiduals}}
#' @examples 
#' vignette("DHARMa", package="DHARMa")
NULL


#' DHARMa standard residual plots
#' 
#' This function creates standard plots for the simulated residuals
#' @param simulationOutput an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @details The function creates two plots. To the left, a qq-uniform plot to detect deviations from overall uniformity of the residuals (calling \code{\link{plotQQunif}}), and to the right, a plot of residuals against predicted values (calling \code{\link{plotResiduals}}). For a correctly specified model, we would expect 
#' 
#' a) a straight 1-1 line in the uniform qq-plot -> evidence for an overal uniform (flat) distribution of the residuals
#' 
#' b) uniformity of residuals in the vertical direction in the res against predictor plot
#' 
#' Deviations of this can be interpreted as for a liner regression. See the vignette for detailed examples. 
#' 
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predited values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small. See further comments on this plot, and options, in \code{\link{plotResiduals}}
#' 
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. This is default for n > 2000. 
#' 
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @import graphics
#' @import utils
#' @export
plot.DHARMa <- function(x, ...){

  oldpar <- par(mfrow = c(1,2), oma = c(0,1,2,1))
  
  plotQQunif(simulationOutput)
  
  plotResiduals(pred = simulationOutput, residuals = NULL, xlab = "Predicted value (rank transformed)", ylab = "Standardized residual", main = "Residual vs. predicted\n lines should match", rank = T, ...)
  
  mtext("DHARMa scaled residual plots", outer = T)
  
  par(oldpar)
}


#' Print simulated residuals
#' 
#' @param x an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... optional arguments for compatibility with genetic, no function implemented
#' @export
print.DHARMa <- function(x, ...){
  cat(paste("Object of Class DHARMa with simulated residuals based on", x$nSim, "simulations with refit =", x$refit , ". See ?DHARMa::simulateResiduals for help."), "\n", "\n")
  if (length(x$scaledResiduals) < 20) cat("Scaled residual values:", x$scaledResiduals)
  else {
    cat("Scaled residual values:", x$scaledResiduals[1:20], "...")
  } 
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

  if(is.matrix(scaledResiduals) | is.data.frame(scaledResiduals)) stop("DHARMa::createDHARMa - matrix provided to parmaeter scaledResiduals - if you want to provide simulations, use parameter simulatedResponse")
  
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
        #message("createDHARMa called with integerResponse = F. Note that this setting ")
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

