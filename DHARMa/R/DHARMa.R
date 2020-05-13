#' @title DHARMa - Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models
#' @name DHARMa
#' @docType package
#' @description The 'DHARMa' package uses a simulation-based approach to create  readily interpretable scaled (quantile) residuals for fitted (generalized) linear mixed models. Currently supported are linear and generalized linear (mixed) models from 'lme4' (classes 'lmerMod', 'glmerMod'), 'glmmTMB' and 'spaMM', generalized additive models ('gam' from 'mgcv'), 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. Moreover, externally created simulations, e.g. posterior predictive simulations from Bayesian software such as 'JAGS', 'STAN', or 'BUGS' can be processed as well. The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial and temporal autocorrelation.
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
#' @param quantileFunction optional - a quantile function to transform the uniform 0/1 scaling of DHARMa to another distribution
#' @param outlierValues if a quantile function with infinite support (such as dnorm) is used, residuals that are 0/1 are mapped to -Inf / Inf. outlierValues allows to convert -Inf / Inf values to an optional min / max value.
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @details the function accesses the slot $scaledResiduals in a fitted DHARMa object, and optionally transforms the standard DHARMa quantile residuals (which have a uniform distribution) to a particular pdf.
#'
#' @note some of the papers on simulated quantile residuals transforming the residuals (which are natively uniform) back to a normal distribution. I presume this is because of the larger familiarity of most users with normal residuals. Personally, I never considered this desirable, for the reasons explained in https://github.com/florianhartig/DHARMa/issues/39, but with this function, I wanted to give users the option to plot normal residuals if they so wish.
#'
#' @export
#' @example inst/examples/simulateResidualsHelp.R
#'
residuals.DHARMa <- function(object, quantileFunction = NULL, outlierValues = NULL, ...){

  if(is.null(quantileFunction)){
    return(object$scaledResiduals)
  } else {
    res = quantileFunction(object$scaledResiduals)
    if(!is.null(outlierValues)){
      res = ifelse(res == -Inf, outlierValues[1], res)
      res = ifelse(res == Inf, outlierValues[2], res)
    }
    return(res)
  }
}



#' Return outliers
#'
#' Returns the outliers of a DHARMa object
#'
#' @param object an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param lowerQuantile lower threshold for outliers. Default is zero = outside simulation envelope
#' @param upperQuantile upper threshold for outliers. Default is 1 = outside simulation envelope
#' @param return wheter to return an indices of outliers or a logical vector
#'
#' @details First of all, note that the standard definition of outlier in the DHARMa plots and outlier tests is an observation that is outside the simulation envelope. How far outside that is depends a lot on how many simulations you do. If you have 100 data points and to 100 simulations, you would expect to have one "outlier" on average, even with a perfectly fitting model. This is in fact what the outlier test tests.
#'
#' Thus, keep in mind that for a small number of simulations, outliers are mostly a technical term: these are points that are outside our simulations, but we don't know how far away they are.
#'
#' If you are seriously interested in HOW FAR outside the expected distribution a data point is, you should increase the number of simulations in \code{\link{simulateResiduals}} to be sure to get the tail of the data distribution correctly. In this case, it may make sense to adjust lowerQuantile and upperQuantile, e.g. to 0.025, 0.975, which would define outliers as values outside the central 95% of the distribution.
#'
#' Also, note that outliers are particularly concerning if they have a strong influence on the model fit. One could test the influence, for example, by removing them from the data, or by some meausures of leverage, e.g. generalisations for Cook's distance as in Pinho, L. G. B., Nobre, J. S., & Singer, J. M. (2015). Cook’s distance for generalized linear mixed models. Computational Statistics & Data Analysis, 82, 126–136. doi:10.1016/j.csda.2014.08.008. At the moment, however, no such function is provided in DHARMa.
#'
#' @export
#'
outliers <- function(object, lowerQuantile = 0, upperQuantile = 1, return = c("index", "logical")){

  return = match.arg(return)

  out = residuals(object) >= upperQuantile | residuals(object) <= lowerQuantile

  if(return == "logical") return(out)
  else(return(which(out)))
}



#' Create a DHARMa object from hand-coded simulations or Bayesian posterior predictive simulations
#'
#' @param simulatedResponse matrix of observations simulated from the fitted model - row index for observations and colum index for simulations
#' @param observedResponse true observations
#' @param fittedPredictedResponse optional fitted predicted response. For Bayesian posterior predictive simulations, using the median posterior prediction as fittedPredictedResponse is recommended. If not provided, the mean simulatedResponse will be used.
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Unlike in \code{\link{simulateResiduals}}, the nature of the data is not automatically detected, so this MUST be set by the user appropriately
#' @param seed the random seed to be used within DHARMa. The default setting, recommended for most users, is keep the random seed on a fixed value 123. This means that you will always get the same randomization and thus teh same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. FALSE = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @param method the quantile randomization method used. The two options implemented at the moment are probability integral transform (PIT-) residuals (current default), and the "traditional" randomization procedure, that was used in DHARMa until version 0.3.0. For details, see \code{\link{getQuantile}}
#' @details The use of this function is to convert simulated residuals (e.g. from a point estimate, or Bayesian p-values) to a DHARMa object, to make use of the plotting / test functions in DHARMa
#' @note Either scaled residuals or (simulatedResponse AND observed response) have to be provided
#' @example inst/examples/createDharmaHelp.R
#' @export
createDHARMa <- function(simulatedResponse , observedResponse , fittedPredictedResponse = NULL, integerResponse = F, seed = 123,  method = c("PIT", "traditional")){

  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  match.arg(method)

  out = list()
  out$simulatedResponse = simulatedResponse
  out$refit = F
  out$integerResponse = integerResponse
  out$observedResponse = observedResponse

  if(!is.matrix(simulatedResponse) & !is.null(observedResponse)) stop("either scaled residuals or simulations and observations have to be provided")
  if(ncol(simulatedResponse) < 2) stop("simulatedResponse with less than 2 simulations provided - cannot calculate residuals on that.")

  if(ncol(simulatedResponse) < 10) warning("simulatedResponse with less than 10 simulations provided. This rarely makes sense")

  out$nObs = length(observedResponse)

  if (out$nObs < 3) stop("warning - number of observations < 3 ... this rarely makes sense")

  if(! (out$nObs == nrow(simulatedResponse))) stop("dimensions of observedResponse and simulatedResponse do not match")

  out$nSim = ncol(simulatedResponse)

  out$scaledResiduals = getQuantile(simulations = simulatedResponse , observed = observedResponse , integerResponse = integerResponse, method = method)


  # makes sure that DHARM plots that rely on this vector won't crash
  if(is.null(fittedPredictedResponse)){
    message("No fitted predicted response provided, using the mean of the simulations")
    fittedPredictedResponse = apply(simulatedResponse, 1, mean)
  }
  out$fittedPredictedResponse = fittedPredictedResponse
  out$randomState = randomState
  class(out) = "DHARMa"
  return(out)
}


#' Ensures that an object is of class DHARMa
#'
#' @param simulationOutput a DHARMa simulation output or an object that can be converted into a DHARMa simulation output
#' @param convert if TRUE, attempts to convert model + numeric to DHARMa, if "Model", converts only supported models to DHARMa
#' @details The
#' @return an object of class DHARMa
#' @keywords internal
ensureDHARMa <- function(simulationOutput,
                         convert = F){

  if(inherits(simulationOutput, "DHARMa")){
    return(simulationOutput)
  } else {

    if(convert == FALSE) stop("wrong argument to function, simulationOutput must be a DHARMa object!")
    else {

      if (class(simulationOutput)[1] %in% getPossibleModels()){
        if (convert == "Model" | convert == T) return(simulateResiduals(simulationOutput))
      } else if(is.vector(simulationOutput, mode = "numeric") & convert == T) {
        out = list()
        out$scaledResiduals = simulationOutput
        out$nObs = length(out$scaledResiduals)
        class(out) = "DHARMa"
        return(out)
      }
    }
  }
  stop("wrong argument to function, simulationOutput must be a DHARMa object or a numeric vector of quantile residuals!")
}


