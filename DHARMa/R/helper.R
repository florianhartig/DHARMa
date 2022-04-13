#' Modified ECDF function
#'
#' @details ensures symmetric ECDF (standard ECDF is <), and that 0 / 1 values are only produced if the data is strictly < > than the observed data
#'
#' @keywords internal
DHARMa.ecdf <- function (x)
{
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    stop(paste("DHARMa.ecdf - length vector < 1", x))
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/ (n +1),
                    method = "linear", yleft = 0, yright = 1, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}



#' calculate quantiles
#'
#' calculates residual quantiles from a given simulation
#'
#' @param simulations a matrix with simulations from a fitted model. Rows = observations, columns = replicate simulations
#' @param observed a vector with the observed data
#' @param integerResponse is the response integer-valued. Only has an effect for method = "traditional"
#' @param method the quantile randomization method used. See details
#' @param rotation optional rotation of the residuals, either provided as a covariance matrix, or specify "estimated", in which case the residual covariance will be approximated by simulations. See comments in details
#'
#' @details The function calculates residual quantiles from the simulated data. For continuous distributions, this will simply the the value of the ecdf.
#'
#' **Randomization procedure for discrete data**
#'
#' For discrete data, there are two options implemented.
#'
#' The current default (available since DHARMa 0.3.1) are probability integral transform (PIT-) residuals (Smith, 1985; Dunn & Smyth, 1996; see also Warton, et al., 2017).
#'
#' Before DHARMa 0.3.1, a different randomization procedure was used, in which the a U(-0.5, 0.5) distribution was added on observations and simulations for discrete distributions. For a completely discrete distribution, the two procedures should deliver equivalent results, but the second method has the disadvantage that a) one has to know if the distribution is discrete (DHARMa tries to recognize this automatically), and b) that it leads to inefficiencies for some distributions such as the Tweedie, which are partly continuous, partly discrete
#' (see e.g. [issue #168](https://github.com/florianhartig/DHARMa/issues/168)).
#' 
#' **Rotation (optional)**
#' 
#' The getQuantile function includes an additional option to rotate residuals. The purpose is to de-correlated residuals in case of residual autocorrelation. If the expected residual autocorrelation is known (e.h. when fitting gls type models), it can be provided as a covariance matrix. If that is note the case, the option "estimated" will try to estimate the covariance from the data simulated by the model. Note, however, that this approximation will tend to have considerable error and may be slow to compute for high-dimensional data. 
#'
#' @references
#'
#' Smith, J. Q. "Diagnostic checks of non-standard time series models." Journal of Forecasting 4.3 (1985): 283-291.
#'
#' Dunn, P.K., & Smyth, G.K. (1996). Randomized quantile residuals. Journal of Computational and Graphical Statistics 5, 236-244.
#'
#' Warton, David I., Loïc Thibaut, and Yi Alice Wang. "The PIT-trap—A “model-free” bootstrap procedure for inference about regression models with discrete, multivariate responses." PloS one 12.7 (2017)
#'
#' @export
getQuantile <- function(simulations, observed, integerResponse, method = c("PIT", "traditional"), rotation = NULL){

  method = match.arg(method)

  n = length(observed)
  if (nrow(simulations) != n) stop("DHARMa::getquantile: wrong dimension of simulations")
  nSim = ncol(simulations)


  if(method == "traditional"){
    
    if(!is.null(rotation)) stop("rotation can only be used with PIT residuals")

    if(integerResponse == F){

      if(any(duplicated(observed))) message("Model family was recognized or set as continuous, but duplicate values were detected in the response. Consider if you are fitting an appropriate model.")

      values = as.vector(simulations)[duplicated(as.vector(simulations))]
      if(length(values) > 0){
        if (all(values%%1==0)){
          integerResponse = T
          message("Model family was recognized or set as continuous, but duplicate values were detected in the simulation - changing to integer residuals (see ?simulateResiduals for details)")
        } else {
          message("Duplicate non-integer values found in the simulation. If this is because you are fitting a non-inter valued discrete response model, note that DHARMa does not perform appropriate randomization for such cases.")
        }

      }
    }

    scaledResiduals = rep(NA, n)
    for (i in 1:n){
      if(integerResponse == T){
        scaledResiduals[i] <- DHARMa.ecdf(simulations[i,] + runif(nSim, -0.5, 0.5))(observed[i] + runif(1, -0.5, 0.5))
      }else{
        scaledResiduals[i] <- DHARMa.ecdf(simulations[i,])(observed[i])
      }
    }

  } else {
    
    # optional rotation before PIT 
    if(!is.null(rotation)){
      if(is.character(rotation) && rotation == "estimated"){
        covar = Matrix::nearPD(cov(t(simulations)))$mat
        L = t(as.matrix(Matrix::chol(covar)))
      } 
      else if(is.matrix(rotation)) L <- t(chol(rotation))
      else stop("DHARMa::getQuantile - wrong argument to rotation parameter")
      observed <- solve(L, observed)
      simulations = apply(simulations, 2, function(a) solve(L, a))
    }

    scaledResiduals = rep(NA, n)
    for (i in 1:n){
      minSim <- mean(simulations[i,] < observed[i]) 
      maxSim <- mean(simulations[i,] <= observed[i]) 
      if (minSim == maxSim) scaledResiduals[i] = minSim
      else scaledResiduals[i] = runif(1, minSim, maxSim)
    }
  }
  
  return(scaledResiduals)
}

#
#
# testData = createData(sampleSize = 200, family = gaussian(),
#                       randomEffectVariance = 0, numGroups = 5)
# fittedModel <- glmmTMB(observedResponse ~ Environment1,
#                    data = testData)
# simulationOutput <- simulateResiduals(fittedModel = fittedModel)
#
# sims = simulationOutput$simulatedResponse
# sims[1, c(1,6,8)] = 0
# any(apply(sims, 1, anyDuplicated))
# getQuantile(simulations = sims, observed = testData$observedResponse, n = 200, integerResponse = F, nSim = 250)
#
#
#



#' Check dot operator
#'
#' @param name variable name
#' @param default variable default
#'
#' @details modified from https://github.com/lcolladotor/dots
#'
#' @keywords internal
checkDots <- function(name, default, ...) {
  args <- list(...)
  if(!name %in% names(args)) {
    ## Default value
    return(default)
  } else {
    ## If the argument was defined in the ... part, return it
    return(args[[name]])
  }
}


securityAssertion <- function(context = "Not provided", stop = F){
  generalMessage = "Message from DHARMa: During the execution of a DHARMa function, some unexpected conditions occurred. Even if you didn't get an error, your results may not be reliable. Please check with the help if you use the functions as intended. If you think that the error is not on your side, I would be grateful if you could report the problem at https://github.com/florianhartig/DHARMa/issues \n\n Context:"
  if (stop == F) warning(paste(generalMessage, context))
  else stop(paste(generalMessage, context))
}

