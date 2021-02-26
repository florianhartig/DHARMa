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
#' @param bigData indicates whether to use optimized calculations for `simulations` objects that are stored on disk as an `ff_matrix`. Defaults to `FALSE`.
#'
#' @details The function calculates residual quantiles from the simulated data. For continuous distributions, this will simply the the value of the ecdf.
#'
#' For discrete data, there are two options implemented.
#'
#' The current default (available since DHARMa 0.3.1) are probability integral transform (PIT-) residuals (Smith, 1985; Dunn & Smyth, 1996; see also see also Warton, et al., 2017).
#'
#' Before DHARMa 0.3.1, a different randomization procedure was used, in which the a U(-0.5, 0.5) distribution was added on observations and simulations for discrete distributions. For a completely discrete distribution, the two procedures should deliver equivalent results, but the second method has the disadvantage that a) one has to know if the distribution is discrete (DHARMa tries to recognize this automatically), and b) that it leads to inefficiencies for some distributions such as the the Tweedie, which are partly continuous, partly discrete (see e.g. https://github.com/florianhartig/DHARMa/issues/168).
#'
#' @references
#'
#' Smith, J. Q. "Diagnostic checks of non-standard time series models." Journal of Forecasting 4.3 (1985): 283-291.
#'
#' Dunn, P.K., & Smyth, G.K. (1996). Randomized quantile residuals. Journal of Computational and Graphical Statistics 5, 236-244.
#'
#' Warton, David I., Loïc Thibaut, and Yi Alice Wang. "The PIT-trap—A “model-free” bootstrap procedure for inference about regression models with discrete, multivariate responses." PloS one 12.7 (2017)
#'
#' @rawNamespace import(ff, except = c(write.csv, write.csv2))
#' @export
getQuantile <- function(simulations, observed, integerResponse, method = c("PIT", "traditional"), bigData = FALSE){

  method = match.arg(method)
  if (bigData == TRUE) {
    if (method == "traditional") {
      message("For large datasets probability integral transform (PIT) residuals are recommended to improve performance. Use method = 'PIT' to achieve this.")
    }
    method = "PIT"
  }

  n = length(observed)
  if (nrow(simulations) != n) stop("DHARMa::getquantile: wrong dimension of simulations")
  nSim = ncol(simulations)


  if(method == "traditional"){

    if(integerResponse == F){

      if(any(duplicated(observed))) message("Model family was recognized or set as continuous, but duplicate values were detected in the response. Consider if you are fitting an appropriate model.")

      if (bigData == FALSE){
        # Check for duplicates across all simulations
        duplicatesDetected = FALSE
        integerDetected = FALSE

        values = as.vector(simulations)[duplicated(as.vector(simulations))]
        if (length(values) > 0) duplicatesDetected <- TRUE
        if (all(values%%1==0)) integerDetected <- TRUE

      } else {
        # Check for duplicates in each simulation independently (i.e. column-wise)

        # Hold results for each column
        duplicatesDetected = rep(FALSE, nSim)
        integerDetected = rep(FALSE, nSim)

        for (i in 1:nSim) {
          values = as.vector(simulations)[duplicated(as.vector(simulations[, i]))]
          if (length(values) > 0) duplicatesDetected[i] = TRUE
          if (duplicatesDetected[i] == TRUE & all(values%%1==0)) integerDetected[i] = TRUE
        }
      }


      if (any(duplicatesDetected) == TRUE){
        if (any(integerDetected == TRUE)){
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

    scaledResiduals = rep(NA, n)

    if (bigData == FALSE){

      # This is only a small performance gain (~ 20%) over putting these
      # calculations into the for loop, as in the original implementation.
      minSim <- rowSums(apply(simulations, MARGIN = 2, function(x) x < observed)) / nSim
      maxSim <- rowSums(apply(simulations, MARGIN = 2, function(x) x <= observed)) / nSim

    } else {

      # TODO: Memory footprint is controlled by BATCHBYTES. Should be set large
      # enough to hold at least two columns of the simulation matrix (if
      # BATCHSIZE = 2, which is the minimum). Use heuristics to set BATCHBYTES
      # based on nObs? Expose as package option?
      minSim <- ffcolapply(rowSums(simulations[,i1:i2] < observed),
                           X=simulations, RETURN = TRUE, CFUN = "csum", BATCHBYTES = 2^30, BATCHSIZE = 2) / nSim
      maxSim <- ffcolapply(rowSums(simulations[,i1:i2] <= observed),
                           X=simulations, RETURN = TRUE, CFUN = "csum", BATCHBYTES = 2^30, BATCHSIZE = 2) / nSim

    }


    for (i in 1:n){
      if (minSim[i] == maxSim[i]) scaledResiduals[i] = minSim[i]
      else scaledResiduals[i] = runif(1, minSim[i], maxSim[i])
    }

    # # Alternative to for-loop with mapply (no apparent improvements in terms
    # # of memory use or processing time):
    #
    # scaledResiduals <-
    #   mapply(function(min, max) if (min == max) min else runif(1, min, max),
    #          minSim, maxSim, SIMPLIFY = TRUE, USE.NAMES = FALSE)


    # # Previous implementation without apply functions:
    #
    # for (i in 1:n){
    #   minSim <- mean(simulations[i,] < observed[i])
    #   maxSim <- mean(simulations[i,] <= observed[i])
    #   else scaledResiduals[i] = runif(1, minSim, maxSim)
    # }

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

