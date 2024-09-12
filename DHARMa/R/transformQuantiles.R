#' Transform quantiles to pdf (deprecated)
#'
#' The purpose of this function was to transform the DHARMa quantile residuals (which have a uniform distribution) to a particular pdf. Since DHARMa 0.3.0, this functionality is integrated in the [residuals.DHARMa] function. Please switch to using this function.
#'
#' @param res an object with simulated residuals created by [simulateResiduals]
#' @param quantileFunction optional - a quantile function to transform the uniform 0/1 scaling of DHARMa to another distribution
#' @param outlierValue if a quantile function with infinite support (such as dnorm) is used, residuals that are 0/1 are mapped to -Inf / Inf. outlierValues allows to convert -Inf / Inf values to an optional min / max value.
#'
#' @export
#'
transformQuantiles <- function(res, quantileFunction = qnorm, outlierValue = 7){
  message("This function is deprecated. Please use residuals() instead")
}
