#' Transform quantiles to pdf (deprecated)
#'
#' The purpose of this function was to transform the DHARMa quantile residuals (which have a uniform distribution) to a particular pdf. Since DHARMa 0.3.0, this functionality is integrated in the \code{\link{residuals.DHARMa}} function. Please switch to using this function.
#' 
#' @export
#'
transformQuantiles <- function(res, quantileFunction = qnorm, outlierValue = 7){
  message("This function is deprecated. Please use residuals() instead")
}