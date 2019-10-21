#' Transform quantiles to pdf
#'
#' The purpose of this function is to transform the DHARMa quantile residuals (which have a uniform distribution) to a particular pdf. 
#' 
#' @param res DHARMa residuals
#' @param quantileFunction the quantile function of the desired distribution
#' @param outlierValue the value that should be assigned to residuals that are 0/1 and thus typically mapped to -Inf / Inf for continous distributions with infinte support
#' 
#' @details Some of the papers on simulated quantile residuals transforming the residuals (which are natively uniform) back to a normal distribution. I presume this is because of the larger familiarity of most users with normal residuals. Personally, I never considered this desirable, for the reasons explained in https://github.com/florianhartig/DHARMa/issues/39, but with this function, I wanted to give users the option to plot normal residuals if they so wish. 
#' @export
#' @example inst/examples/transformQuantilesHelp.R
#'
transformQuantiles <- function(res, quantileFunction = qnorm, outlierValue = 7){
  
  res = quantileFunction(res$scaledResiduals)
  res = ifelse(res == -Inf, -outlierValue, res)
  res = ifelse(res == Inf, outlierValue, res)
  return(res)
}