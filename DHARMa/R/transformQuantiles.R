#' Transform quantiles to pdf
#'
#' The purpose of this function is to transform the DHARMa quantile residuals (which have a uniform distribution) to a particular pdf. 
#' 
#' @param res DHARMa residuals
#' @param quantileFunction the quantile function of the desired distribution
#' @param outlierValue the value that should be assigned to residuals that are 0/1 and thus typically mapped to -Inf / Inf for continous distributions with infinte support
#' 
#' @details See also https://github.com/florianhartig/DHARMa/issues/39
#' 
#' @example inst/examples/transformQuantilesHelp.R
#'
transformQuantiles <- function(res, quantileFunction = qnorm, outlierValue = 7){
  
  res = quantileFunction(res$scaledResiduals)
  res = ifelse(res == -Inf, -outlierValue, res)
  res = ifelse(res == Inf, outlierValue, res)
  return(res)
}






