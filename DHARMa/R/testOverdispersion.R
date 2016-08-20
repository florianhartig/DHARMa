#' Simulated overdipersion test
#' @param model a fitted model object. See details for possible models
#' @param alternative whether to test for overdispersion, underdispersion, or both
#' @details This test compares the approximate deviance (via squared pearson residuals) with the same quantity from a number of simulated models.
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testOverdispersion <- function(simulationOutput, alternative = "overdispersion", print = F, plot = F){
  
  if(is.null(simulationOutput$refittedPearsonResiduals)) stop("Overdispersion test requires a simulation object with refit = T")
  
  observed = sum(residuals(simulationOutput$fittedModel, type = "pearson")^2)
  
  ss = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
  
  p = ecdf(ss)(observed)
  
  if(alternative == "overdispersion") p = 1-p
  if(alternative == "underdispersion") p = p  
  if(alternative == "both") p = min(p, 1-p) * 2     
  
  out = list()
  out$statistic = c(dispersion = observed / mean(ss))
  out$method = "Overdispersion test via comparison to simulation under H0"
  out$alternative = alternative
  out$p.value = p
  out$data.name = fittedModel@call$family
  
  class(out) = "htest"

  if(print == T) out
  if(plot == T) {
    hist(ss, xlim = range(ss, observed ))
    abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}
