#' Performs automatic tests of the simulated residuals
#' @param simulationOutput a simulation output
#' @export
testSimulatedResiduals <- function(simulationOutput){
 
  pValueUnivariate <- testUnivariateDistribution(simulationOutput$scaledResiduals)
  
  out = list(pValueUnivariate = pValueUnivariate)
  
  return(out)
}
#plotConcentionalResiduals(fittedModel)

#' Tests residuals against a uniform distribution
#' @details Tests against uniform distribution with KS test 
#' @export
testUnivariateDistribution <- function(residuals, print = T){
  
  pValueUnivariate <- suppressWarnings(ks.test(residuals, 'punif'))
  if(print == T) pValueUnivariate
  return(pValueUnivariate)
}
