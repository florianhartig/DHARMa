#' DHARMa residual tests
#' 
#' Wrapper for all residual tests implemented in DHARMa
#'
#' This function is a wrapper for the various test functions implemented in DHARNa. Currently, this function calls only the \code{\link{testUniformity}} function. Other tests (see below) have to be called by hand 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param test a string or a vector of string with the tests to be performed. 
#' @param ... parameters to be passed on to the test.
#' @details Currently, this function calls only the \code{\link{testUniformity}} function. All other tests (see below) have to be called by hand. 
#' @export
#' @seealso \code{\link{testUniformity}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{te
testResiduals <- function(simulationOutput, test = c(), ...){
  testUniformity(simulationOutput, ...)
}


#' Residual tests
#' 
#' @details Deprecated, switch your code to using the \code{\link{testResiduals}} function
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param ... additional arguments to \code{\link{testResiduals}}
#' @export
testSimulatedResiduals <- function(simulationOutput){
  message("testSimulatedResiduals is deprecated, switch your code to using the testResiduals function")
  testResiduals(simulationOutput, ...)
}


#' Test for overall uniformity 
#' 
#' This function tests the overall uniformity of the simulated residuals in a DHARMa object
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @details The function applies a KS test for uniformity on the simulated residuals
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @export
testUniformity<- function(simulationOutput){
  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif'))
  return(out)
}

