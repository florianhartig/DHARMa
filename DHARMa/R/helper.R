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



#' Quantile calculations
#' 
#' @keywords internal
getQuantile <- function(simulations, observed, n, nSim, integerResponse, seed){
  
  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  
  scaledResiduals = rep(NA, n)
  
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
  
  for (i in 1:n){
    if(integerResponse == T){
      scaledResiduals[i] <- DHARMa.ecdf(simulations[i,] + runif(nSim, -0.5, 0.5))(observed[i] + runif(1, -0.5, 0.5))
      
    }else{
      scaledResiduals[i] <- DHARMa.ecdf(simulations[i,])(observed[i])
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
#' @details modified from https://github.com/lcolladotor/dots
#' 
#' @keywords internal
checkDots <- function(name, value, ...) {
  args <- list(...)
  if(!name %in% names(args)) {
    ## Default value
    return(value)
  } else {
    ## If the argument was defined in the ... part, return it
    return(args[[name]])
  }
}
