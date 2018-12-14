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
    stop("'x' must have 1 or more non-missing values")
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
getQuantile <- function(simulations, observed, n, nSim, integerResponse){
  
  scaledResiduals = rep(NA, n)
  
  if(integerResponse == F){
    
    values = c(as.vector(t(simulations)), observed)
    
    if(any (duplicated(values))){
      integerResponse = T
      # repeated = unique(values[which(duplicated(values))])      
      if(any(integerResponse)) message("Model family was recognized or set as continuous, but duplicate values were detected in the simulation - changing to integer residuals (see ?simulateResiduals for details)")
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
