#' Function to simulate data
#' @export
#' @param overdispersion if this is a numeric value, it will be used as the sd of a random normal variate that is added to the linear predictor. Alternatively, a random function can be provided that takes as input the linear predictor. 
#' @example /inst/examples/createDataHelp.R
createData <- function(replicates=1, sampleSize = 10, intercept = 0, fixedEffects = 1, numGroups = 10, randomEffectVariance = 1, overdispersion = 0.5, family = poisson(), scale = 1, cor = 0, roundPoissonVariance = NULL, quadraticFixedEffects = NULL){
  
  nPredictors = length(fixedEffects)
  
  out = list()
  for (i in 1:replicates){
    
    predictors = matrix(runif(nPredictors*sampleSize, min = -1), ncol = nPredictors)
    
    if (cor != 0){
      predTemp <- runif(sampleSize, min = -1)
      predictors  = (1-cor) * predictors + cor * matrix(rep(predTemp, nPredictors), ncol = nPredictors)
    }
    
    colnames(predictors) = paste("Environment", 1:nPredictors, sep = "")
    

    
    group = rep(1:numGroups, each = sampleSize/numGroups)
    groupRandom = rnorm(numGroups, sd = randomEffectVariance)
    
    
    linearResponse = intercept + predictors %*% fixedEffects + groupRandom[group] 
    
    if(!is.null(quadraticFixedEffects)){
      linearResponse = linearResponse + predictors^2 %*% quadraticFixedEffects
    }
    
    if(is.numeric(overdispersion)) linearResponse = linearResponse + rnorm(sampleSize, sd = overdispersion)
    if(is.function(overdispersion)) linearResponse = linearResponse + overdispersion(linearResponse)
    
    linkResponse = family$linkinv(linearResponse)
    
    if (family$family == "gaussian") observedResponse = rnorm(n = sampleSize, mean = linkResponse, sd = scale)  
    else if (family$family == "binomial") observedResponse = rbinom(n = sampleSize, 1, prob = linkResponse)
    else if (family$family == "poisson") {
      if(is.null(roundPoissonVariance)) observedResponse = rpois(n = sampleSize, lambda = linkResponse)
      else observedResponse = round(rnorm(n = length(linkResponse), mean = linkResponse, sd = roundPoissonVariance))
    }
    
    else stop("wrong link argument supplied")
    
    out[[i]] <- data.frame(cbind(ID = 1:sampleSize, observedResponse, predictors, group))
  }
  if(length(out) == 1) out = out[[1]]
  return(out)
}
#createData()
