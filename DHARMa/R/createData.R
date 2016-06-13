#' Function to simulate data
#' @export
createData <- function(replicates=1, sampleSize = 10, intercept = 0, fixedEffects = 1, numGroups = 10, randomEffectVariance = 1, overdispersion = 0.5, family = poisson(), scale = 1, cor = 0){
  
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
    
    
    linearResponse = intercept + predictors %*% fixedEffects + groupRandom[group] + rnorm(sampleSize, sd = overdispersion)
    linkResponse = family$linkinv(linearResponse)
    
    if (family$family == "gaussian") observedResponse = rnorm(n = sampleSize, mean = linkResponse, sd = scale)  
    else if (family$family == "binomial") observedResponse = rbinom(n = sampleSize, 1, prob = linkResponse)
    else if (family$family == "poisson") observedResponse = rpois(n = sampleSize, lambda = linkResponse)
    else stop("wrong link argument supplied")
    
    out[[i]] <- data.frame(cbind(ID = 1:sampleSize, observedResponse, predictors, group))
  }
  if(length(out) == 1) out = out[[1]]
  return(out)
}
#createData()
