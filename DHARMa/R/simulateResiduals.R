#' @export
simulateResiduals <- function(x, ...) UseMethod("simulateResiduals")



#' Creates scaled residuals by simulation
#' @param fittedModel fitted model, currently restricted to lme4 models
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F){

  ptm <- proc.time()  
  
  nObs = nobs(fittedModel)
  out = list(
    nObs = nObs,
    observedResponse = model.frame(fittedModel)[,1],
    fittedPredictedResponse = predict(fittedModel, type = "response"),
    fittedFixedEffects = fixef(fittedModel), ## returns fixed effects 
    fittedRandomEffects = ranef(fittedModel), ## returns random effects
    
    simulatedResponse = data.matrix(simulate(fittedModel, nsim = n, use.u =F)),   
    scaledResiduals = rep(NA, nObs),
    
    # only for refit = T
    refittedPredictedResponse = NULL,
    refittedFixedEffects = NULL,
    refittedRandomEffects = NULL
  )

  if (refit == F){
 
    for (i in 1:out$nObs){
      out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
    }
    
  } else {
    
    out$refittedPredictedResponse <- matrix(nrow = out$nObs, ncol = n )  
    out$refittedFixedEffects <- matrix(nrow = length(out$fittedFixedEffects), ncol = n )  
    #out$refittedRandomEffects <- matrix(nrow = length(out$fittedRandomEffects), ncol = n )  
    
    newData <-model.frame(fittedModel)  
    
    for (i in 1:n){
      newData[,1] = out$simulatedResponse[,i]
      refittedModel = update(fittedModel, data = newData )
      out$refittedPredictedResponse[,i] = predict(refittedModel, type = "response")
      out$refittedFixedEffects[,i]  = fixef(refittedModel)
      #out$refittedRandomEffects[,i]  = ranef(refittedModel)
    }
  }
  out$time = proc.time() - ptm
  return(out)
}
