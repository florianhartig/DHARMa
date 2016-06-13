#' @export
simulateResiduals <- function(x, ...) UseMethod("simulateResiduals")



#' Creates scaled residuals by simulation
#' @param fittedModel fitted model, currently restricted to lme4 models
#' @param n number of simulations to run. Set at least 250, better 1000
#' @param refit should the model be refit to do a parametric bootstrap
#' @param integer is this a model with an integer distribution. If not provided, the function will attept to find out by itself, may not work for all families
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integer = NULL){

  ptm <- proc.time()  
  
  if(is.null(integer)){
    family = fittedModel@resp$family$family
    if (family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson")) integer = T
    else integer = F
  }
  
  nObs = nobs(fittedModel)
  out = list(
    fittedModel = fittedModel,
    nObs = nObs,
    observedResponse = model.frame(fittedModel)[,1],
    fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0),
    fittedFixedEffects = fixef(fittedModel), ## returns fixed effects 
    fittedRandomEffects = ranef(fittedModel), ## returns random effects
    
    simulatedResponse = data.matrix(simulate(fittedModel, nsim = n, use.u =F)),   
    scaledResiduals = rep(NA, nObs),
    
    # only for refit = T
    refittedPredictedResponse = NULL,
    refittedFixedEffects = NULL,
    refittedRandomEffects = NULL
  )
  
  if(integer == T){
    out$simulatedResponseNoised = out$simulatedResponse + matrix(runif(nObs*n, -0.5, 0.5), ncol = n)
    out$observedResponseNoised = out$observedResponse + runif(nObs, -0.5, 0.5)
  }

  if (refit == F){
 
    for (i in 1:out$nObs){
      
      if(integer == T){
        out$scaledResiduals[i] <- ecdf(out$simulatedResponseNoised[i,])(out$observedResponseNoised[i])           
      }else{
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
      }
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
