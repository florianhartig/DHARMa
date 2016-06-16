#' @export
simulateResiduals <- function(x, ...) UseMethod("simulateResiduals")



#' Creates scaled residuals by simulation
#' @param fittedModel fitted model, currently restricted to lme4 models
#' @param n number of simulations to run. Set at least 250, better 1000
#' @param refit should the model be refit to do a parametric bootstrap
#' @param integer is this a model with an integer distribution. If not provided, the function will attept to find out by itself, may not work for all families
#' @details The integer option essentially adds a uniform noise from -0.5 to 0.5 on the simulated and observed response. Note that this works because the expected distribution of this is flat - you can see this via hist(ecdf(runif(10000))(runif(10000))) 
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integer = NULL){

  ptm <- proc.time()  
  
  if(!(class(fittedModel) %in% getPossibleModels())) warning("DHARMa: fittedModel not in class of supported models. No guarantee that this works!")
  
  
  family = family(fittedModel)
  
  if(is.null(integer)){
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson")) integer = T
    else integer = F
  }
  
  out = list()
  
  out$fittedModel = fittedModel
  out$nObs = nobs(fittedModel)
  out$observedResponse = model.frame(fittedModel)[,1]
  out$fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0)
  
  if("glm" %in% class(fittedModel)){
    out$fittedFixedEffects = coef(fittedModel)
  }
  if("merMod" %in% class(fittedModel)){
    out$fittedFixedEffects = fixef(fittedModel) ## returns fixed effects 
    out$fittedRandomEffects = ranef(fittedModel) ## returns random effects    
  }

  out$fittedResiduals = residuals(fittedModel, type = "response")

  out$simulatedResponse = data.matrix(simulate(fittedModel, nsim = n, use.u =F))  
  out$scaledResiduals = rep(NA, out$nObs)

  if (refit == F){
 
    for (i in 1:out$nObs){
      
      if(integer == T){
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,] + runif(out$nObs, -0.5, 0.5))(out$observedResponse[i] + runif(1, -0.5, 0.5))           
      }else{
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
      }
    }
    
  } else {
    
    out$refittedPredictedResponse <- matrix(nrow = out$nObs, ncol = n )  
    out$refittedFixedEffects <- matrix(nrow = length(out$fittedFixedEffects), ncol = n )  
    #out$refittedRandomEffects <- matrix(nrow = length(out$fittedRandomEffects), ncol = n )  
    out$refittedResiduals = matrix(nrow = out$nObs, ncol = n)   
    
    newData <-model.frame(fittedModel)  
    
    for (i in 1:n){
      newData[,1] = out$simulatedResponse[,i]
      refittedModel = update(fittedModel, data = newData )
      out$refittedPredictedResponse[,i] = predict(refittedModel, type = "response")
      out$refittedFixedEffects[,i]  = fixef(refittedModel)
      out$refittedResiduals[,i] = residuals(refittedModel, type = "response")
      #out$refittedRandomEffects[,i]  = ranef(refittedModel)
    }
    
    for (i in 1:out$nObs){
    
      if(integer == T){
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,] + runif(out$nObs, -0.5, 0.5))(fittedResiduals[i] + runif(1, -0.5, 0.5))           
      }else{
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,])(out$fittedResiduals[i])
      }
    }

  }
  out$time = proc.time() - ptm
  return(out)
}

getPossibleModels<-function()c("lm", "glm", "lmerMod", "glmerMod") 

