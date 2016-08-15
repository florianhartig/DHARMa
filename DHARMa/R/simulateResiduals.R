#' Creates scaled residuals by simulation
#' @param fittedModel fitted model object, currently restricted to lme4, lm, or glm models
#' @param n integer number > 1, number of simulations to run. Set at least 250, better 1000
#' @param refit (experimental) if T, the model will be refit with a parametric bootstrap
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if T, \code{\link{plotSimulatedResiduals}} will be directly run after the simulations have terminated
#' @details The integerResponse option essentially adds a uniform noise from -0.5 to 0.5 on the simulated and observed response. Note that this works because the expected distribution of this is flat - you can see this via hist(ecdf(runif(10000))(runif(10000))) 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{plotSimulatedResiduals}}
#' @example inst/examples/simulateResidualsHelp.R
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integerResponse = NULL, plot = F){

  ptm <- proc.time()  
  
  if(!(class(fittedModel)[1] %in% DHARMa:::getPossibleModels())) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
  
  family = family(fittedModel)
  
  if(is.null(integerResponse)){
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson")) integerResponse = T
    else integerResponse = F
  }
  
  out = list()
  
  out$fittedModel = fittedModel
  out$nObs = nobs(fittedModel)
  out$observedResponse = model.frame(fittedModel)[,1]
  out$fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0)
  
  if("glm" %in% class(fittedModel)[1]){
    out$fittedFixedEffects = coef(fittedModel)
  }
  
  if(class(fittedModel)[1] %in% c("glmerMod", "lmerMod")){
    out$fittedFixedEffects = fixef(fittedModel) ## returns fixed effects 
    out$fittedRandomEffects = ranef(fittedModel) ## returns random effects
  }

  out$fittedResiduals = residuals(fittedModel, type = "response")
  
  simulations = simulate(fittedModel, nsim = n, use.u =F)
  
  if(is.vector(simulations[[1]]))  out$simulatedResponse = data.matrix(simulations)  
  else if (is.matrix(simulations[[1]])) out$simulatedResponse = as.matrix(simulations)[,seq(1, (2*n), by = 2)]
  else stop("wrong class")
  
  out$scaledResiduals = rep(NA, out$nObs)

  if (refit == F){
 
    for (i in 1:out$nObs){
      
      if(integerResponse == T){
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,] + runif(n, -0.5, 0.5))(out$observedResponse[i] + runif(1, -0.5, 0.5))           
      }else{
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
      }
    }
    
  } else {
    
    # Adding new outputs
    
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
    
      if(integerResponse == T){
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,] + runif(n, -0.5, 0.5))(out$fittedResiduals[i] + runif(1, -0.5, 0.5))           
      }else{
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,])(out$fittedResiduals[i])
      }
    }

  }
  
  if(plot == T) plotSimulatedResiduals(out)
  
  out$time = proc.time() - ptm
  return(out)
}

getPossibleModels<-function()c("lm", "glm", "lmerMod", "glmerMod") 

