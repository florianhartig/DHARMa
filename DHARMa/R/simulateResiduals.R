#' Creates scaled residuals by simulation
#' @param fittedModel fitted model object, currently restricted to lme4, lm, or glm models
#' @param n integer number > 1, number of simulations to run. If possible, set to at least 250, better 1000. See also details
#' @param refit (experimental) if T, the model will be refit with a parametric bootstrap
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if T, \code{\link{plotSimulatedResiduals}} will be directly run after the simulations have terminated
#' @param ... parameters to pass to the simulate function of the model object. An important use of this is to specify whether simulations should be conditional on the current random effect estimates. See details.
#' @return A list with various objects. The most important are scaledResiduals, which contain the scaled residuals, and scaledResidualsNormal, which are the the scaled residuals transformed to a normal distribution
#' @details There are a number of important considerations when simulating from a more complex (hierarchical) model. The first is that in a hierarchical model, several layers of stochasticity are aligned on top of each other. Specifically, in a GLMM, we have a lower level stochastic process (random effect), whose result enters into a higher level (e.g. Poisson distribution). For other hierarchical models such as state-space models, similar considerations apply. When simulating, we have to decide if we want to re-simulate all stochastic levels, or only a subset of those. For example, in a GLMM, it is common to only simulate the last stochastic level (e.g. Poisson) conditional on the fitted random effects. 
#' 
#' For controlling how many levels should be re-simulated, the simulateResidual function allows to pass on parameters to the simulate function of the fitted model object. Please refer to the help of the different simulate functions (e.g. ?simulate.merMod) for details. For merMod (lme4) model objects, the relevant parameters are parameters are use.u, and re.form
#' 
#' The simulated residuals should be flat against all options. The most thorough testing procedure would therefore be to test all possible options. If testing only one option, I would recommend to re-simulate all levels, because this esentially tests the whole model structure together. This is the default setting in the package. A potential drawback is that re-simulating the lower-level random effects creates more variability, which may reduce power for detecing problems in the upper-level stochatic processes. 
#' 
#' A further complication is the treatment of inter responses. Imaging we have observed a 0, and we predict 30% zeros - what is the quantile that we should display for the residual? To deal with this problem and maintain a unifor response, the option integerResponse adds a uniform noise from -0.5 to 0.5 on the simulated and observed response. Note that this works because the expected distribution of this is flat - you can see this via hist(ecdf(runif(10000))(runif(10000))) 
#' 
#' About the choice of n: my simulations didn't show major problems with a small n (even down to the order of 10), but just to be on the safe side, I would recommend to use a high value (e.g. 1000)
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{plotSimulatedResiduals}}
#' @example inst/examples/simulateResidualsHelp.R
#' @import stats
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integerResponse = NULL, plot = F, ...){

  ptm <- proc.time()  
  
  if(!(class(fittedModel)[1] %in% getPossibleModels())) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
  
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
    out$refittedPearsonResiduals = matrix(nrow = out$nObs, ncol = n)   
    
    newData <-model.frame(fittedModel)  
    
    for (i in 1:n){
      newData[,1] = out$simulatedResponse[,i]
      refittedModel = update(fittedModel, data = newData )
      out$refittedPredictedResponse[,i] = predict(refittedModel, type = "response")
      out$refittedFixedEffects[,i]  = fixef(refittedModel)
      out$refittedResiduals[,i] = residuals(refittedModel, type = "response")
      out$refittedPearsonResiduals[,i] = residuals(refittedModel, type = "pearson")
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
  
  out$scaledResidualsNormal = qnorm(out$scaledResiduals + 0.00 )

  if(plot == T) plotSimulatedResiduals(out)
  
  out$time = proc.time() - ptm
  return(out)
}

getPossibleModels<-function()c("lm", "glm", "lmerMod", "glmerMod") 

