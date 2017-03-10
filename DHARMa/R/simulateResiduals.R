#' Create simulated residuals
#' 
#' The function creates scaled residuals by simulating from the fitted model
#' @param fittedModel fitted model object. Supported are generalized linear mixed models from 'lme4' (classes 'lmerMod', 'glmerMod'), generalized additive models ('gam' from 'mgcv', excluding extended families from 'mgcv'), 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. 
#' @param n integer number > 1, number of simulations to run. If possible, set to at least 250, better 1000. See also details
#' @param refit if F, new data will be simulated and scaled residuals will be created by comparing observed data with new data. If T, the model will be refit on the simulated data (parametric bootstrap), and scaled residuals will be created by comparing observed with refitted residuals.
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if T, \code{\link{plotSimulatedResiduals}} will be directly run after the simulations have terminated
#' @param ... parameters to pass to the simulate function of the model object. An important use of this is to specify whether simulations should be conditional on the current random effect estimates. See details.
#' @return A list with various objects. The most important are scaledResiduals, which contain the scaled residuals, and scaledResidualsNormal, which are the the scaled residuals transformed to a normal distribution
#' @details There are a number of important considerations when simulating from a more complex (hierarchical) model. 
#' 
#' \strong{Re-simulating random effects / hierarchical structure}: the first is that in a hierarchical model, several layers of stochasticity are aligned on top of each other. Specifically, in a GLMM, we have a lower level stochastic process (random effect), whose result enters into a higher level (e.g. Poisson distribution). For other hierarchical models such as state-space models, similar considerations apply. When simulating, we have to decide if we want to re-simulate all stochastic levels, or only a subset of those. For example, in a GLMM, it is common to only simulate the last stochastic level (e.g. Poisson) conditional on the fitted random effects. 
#' 
#' For controlling how many levels should be re-simulated, the simulateResidual function allows to pass on parameters to the simulate function of the fitted model object. Please refer to the help of the different simulate functions (e.g. ?simulate.merMod) for details. For merMod (lme4) model objects, the relevant parameters are parameters are use.u, and re.form
#' 
#' If the model is correctly specified, the simulated residuals should be flat regardles how many hierarchical levels we re-simulate. The most thorough procedure would therefore be to test all possible options. If testing only one option, I would recommend to re-simulate all levels, because this esentially tests the model structure as a whole. This is the default setting in the DHARMa package. A potential drawback is that re-simulating the lower-level random effects creates more variability, which may reduce power for detecing problems in the upper-level stochatic processes. 
#' 
#' \strong{Integer responses}: a second complication is the treatment of inter responses. Imaging we have observed a 0, and we predict 30\% zeros - what is the quantile that we should display for the residual? To deal with this problem and maintain a unifor response, the option integerResponse adds a uniform noise from -0.5 to 0.5 on the simulated and observed response. Note that this works because the expected distribution of this is flat - you can see this via hist(ecdf(runif(10000))(runif(10000))) 
#' 
#' \strong{Refitting or not}: a third issue is how residuals are calculated. simulateResiduals has two options that are controlled by the refit parameter:
#' 
#' 1. if refit = F (default), new data is simulated from the fitted model, and residuals are calculated by comparing the observed data to the new data
#' 
#' 2. if refit = T, a parametric bootstrap is performed, meaning that the model is refit on the new data, and residuals are created by comparing observed residuals against refitted residuals 
#' 
#' The second option is much slower, and only important for running tests that rely on comparing observed to simulated residuals, e.g. the  \code{\link{testOverdispersion}} function
#' 
#' #' \strong{How many simulations}: about the choice of n: my simulations didn't show major problems with a small n (if you get down to the order of a few 10, you will start seeing discretization artifacts from the empirical cummulative density estimates though). The default of 250 seems safe to me. If you want to be on the safe side, choose a high value (e.g. 1000) for producing your definite results.
#'  
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{plotSimulatedResiduals}}
#' @example inst/examples/simulateResidualsHelp.R
#' @import stats
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integerResponse = NULL, plot = F, ...){
  
  # assertions
  
  if (n < 2) stop("error in DHARMa::simulateResiduals: n > 1 is required to calculate scaled residuals")
  
  checkModel(fittedModel)
  
  ptm <- proc.time() 
  
  family = family(fittedModel)
  
  if(is.null(integerResponse)){
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson", "Negative Binom") | grepl("Negative Binomial",family$family) ) integerResponse = T
    else integerResponse = F
  }
  
  out = list()
  
  out$fittedModel = fittedModel
  out$nObs = nobs(fittedModel)
  out$nSim = n
  out$refit = refit
  out$observedResponse = model.frame(fittedModel)[,1]  
  out$integerResponse = integerResponse
  
  out$problems = list()
  
  ## following block re-used below, create function for this 
  
  out$fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0) # sensible to have re-form set to ~0?
  
  if(class(fittedModel)[1] %in% c("glm", "lm", "gam") ){
    out$fittedFixedEffects = coef(fittedModel)
  }
  
  if(class(fittedModel)[1] %in% c("glmerMod", "lmerMod")){
    out$fittedFixedEffects = lme4::fixef(fittedModel) ## returns fixed effects 
    out$fittedRandomEffects = lme4::ranef(fittedModel) ## returns random effects
  }

  out$fittedResiduals = residuals(fittedModel, type = "response")
  
  simulations = simulate(fittedModel, nsim = n, ...)
 
  if(is.vector(simulations[[1]])){
    out$simulatedResponse = data.matrix(simulations)
  } else if (is.matrix(simulations[[1]])){
    out$simulatedResponse = as.matrix(simulations)[,seq(1, (2*n), by = 2)]
  } else if(is.factor(simulations[[1]])){
    if(nlevels(simulations[[1]]) != 2) warning("The fitted model has a factorial response with number of levels not equal to 2 - there is currently no sensible application in DHARMa that would lead to this situation. Likely, you are trying something that doesn't work.")
    out$simulatedResponse = data.matrix(simulations) - 1
    out$observedResponse = as.numeric(out$observedResponse) - 1
  } else stop("DHARMa error - simulations resulted in unsupported class - if this happens for a supported model please report an error at https://github.com/florianhartig/DHARMa/issues")
  
  out$scaledResiduals = rep(NA, out$nObs)

  if (refit == F){
 
    for (i in 1:out$nObs){
      
      if(integerResponse == T){
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,] + runif(out$nSim, -0.5, 0.5))(out$observedResponse[i] + runif(1, -0.5, 0.5))           
      }else{
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
      }
    }
    
  } else {
    
    # make sure that we use here the same random effect options as for simulate
    out$fittedPredictedResponse = predict(fittedModel, type = "response", ... ) # sensible to have re-form set to ~0?
    
    # Adding new outputs
    
    out$refittedPredictedResponse <- matrix(nrow = out$nObs, ncol = n )  
    out$refittedFixedEffects <- matrix(nrow = length(out$fittedFixedEffects), ncol = n )  
    #out$refittedRandomEffects <- matrix(nrow = length(out$fittedRandomEffects), ncol = n )  
    out$refittedResiduals = matrix(nrow = out$nObs, ncol = n)   
    out$refittedPearsonResiduals = matrix(nrow = out$nObs, ncol = n)   
    
    newData <-model.frame(fittedModel)  
    
    for (i in 1:n){
      
      simObserved = simulations[[i]]
      
      if(is.vector(simObserved)){
        newData[,1] = simObserved
      } else if (is.factor(simObserved)){
        # Hack to make the factor binomial case work
        newData[,1] = as.numeric(simObserved) - 1
      } else {
        # Hack to make the binomial n/k case work
        newData[[1]] = NULL
        newData = cbind(simObserved, newData)
      }
      
      #tryCatch()
      try({
        
        # for testing
        # if (i==3) stop("x")
        # Note: also set silet = T for production
        
      refittedModel = update(fittedModel, data = newData)
      out$refittedPredictedResponse[,i] = predict(refittedModel, type = "response")
      
      if(class(fittedModel)[1] %in% c("glm", "lm", "gam") ){
        out$refittedFixedEffects[,i]  = coef(refittedModel)
      }
      
      if(class(fittedModel)[1] %in% c("glmerMod", "lmerMod")){
        out$refittedFixedEffects[,i]  = lme4::fixef(refittedModel)
        #out$fittedRandomEffects = ranef(fittedModel) ## returns random effects
      }
      
      out$refittedResiduals[,i] = residuals(refittedModel, type = "response")
      out$refittedPearsonResiduals[,i] = residuals(refittedModel, type = "pearson")
      #out$refittedRandomEffects[,i]  = ranef(refittedModel)
      }, silent = T)
    }

    if(anyNA(out$refittedResiduals)) warning("DHARMa::simulateResiduals warning: on refit = T, at least one of the refitted models produced an error. Inspect the refitted model values. Results may not be reliable.")
    
    ## check for convergece problems
    
    dup = sum(duplicated(out$refittedFixedEffects, MARGIN = 2))
    if (dup > 0){
      if (dup < n/3){
        warning(paste("There were", dup, "of", n ,"duplicate parameter estimates in the refitted models. This may hint towards a problem with optimizer convergence in the fitted models. Results may not be reliable. The suggested action is to not use the refitting procedure, and diagnose with tools available for the normal (not refitted) simulated residuals. If you absolutely require the refitting procedure, try changing tolerance / iterations in the optimizer settings."))
      } else warning(paste("There were", dup, "of", n ,"duplicate parameter estimates in the refitted models. This may hint towards a problem with optimizer convergence in the fitted models. Results are likely not reliable. The suggested action is to not use the refitting procedure, and diagnose with tools available for the normal (not refitted) simulated residuals. If you absolutely require the refitting procedure, try changing tolerance / iterations in the optimizer settings."))
      out$problems[[length(out$problems)+ 1]] = "error in refit"
    } 
    

    for (i in 1:out$nObs){
    
      if(integerResponse == T){
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,] + runif(out$nSim, -0.5, 0.5))(out$fittedResiduals[i] + runif(1, -0.5, 0.5))           
      }else{
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,])(out$fittedResiduals[i])
      }
    }

  }
  
  out$scaledResidualsNormal = qnorm(out$scaledResiduals + 0.00 )

  if(plot == T) plotSimulatedResiduals(out)
  
  out$time = proc.time() - ptm
  class(out) = "DHARMa"
  return(out)
}

getPossibleModels<-function()c("lm", "glm", "negbin", "lmerMod", "glmerMod", "gam") 


checkModel <- function(fittedModel){
  if(!(class(fittedModel)[1] %in% getPossibleModels())) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
  
  if (class(fittedModel)[1] == "gam" ) if (class(fittedModel$family)[1] == "extended.family") stop("It seems you are trying to fit a model from mgcv that was fit with an extended.family. Simulation functions for these families are not yet implemented in DHARMa. See issue https://github.com/florianhartig/DHARMa/issues/11 for updates about this")
}

















