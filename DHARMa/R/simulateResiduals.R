#' Create simulated residuals
#' 
#' The function creates scaled residuals by simulating from the fitted model
#' @param fittedModel fitted model object. Supported are generalized linear mixed models from 'lme4' (classes 'lmerMod', 'glmerMod'), generalized additive models ('gam' from 'mgcv', excluding extended families from 'mgcv'), 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. 
#' @param n integer number > 1, number of simulations to run. If possible, set to at least 250, better 1000. See also details
#' @param refit if F, new data will be simulated and scaled residuals will be created by comparing observed data with new data. If T, the model will be refit on the simulated data (parametric bootstrap), and scaled residuals will be created by comparing observed with refitted residuals.
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if T, \code{\link{plotSimulatedResiduals}} will be directly run after the simulations have terminated
#' @param ... parameters to pass to the simulate function of the model object. An important use of this is to specify whether simulations should be conditional on the current random effect estimates. See details.
#' @param seed the random seed. The default setting, recommended for any type of data analysis, is to reset the random number generator each time the function is run, meaning that you will always get the same result when running the same code. Setting seed = NA avoids the reset. This is only recommended for simulation experiments. See vignette for details.
#' @return A list with various objects. The most important are scaledResiduals, which contain the scaled residuals, and scaledResidualsNormal, which are the the scaled residuals transformed to a normal distribution. 
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
#' @seealso \code{\link{testResiduals}}, \code{\link{plot.DHARMa}}, \code{\link{print.DHARMa}}, \code{\link{recalculateResiduals}}
#' @example inst/examples/simulateResidualsHelp.R
#' @import stats
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integerResponse = NULL, plot = F, seed = 123, ...){

  ######## general assertions and startup calculations ##########
  
  if (n < 2) stop("error in DHARMa::simulateResiduals: n > 1 is required to calculate scaled residuals")
  checkModel(fittedModel)  
  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  ptm <- proc.time() 

  ####### extract model info ############
  
  out = list()
  
  family = family(fittedModel)
  if(is.null(integerResponse)){
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson", "Negative Binom", "nbinom2", "nbinom1", "genpois", "compois", "truncated_poisson", "truncated_nbinom2", "truncated_nbinom1", "betabinomial") | grepl("Negative Binomial",family$family) ) integerResponse = T
    else integerResponse = F
  }
  
  out$fittedModel = fittedModel
  out$modelClass = class(fittedModel)[1]
  
  out$nObs = nobs(fittedModel)
  out$nSim = n
  out$refit = refit
  out$observedResponse = model.frame(fittedModel)[,1] 
  
  # TODO - check if that works 
  nKcase = is.matrix(out$observedResponse)
  if(nKcase){
    if(family$family != "binomial") securityAssertion("nKcase")
    if(! (ncol(out$observedResponse)==2)) securityAssertion("nKcase")
    out$observedResponse = out$observedResponse[,1]
  }

  out$integerResponse = integerResponse
  out$problems = list()
  out$scaledResiduals = rep(NA, out$nObs)

  ## following block re-used below, create function for this 

  ##### calculating predictions #####
  
  # re-form should be set to ~0 to avoid spurious residual patterns, see https://github.com/florianhartig/DHARMa/issues/43
    
  if(out$modelClass %in% c("glmmTMB")){
    out$fittedPredictedResponse = predict(fittedModel, type = "response") 
  }else{
    out$fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0) 
  }
  
  out$fittedFixedEffects = getFixedEffects(fittedModel)
  out$fittedResiduals = residuals(fittedModel, type = "response")
  
  ######## simulations ##################
  
  simulations = simulate(fittedModel, nsim = n, ...)
  
  if(out$modelClass == "glmmTMB"){
    if(ncol(simulations) == 2*n){
      out$simulatedResponse = simulations[,seq(1, (2*n), by = 2)]
    }else{
      out$simulatedResponse = simulations
    }
    # observation is factor - unlike lme4 and older, glmmTMB simulates nevertheless as numeric
    if(is.factor(out$observedResponse)) out$observedResponse = as.numeric(out$observedResponse) - 1
  }else{
    if(is.vector(simulations[[1]])){
      out$simulatedResponse = data.matrix(simulations)
    } else if (is.matrix(simulations[[1]])){ 
      # this is for the k/n binomial case
      out$simulatedResponse = as.matrix(simulations)[,seq(1, (2*n), by = 2)]
    } else if(is.factor(simulations[[1]])){
      if(nlevels(simulations[[1]]) != 2) warning("The fitted model has a factorial response with number of levels not equal to 2 - there is currently no sensible application in DHARMa that would lead to this situation. Likely, you are trying something that doesn't work.")
      out$simulatedResponse = data.matrix(simulations) - 1
      out$observedResponse = as.numeric(out$observedResponse) - 1
    } else securityAssertion("Simulation results produced unsupported data structure", stop = T)
  }
    
  if(any(dim(out$simulatedResponse) != c(out$nObs, out$nSim) )) securityAssertion("Simulation results have wrong dimension", stop = T)
  
  ######## refit = F ################## 

  if (refit == F){
 
    for (i in 1:out$nObs){
      
      if(integerResponse == T){
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,] + runif(out$nSim, -0.5, 0.5))(out$observedResponse[i] + runif(1, -0.5, 0.5))
        #This option doesn't work!
        #out$scaledResiduals[i] <- sum((out$simulatedResponse[i,] + runif(out$nSim, -0.5, 0.5)) < (out$observedResponse[i] + runif(out$nSim, -0.5, 0.5))) / out$nSim  
      }else{
        out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
      }
    }
  
  ######## refit = T ################## 
    
  } else {

    # Adding new outputs

    out$refittedPredictedResponse <- matrix(nrow = out$nObs, ncol = n )  
    out$refittedFixedEffects <- matrix(nrow = length(out$fittedFixedEffects), ncol = n )  
    #out$refittedRandomEffects <- matrix(nrow = length(out$fittedRandomEffects), ncol = n )  
    out$refittedResiduals = matrix(nrow = out$nObs, ncol = n)   
    out$refittedPearsonResiduals = matrix(nrow = out$nObs, ncol = n)   
    
    for (i in 1:n){
      #tryCatch()
      
      if (out$modelClass == "glmmTMB" & ncol(simulations) == 2*n) simObserved = simulations[,(1+(2*(i-1))):(2+(2*(i-1)))]
      else simObserved = simulations[[i]]
      
      try({
        
        # for testing
        # if (i==3) stop("x")
        # Note: also set silet = T for production
    
        refittedModel = refit(fittedModel, simObserved)
        
        out$refittedPredictedResponse[,i] = predict(refittedModel, type = "response")
        out$refittedFixedEffects[,i] = getFixedEffects(refittedModel)
        out$refittedResiduals[,i] = residuals(refittedModel, type = "response")
        out$refittedPearsonResiduals[,i] = residuals(refittedModel, type = "pearson")
        #out$refittedRandomEffects[,i]  = ranef(refittedModel)
      }, silent = T)
    }
    
    ######### residual checks ###########

    if(anyNA(out$refittedResiduals)) warning("DHARMa::simulateResiduals warning: on refit = T, at least one of the refitted models produced an error. Inspect the refitted model values. Results may not be reliable.")
    
    ## check for convergece problems
    
    dup = sum(duplicated(out$refittedFixedEffects, MARGIN = 2))
    if (dup > 0){
      if (dup < n/3){
        warning(paste("There were", dup, "of", n ,"duplicate parameter estimates in the refitted models. This may hint towards a problem with optimizer convergence in the fitted models. Results may not be reliable. The suggested action is to not use the refitting procedure, and diagnose with tools available for the normal (not refitted) simulated residuals. If you absolutely require the refitting procedure, try changing tolerance / iterations in the optimizer settings."))
      } else {
        warning(paste("There were", dup, "of", n ,"duplicate parameter estimates in the refitted models. This may hint towards a problem with optimizer convergence in the fitted models. Results are likely not reliable. The suggested action is to not use the refitting procedure, and diagnose with tools available for the normal (not refitted) simulated residuals. If you absolutely require the refitting procedure, try changing tolerance / iterations in the optimizer settings."))
        out$problems[[length(out$problems)+ 1]] = "error in refit" 
      }
    } 
    
    ######### residual calculations ###########

    for (i in 1:out$nObs){
    
      if(integerResponse == T){
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,] + runif(out$nSim, -0.5, 0.5))(out$fittedResiduals[i] + runif(1, -0.5, 0.5)) 
        #This option doesn't work!
        #out$scaledResiduals[i] <- sum((out$refittedResiduals[i,] + runif(out$nSim, -0.5, 0.5)) < (out$fittedResiduals[i] + runif(out$nSim, -0.5, 0.5))) / out$nSim
      }else{
        out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,])(out$fittedResiduals[i])
      }
    }

  }

  ########### Wrapup ############
  
  out$scaledResidualsNormal = qnorm(out$scaledResiduals + 0.00 )

  if(plot == T) plot(out)
  
  out$time = proc.time() - ptm
  out$randomState = randomState

  class(out) = "DHARMa"
  return(out)
}

getPossibleModels<-function()c("lm", "glm", "negbin", "lmerMod", "glmerMod", "gam", "glmmTMB") 

checkModel <- function(fittedModel){
  if(!(class(fittedModel)[1] %in% getPossibleModels())) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
  
  if (class(fittedModel)[1] == "gam" ) if (class(fittedModel$family)[1] == "extended.family") stop("It seems you are trying to fit a model from mgcv that was fit with an extended.family. Simulation functions for these families are not yet implemented in DHARMa. See issue https://github.com/florianhartig/DHARMa/issues/11 for updates about this")

}


getFixedEffects <- function(fittedModel){
  
  if(class(fittedModel)[1] %in% c("glm", "lm", "gam", "negbin") ){
    out  = coef(fittedModel)
  } else if(class(fittedModel)[1] %in% c("glmerMod", "lmerMod")){
    out = lme4::fixef(fittedModel)
  } else if(class(fittedModel)[1] %in% c("glmmTMB")){
    out = glmmTMB::fixef(fittedModel)
    out = out$cond
  } else stop()
  return(out)
}


#' @importFrom lme4 refit
NULL


#' Refit a Model with a Different Response
#' 
#' @param object a fitted model
#' @param newresp a new response
#' @param ... further arguments, no effect implemented for this S3 class
#' @example inst/examples/helpRefit.R
#' @export
refit.lm <- function(object, newresp, ...){
  
  newData <-model.frame(object)  

  if(is.vector(newresp)){
    newData[,1] = newresp
  } else if (is.factor(newresp)){
    # Hack to make the factor binomial case work
    newData[,1] = as.numeric(newresp) - 1
  } else {
    # Hack to make the binomial n/k case work
    newData[[1]] = NULL
    newData = cbind(newresp, newData)
  }     
  
  refittedModel = update(object, data = newData)
  return(refittedModel)
}

#' Refit a Model with a Different Response
#' 
#' @param object a fitted model
#' @param newresp a new response
#' @param ... further arguments, no effect implemented for this S3 class
#' @example inst/examples/helpRefit.R
#' @export
refit.glmmTMB <- function(object, newresp, ...){
  
  newData <-model.frame(object)  
  
  matrixResp = is.matrix(newData[[1]])

  if(matrixResp & !is.null(ncol(newresp))){
    # Hack to make the factor binomial case work
    tmp = colnames(newData[[1]])
    newData[[1]] = NULL
    newData = cbind(newresp, newData)
    colnames(newData)[1:2] = tmp
  } else if(!is.null(ncol(newresp))){
    newData[[1]] = newresp[,1]
  } else {
    newData[[1]] = newresp 
  }

  refittedModel = update(object, data = newData)
  return(refittedModel)
}


securityAssertion <- function(context = "Not provided", stop = F){
  generalMessage = "Message from DHARMa package: a security assertion was not met. This means that during the execution of a DHARMa function, some unexpected conditions ocurred. Even if you didn't get an error, your results may not be reliable. Please check with the help if you use the functions as intended. If you think that the error is not on your side, I would be grateful if you could report the problem at https://github.com/florianhartig/DHARMa/issues \n\n Context:"
  if (stop == F) warning(paste(generalMessage, context))  
  else stop(paste(generalMessage, context))  
}


#' Recalculate residuals with grouping
#'
#' @param simulationOutput an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param group group of each data point
#' 
#' @return an object of class DHARMa, simular to what is returned by \code{\link{simulateResiduals}}, but with additional outputs for the new grouped calculations. Note that the relevant outputs are 2x in the object, the first is the grouped calculations (which is returned by $name access), and later another time, under identical name, the original output
#' 
#' @example inst/examples/simulateResidualsHelp.R
#' @export
recalculateResiduals <- function(simulationOutput, group = NULL){

  if(!is.null(simulationOutput$original)) simulationOutput = simulationOutput$original

  out = list()
  
  if(is.null(group)) return(simulationOutput)
  else group =as.factor(group)
  out$nGroups = nlevels(group)

  sumGroup <- function(x) aggregate(x, by=list(group), FUN=sum)[,2]
  
  out$observedResponse = sumGroup(simulationOutput$observedResponse)
  out$fittedPredictedResponse = sumGroup(simulationOutput$fittedPredictedResponse)
  out$simulatedResponse = apply(simulationOutput$simulatedResponse, 2, sumGroup)
  out$scaledResiduals = rep(NA, out$nGroups)

  if (simulationOutput$refit == F){
    if(simulationOutput$integerResponse == T){
      for (i in 1:out$nGroups) out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,] + runif(out$nGroups, -0.5, 0.5))(out$observedResponse[i] + runif(1, -0.5, 0.5))
    } else {
      for (i in 1:out$nGroups) out$scaledResiduals[i] <- ecdf(out$simulatedResponse[i,])(out$observedResponse[i])
    } 
  ######## refit = T ##################   
  } else {

    out$refittedPredictedResponse <- apply(simulationOutput$refittedPredictedResponse, 2, sumGroup)
    out$fittedResiduals = sumGroup(simulationOutput$fittedResiduals)
    out$refittedResiduals = apply(simulationOutput$refittedResiduals, 2, sumGroup)
    out$refittedPearsonResiduals = apply(simulationOutput$refittedPearsonResiduals, 2, sumGroup)
    
    if(simulationOutput$integerResponse == T){
      for (i in 1:out$nGroups) out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,] + runif(out$nGroups, -0.5, 0.5))(out$fittedResiduals[i] + runif(1, -0.5, 0.5))
    } else {
      for (i in 1:out$nGroups) out$scaledResiduals[i] <- ecdf(out$refittedResiduals[i,])(out$fittedResiduals[i])
    } 
  }
  # hack - the c here will result in both old and new outputs to be present resulting output, but a named access should refer to the new, grouped calculations
  out = c(out, simulationOutput)
  out$original = simulationOutput
  class(out) = "DHARMa"
  return(out)
}
  





