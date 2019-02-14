#' Create simulated residuals
#' 
#' The function creates scaled residuals by simulating from the fitted model
#' @param fittedModel a fitted model  of a class supported by DHARMa
#' @param n number of simulations. Default is 100. A more save value would be 250 or even 1000. The smaller the number, the higher the stochastic error on the residuals. Also, for very small n, discretization artefacts can influence the tests. 
#' @param refit if F, new data will be simulated and scaled residuals will be created by comparing observed data with new data. If T, the model will be refit on the simulated data (parametric bootstrap), and scaled residuals will be created by comparing observed with refitted residuals.
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if T, \code{\link{plotSimulatedResiduals}} will be directly run after the simulations have terminated
#' @param ... parameters to pass to the simulate function of the model object. An important use of this is to specify whether simulations should be conditional on the current random effect estimates. See also details
#' @param seed the random seed. The default setting, recommended for any type of data analysis, is to reset the random number generator each time the function is run, meaning that you will always get the same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. F = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @return An S3 class of type "DHARMa", essentially a list with various elements. Implemented S3 functions include plot, print and \code{\link{residuals.DHARMa}}. Residuals returns the calculated scaled residuals, which can also be accessed via $scaledResiduals. The returned object additionally contains an element 'scaledResidualsNormal', which contains the scaled residuals transformed to a normal distribution (for stability reasons not recommended)
#' 
#' @details There are a number of important considerations when simulating from a more complex (hierarchical) model: 
#' 
#' \strong{Re-simulating random effects / hierarchical structure}: the first is that in a hierarchical model, several layers of stochasticity are aligned on top of each other. Specifically, in a GLMM, we have a lower level stochastic process (random effect), whose result enters into a higher level (e.g. Poisson distribution). For other hierarchical models such as state-space models, similar considerations apply. When simulating, we have to decide if we want to re-simulate all stochastic levels, or only a subset of those. For example, in a GLMM, it is common to only simulate the last stochastic level (e.g. Poisson) conditional on the fitted random effects. 
#' 
#' For controlling how many levels should be re-simulated, the simulateResidual function allows to pass on parameters to the simulate function of the fitted model object. Please refer to the help of the different simulate functions (e.g. ?simulate.merMod) for details. For merMod (lme4) model objects, the relevant parameters are parameters are use.u and re.form
#' 
#' If the model is correctly specified, the simulated residuals should be flat regardles how many hierarchical levels we re-simulate. The most thorough procedure would therefore be to test all possible options. If testing only one option, I would recommend to re-simulate all levels, because this esentially tests the model structure as a whole. This is the default setting in the DHARMa package. A potential drawback is that re-simulating the lower-level random effects creates more variability, which may reduce power for detecing problems in the upper-level stochatic processes. 
#' 
#' \strong{Integer responses}: a second complication is the treatment of inter responses. Imaging we have observed a 0, and we predict 30\% zeros - what is the quantile that we should display for the residual? To deal with this problem and maintain a uniform response, the option integerResponse adds a uniform noise from -0.5 to 0.5 on the simulated and observed response, which creates a uniform distribution - you can see this via hist(ecdf(runif(10000))(runif(10000))).
#' 
#'  DHARMa will try to automatically if the fitted model has an integer or discrete distribution via the family argument. However, in some cases the family does not allow to uniquely identify the distribution type. For example, a tweedie distribution can be inter or continous. Therefore, DHARMa will additionally check the simulation results for repeated values, and will change the distribution type if repeated values are found (a message is displayed in this case). 
#' 
#' \strong{Refitting or not}: a third issue is how residuals are calculated. simulateResiduals has two options that are controlled by the refit parameter:
#' 
#' 1. if refit = F (default), new data is simulated from the fitted model, and residuals are calculated by comparing the observed data to the new data
#' 
#' 2. if refit = T, a parametric bootstrap is performed, meaning that the model is refit on the new data, and residuals are created by comparing observed residuals against refitted residuals 
#' 
#' The second option is much slower, and only important for running tests that rely on comparing observed to simulated residuals, e.g. the  \code{\link{testOverdispersion}} function
#' 
#' \strong{Residuals per group}: In many situations, it can be useful to look at residuals per group, e.g. to see how much the model over / underpredicts per plot, year or subject. To do this, use \code{\link{recalculateResiduals}}, together with a grouping variable (see also help)
#' 
#' @note See \code{\link{testResiduals}} for an overview of residual tests, \code{\link{plot.DHARMa}} for an overview of available plots. 
#' 
#' @seealso \code{\link{testResiduals}}, \code{\link{plot.DHARMa}}, \code{\link{print.DHARMa}}, \code{\link{residuals.DHARMa}}, \code{\link{recalculateResiduals}}
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
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson", "Negative Binom", "nbinom2", "nbinom1", "genpois", "compois", "truncated_poisson", "truncated_nbinom2", "truncated_nbinom1", "betabinomial", "Poisson", "Tpoisson", "COMPoisson", "negbin", "Tnegbin") | grepl("Negative Binomial",family$family) ) integerResponse = T
    else integerResponse = F
  }
  
  out$fittedModel = fittedModel
  out$modelClass = class(fittedModel)[1]
  
  out$nObs = nobs(fittedModel, level = 1)
  out$nSim = n
  out$refit = refit
  out$observedResponse = getResponse(fittedModel) 
  
  # TODO - check if that works 
  nKcase = is.matrix(out$observedResponse)
  if(nKcase){
    if(! (family$family %in% c("binomial", "betabinomial"))) securityAssertion("nKcase - wrong family")
    if(! (ncol(out$observedResponse)==2)) securityAssertion("nKcase - wrong dimensions of response")
    out$observedResponse = out$observedResponse[,1]
  }

  out$integerResponse = integerResponse
  out$problems = list()
  out$scaledResiduals = rep(NA, out$nObs)
  out$fittedPredictedResponse = getPredictions(fittedModel)
  out$fittedFixedEffects = getFixedEffects(fittedModel)
  out$fittedResiduals = getResiduals(fittedModel)
  
  ######## simulations ##################
  
  simulations = getSimulations(fittedModel, nsim = n, ...)
  
  if(out$modelClass == "glmmTMB"){
    if(is.vector(simulations[[1]])){
      out$simulatedResponse = data.matrix(simulations)
    } else if (is.matrix(simulations[[1]])){ 
      # this is for the k/n binomial case
      out$simulatedResponse = as.matrix(simulations)[,seq(1, (2*n), by = 2)]
    } else securityAssertion("Simulation results produced unsupported data structure", stop = T)
    
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
    
    out$scaledResiduals = getQuantile(simulations = out$simulatedResponse , observed = out$observedResponse , n = out$nObs, nSim = out$nSim, integerResponse = integerResponse)

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
        
        out$refittedPredictedResponse[,i] = getPredictions(refittedModel)
        out$refittedFixedEffects[,i] = getFixedEffects(refittedModel)
        out$refittedResiduals[,i] = getResiduals(refittedModel)
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

    out$scaledResiduals = getQuantile(simulations = out$refittedResiduals, observed = out$fittedResiduals, n = out$nObs, nSim = out$nSim, integerResponse = integerResponse)
  }

  ########### Wrapup ############
  
  out$scaledResidualsNormal = qnorm(out$scaledResiduals + 0.00 )

  out$time = proc.time() - ptm
  out$randomState = randomState

  class(out) = "DHARMa"
  
  if(plot == T) plot(out)
  
  return(out)
}

getPossibleModels<-function()c("lm", "glm", "negbin", "lmerMod", "glmerMod", "gam", "bam", "glmmTMB", "HLfit", "MixMod") 

checkModel <- function(fittedModel){
  if(!(class(fittedModel)[1] %in% getPossibleModels())) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
  
  if (class(fittedModel)[1] == "glmmTMB") message("It seems you are diagnosing a glmmTBM model. There are still a few minor limitations associatd with this package. The most important is that glmmTMB doesn't implement an option to create unconditional predictions from the model, which means that predicted values (in res ~ pred) plots include the random effects. With strong random effects, this can sometimes create diagonal patterns from bottom left to top right in the res ~ pred plot. All other tests and plots should work as desired. Please see https://github.com/florianhartig/DHARMa/issues/16 for further details.")

  if (class(fittedModel)[1] == "gam" ) if (class(fittedModel$family)[1] == "extended.family") stop("It seems you are trying to fit a model from mgcv that was fit with an extended.family. Simulation functions for these families are not yet implemented in DHARMa. See issue https://github.com/florianhartig/DHARMa/issues/11 for updates about this")

}



securityAssertion <- function(context = "Not provided", stop = F){
  generalMessage = "Message from DHARMa package: a security assertion was not met. This means that during the execution of a DHARMa function, some unexpected conditions ocurred. Even if you didn't get an error, your results may not be reliable. Please check with the help if you use the functions as intended. If you think that the error is not on your side, I would be grateful if you could report the problem at https://github.com/florianhartig/DHARMa/issues \n\n Context:"
  if (stop == F) warning(paste(generalMessage, context))  
  else stop(paste(generalMessage, context))  
}


#' Recalculate residuals with grouping
#' 
#' The purpose of this function is to recalculate scaled residuals per group, based on the simulations done by \code{\link{simulateResiduals}}
#'
#' @param simulationOutput an object with simualted residuals created by \code{\link{simulateResiduals}}
#' @param group group of each data point
#' @param aggregateBy function for the aggregation. Default is sum. This should only be changed if you know what you are doing. Note in particular that the expected residual distribution might not be flat any more if you choose general functions, such as sd etc. 
#' 
#' @return an object of class DHARMa, similar to what is returned by \code{\link{simulateResiduals}}, but with additional outputs for the new grouped calculations. Note that the relevant outputs are 2x in the object, the first is the grouped calculations (which is returned by $name access), and later another time, under identical name, the original output. Moreover, there is a function 'aggregateByGroup', which can be used to aggregate predictor variables in the same way as the variables calculated here 
#' 
#' @example inst/examples/simulateResidualsHelp.R
#' @export
recalculateResiduals <- function(simulationOutput, group = NULL, aggregateBy = sum){

  if(!is.null(simulationOutput$original)) simulationOutput = simulationOutput$original

  out = list()
  out$original = simulationOutput
  
  if(is.null(group)) return(simulationOutput)
  else group =as.factor(group)
  out$nGroups = nlevels(group)

  aggregateByGroup <- function(x) aggregate(x, by=list(group), FUN=aggregateBy)[,2]
  
  out$observedResponse = aggregateByGroup(simulationOutput$observedResponse)
  out$fittedPredictedResponse = aggregateByGroup(simulationOutput$fittedPredictedResponse)
  out$simulatedResponse = apply(simulationOutput$simulatedResponse, 2, aggregateByGroup)
  
  if (simulationOutput$refit == F){
    
    out$scaledResiduals = getQuantile(simulations = out$simulatedResponse , observed = out$observedResponse , n = out$nGroups, nSim = simulationOutput$nSim, integerResponse = simulationOutput$integerResponse)
 
  ######## refit = T ##################   
  } else {

    out$refittedPredictedResponse <- apply(simulationOutput$refittedPredictedResponse, 2, aggregateByGroup)
    out$fittedResiduals = aggregateByGroup(simulationOutput$fittedResiduals)
    out$refittedResiduals = apply(simulationOutput$refittedResiduals, 2, aggregateByGroup)
    out$refittedPearsonResiduals = apply(simulationOutput$refittedPearsonResiduals, 2, aggregateByGroup)
  
    out$scaledResiduals = getQuantile(simulations = out$refittedResiduals , observed = out$fittedResiduals , n = out$nGroups, nSim = simulationOutput$nSim, integerResponse = simulationOutput$integerResponse)

  }
  
  # hack - the c here will result in both old and new outputs to be present resulting output, but a named access should refer to the new, grouped calculations
  # question to myself - what's the use of that, why not erase the old outputs? they are anyway saved in the old object
  
  out$aggregateByGroup = aggregateByGroup
  out = c(out, simulationOutput)
  class(out) = "DHARMa"
  return(out)
}
  
