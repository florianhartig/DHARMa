#' Create simulated residuals
#'
#' The function creates scaled residuals by simulating from the fitted model. Residuals can be extracted with \code{\link{residuals.DHARMa}}. See \code{\link{testResiduals}} for an overview of residual tests, \code{\link{plot.DHARMa}} for an overview of available plots.
#'
#' @param fittedModel a fitted model  of a class supported by DHARMa
#' @param n number of simulations. The smaller the number, the higher the stochastic error on the residuals. Also, for very small n, discretization artefacts can influence the tests. Default is 250, which is a relatively safe value. You can consider increasing to 1000 to stabilize the simulated values. 
#' @param refit if FALSE, new data will be simulated and scaled residuals will be created by comparing observed data with new data. If TRUE, the model will be refit on the simulated data (parametric bootstrap), and scaled residuals will be created by comparing observed with refitted residuals.
#' @param integerResponse if TRUE, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if TRUE, \code{\link{plotResiduals}} will be directly run after the residuals have been calculated
#' @param ... parameters to pass to the simulate function of the model object. An important use of this is to specify whether simulations should be conditional on the current random effect estimates, e.g. via re.form. Note that not all models support syntax to specify conditional or unconditional simulations. See also details
#' @param seed the random seed to be used within DHARMa. The default setting, recommended for most users, is keep the random seed on a fixed value 123. This means that you will always get the same randomization and thus the same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. FALSE = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @param method for refit = F, the quantile randomization method used. The two options implemented at the moment are probability integral transform (PIT-) residuals (current default), and the "traditional" randomization procedure, that was used in DHARMa until version 0.3.0. Refit = T will always use "traditional", respectively of the value of method. For details, see \code{\link{getQuantile}}
#' @param rotation optional rotation of the residual space prior to calculating the quantile residuals. The main purpose of this is to remove residual autocorrelation. See details below, section *residual auto-correlation*, and help of [getQuantile]. 
#'
#' @details There are a number of important considerations when simulating from a more complex (hierarchical) model:
#'
#' \strong{Re-simulating random effects / hierarchical structure}: in a hierarchical model, we have several stochastic processes aligned on top of each other. Specifically, in a GLMM, we have a lower level stochastic process (random effect), whose result enters into a higher level (e.g. Poisson distribution). For other hierarchical models such as state-space models, similar considerations apply.
#'
#' In such a situation, we have to decide if we want to re-simulate all stochastic levels, or only a subset of those. For example, in a GLMM, it is common to only simulate the last stochastic level (e.g. Poisson) conditional on the fitted random effects. This is often referred to as a conditional simuation. For controlling how many levels should be re-simulated, the simulateResidual function allows to pass on parameters to the simulate function of the fitted model object. Please refer to the help of the different simulate functions (e.g. ?simulate.merMod) for details. For merMod (lme4) model objects, the relevant parameters are parameters are use.u and re.form
#'
#' If the model is correctly specified, the simulated residuals should be flat regardless how many hierarchical levels we re-simulate. The most thorough procedure would therefore be to test all possible options. If testing only one option, I would recommend to re-simulate all levels, because this essentially tests the model structure as a whole. This is the default setting in the DHARMa package. A potential drawback is that re-simulating the lower-level random effects creates more variability, which may reduce power for detecting problems in the upper-level stochastic processes. In particular dispersion tests may produce different results when switching from conditional to unconditional simulations, and often the conditional simulation is more sensitive.
#'
#' \strong{Refitting or not}: a third issue is how residuals are calculated. simulateResiduals has two options that are controlled by the refit parameter:
#'
#' 1. if refit = FALSE (default), new data is simulated from the fitted model, and residuals are calculated by comparing the observed data to the new data
#'
#' 2. if refit = TRUE, a parametric bootstrap is performed, meaning that the model is refit on the new data, and residuals are created by comparing observed residuals against refitted residuals. I advise against using this method per default (see more comments in the vignette), unless you are really sure that you need it.
#'
#' \strong{Residuals per group}: In many situations, it can be useful to look at residuals per group, e.g. to see how much the model over / underpredicts per plot, year or subject. To do this, use \code{\link{recalculateResiduals}}, together with a grouping variable (see also help)
#'
#' \strong{Transformation to other distributions}: DHARMa calculates residuals for which the theoretical expectation (assuming a correctly specified model) is uniform. To transform this residuals to another distribution (e.g. so that a correctly specified model will have normal residuals) see \code{\link{residuals.DHARMa}}.
#' 
#' \strong{Integer responses}: this is only relevant if method = "traditional", in which case it activates the randomization of the residuals. Usually, this does not need to be changed, as DHARMa will try to automatically if the fitted model has an integer or discrete distribution via the family argument. However, in some cases the family does not allow to uniquely identify the distribution type. For example, a tweedie distribution can be inter or continuous. Therefore, DHARMa will additionally check the simulation results for repeated values, and will change the distribution type if repeated values are found (a message is displayed in this case).
#' 
#' \strong{Residual auto-correlation}: a common problem is residual autocorrelation. Spatial, temporal and phylogenetic autocorrelation can be tested with [testSpatialAutocorrelation] and [testTemporalAutocorrelation]. If simulations are unconditional, residual correlations will be maintained, even if the autocorrelation is addressed by an appropriate CAR structure. This may be a problem, because autocorrelation may create apparently systematic patterns in plots or tests such as [testUniformity]. To reduce this problem, either simulate conditional on fitted correlated REs, or you could try to rotate residuals via the rotation parameter (the latter will likely only work in approximately linear models). See [getQuantile] for details on the rotation. 
#' 
#' @return An S3 class of type "DHARMa". Implemented S3 functions include \code{\link{plot.DHARMa}}, \code{\link{print.DHARMa}} and \code{\link{residuals.DHARMa}}. For other functions that can be used on a DHARMa object, see section "See Also" below.
#'
#' @seealso \code{\link{testResiduals}}, \code{\link{plotResiduals}}, \code{\link{recalculateResiduals}}, \code{\link{outliers}}
#'
#' @example inst/examples/simulateResidualsHelp.R
#' @import stats
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integerResponse = NULL, plot = F, seed = 123, method = c("PIT", "traditional"), rotation = NULL, ...){
  
  ######## general assertions and startup calculations ##########
  
  if (n < 2) stop("error in DHARMa::simulateResiduals: n > 1 is required to calculate scaled residuals")
  checkModel(fittedModel)
  match.arg(method)
  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  ptm <- proc.time()
  
  ####### extract model info ############
  
  out = list()
  
  family = family(fittedModel)
  out$fittedModel = fittedModel
  out$modelClass = class(fittedModel)[1]
  
  out$additionalParameters = list(...)
  
  out$nObs = nobs(fittedModel)
  out$nSim = n
  out$refit = refit
  out$observedResponse = getObservedResponse(fittedModel)
  
  if(is.null(integerResponse)){
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson", "Negative Binom", "nbinom2", "nbinom1", "genpois", "compois", "truncated_poisson", "truncated_nbinom2", "truncated_nbinom1", "betabinomial", "Poisson", "Tpoisson", "COMPoisson", "negbin", "Tnegbin") | grepl("Negative Binomial",family$family) ) integerResponse = TRUE
    else integerResponse = FALSE
  }
  out$integerResponse = integerResponse
  
  out$problems = list()
  
  out$fittedPredictedResponse = getFitted(fittedModel)
  out$fittedFixedEffects = getFixedEffects(fittedModel)
  out$fittedResiduals = getResiduals(fittedModel)
  
  ######## refit = F ##################
  
  if (refit == FALSE){
    
    out$method = method
    
    out$simulatedResponse = getSimulations(fittedModel, nsim = n, type = "normal", ...)
    
    checkSimulations(out$simulatedResponse, out$nObs, out$nSim)
    
    out$scaledResiduals = getQuantile(simulations = out$simulatedResponse , observed = out$observedResponse , integerResponse = integerResponse, method = method, rotation = rotation)
    
    ######## refit = T ##################
  } else {
    
    out$method = "traditional"
    
    # Adding new outputs
    
    out$refittedPredictedResponse <- matrix(nrow = out$nObs, ncol = n )
    out$refittedFixedEffects <- matrix(nrow = length(out$fittedFixedEffects), ncol = n )
    #out$refittedRandomEffects <- matrix(nrow = length(out$fittedRandomEffects), ncol = n )
    out$refittedResiduals = matrix(nrow = out$nObs, ncol = n)
    out$refittedPearsonResiduals = matrix(nrow = out$nObs, ncol = n)
    
    out$simulatedResponse = getSimulations(fittedModel, nsim = n, type = "refit", ...)
    
    for (i in 1:n){
      
      simObserved = out$simulatedResponse[[i]]
      
      try({
        
        # for testing
        # if (i==3) stop("x")
        # Note: also set silent = T for production
        
        refittedModel = getRefit(fittedModel, simObserved)
        
        out$refittedPredictedResponse[,i] = getFitted(refittedModel)
        out$refittedFixedEffects[,i] = getFixedEffects(refittedModel)
        out$refittedResiduals[,i] = getResiduals(refittedModel)
        out$refittedPearsonResiduals[,i] = residuals(refittedModel, type = "pearson")
        #out$refittedRandomEffects[,i]  = ranef(refittedModel)
      }, silent = TRUE)
    }
    
    ######### residual checks ###########
    
    if(anyNA(out$refittedResiduals)) warning("DHARMa::simulateResiduals warning: on refit = TRUE, at least one of the refitted models produced an error. Inspect the refitted model values. Results may not be reliable.")
    
    ## check for convergence problems
    
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
    
    out$scaledResiduals = getQuantile(simulations = out$refittedResiduals, observed = out$fittedResiduals, integerResponse = integerResponse, method = "traditional", rotation = rotation)
  }
  
  ########### Wrapup ############
  
  out$time = proc.time() - ptm
  out$randomState = randomState
  
  class(out) = "DHARMa"
  
  if(plot == TRUE) plot(out)
  
  return(out)
}


#' Check simulated data
#'
#' The function checks if the simulated data seems fine
#'
#' @param simulatedResponse the simulated response
#' @param nObs number of observations
#' @param nSim number of simulations
#'
#' @keywords internal
checkSimulations <- function(simulatedResponse, nObs, nSim){
  
  if(!inherits(simulatedResponse, "matrix")) securityAssertion("Simulation from the model produced wrong class", stop = T)
  
  if(any(dim(simulatedResponse) != c(nObs, nSim) )) securityAssertion("Simulation from the model produced wrong dimension", stop = T)
  
  if(any(!is.finite(simulatedResponse))) message("Simulations from your fitted model produce infinite values. Consider if this is sensible")
  
  if(any(is.nan(simulatedResponse))) securityAssertion("Simulations from your fitted model produce NaN values. DHARMa cannot calculated residuals for this. This is nearly certainly an error of the regression package you are using", stop = T)
  if(any(is.na(simulatedResponse))) securityAssertion("Simulations from your fitted model produce NA values. DHARMa cannot calculated residuals for this. This is nearly certainly an error of the regression package you are using", stop = T)
  
}




#' Recalculate residuals with grouping
#'
#' The purpose of this function is to recalculate scaled residuals per group, based on the simulations done by \code{\link{simulateResiduals}}
#'
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param group group of each data point
#' @param aggregateBy function for the aggregation. Default is sum. This should only be changed if you know what you are doing. Note in particular that the expected residual distribution might not be flat any more if you choose general functions, such as sd etc.
#' @param sel an optional vector for selecting the data to be aggregated
#' @param seed the random seed to be used within DHARMa. The default setting, recommended for most users, is keep the random seed on a fixed value 123. This means that you will always get the same randomization and thus teh same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. FALSE = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @param method the quantile randomization method used. The two options implemented at the moment are probability integral transform (PIT-) residuals (current default), and the "traditional" randomization procedure, that was used in DHARMa until version 0.3.0. For details, see \code{\link{getQuantile}}
#' @param rotation optional rotation of the residual space to remove residual autocorrelation. See details in [simulateResiduals], section *residual auto-correlation* for an extended explanation, and [getQuantile] for syntax. 
#' @return an object of class DHARMa, similar to what is returned by \code{\link{simulateResiduals}}, but with additional outputs for the new grouped calculations. Note that the relevant outputs are 2x in the object, the first is the grouped calculations (which is returned by $name access), and later another time, under identical name, the original output. Moreover, there is a function 'aggregateByGroup', which can be used to aggregate predictor variables in the same way as the variables calculated here
#'
#' @example inst/examples/simulateResidualsHelp.R
#' @export
recalculateResiduals <- function(simulationOutput, group = NULL, aggregateBy = sum, sel = NULL, seed = 123, method = c("PIT", "traditional"), rotation = NULL){
  
  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  match.arg(method)
  
  # ensures that the base simulation is always used for recalculate
  if(!is.null(simulationOutput$original)) simulationOutput = simulationOutput$original
  
  out = list()
  out$original = simulationOutput
  out$group = group
  out$method = method
  out$aggregateBy = aggregateBy
  
  if(is.null(group) & is.null(sel)) return(simulationOutput)
  else {
    if(is.null(group)) group = 1:simulationOutput$nObs
    group =as.factor(group)
    out$nGroups = nlevels(group)
    if(is.null(sel)) sel = 1:simulationOutput$nObs
    out$sel = sel
    
    aggregateByGroup <- function(x) aggregate(x[sel], by=list(group[sel]), FUN=aggregateBy)[,2]
    
    out$observedResponse = aggregateByGroup(simulationOutput$observedResponse)
    out$fittedPredictedResponse = aggregateByGroup(simulationOutput$fittedPredictedResponse)
    
    if (simulationOutput$refit == F){
      
      out$simulatedResponse = apply(simulationOutput$simulatedResponse, 2, aggregateByGroup)
      out$scaledResiduals = getQuantile(simulations = out$simulatedResponse , observed = out$observedResponse, integerResponse = simulationOutput$integerResponse, method = method, rotation = rotation)
      
      ######## refit = T ##################
    } else {
      
      out$refittedPredictedResponse <- apply(simulationOutput$refittedPredictedResponse, 2, aggregateByGroup)
      out$fittedResiduals = aggregateByGroup(simulationOutput$fittedResiduals)
      out$refittedResiduals = apply(simulationOutput$refittedResiduals, 2, aggregateByGroup)
      out$refittedPearsonResiduals = apply(simulationOutput$refittedPearsonResiduals, 2, aggregateByGroup)
      
      out$scaledResiduals = getQuantile(simulations = out$refittedResiduals , observed = out$fittedResiduals , integerResponse = simulationOutput$integerResponse, method = method, rotation = rotation)
    }
    # hack - the c here will result in both old and new outputs to be present resulting output, but a named access should refer to the new, grouped calculations
    # question to myself - what's the use of that, why not erase the old outputs? they are anyway saved in the old object
    
    out$aggregateByGroup = aggregateByGroup
    out = c(out, simulationOutput)
    out$randomState = randomState
    class(out) = "DHARMa"
    return(out)
  }
}
