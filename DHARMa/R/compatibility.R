# New S3 methods

#' Get model response
#' 
#' Extract the response of a fitted model 
#' 
#' The purpose of this function is to savely extract the response (dependent variable) of the fitted model classes
#' 
#' @param object a fitted model
#' @param ... additional parameters 
#' 
#' @author Florian Hartig
#' @export
getResponse <- function (object, ...) {
  UseMethod("getResponse", object)
}

#' @rdname getResponse
#' @export
getResponse.default <- function (object, ...){
  x = model.frame(object)[,1] 
  if(is.factor(x)){
    if(nlevels(x) != 2) warning("The fitted model has a factorial response with number of levels not equal to 2 - there is currently no sensible application in DHARMa that would lead to this situation. Likely, you are trying something that doesn't work.")
    else x = as.numeric(x) - 1
  } 
  if(is.matrix(x) || is.data.frame(x)) x = x[,1]
  return(x)
}


#' Get model residuals
#' 
#' Extract the residuals of a fitted model 
#' 
#' The purpose of this function is to savely extract the residuals of the fitted model classes
#' 
#' @param object a fitted model
#' @param ... additional parameters 
#' 
#' @author Florian Hartig
#' @export
getResiduals <- function (object, ...) {
  UseMethod("getResiduals", object)
}

#' @rdname getResiduals
#' 
#' @param object a fitted model
#' @param type the residual type
#' @param var a simulation-based estimate of var
#' 
#' @export
getResiduals.default <- function (object, type = "response", var = NULL, ...){
  residuals(object, type = type, ...)
}


#' Get model predictions
#' 
#' Extract the predictions of a fitted model 
#' 
#' The purpose of this function is to savely extract the predictions of the fitted model classes
#' 
#' @param object a fitted model
#' @param ... additional parameters 
#' 
#' @author Florian Hartig
#' @export
getPredictions <- function (object, ...) {
  UseMethod("getPredictions", object)
}

# re-form should be set to ~0 to avoid spurious residual patterns, see https://github.com/florianhartig/DHARMa/issues/43

#' @rdname getPredictions
#' @export
#' 
getPredictions.default <- function (object, type = "response", re.form = ~0, ...){
  predict(object, type = type, re.form = re.form, ...)
}


#' Get model simulations
#' 
#' Wrapper to simulate from a fitted model
#' 
#' The purpose of this wrapper for for the simulate function is to standardize the simulations from a model in a standardized way
#' 
#' @param object a fitted model
#' @param ... additional parameters to be passed on, usually to the simulate function of the respective model class
#' 
#' @author Florian Hartig
#' @export
getSimulations <- function (object, nsim = 1, ...) {
  UseMethod("getSimulations", object)
}

#' @rdname getSimulations
#' @export
getSimulations.default <- function (object, nsim = 1, ...){
  
  out = list()
  out$simOriginal = simulate(object, nsim = nsim, ...)
  out$simStandardized = data.matrix(out$simOriginal)

  if(ncol(out$simStandardized) == 2 * nsim){
    out$simStandardized = out$simStandardized[,seq(1, (2*nsim), by = 2)]   
    # if (class(object) == "glmmTMB")
  }
  out$simScale = apply(out$simStandardized, 1, sd)
  
  out$simRefit = as.data.frame(out$simOriginal)
  
  # if ( & ncol(simulations) == 2*n){
  #   simObserved = simulations[,(1+(2*(i-1))):(2+(2*(i-1)))]
  # } else {
  #   simObserved = simulations[,i]
  # } 
  return(out)
}


#' @importFrom lme4 refit
NULL

#' Get fixed effects
#' 
#' @export
getFixedEffects <- function(fittedModel){
  
  if(class(fittedModel)[1] %in% c("glm", "lm", "gam", "bam", "negbin") ){
    out  = coef(fittedModel)
  } else if(class(fittedModel)[1] %in% c("glmerMod", "lmerMod", "HLfit")){
    out = fixef(fittedModel)
  } else if(class(fittedModel)[1] %in% c("glmmTMB")){
    out = glmmTMB::fixef(fittedModel)
    out = out$cond
  } else {
    out = coef(fittedModel)
    if(is.null(out)) out = fixef(fittedModel)
  }
  return(out)
}


######### LM #############

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



######## MGCV ############

#' 
#' 
#' This function overwrites the standard fitted function for GAM
#' @note See explanation at 
#' @param object fitted model
#' @param ... arguments to be passed on to stats::fitted
#' @export
fitted.gam <- function(object, ...){
  class(object) = "glm"
  out = stats::fitted(object, ...)
  names(out) = as.character(1:length(out))
  out
}

# Check that this works
# plot(fitted(fittedModelGAM), predict(fittedModelGAM, type = "response"))



######## glmmTMB ######

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


#' @export
getPredictions.glmmTMB <- function (object, type = "response", ...){
  predict(object, type = type, ...)
}

#######  spaMM #########

#' @rdname getPredictions
#' @export
getPredictions.HLfit <- function (object, type = "response", re.form = ~0, ...){
  predict(object, type = type, re.form = re.form, ...)[,1L] 
}

#' @export
getResponse.HLfit <- function(object, ...){
  return(response(object, ...))
}

#' @export
refit.HLfit <- function(object, newresp, ...) {
  update_resp(object, newresp, evaluate = TRUE)
}


#######  GLMMadaptive #########


#' Refit a Model with a Different Response
#' 
#' @param object a fitted model
#' @param newresp a new response
#' @param ... further arguments, no effect implemented for this S3 class
#' @example inst/examples/helpRefit.R
#' @export
refit.MixMod <- function(object, newresp, ...){
  
  newData <-model.frame(object)  
  
  # It is stored in the component id and its name in the component id_name. Hence, you could do something like mframe_RE[[mod$id_name]] <- mod$id to include it in the model.frame of the random effects design matrix.
  
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

#' @rdname getPredictions
#' @export
getPredictions.MixMod <- function(object, type_pred = "response", type = "mean_subject", ...){
  return(predict(object, type_pred = type_pred, type = type, ...))
}


# note: I chose here subject specific residuals to conform to the other packages, but other choices would probably be possible.

# changed to mean_subject because of https://github.com/drizopoulos/GLMMadaptive/issues/12

# reimplemented because of https://github.com/drizopoulos/GLMMadaptive/issues/11

#' @rdname getResiduals
#' @export
getResiduals.MixMod <- function (object, type = "subject_specific", ...){
  residuals(fittedModel, type = type, ...)
}




