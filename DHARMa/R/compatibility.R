
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

#' @export
getResponse.default <- function (object, ...){
  model.frame(object)[,1] 
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
getSimulations <- function (object, ...) {
  UseMethod("getSimulations", object)
}

#' @export
getSimulations.default <- function (object, ...){
  simulate(object, ...)
}


#' @importFrom lme4 refit
NULL

#' getFixedEffects 
#' 
#' @importFrom lme4 fixef 
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


#######  spaMM #########

#' @export
#' @importFrom spaMM response 
getResponse.HLfit <- function(object, ...){
  return(response(object, ...))
}

#' @export
getSimulations.HLfit <- function(object, ...){
  return(as.data.frame(simulate(object, ...)))
}

#' @export
#' @importFrom spaMM update_resp 
refit.HLfit <- function(object, newresp, ...) {
  update_resp(object, newresp, evaluate = TRUE)
}




