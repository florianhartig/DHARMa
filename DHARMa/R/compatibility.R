


#' Test DHARMa compatibility
#' 
#' The function tests the compatibility of a model with DHARMa
#' 
#' @importFrom lme4 fixef
#' @importFrom lme4 ranef
#' @importFrom spaMM response
#' @importFrom spaMM update_resp
#' 
#' @author Florian Hartig
#' @export
#' @return A report of possible problems
#' 
testModel <-function(fittedModel){
  
  try(family(fittedModel))
  try(class(fittedModel)[1])
  try(nobs(fittedModel))
  try(getResponse(fittedModel))
  try(simulate(fittedModel, nsim = 10))
  try(predict(fittedModel))
  try(coef(fittedModel))
  try(ranef(fittedModel))
  try(fixef(fittedModel))
  try(refit(fittedModel, newresp = DHARMa:::getResponse(fittedModel)))
  
}

# New S3 methods

#' Get model response
#' 
#' Extract the response of a fitted model 
#' 
#' The purpose of this function is to savely extract the response (dependent variable) of the fitted model classes
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
#' Simulate from a fitted model
#' 
#' The purpose of this function is to standardize the simulations from a model in a standardized way
#' 
#' @author Florian Hartig
#' @export
getSimulations <- function (object, ...) {
  UseMethod("getSimulations", object)
}

#' @export
getSimulations <- function (object, ...){
  simulate(object, ...)
}


#' @importFrom lme4 refit
NULL


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
getResponse.HLfit <- function(object, ...){
  return(response(object, ...))
}

#' @export
getSimulations.HLfit <- function(object, ...){
  return(as.data.frame(simulate(object, ...)))
}

#' @export
refit.HLfit <- function(object, newresp, ...) {
  update_resp(object, newresp, evaluate = TRUE)
}




