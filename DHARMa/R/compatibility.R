
#' @export
getResponse <- function (object, ...) {
  UseMethod("getResponse", object)
}

#' @export
getResponse.default <- function (object, ...){
  model.frame(object)[,1] 
}


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



#######  spaMM #########

#' @export
family.HLfit <- function(object, ...){
  return(object$family)
}

#' @export
getResponse.HLfit <- function(object, ...){
  object$y[,1L]
}

###### REFIT PROCEDURES #######


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



