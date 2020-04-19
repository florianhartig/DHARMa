
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
#' @example inst/examples/wrappersHelp.R
#' 
#' @seealso \code{\link{getRefit}}, \code{\link{getSimulations}}, \code{\link{getFixedEffects}}, \code{\link{getFitted}}
#' @author Florian Hartig
#' @export
getObservedResponse <- function (object, ...) {
  UseMethod("getObservedResponse", object)
}

#' @rdname getObservedResponse
#' @export
getObservedResponse.default <- function (object, ...){
  out = model.frame(object)[,1] 

  # check for weights in k/n case
  if(family(object)$family %in% c("binomial", "betabinomial") & "(weights)" %in% colnames(model.frame(object))){
    x = model.frame(object)
    out = out * x$`(weights)`
  }
    
  # check for k/n binomial
  if(is.matrix(out)){
    if(!(ncol(out) == 2)) securityAssertion("nKcase - wrong dimensions of response")
    if(!(family(object)$family %in% c("binomial", "betabinomial"))) securityAssertion("nKcase - wrong family")

    out = out[,1]
  } 
  
  # observation is factor - unlike lme4 and older, glmmTMB simulates nevertheless as numeric
  if(is.factor(out)) out = as.numeric(out) - 1

  return(out)
}

#' Get model simulations
#' 
#' Wrapper to simulate from a fitted model
#' 
#' The purpose of this wrapper for for the simulate function is to standardize the simulations from a model in a standardized way
#' 
#' @param object a fitted model
#' @param nsim number of simulations
#' @param type if simulations should be prepared for getQuantile or for refit
#' @param ... additional parameters to be passed on, usually to the simulate function of the respective model class
#' 
#' @return a matrix with simulations
#' @example inst/examples/wrappersHelp.R
#' 
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getRefit}}, \code{\link{getFixedEffects}}, \code{\link{getFitted}}
#' @author Florian Hartig
#' @export
getSimulations <- function (object, nsim = 1 , type = c("normal", "refit"), ...) {
  UseMethod("getSimulations", object)
}

#' @rdname getSimulations
#' @export
getSimulations.default <- function (object, nsim = 1, type = c("normal", "refit"), ...){
  
  type <- match.arg(type)
  
  out = simulate(object, nsim = nsim , ...)
  
  if (type == "normal"){
    if(family(object)$family %in% c("binomial", "betabinomial")){
      if("(weights)" %in% colnames(model.frame(object))){
        x = model.frame(object)
        out = out * x$`(weights)`
      } else if (is.matrix(out[[1]])){ 
        # this is for the k/n binomial case
        out = as.matrix(out)[,seq(1, (2*nsim), by = 2)]
      } else if(is.factor(out[[1]])){
        if(nlevels(out[[1]]) != 2){
          warning("The fitted model has a factorial response with number of levels not equal to 2 - there is currently no sensible application in DHARMa that would lead to this situation. Likely, you are trying something that doesn't work.")
        } 
        else{
          out = data.matrix(out) - 1
        }
      } 
    } 
    
    if(!is.matrix(out)) out = data.matrix(out)
  } else{
    if(family(object)$family %in% c("binomial", "betabinomial")){
      if (!is.matrix(out[[1]]) & !is.numeric(out)) data.frame(data.matrix(out)-1)
    }
  }
  
  return(out)
}


#' @importFrom lme4 refit
#' @importFrom lme4 fixef 
NULL


#' Extract fixed effects of a supported model
#' 
#' A wrapper to extract fixed effects of a supported model
#' 
#' @param fittedModel a fitted model
#' 
#' @example inst/examples/wrappersHelp.R
#' 
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getRefit}}, \code{\link{getFitted}}
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

#' Get model refit
#' 
#' Wrapper to refit a fitted model
#' 
#' @param object a fitted model
#' @param newresp the new response that should be used to refit the model
#' @param ... additional parameters to be passed on to the refit or update class that is used to refit the model
#' 
#' @details The purpose of this wrapper is to standardize the refit of a model. The behavior of this function depends on the supplied model. When available, it uses the refit method, otherwise it will use update. For glmmTMB: since version 1.0, glmmTMB has a refit function, but this didn't work, so I switched back to this implementation, which is a hack based on the update function.
#' 
#' @example inst/examples/wrappersHelp.R
#' 
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getFixedEffects}}
#' @author Florian Hartig
#' @export
getRefit <- function (object, newresp, ...) {
  UseMethod("getRefit", object)
}

#' @rdname getRefit
#' @export
getRefit.default <- function (object, newresp, ...){
  refit(object, newresp, ...)
}


#' Get model fitted
#' 
#' Wrapper to get the fitted value a fitted model
#' 
#' The purpose of this wrapper is to standardize extract the fitted values 
#' 
#' @param object a fitted model
#' @param ... additional parameters to be passed on, usually to the simulate function of the respective model class
#' 
#' @example inst/examples/wrappersHelp.R
#' 
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getRefit}}, \code{\link{getFixedEffects}}
#' 
#' @author Florian Hartig
#' @export
getFitted <- function (object, ...) {
  UseMethod("getFitted", object)
}

#' @rdname getFitted
#' @export
getFitted.default <- function (object,...){
  fitted(object, ...)
}

#' Check weights
#' 
#' Checks if a model was fit with the weight argument
#' 
#' The purpose of this function is to check if a model was fit with the weights
#' 
#' @param object a fitted model
#' @param ... additional parameters 
#' 
#' @author Florian Hartig
#' @export
hasWeigths <- function (object, ...) {
  UseMethod("hasWeigths", object)
}

#' @export
hasWeigths.default <- function (object, ...){
  if("(weights)" %in% colnames(model.frame(object))) return(TRUE)
  else (return(FALSE))
}


hasNA <- function(fittedModel){
  x = rownames(model.frame(fittedModel))
  if(length(x) < as.numeric(x[length(x) ])) return(TRUE)
  else return(FALSE)
}



######### LM #############

#' @rdname getRefit
#' @export
getRefit.lm <- function(object, newresp, ...){
  
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


hasWeigths.lm <- function(object, ...){
  if(length(unique(object$prior.weights)) != 1) return(TRUE)
  else return(FALSE)
}


######### GLM #############



######## MGCV ############

# This function overwrites the standard fitted function for GAM

#' @rdname getFitted
#' @export
getFitted.gam <- function(object, ...){
  class(object) = "glm"
  out = stats::fitted(object, ...)
  names(out) = as.character(1:length(out))
  out
}

# Check that this works
# plot(fitted(fittedModelGAM), predict(fittedModelGAM, type = "response"))



######## glmmTMB ######

#' @rdname getRefit
#' @export
getRefit.glmmTMB <- function(object, newresp, ...){
  newData <-model.frame(object)    
  
  # hack to make update work - for some reason, glmmTMB wants the matrix embedded in the df for update to work  ... should be solved ideally, see https://github.com/glmmTMB/glmmTMB/issues/549
  if(is.matrix(newresp)){
    tmp = colnames(newData[[1]])
    newData[[1]] = NULL
    newData = cbind(newresp, newData)
    colnames(newData)[1:2] = tmp    
  } else {
    newData[[1]] = newresp
  }
  refittedModel = update(object, data = newData)
  return(refittedModel)
}


# glmmTMB simulates normal counts (and not proportions in any case, so the check for the other models is not needed), see #158
# note that if observation is factor - unlike lme4 and older, glmmTMB simulates nevertheless as numeric

#' @rdname getSimulations
#' @export
getSimulations.glmmTMB <- function (object, nsim = 1, type = c("normal", "refit"), ...){
  
  type <- match.arg(type)
  
  out = simulate(object, nsim = nsim, ...)
  
  if (type == "normal"){
    if (is.matrix(out[[1]])){ 
      # this is for the k/n binomial case
      out = as.matrix(out)[,seq(1, (2*nsim), by = 2)]
    } 
    if(!is.matrix(out)) out = data.matrix(out)
  }else{
    
    # check for weights in k/n case
    if(family(object)$family %in% c("binomial", "betabinomial")){
      if("(weights)" %in% colnames(model.frame(object))){
        w = model.frame(object)
        w = w$`(weights)`
        tmp <- function(x)x/w
        out = apply(out, 2, tmp)
        out = as.data.frame(out)
      }
      else if(is.matrix(out[[1]]) & !is.matrix(model.frame(object)[[1]])){
        out = as.data.frame(as.matrix(out)[,seq(1, (2*nsim), by = 2)])
      }
    }
       
       
       

    
    
    # matrixResp = 
    # 
    # if(matrixResp & !is.null(ncol(newresp))){
    #   # Hack to make the factor binomial case work
    #   tmp = colnames(newData[[1]])
    #   newData[[1]] = NULL
    #   newData = cbind(newresp, newData)
    #   colnames(newData)[1:2] = tmp
    # } else if(!is.null(ncol(newresp))){
    #   newData[[1]] = newresp[,1]
    # } else {
    #   newData[[1]] = newresp 
    # }
    
    
    # if (out$modelClass == "glmmTMB" & ncol(simulations) == 2*n) simObserved = simulations[,(1+(2*(i-1))):(2+(2*(i-1)))]    
  }
  
  # else securityAssertion("Simulation results produced unsupported data structure", stop = TRUE)
  
  return(out)
}

#######  spaMM #########

#' @rdname getObservedResponse
#' @export
#' @importFrom spaMM response 
getObservedResponse.HLfit <- function(object, ...){
  out = response(object, ...)

  nKcase = is.matrix(out)
  if(nKcase){
    if(! (family(object) %in% c("binomial", "betabinomial"))) securityAssertion("nKcase - wrong family")
    if(! (ncol(out)==2)) securityAssertion("nKcase - wrong dimensions of response")
    out = out[,1]
  }
  
  if(!is.numeric(out)) out = as.numeric(out) 
  
  return(out)
  
}

#' @rdname getSimulations
#' @export
getSimulations.HLfit <- function(object, nsim = 1, type = c("normal", "refit"), ...){

  type <- match.arg(type)
  
  capture.output({out = simulate(object, nsim = nsim, ...)})
  
  if(type == "normal"){
    if(!is.matrix(out)) out = data.matrix(out)
  }else{
    out = as.data.frame(out)
  }
  return(out)
}

#' @rdname getRefit
#' @export
#' @importFrom spaMM update_resp 
getRefit.HLfit <- function(object, newresp, ...) {
  update_resp(object, newresp, evaluate = TRUE)
}

#' @export
hasWeigths.HLfit <- function(object, ...){
  if(length(unique(object$prior.weights)) != 1) return(TRUE)
  else return(FALSE)
}



