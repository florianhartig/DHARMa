
# General notes -----------------------------------------------------------


# This file contains the wrappers for the models supported by DHARMa. DHARMa is interaction with packages ONLY via the wrappers. Below are
#
# 1) supported models in getPossibleModels
# 2) generic S3 wrapper functions (function names see "New Class Template"), including default functions. 
# 3) package / class specific wrappers.

# The general approach for integrating a package in DHARMa is
# 
# i) copy new class template
# ii) test if S3 default functions work
# iii) if not, define class-specific S3 functions

# See comments in the help of the generic S3 functions for guidance about how to implement each function


# Checks -----------------------------------------------------------

#' Check if the fitted model is supported by DHARMa
#'
#' The function checks if the fitted model is supported by DHARMa, and if there are other issues that could create problems
#'
#' @param fittedModel a fitted model
#' @param stop whether to throw an error if the model is not supported by DHARMa
#'
#' @details The main purpose of this function os to check if the fitted model class is supported by DHARMa. The function additionally checks for properties of the fitted model that could create problems for calculating residuals or working with the resuls in DHARMa.
#'
#'
#' @keywords internal
checkModel <- function(fittedModel, stop = F){
  
  out = T
  
  if(!(class(fittedModel)[1] %in% getPossibleModels())){
    if(stop == FALSE) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
    else stop("DHARMa: fittedModel not in class of supported models")
  }
  
  # if(hasNA(fittedModel)) message("It seems there were NA values in the data used for fitting the model. This can create problems if you supply additional data to DHARMa functions. See ?checkModel for details")
  
  # TODO: check as implemented does not work reliably, check if there is any other option to check for NA
  # #' @example inst/examples/checkModelHelp.R
  
  #  NA values in the data: checkModel will detect if there were NA values in the data frame. For NA values, most regression models will remove the entire observation from the data. This is not a problem for DHARMa - residuals are then only calculated for non-NA rows in the data. However, if you provide additional predictors to DHARMa, for example to plot residuals against a predictor, you will have to remove all NA rows that were also removed in the model. For most models, you can get the rows of the data that were actually used in the fit via rownames(model.frame(fittedModel))
  
  
  #if (class(fittedModel)[1] == "gam" ) if (class(fittedModel$family)[1] == "extended.family") stop("It seems you are trying to fit a model from mgcv that was fit with an extended.family. Simulation functions for these families are not yet implemented in DHARMa. See issue https://github.com/florianhartig/DHARMa/issues/11 for updates about this")
  
}

#' get possible models
#' 
#' returns a list of supported model classes 
#' 
#' @keywords internal
getPossibleModels<-function()c("lm", "glm", "negbin", "lmerMod", "lmerModLmerTest", "glmerMod", "gam", "bam", "glmmTMB", "HLfit", "MixMod")


weightsWarning = "Model was fit with prior weights. These will be ignored in the simulation. See ?getSimulations for details"


######### Generic S3 Wrappers #############

#' Get model response
#'
#' Extract the response of a fitted model
#'
#' The purpose of this function is to safely extract the observed response (dependent variable) of the fitted model classes
#' 
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


#' Get model simulations
#'
#' Wrapper to simulate from a fitted model
#'
#' The purpose of this wrapper for for the simulate function is to return the simulations from a model in a standardized way
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
#'
#' @details The purpose of this function is to wrap or implement the simulate function of different model classes and thus return simulations from fitted models in a standardized way.
#'
#' Note: GLMM and other regression packages often differ in how simulations are produced, and which parameters can be used to modify this behavior.
#' 
#' One important difference is how to modifiy which hierarchical levels are held constant, and which are re-simulated. In lme4, this is controlled by the re.form argument (see [lme4::simulate.merMod]). For other packages, please consort the help. 
#' 
#' If the model was fit with weights and the respective model class does not include the weights in the simulations, getSimulations will throw a warning. The background is if weights are used on the likelihood directly, then what is fitted is effectively a pseudo likelihood, and there is no way to directly simulate from the specified likelihood. Whether or not residuals can be used in this case depends very much on what is tested and how weights are used. I'm sorry to say that it is hard to give a general recommendation, you have to consult someone that understands how weights are processed in the respective model class. 
#'
#' @author Florian Hartig
#' @export
getSimulations <- function (object, nsim = 1 , type = c("normal", "refit"), ...) {
  UseMethod("getSimulations", object)
}

#' Extract fixed effects of a supported model
#'
#' A wrapper to extract fixed effects of a supported model
#'
#' @param object a fitted model
#' @param ... additional parameters
#' @example inst/examples/wrappersHelp.R
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getRefit}}, \code{\link{getFitted}}
#' @export
getFixedEffects <- function(object, ...){
  UseMethod("getFixedEffects", object)
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

#' Get fitted / predicted values
#'
#' Wrapper to get the fitted / predicted response of model at the response scale
#'
#' The purpose of this wrapper is to standardize extract the fitted values, which is implemented via predict(model, type = "response") for most model classes.
#' 
#' If you implement this function for a new model class, you should include an option to modifying which REs are included in the predictions. If this option is not available, it is essential that predictions are provided marginally / unconditionally, i.e. without the random effect estimates (because of https://github.com/florianhartig/DHARMa/issues/43), which corresponds to re-form = ~0 in lme4
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

# NOTE - a bit unclear if fitted or predict should be used


#' Get model residuals
#'
#' Wrapper to get the residuals of a fitted model
#'
#' The purpose of this wrapper is to standardize extract the model residuals. Similar to some other functions, a key question is whether to calculate those conditional or unconditional on the fitted REs. 
#'
#' @param object a fitted model
#' @param ... additional parameters to be passed on, usually to the residual function of the respective model class
#'
#' @example inst/examples/wrappersHelp.R
#'
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getRefit}}, \code{\link{getFixedEffects}}, \code{\link{getFitted}}
#'
#' @author Florian Hartig
#' @export
getResiduals <- function (object, ...) {
  UseMethod("getResiduals", object)
}

# NOTE - a bit unclear if fitted or predict should be used




# default -----------------------------------------------------------------

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

#' @rdname getRefit
#' @importFrom lme4 refit
#' @export
getRefit.default <- function (object, newresp, ...){
  refit(object, newresp, ...)
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


#' @rdname getFixedEffects
#' @importFrom lme4 fixef
#' @export
getFixedEffects.default <- function(object, ...){
  
  if(class(object)[1] %in% c("glm", "lm", "gam", "bam", "negbin") ){
    out  = coef(object)
  } else if(class(object)[1] %in% c("glmerMod", "lmerMod", "HLfit")){
    out = fixef(object)
  } else if(class(object)[1] %in% c("glmmTMB")){
    out = glmmTMB::fixef(object)
    out = out$cond
  } else {
    out = coef(object)
    if(is.null(out)) out = fixef(object)
  }
  return(out)
}

#' @rdname getFitted
#' @export
getFitted.default <- function (object,...){
  out = predict(object, type = "response", re.form = ~0)
  out = as.vector(out) # introduced because of phyr error
}

#' @rdname getFitted
#' @export
getResiduals.default <- function (object,...){
  residuals(object, type = "response")
}

#' has NA
#'
#' checks if the fitted model excluded NA values
#'
#' @param object a fitted model
#'
#' @details Checks if the fitted model excluded NA values
#'
#' @export

# hasNA <- function(object){
#   x = rownames(model.frame(object))
#   if(length(x) < as.numeric(x[length(x) ])) return(TRUE)
#   else return(FALSE)
# }



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

#' @rdname getSimulations
#' @export
getSimulations.negbin<- function (object, nsim = 1, type = c("normal", "refit"), ...){
  if("(weights)" %in% colnames(model.frame(object))) warning(weightsWarning)
  getSimulations.default(object = object, nsim = nsim, type = type, ...)
}


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


# Get Simulations of gam object

#' @rdname getSimulations
#' @param mgcViz whether simulations should be created with mgcViz (if mgcViz is available)
#' @export
getSimulations.gam <- function(object, nsim = 1, type = c("normal", "refit"), mgcViz = TRUE, ...){
  
  type <- match.arg(type)
  
  if("mgcViz" %in% installed.packages() & mgcViz == T){
    
    if("(weights)" %in% colnames(model.frame(object)) & ! family(object)$family %in% c("binomial", "betabinomial")) warning(weightsWarning)
    
    # use mgcViz if available
    out = mgcViz::simulate.gam(object, nsim = nsim , ...)
    out = as.data.frame(out)
    
    # from here to end identical to default
    
    if (type == "normal"){
      if(family(object)$family %in% c("binomial", "betabinomial")){
        if("(weights)" %in% colnames(model.frame(object))){
          x = model.frame(object)
          out = out * x$`(weights)`
        } else if (is.matrix(model.frame(object)[[1]])){
          # this is for the k/n binomial case
          out = out * rowSums(model.frame(object)[[1]])
        }
      }
      if(!is.matrix(out)) out = data.matrix(out)
    } else{
      if(family(object)$family %in% c("binomial", "betabinomial")){
        if (is.matrix(model.frame(object)[[1]])){

          # need to convert to k,n-k format for refit. Syntax here is from
          # https://stackoverflow.com/questions/6143697/data-frame-with-a-column-containing-a-matrix-in-r

          trials = rowSums(model.frame(object)[[1]])
          oldnames = names(out)
          oldrownames = row.names(out)
          out = out * trials
          out = as.list(out)
          for(i in 1:length(out)){
            out[[i]] = cbind(out[[i]], trials - out[[i]])
            colnames(out[[i]]) = colnames(model.frame(object)[[1]])
          }
          class(out) = "data.frame"
          names(out) = oldnames
          row.names(out) = oldrownames
        }
      }
    }
  } else {
    message("It seems you don't have mgcViz installed on this computer. When using DHARMa with mgcv objects, it is highly recommended to also install mgcViz, which will extend the ability of DHARMa to simulate from various gam objects. Withoug mgcViz, simulations will fail in various situations. See vignette for details!")
    out = getSimulations.default(object, nsim, type, ...)
  }
  return(out)
}

######## lme4 ############


#' @rdname getSimulations
#' @export
getSimulations.lmerMod <- function (object, nsim = 1, type = c("normal", "refit"), ...){
  
  if("(weights)" %in% colnames(model.frame(object))) warning(weightsWarning)
  getSimulations.default(object = object, nsim = nsim, type = type, ...)
}


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
  
  if("(weights)" %in% colnames(model.frame(object)) & ! family(object)$family %in% c("binomial", "betabinomial")) warning(weightsWarning)
  
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
getObservedResponse.HLfit <- function(object, ...){
  out = spaMM::response(object, ...)
  
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
getRefit.HLfit <- function(object, newresp, ...) {
  spaMM::update_resp(object, newresp, evaluate = TRUE)
}

#' @rdname getFitted
#' @export
getFitted.HLfit <- function (object,...){
  predict(object, type = "response", re.form = ~0)[,1L]
}

####### GLMMadaptive #########

### getObservedResponse - seems getObservedResponse.default is working

#' @rdname getSimulations
#' @export
getSimulations.MixMod <- function(object, nsim = 1, type = c("normal", "refit"), ...){
  
  if ("weights" %in% names(object)) warning(weightsWarning)
  
  type <- match.arg(type)
  
  out = simulate(object, nsim = nsim , ...)
  
  if(type == "normal"){
    if(!is.matrix(out)) out = data.matrix(out)
  }else{
    out = as.data.frame(out)
  }
  return(out)
}

#' @rdname getFixedEffects
#' @export
getFixedEffects.MixMod <- function(object, ...){
  out <- fixef(object, sub_model = "main")   
  return(out)
}

# TODO: this could go wrong if the DF has no column names, although I guess then one couldn't use formula
# TODO: goes wrong for k/n binomial with c(s,f) ~ pred syntax

#' @rdname getRefit
#' @export
getRefit.MixMod <- function(object, newresp, ...) {
  responsename = colnames(model.frame(object))[1] 
  newDat = object$data
  newDat[, match(responsename,names(newDat))] = newresp
  update(object, data = newDat)
}

#' @rdname getFitted
#' @export
getFitted.MixMod <- function (object,...){
  predict(object, type = "mean_subject")
}

#' @rdname getResiduals
#' @export
getResiduals.MixMod <- function (object,...){
  residuals(object, type = "subject_specific")
}




