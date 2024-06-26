
library(sjSDM)

community = simulate_SDM(env = 3L, species = 10L, sites = 100L)

Occ <- community$response
Env <- community$env_weights
SP <- data.frame(matrix(rnorm(200, 0, 0.3), 100, 2)) # spatial coordinates


# fit model:
model <- sjSDM(Y = Occ, 
               env = linear(data = Env, formula = ~X1+X2+X3), 
               spatial = linear(data = SP, formula = ~0+X1*X2), 
               family=binomial("probit"),
               iter = 20) # increase iter for real analysis


DHARMa:::getObservedResponse(model)


getObservedResponse.sjSDM <- function (object, ...){
  out = vector(object$data$Y)
  return(out)
}

getRefit.sjSDM <- function (object, newresp, ...){
  error(object, newresp, ...)
}

#' @rdname getSimulations
#' @export
getSimulations.default <- function (object, nsim = 1, type = c("normal", "refit"), ...){
  
  type <- match.arg(type)
  
  out = simulate(object, nsim = nsim , ...)
  
  if (type == "normal"){
    if(family(object)$family %in% c("binomial", "betabinomial")){
      
      if(is.factor(out[[1]])){
        if(nlevels(out[[1]]) != 2){
          warning("The fitted model has a factorial response with number of levels not equal to 2 - there is currently no sensible application in DHARMa that would lead to this situation. Likely, you are trying something that doesn't work.")
        }
        else{
          out = data.matrix(out) - 1
        }
      }
      
      if("(weights)" %in% colnames(model.frame(object))){
        x = model.frame(object)
        out = out * x$`(weights)`
      } else if (is.matrix(out[[1]])){
        # this is for the k/n binomial case
        out = as.matrix(out)[,seq(1, (2*nsim), by = 2)]
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
  } else if(class(object)[1] %in% c("glmerMod", "lmerMod", "HLfit", "lmerTest")){
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

#' @rdname getResiduals
#' @export
getResiduals.default <- function (object, ...){
  residuals(object, type = "response", ...)
}

#' @rdname getPearsonResiduals
#' @export
getPearsonResiduals.default <- function (object, ...){
  residuals(object, type = "pearson", ...)
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


