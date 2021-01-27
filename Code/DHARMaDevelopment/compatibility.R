
### generic Wrappers ###

#' @export
getResponse <- function (x, ...) {
  UseMethod("getResponse", x)
}

getResponse <- function(fittedModel){
  model.frame(fittedModel)[,1]  
}

predictGeneric <- function(fittedModel, ...){
  predictedResponse = predict(fm1, response = T)
}

x = gaussian()


#' Generic functions to simulate from a fitted model
#' 
#' A generic method to create simulations from model classes that don't provide a simulate function.
#' @param fittedModel fitted model object
#' @param n integer number > 1, number of simulations to run.
simulateGeneric <- function(predictions, family, nsim = n){
  
  nObs = length(predictions)
}


### PACKAGE nlme ####

## Class lme

# this adds a family function for lme models 
family.lme <- function(fittedModel){
  return(gaussian())
}

simulate.lme <- function(fittedModel){

}




#' @method getResponse lme 
#' @export
getResponse.lme <-function(fittedModel){
  name =  as.character(fittedModel$terms[[2]])
  response = fm1$data[[name]]
  return(response)
}



## The lm method includes objects of class "glm"
simulate.lm <- function(object, nsim = 1, seed = NULL, ...)
{
  if(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
    runif(1)                     # initialize the RNG if necessary
  if(is.null(seed))
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
    R.seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    RNGstate <- structure(seed, kind = as.list(RNGkind()))
    on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
  }
  fam <- if(inherits(object, "glm")) object$family$family else "gaussian"
  ftd <- fitted(object)             # == napredict(*, object$fitted)
  isMlm <- identical(fam, "gaussian") && is.matrix(ftd)
  nm <- if(isMlm) dimnames(ftd) else names(ftd)
  if(isMlm) ## Not hard. Biggest question: how exactly the data frame should look
    stop("simulate() is not yet implemented for multivariate lm()")
  n <- length(ftd)
  ntot <- n * nsim
  val <- switch(fam,
                "gaussian" = {
                  vars <- deviance(object)/ df.residual(object)
                  if(isMlm) {
                    ## _TODO_
                    ## weights ==> "vars / weights" as matrix with  dim(ftd)
                  } else {
                    if (!is.null(object$weights)) vars <- vars/object$weights
                    ftd + rnorm(ntot, sd = sqrt(vars))
                  }
                },
                if(!is.null(object$family$simulate))
                  object$family$simulate(object, nsim)
                else stop(gettextf("family '%s' not implemented", fam),
                          domain = NA)
  )
  
  if(isMlm) {
    ## _TODO_
  } else if(!is.list(val)) {
    dim(val) <- c(n, nsim)
    val <- as.data.frame(val)
  } else
    class(val) <- "data.frame"
  ## isMlm: conceptually, each "sim_i" could be a *matrix* [unusually]
  names(val) <- paste("sim", seq_len(nsim), sep="_")
  if (!is.null(nm)) row.names(val) <- nm
  attr(val, "seed") <- RNGstate
  val
}