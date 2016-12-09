
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