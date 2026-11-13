



#' Test DHARMa compatibility
#' 
#' This helper function tests the compatibility of a model with DHARMa by trying to run various functions that are needed
#' 
#' @importFrom lme4 fixef
#' @importFrom lme4 ranef
#' @importFrom spaMM response
#' @importFrom spaMM update_resp
#' 
#' @param fittedModel the fitted model
#' 
#' @author Florian Hartig
#' @export
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
  try(refit(fittedModel, newresp = getResponse(fittedModel)))
  
}


dat = createData()
fittedModel <- glm(observedResponse ~ Environment1, data = dat)

fittedModel


try(family(fittedModel))
try(class(fittedModel)[1])
try(nobs(fittedModel))
try(getResponse(fittedModel))
try(simulate(fittedModel, nsim = 10))
try(predict(fittedModel))
try(coef(fittedModel))
try(ranef(fittedModel))
try(fixef(fittedModel))
try(refit(fittedModel, newresp = getResponse(fittedModel)))



testModel(fittedModel)

