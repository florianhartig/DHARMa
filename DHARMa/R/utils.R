securityAssertion <- function(context = "Not provided", stop = F){
  generalMessage = "Message from DHARMa package: a security assertion was not met. This means that during the execution of a DHARMa function, some unexpected conditions ocurred. Even if you didn't get an error, your results may not be reliable. Please check with the help if you use the functions as intended. If you think that the error is not on your side, I would be grateful if you could report the problem at https://github.com/florianhartig/DHARMa/issues \n\n Context:"
  if (stop == F) warning(paste(generalMessage, context))  
  else stop(paste(generalMessage, context))  
}


checkOutput <- function(simulationOutput){
  
  print(simulationOutput)
  
  if(any(simulationOutput$scaledResiduals < 0)) stop()
  if(any(simulationOutput$scaledResiduals > 1)) stop()
  if(any(is.na(simulationOutput$scaledResiduals))) stop()
  
  if(length(simulationOutput$scaledResiduals) != length(simulationOutput$observedResponse)) stop()
  if(length(simulationOutput$fittedPredictedResponse) != length(simulationOutput$observedResponse)) stop()
  
}