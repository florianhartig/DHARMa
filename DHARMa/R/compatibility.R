
#' 
#' 
#' This function overwrites the standard fitted function for GAM
#' @note See explanation at 
#' @param object fitted model
#' @export
fitted.gam <- function(object, ...){
  class(object) = "glm"
  out = stats::fitted(object, ...)
  names(out) = as.character(1:length(out))
  out
}

# Check that this works
# plot(fitted(fittedModelGAM), predict(fittedModelGAM, type = "response"))