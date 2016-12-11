
#' 
#' 
#' This function overwrites the standard fitted function for GAM
#' @note See explanation at 
#' @param x fitted model
#' @export
fitted.gam <- function(x){
  class(x) = "glm"
  out = stats::fitted(x)
  names(out) = as.character(1:length(out))
  out
}

# Check that this works
# plot(fitted(fittedModelGAM), predict(fittedModelGAM, type = "response"))