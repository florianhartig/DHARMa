#' Record and restore a random state
#' 
#' The aim of this function is to record, manipulate and restore a random state
#' 
#' @param seed seed argument to set.seed(), typically a number. Additional options: NULL = no seed is set, but return includes function for restoring random seed. F = function does nothing, i.e. neither seed is changed, nor does the returned function do anything 
#' 
#' @details This function is intended for two (not mutually exclusive tasks)
#' 
#' a) record the current random state
#' 
#' b) change the current random state in a way that the previous state can be restored
#' 
#' @return a list with various infos about the random state that after function execution, as well as a function to restore the previous state before the function execution
#' @export
#' @example inst/examples/getRandomStateHelp.R
#' @author Florian Hartig
#' 
getRandomState <- function(seed = NULL){
  
  # better to explicitly access the global RS?
  # current = get(".Random.seed", .GlobalEnv, ifnotfound = NULL)
  
  current = mget(".Random.seed", envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
  
  if(!is.null(seed) && is.logical(seed) && seed == F){
    restoreCurrent <- function(){}    
  }else{
    restoreCurrent <- function(){
      if(is.null(current)) rm(".Random.seed", envir = .GlobalEnv) else assign(".Random.seed", current , envir = .GlobalEnv)
    }    
  }

  # setting seed
  if(is.numeric(seed)) set.seed(seed)

  # ensuring that RNG has been initialized
  if (is.null(current))runif(1) 
  
  randomState = list(seed, state = get(".Random.seed", globalenv()), kind = RNGkind(), restoreCurrent = restoreCurrent)  
  return(randomState)
}

