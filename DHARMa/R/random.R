#' Record and restore a random state
#' 
#' The aim of this function is to record, manipualate and restor a random state
#' 
#' @details This function is intended for two (not mutually exclusive tasks)
#' 
#' a) record the current random state
#' 
#' b) change the current random state in a way that the previous state can be restored
#' 
#' @return a list with various infos about the random state that after function execution, as well as a function to restore the previous state before the function execution
#' 
#' @param seed seed argument to set.seed()
#' @export
#' @example inst/examples/getRandomStateHelp.R
#' @author Florian Hartig
#' 
getRandomState <- function(seed = NULL){
  
  # better to explicitly access the global RS?
  # current = get(".Random.seed", .GlobalEnv, ifnotfound = NULL)
  
  current = mget(".Random.seed", envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
  
  restoreCurrent <- function(){
    if(is.null(current)) rm(".Random.seed", envir = .GlobalEnv) else assign(".Random.seed", current , envir = .GlobalEnv)
  }
  
  # setting seed
  if(is.numeric(seed)) set.seed(seed)

  # ensuring that RNG has been initialized
  if (is.null(current))runif(1) 
  
  randomState = list(seed, state = get(".Random.seed", globalenv()), kind = RNGkind(), restoreCurrent = restoreCurrent)  
  return(randomState)
}

