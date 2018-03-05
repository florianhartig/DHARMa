# getRandomState
# 
getRandomState <- function(seed){
  # random state
  if(is.na(seed)){
    if (!exists(".Random.seed"))runif(1) 
  }else{
    set.seed(seed)    
  }
  randomState = list(seed = seed, state = .Random.seed)  
}

