
set.seed(13)
runif(1)

# testing the function in standard settings
currentSeed = .Random.seed
x = getRandomState(123)
runif(1)
x$restoreCurrent()
all(.Random.seed == currentSeed)

# if no seed was set in env, this will also be restored

rm(.Random.seed) # now, there is no random seed
x = getRandomState(123)
exists(".Random.seed")  # TRUE
runif(1)
x$restoreCurrent()
exists(".Random.seed") # False
runif(1) # re-create a seed

# with seed = false 
currentSeed = .Random.seed
x = getRandomState(FALSE)
runif(1)
x$restoreCurrent()
all(.Random.seed == currentSeed)

# with seed = NULL 
currentSeed = .Random.seed
x = getRandomState(NULL)
runif(1)
x$restoreCurrent()
all(.Random.seed == currentSeed)
