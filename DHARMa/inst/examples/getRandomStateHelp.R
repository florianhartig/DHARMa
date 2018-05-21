# testing the function in standard settings

set.seed(13)
runif(1)
x = getRandomState(123)
runif(1)
x$restoreCurrent()
runif(1)

# values outside set /restore are identical to

set.seed(13)
runif(2)

# if no seed is set, this will also be restored

rm(.Random.seed)

x = getRandomState(123)
runif(1)
x$restoreCurrent()
exists(".Random.seed")

# with false 

rm(.Random.seed)
x = getRandomState(seed = FALSE)
exists(".Random.seed")
runif(1)
x$restoreCurrent()
exists(".Random.seed")
