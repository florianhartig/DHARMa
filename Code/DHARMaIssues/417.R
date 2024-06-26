hist(rTweedie(rep(1,1000),p=1.5,phi=1.3))


f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
  (10 * x)^3 * (1 - x)^10
n <- 3000
x <- runif(n)
mu <- exp(f2(x)/3+.1);x <- x*10 - 4
y <- rTweedie(mu,p=1.5,phi=1.3)

# correct p
b <- gam(y~s(x,k=20),family=Tweedie(p=1.5))

res = simulateResiduals(b, plot = T)
testDispersion(res)
testDispersion(res, type = "PearsonChisq") # got wrong results before the fix 

# incorrect p
b <- gam(y~s(x,k=20),family=Tweedie(p=1.1))

res = simulateResiduals(b, plot = T)
testDispersion(res)
testDispersion(res, type = "PearsonChisq")

x = residuals(b, type = "scaled.pearson")
sd(x)

x = residuals(b, type = "pearson")
sd(x)



# testing if poisson problems are recognized correctly with the fix

?simulateResiduals


testData = createData(sampleSize = 100, overdispersion = 0.5, family = poisson())
fittedModel <- gam(observedResponse ~ Environment1 , 
                     family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# standard plot 
plot(simulationOutput)

testDispersion(simulationOutput)
testDispersion(simulationOutput, type = "PearsonChisq")



testData = createData(sampleSize = 100, overdispersion = 0.5, randomEffectVariance = 0, family = poisson())
fittedModel <- gam(observedResponse ~ Environment1 , 
                   family = "poisson", data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# standard plot 
plot(simulationOutput)

testDispersion(simulationOutput)
testDispersion(simulationOutput, type = "PearsonChisq")


