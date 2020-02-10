testData = createData(sampleSize = 200, overdispersion = 0, family = gaussian())

fittedModel <- lmer(observedResponse ~ Environment1 + (1|group), 
                     data = testData)

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# plot residuals, quantreg = T is better but costs more time
plot(simulationOutput, quantreg = T)

x = plotResiduals(simulationOutput, testData$Environment1, rank = T)


library(SHT)
# x = data.frame(runif(100), runif(100)) # type I seems fine in simulations
unif.2017YMq(as.matrix(x))
ks.test(x$pred, punif)
ks.test(x$res, punif)

# -> problem is that this is nearly always giving significance, not 100% sure why

library(spatstat)

spatial = ppp(x$pred,x$res, c(0,1), c(0,1))
plot(spatial)
x <- ppm(spatial ~ x * y + polynom(y,2), Poisson())
summary(x)
plot(x)



hist(simulationOutput)

testResiduals(simulationOutput)
