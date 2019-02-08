testData = createData(sampleSize = 20, overdispersion = 0, randomEffectVariance = 1, family = binomial(), binomialTrials = 20)

testData$group2 = 1:20

fittedModel <- glmmTMB(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
res <- simulateResiduals(fittedModel)
plot(res)
getSimulations(fittedModel, nsim = 2)
x = getSimulations(fittedModel, nsim = 2)
x 
str(x)
res$simulatedResponse


fittedModel <- glmer(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
res <- simulateResiduals(fittedModel)
plot(res)
x = getSimulations(fittedModel, nsim = 2)
x 
str(x)

fittedModel <- glmmTMB(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "betabinomial", data = testData)
runEverything(fittedModel, testData)


out$simulatedResponse = getSimulations(fittedModel, nsim = 2)



if(any(dim(out$simulatedResponse) != c(out$nObs, out$nSim) )) securityAssertion("Simulation results have wrong dimension", stop = T)
