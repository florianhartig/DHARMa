library(spaMM)

data("Loaloa") ## from spaMM package
loafit <- corrHLfit(cbind(npos,ntot-npos)~maxNDVI1+seNDVI
                    +Matern(1|longitude+latitude),
                    data=Loaloa[1:30,],family=binomial(),
                    init.corrHLfit=list(Nugget=0.1),ranFix=list(nu=0.5))


########################

fittedModel = loafit

family(fittedModel)
class(fittedModel)[1]
nobs(fittedModel)
getResponse(fittedModel)
<<<<<<< HEAD
=======

getResponse.HLfit <- function(object, ...){
  response(object, ...)
}

>>>>>>> master
simulate.HLfit(fittedModel, nsim = 2)
predict.HLfit(fittedModel)
coef(fittedModel)
ranef(fittedModel)
fixef(fittedModel)

DHARMa:::getFixedEffects(fittedModel)

res = simulateResiduals(fittedModel)
plot(res)

coef(fittedModel)
fixef(fittedModel)


testData = createData(sampleSize = 200, overdispersion = 0.0, randomEffectVariance = 1, family = poisson())
fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)
testUniformity(res)$p.value < 0.001

testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
fittedModel <- HLfit(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)
testUniformity(res)$p.value < 0.001


testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial())
fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "binomial",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)
testUniformity(res)$p.value < 0.001


########################


simsCond = simulate(nsim=250, loafit, type="residual") # I think that this is what I call a conditional simulat, i.e. simulations are conditinal on the fitted REs
simsUncond = simulate(nsim=250, loafit, type="marginal") # I think that this is what I call a unconditional simulation, i.e. random effects and glm distribution are re-siulated?

createDHARMa(simulatedResponse = sims, observedResponse = loafit$y,
             fittedPredictedResponse = predict(loafit, re.form=~0)[,1L], integer = T)


