library(spaMM)

data("Loaloa") ## from spaMM package
fittedModel <- corrHLfit(cbind(npos,ntot-npos)~maxNDVI1+seNDVI
                    +Matern(1|longitude+latitude),
                    data=Loaloa[1:30,],family=binomial(),
                    init.corrHLfit=list(Nugget=0.1),ranFix=list(nu=0.5))

testModel(fittedModel)

res = simulateResiduals(fittedModel)
plot(res)


data("blackcap")
fullfit <- corrHLfit(migStatus ~ means+ Matern(1|latitude+longitude),data=blackcap,
                     HLmethod="ML") 


res = simulateResiduals(fullfit)
plot(res)


nullfit <- corrHLfit(migStatus ~ 1 + Matern(1|latitude+longitude),data=blackcap,
                     HLmethod="ML",init.corrHLfit=list(phi=1e-6)) 

res = simulateResiduals(fullfit)
plot(res)

data("scotlip") ## loads 'scotlip' data frame, but also 'Nmatrix'
fit = HLCor(cases~I(prop.ag/10) +adjacency(1|gridcode)+offset(log(expec)),
      adjMatrix=Nmatrix,family=poisson(),data=scotlip) 

res = simulateResiduals(fit)
plot(res)


fit = HLCor(cases~I(prop.ag/10) +(1|gridcode)+adjacency(1|gridcode)
      +offset(log(expec)),
      data=scotlip,family=poisson(),rand.family=list(Gamma(log),gaussian()),
      adjMatrix=Nmatrix)

res = simulateResiduals(fit)
plot(res)

data("salamander")
fit = HLfit(cbind(Mate,1-Mate)~1+(1|Female)+(1|Male),family=binomial(),
      rand.family=list(gaussian(),Beta(logit)),data=salamander,HLmethod="ML")
res = simulateResiduals(fit)
plot(res)

fit = HLfit(cbind(Mate,1-Mate)~1+(1|Female/Male),
      family=binomial(),rand.family=Beta(logit),data=salamander,HLmethod="ML")
res = simulateResiduals(fit)
plot(res)

data("wafers")
fit <- HLfit(y ~X1*X3+X2*X3+I(X2^2)+(1|batch),family=Gamma(log),
      data=wafers)
res = simulateResiduals(fit)
plot(res)

fit <- HLfit(y ~X1*X3+X2*X3+I(X2^2)+(1|batch),family=Gamma(log),
      resid.model = ~ X3+I(X3^2) ,data=wafers)
res = simulateResiduals(fit)
plot(res)



fit <- HLfit(y~X1+(X2|batch),data=wafers)
res = simulateResiduals(fit)
plot(res, rank = F)

# Simulations 

testData = createData(sampleSize = 1000, overdispersion = 0.0, randomEffectVariance = 1, family = poisson())

fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
summary(fittedModel)
res = simulateResiduals(fittedModel, re.form = ~0)
plot(res)


testData = createData(sampleSize = 200, overdispersion = 0.0, randomEffectVariance = 1, family = poisson())
fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "poisson",  data = testData)
res = simulateResiduals(fittedModel, refit = T)
plot(res)


testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
fittedModel <- HLfit(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)

testData = createData(sampleSize = 200, fixedEffects = c(1,0), overdispersion = 0, randomEffectVariance = 0, family = binomial())
fittedModel <- HLfit(observedResponse ~ Environment1 + (1|group) , family = "binomial",  data = testData)
res = simulateResiduals(fittedModel)
plot(res)
testResiduals(res)



