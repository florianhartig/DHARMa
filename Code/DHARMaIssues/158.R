library(glmmTMB)
library(DHARMa)
library(lme4) # for data
library(mgcv)


tmpMatrix = glmmTMB(cbind(incidence, size - incidence) ~ period + (1 | herd),
               data = cbpp, family = binomial)

lme4Matrix = glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial)


cbpp2 = transform(cbpp, prop=incidence/size)

tmbWeights = glmmTMB(prop ~ period + (1 | herd),
               data = cbpp2, weights = size, family = binomial)

lme4Weights = glmer(prop ~ period + (1 | herd),
                      data = cbpp2, weights = size, family = binomial)


glmMatrix = glm(cbind(incidence, size - incidence) ~ period,
                data = cbpp, family = binomial)

glmWeights = glm(prop ~ period ,
                    data = cbpp2, weights = size, family = binomial)


gamMatrix = gam(cbind(incidence, size - incidence) ~ period,
                data = cbpp, family = binomial)

gamWeights = gam(prop ~ period ,
                 data = cbpp2, weights = size, family = binomial)



all.equal(fixef(tmpMatrix), fixef(tmbWeights)) #TRUE
all.equal(ranef(tmpMatrix), ranef(tmbWeights)) #TRUE
all.equal(summary(tmpMatrix), summary(tmbWeights)) #all except "call"


simulate(tmpMatrix, n = 10, seed=111)
simulate(tmbWeights, n = 10, seed=111)

simulate(lme4Matrix, nsim = 10)
simulate(lme4Weights, nsim = 10)


model.frame(tmpMatrix)
model.frame(tmpMatrix)[,1]
model.frame(tmbWeights)

model.frame(lme4Matrix)
model.frame(lme4Matrix)[,1]
model.frame(lme4Weights)

# conclusion: for weights, lm4 fits and simulates proportions
# TMB fits proportion, but simulates counts


getObservedResponse(tmpMatrix)
getObservedResponse(tmbWeights)

getObservedResponse(lme4Matrix)
getObservedResponse(lme4Weights)

getObservedResponse(glmMatrix)
getObservedResponse(glmWeights)

getObservedResponse(gamMatrix)
getObservedResponse(gamWeights)

getSimulations(tmpMatrix, 1)
getSimulations(tmbWeights)

getSimulations(lme4Matrix)
getSimulations(lme4Weights)

getSimulations(glmMatrix)
getSimulations(glmWeights)

getSimulations(gamMatrix)
getSimulations(gamWeights)







all.equal(testUniformity(restmpMatrix), testUniformity(restmbWeights)) #big differences
testDispersion(restmpMatrix) #OK
testDispersion(restmbWeights) #BAD


x1 = simulate(tmpMatrix)
x2 = simulate(tmbWeights)

str(x1)
str(x2)

restmpMatrix$observedResponse
restmbWeights$observedResponse



restmpMatrix = simulateResiduals(lme4Matrix, n = 1000, seed=111)
restmbWeights = simulateResiduals(lme4Weights, n = 1000, seed=111)

restmpMatrix$observedResponse
restmbWeights$observedResponse

restmpMatrix$simulatedResponse
restmbWeights$simulatedResponse


x1 = simulate(lme4Matrix)
x2 = simulate(lme4Weights)

lme4Matrix = glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
              data = cbpp, family = binomial)


cbpp2 = transform(cbpp, prop=incidence/size)

tmbWeights = glmmTMB(prop ~ period + (1 | herd),
               data = cbpp2, weights = size, family = binomial)

lme4Weights = glmer(prop ~ period + (1 | herd),
              data = cbpp2, weights = size, family = binomial)




