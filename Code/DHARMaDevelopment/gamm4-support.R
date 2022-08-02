library(gamm4)
library(DHARMa)

dat <- createData(sampleSize = 200, fixedEffects = c(1,0), 
                  overdispersion = 0, randomEffectVariance = 1, 
                  family = binomial(), factorResponse = T)

fit <- gamm4(observedResponse ~ s(Environment1), random= ~ (1|group), 
                            family = "binomial", data = dat)

fitLME4 <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData$binomial_yn)

res <- simulateResiduals(fit$mer)
plot(res)

class(fit$mer)
class(fitLME4)

x1 = simulate(fit$mer, 3)
x2 = simulate(fitLME4, 3)

str(x1)
str(x2)

x1 = getSimulations(fit$mer, 3)
x2 = getSimulations(fitLME4, 3)


test = createDHARMa(simulatedResponse = , observedResponse = dat$observedResponse, fittedPredictedResponse = predict(fit$mer))


getObservedResponse(fit$mer)

class(fit)

model.frame(fit$mer)

summary(fit$mer)

res <- simulateResiduals(fit$gam)

simulate(fit$gam)

summary(fit$mer)
summary(fit$gam)








