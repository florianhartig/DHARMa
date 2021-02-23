library(DHARMa)
library(lme4)
library(glmmTMB)
library(spaMM)

testData = createData(sampleSize = 200, overdispersion = 0, family = gaussian())

# lm weights are considered in simulate()
fit <- lm(observedResponse ~ Environment1 , weights = Environment1 +1, 
            data = testData)
simulateResiduals(fittedModel = fit, plot = T)


# glm gaussian still the same
fit <- glm(observedResponse ~ Environment1 , weights = Environment1 +1, 
          data = testData)
simulateResiduals(fittedModel = fit, plot = T)

# glm behaves nice, throws a warning that simulate ignores weights for poisson
fit <- glm.nb(observedResponse ~ Environment1 , weights = Environment1 +1, 
           data = testData)

model.frame(fit)
fit$model$`(weights)` = 1

class(fit)
simulateResiduals(fittedModel = fit, plot = T, rank = T)

# lmer behaves bad, doesn't include weights even for Gaussian, but doesn't warn either
fit <- lmer(observedResponse ~ Environment1 + (1|group), weights = Environment1 +1, 
            data = testData)
simulateResiduals(fittedModel = fit, plot = T)

# glmer warns
fit <- glmer(ceiling(exp(observedResponse)) ~ Environment1 + (1|group), weights = Environment1 +1, 
            data = testData, family = "poisson")
simulateResiduals(fittedModel = fit, plot = T)

# glmmTMB does not warn

fit <- glmmTMB(ceiling(exp(observedResponse)) ~ Environment1 + (1|group), weights = Environment1 +1, 
             data = testData, family = "poisson")
simulateResiduals(fittedModel = fit, plot = T)

# spaMM does not warn, but seems to be simulating with correct (heteroskedastic) variance. How exactly 
fit <- HLfit(ceiling(exp(observedResponse)) ~ Environment1 + (1|group), prior.weights = Environment1 +1, 
               data = testData, family=poisson())
summary(fit)
simulateResiduals(fittedModel = fit, plot = T)



