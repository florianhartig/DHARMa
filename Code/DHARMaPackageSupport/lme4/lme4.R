library(lme4)
library(DHARMa)

testData = createData(sampleSize = 300, overdispersion = 0, randomEffectVariance = 0, family = poisson())

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

res =  simulateResiduals(fittedModel)
plot(res)


res =  simulateResiduals(fittedModel, refit = T)
plot(res)



## GLMM with individual-level variability (accounting for overdispersion)
## For this data set the model is the same as one allowing for a period:herd
## interaction, which the plot indicates could be needed.
cbpp$obs <- 1:nrow(cbpp)
gm2 <- glmer(cbind(incidence, size - incidence) ~ period +
                (1 | herd) +  (1|obs),
              family = binomial, data = cbpp)

res = simulate(gm2)
plot(res)



library(lme4)

testData = createData(sampleSize = 200, overdispersion = 0.5, family = poisson())
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), 
                     family = "poisson", data = testData,
                     control=glmerControl(optCtrl=list(maxfun=20000) ))

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# plot residuals, quantreg = T is better but costs more time
plot(simulationOutput, quantreg = FALSE)
