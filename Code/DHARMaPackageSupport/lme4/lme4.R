library(lme4)
library(DHARMa)

testData = createData(sampleSize = 300, overdispersion = 2, randomEffectVariance = 0, family = poisson())

##################################################
# fitted poisson - clear overdispersion
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = 
                       "poisson", data = testData)

testModel(fittedModel)

x = simulate(fittedModel)
plot(x)
class(x)



## GLMM with individual-level variability (accounting for overdispersion)
## For this data set the model is the same as one allowing for a period:herd
## interaction, which the plot indicates could be needed.
cbpp$obs <- 1:nrow(cbpp)
gm2 <- glmer(cbind(incidence, size - incidence) ~ period +
                (1 | herd) +  (1|obs),
              family = binomial, data = cbpp)

res = simulate(gm2)
plot(res)
