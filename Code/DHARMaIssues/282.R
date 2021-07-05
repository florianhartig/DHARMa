library(DHARMa)
testData = createData(sampleSize = 1000, randomEffectVariance = 3, family = gaussian(),  numGroups = 10)
library(mgcv)
fit <- gam(observedResponse ~ Environment1 +  s(group, bs="re"), data=testData)
simulateResiduals(fit, plot = T)

plotResiduals(fit, predict.gam(fit, exclude ="s(group)"))


library(lme4)
fit <- lmer(observedResponse ~ Environment1 +  (1|group), data=testData)
simulateResiduals(fit, plot = T)
summary(fit)

