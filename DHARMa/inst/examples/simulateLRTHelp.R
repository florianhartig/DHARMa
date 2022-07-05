
library(DHARMa)
library(lme4)

set.seed(123)
dat <- createData(sampleSize = 200, randomEffectVariance = 1)

m1 = glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")


m0 = glm(observedResponse ~ Environment1 , data = dat, family = "poisson")

out = simulateLRT(m0, m1)
out
