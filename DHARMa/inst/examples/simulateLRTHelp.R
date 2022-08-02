
library(DHARMa)
library(lme4)

set.seed(123)
dat <- createData(sampleSize = 200, randomEffectVariance = 1)

m1 = glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")


m0 = glm(observedResponse ~ Environment1 , data = dat, family = "poisson")

\dontrun{
out = simulateLRT(m0, m1, n = 10)

# LRT produced warnings, can inspect what's going on
out = simulateLRT(m0, m1, saveModels = T, suppressWarnings = T, n = 10)
summary(out$saveModels[[2]]$refittedM1) # RE SD = 0
# Could try changing the optimizer to reduce warnings
}
