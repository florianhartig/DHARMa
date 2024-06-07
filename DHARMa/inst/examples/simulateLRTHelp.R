
library(DHARMa)
library(lme4)

# create test data
set.seed(123)
dat <- createData(sampleSize = 200, randomEffectVariance = 1)

# define Null and alternative model (should be nested)
m1 = glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")
m0 = glm(observedResponse ~ Environment1 , data = dat, family = "poisson")

\dontrun{
# run LRT - n should be increased to at least 250 for a real study
out = simulateLRT(m0, m1, n = 10)

# To inspect warnings thrown during the refits:
out = simulateLRT(m0, m1, saveModels = TRUE, suppressWarnings = FALSE, n = 10)
summary(out$saveModels[[2]]$refittedM1) # RE SD = 0, no problem
# If there are warnings that seem problematic, 
# could try changing the optimizer or iterations
}
