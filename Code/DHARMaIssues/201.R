library(DHARMa)
library(lme4)

overdispersion = 0.3


testData = createData(sampleSize = 5000, fixedEffects = 1, family = poisson(), randomEffectVariance = 1, overdispersion = overdispersion, numGroups = 1000)

# fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

# res <- simulateResiduals(fittedModel = fittedModel, n = 100)
# testDispersion(res)
# side note: I could find no inflated type I error of any of the tests, even for very high sampleSize, as long as I moved n to 1000 or higher

###### Current default DHARMa tests ######

res <- simulateResiduals(fittedModel = fittedModel, n = 1000)
testDispersion(res)

# Condition on random effects
res <- simulateResiduals(fittedModel = fittedModel, re.form  = NULL)
testDispersion(res)


###### New parametric test (glmmWIKI) ######

# weird, two-sided test is always significant, always tests for underdispersion under H0 ... maybe this is the reason why they all test only for overdispersion in the packages
testDispersion(res, type = "Pearson")

# one-sided test seems to behave OK
testDispersion(res, type = "Pearson", alternative = "greater")


###### NEW OPTIONS ######

# note - could be run with re.form or without
res <- simulateResiduals(fittedModel = fittedModel, n = 1000)

# NEW Option 1: dispersion measured by variance of residuals against uniform 
obs = var(res$scaledResiduals) * 12
sim = replicate(1000, {var(runif(length(res$scaledResiduals)))}) * 12
DHARMa:::getP(simulated = sim, observed = obs, alternative = "two.sided", plot = T)

# NEW Option 2: dispersion measured by variance of the normal, which should be something like a simulation-based deviance residual? 
# issue is how to handle the outliers
obs = var(residuals(res, quantileFunction = qnorm, outlierValues = c(-7,7))) 
sim = replicate(1000, {var(rnorm(length(res$scaledResiduals)))}) # I know we could do this analytical, but I'm lazy
DHARMa:::getP(simulated = sim, observed = obs, alternative = "two.sided", plot = T)

# NEW Option 3 - probably problematic for situations with binomial / possion only 0 / 1 simulations
res <- simulateResiduals(fittedModel = fittedModel, re.form  = NULL)
expectedSD = apply(res$simulatedResponse, 1, sd)
spread <- function(x) sd((x - res$fittedPredictedResponse) / expectedSD) 
testGeneric(res, summary = spread)







