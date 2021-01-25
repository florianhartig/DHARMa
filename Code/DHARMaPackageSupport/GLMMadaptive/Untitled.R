devtools::install_github("drizopoulos/GLMMadaptive")
library(GLMMadaptive)
library(DHARMa)


# simulate some data
set.seed(123L)

DF = testData = createData(sampleSize = 500, overdispersion = 0, randomEffectVariance = 0.000, family = poisson())
fm1 <-  fittedModel <- mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 | group, data = DF, family = poisson())

res <- simulateResiduals(fm1)


getFixedEffects(fm1)

# fixed effects
fixef(fm1, sub_model = "main")

# random effects
head(ranef(fm1))

# detailed output
summary(fm1)

# fitted values for the 'mean subject', i.e., with
# random effects values equal to 0
head(fitted(fm1, type = "mean_subject"))

# fitted values for the conditioning on the estimated random effects
head(fitted(fm1, type = "subject_specific"))


fixef(fm1)
ranef(fm1)
family(fm1)
model.frame(fm1)
simulate(fm1)

# Check update behavior

DF2 = testData = createData(sampleSize = 500, overdispersion = 0, randomEffectVariance = 0.000, family = poisson())
fm2 = update(fm1, data = DF2)
fm3 <-  fittedModel <- mixed_model(fixed = observedResponse ~ Environment1, random = ~ 1 | group, data = DF2, family = poisson())



