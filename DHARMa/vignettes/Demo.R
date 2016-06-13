## ------------------------------------------------------------------------
library(DHARMa)
library(lme4)

## ------------------------------------------------------------------------
testData = createPoissonData()[[1]]
# see ?createPoissonData for paramter values

## ------------------------------------------------------------------------
fittedModel <- glmer(counts ~ environment1 + (1|group) + (1|ID) , family = "poisson", data = testData)
summary(fittedModel)
plot(fittedModel, resid(., type = "deviance") ~ log(fitted(.)))
plot(fittedModel, resid(., type = "pearson") ~ log(fitted(.)))
plot(fittedModel, resid(., type = "response") ~ log(fitted(.)))


