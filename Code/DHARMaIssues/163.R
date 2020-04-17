library(DHARMa)
testData = createData(intercept = 5, family = gaussian())

library(lme4)
fittedModel <- glmer(observedResponse ~ log(Environment1+0.01) + (1|group), 
                     family = Gamma(link = "identity"), data = testData,
                     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

simulateResiduals()