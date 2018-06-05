testData = createData(sampleSize = 200, family = poisson())


fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
newResponse = simulate(fittedModel)
refit(fittedModel, newResponse[,1])

fittedModel <- glm(observedResponse ~ Environment1 , data = testData, family = "poisson")
newResponse = simulate(fittedModel)
refit(fittedModel, newResponse[,1])

fittedModel <- mgcv::gam(observedResponse ~ s(Environment1) , data = testData, family = "poisson")
newResponse = simulate(fittedModel)
refit(fittedModel, newResponse[,1])

fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group) , data = testData)
newResponse = simulate(fittedModel)
refit(fittedModel, newResponse[,1])

fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group) , data = testData, family = "poisson")
newResponse = simulate(fittedModel)
refit(fittedModel, newResponse[,1])

fittedModel <- glmmTMB::glmmTMB(observedResponse ~ Environment1 + (1|group) , data = testData)
newResponse = simulate(fittedModel)
refit(fittedModel, newResponse[,1])
