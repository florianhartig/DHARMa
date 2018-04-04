library(DHARMa)

testData = createData(sampleSize = 200, randomEffectVariance = 0, family = binomial(), binomialTrials = 2)

# glm help - For binomial and quasibinomial families the response can also be specified as a factor (when the first level denotes failure and all others success) or as a two-column matrix with the columns giving the numbers of successes and failures. 
# count successes
sum(testData$observedResponse1 == 0)
sum(testData$observedResponse0 == 0)

fittedModel <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)
summary(fittedModel)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 200)
testZeroInflation(simulationOutput)

# now with weights - glm help: Non-NULL weights can be used to indicate that different observations have different dispersions (with the values in weights being inversely proportional to the dispersions); or equivalently, when the elements of weights are positive integers w_i, that each response y_i is the mean of w_i unit-weight observations. For a binomial GLM prior weights are used to give the number of trials when the response is the proportion of successes: they would rarely be used for a Poisson GLM.

fittedModel <- glm(observedResponse1/2 ~ Environment1 , weights = rep(2, 200), family = "binomial", data = testData)
summary(fittedModel)
simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 200)
testZeroInflation(simulationOutput)

# seems to work fine