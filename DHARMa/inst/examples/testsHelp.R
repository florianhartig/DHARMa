# creating test data

testData = createData(sampleSize = 200, overdispersion = 0.4, pZeroInflation = 0, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

plot(simulationOutput)

###### Distribution tests #####

testUniformity(simulationOutput)

###### Dispersion tests #######

simulationOutput <- simulateResiduals(fittedModel = fittedModel)
testDispersion(simulationOutput)

simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = T)
testDispersion(simulationOutput)

###### Special tests ##########

simulationOutput <- simulateResiduals(fittedModel = fittedModel)

# testing zero inflation
testZeroInflation(simulationOutput)

# testing generic summaries
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testSummary(simulationOutput, summary = countOnes) # 1-inflation
testSummary(simulationOutput, summary = countOnes, alternative = "less") # 1-deficit

means <- function(x) mean(x) # testing if mean prediction fits
testSummary(simulationOutput, summary = means) 

spread <- function(x) sd(x) # testing if mean sd fits
testSummary(simulationOutput, summary = spread) 

library(AER)
dispersiontest(fittedModel)
