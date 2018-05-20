# creating test data

testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel)

plot(simulationOutput, quantreg = F)

###### Distribution tests #####

testUniformity(simulationOutput)

###### Dispersion tests #######

testDispersion(simulationOutput, alternative = "less") # underdispersion

###### Both together###########

testResiduals(simulationOutput)

###### Special tests ##########

# testing zero inflation
testZeroInflation(simulationOutput)

# testing generic summaries
countOnes <- function(x) sum(x == 1)  # testing for number of 1s
testGeneric(simulationOutput, summary = countOnes) # 1-inflation
testGeneric(simulationOutput, summary = countOnes, alternative = "less") # 1-deficit

means <- function(x) mean(x) # testing if mean prediction fits
testGeneric(simulationOutput, summary = means) 

spread <- function(x) sd(x) # testing if mean sd fits
testGeneric(simulationOutput, summary = spread) 

###### Refited ##############

# if model is refitted, a different test will be called

simulationOutput <- simulateResiduals(fittedModel = fittedModel, refit = TRUE, seed = 12)
testDispersion(simulationOutput)


