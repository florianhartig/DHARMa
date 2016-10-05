context("Tests DHARMa functions on all implemented model types")

library(MASS)
library(lme4)


runEverything = function(fittedModel, testData, DHARMaData = T){
  simulationOutput <- simulateResiduals(fittedModel = fittedModel)
  
  print(simulationOutput)
  plot(simulationOutput, quantreg = F)
  
  plotSimulatedResiduals(simulationOutput = simulationOutput)
  if(DHARMaData == T) plotResiduals(pred = testData$Environment1, simulationOutput$scaledResiduals, quantreg = F)

  testUniformity(simulationOutput = simulationOutput)
  testZeroInflation(simulationOutput = simulationOutput)
  testTemporalAutocorrelation(simulationOutput = simulationOutput, time = runif(length(simulationOutput$scaledResiduals )))
  testSpatialAutocorrelation(simulationOutput = simulationOutput, x = runif(length(simulationOutput$scaledResiduals )), y =  runif(length(simulationOutput$scaledResiduals )))

  
  simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, refit = T, n = 10) # n=10 is very low, set higher for serious tests
  
  print(simulationOutput2)
  plot(simulationOutput2, quantreg = F)
  
  testOverdispersion(simulationOutput2)
  testOverdispersionParametric(fittedModel)
  
}


test_that("lm gaussian works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm gaussian works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = gaussian())
            fittedModel <- glm(observedResponse ~ Environment1 , data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("lmer gaussian works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 100, overdispersion = 0, randomEffectVariance = 1, family = gaussian())
            fittedModel <- lmer(observedResponse ~ Environment1 + (1|group) , data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm binomial 1/0 works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial())
            fittedModel <- glm(observedResponse ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm binomial n/k works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = binomial(), binomialTrials = 20)
            fittedModel <- glm(cbind(observedResponse1,observedResponse0)  ~ Environment1 , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)



test_that("glmer binomial 1/0 works", 
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, overdispersion = 2, family = binomial())
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glmer binomial n/k works", 
          {
            skip_on_cran()
            
            testData = createData(sampleSize = 200, overdispersion = 2, family = binomial(), binomialTrials = 20)
            fittedModel <- glmer(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)
            runEverything(fittedModel, testData)
          }
)


test_that("glm poisson works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- glm(observedResponse ~ Environment1 , family = "poisson", data = testData)
            runEverything(fittedModel, testData)
          }
)



test_that("glmer poisson works", 
          {
            skip_on_cran()
            testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 1, family = poisson(), roundPoissonVariance = 0.1, pZeroInflation = 0.1)
            fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) + (1|ID), family = "poisson", data = testData, control=glmerControl(optCtrl=list(maxfun=20000) ))
            runEverything(fittedModel, testData)
          }
)


# Negative binomial models 


test_that("glmer.nb works", 
          {
            skip_on_cran()
            
            
            set.seed(101)
            dd <- expand.grid(f1 = factor(1:3),
                              f2 = LETTERS[1:2], g=1:9, rep=1:15,
                              KEEP.OUT.ATTRS=FALSE)
            summary(mu <- 5*(-4 + with(dd, as.integer(f1) + 4*as.numeric(f2))))
            dd$y <- rnbinom(nrow(dd), mu = mu, size = 0.5)
            str(dd)
            
            testData = dd
            
            fittedModel <- glmer.nb(y ~ f1*f2 + (1|g), data=testData, verbose=TRUE)

            runEverything(fittedModel, testData, DHARMaData = F)
          }
)



test_that("glmer.nb works", 
          {
            skip_on_cran()
            testData = quine
            fittedModel <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = testData)
            runEverything(fittedModel, testData, DHARMaData = F)
          }
)


