## Testing the createData with/without temporal autocorrelation
# Melina Leite
# 24 sept 2024


# While doing unit testing for Mixed Models, I realized that if the model doesn't estiamte the correct variance of the group RE, this extra variability may end in the temporal autocorrelation becuase the way we create the column `time` in the dataset is sequential (which is also the same for creating groups), so the time is clustered in the groups.

set.seed(123)
testData <- createData(sampleSize = 200,
                       overdispersion = 0, randomEffectVariance = 0.5,
                       family = gaussian())
head(testData)

fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group),
                          data = testData)
summary(fittedModel)
simulationOutput <- simulateResiduals(fittedModel, n=200)
testTemporalAutocorrelation(simulationOutput = simulationOutput,
                            time = testData$time)


set.seed(123)
testData <- createData(sampleSize = 200, temporalAutocorrelation = 1,
                       overdispersion = 0, randomEffectVariance = 0.5,
                       family = gaussian())
head(testData)

fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group),
                          data = testData)
summary(fittedModel)
simulationOutput <- simulateResiduals(fittedModel, n=200)
testTemporalAutocorrelation(simulationOutput = simulationOutput,
                            time = testData$time)



createData2 <- function(sampleSize = 100, intercept = 0, fixedEffects = 1,
                       quadraticFixedEffects = NULL, numGroups = 10,
                       randomEffectVariance = 1, overdispersion = 0,
                       family = poisson(), scale = 1, cor = 0,
                       roundPoissonVariance = NULL,  pZeroInflation = 0,
                       binomialTrials = 1, temporalAutocorrelation = 0,
                       spatialAutocorrelation = 0, factorResponse = FALSE,
                       replicates = 1, hasNA = FALSE){

  nPredictors = length(fixedEffects)

  out = list()

  time = sample.int(sampleSizex) # change here
  x = runif(sampleSize)
  y = runif(sampleSize)

  for (i in 1:replicates){

    ########################################################################
    # Create predictors

    predictors = matrix(runif(nPredictors*sampleSize, min = -1), ncol = nPredictors)

    if (cor != 0){
      predTemp <- runif(sampleSize, min = -1)
      predictors  = (1-cor) * predictors + cor * matrix(rep(predTemp, nPredictors), ncol = nPredictors)
    }

    colnames(predictors) = paste("Environment", 1:nPredictors, sep = "")

    ########################################################################
    # Create random effects

    group = rep(1:numGroups, each = sampleSize/numGroups)
    groupRandom = rnorm(numGroups, sd = sqrt(randomEffectVariance))

    ########################################################################
    # Creation of linear prediction

    linearResponse = intercept + predictors %*% fixedEffects + groupRandom[group]

    if(!is.null(quadraticFixedEffects)){
      linearResponse = linearResponse + predictors^2 %*% quadraticFixedEffects
    }

    ########################################################################
    # Overdispersion on linear predictor


    if(is.numeric(overdispersion)) linearResponse = linearResponse + rnorm(sampleSize, sd = overdispersion)
    if(is.function(overdispersion)) linearResponse = linearResponse + overdispersion(linearResponse)


    ########################################################################
    # Autocorrelation

    if(!(temporalAutocorrelation == 0)){
    #  t = 1:sampleSize             # INCLUDING CODE HERE
      distMat <- as.matrix(dist(time))

      invDistMat <- 1/distMat * 5000
      diag(invDistMat) <- 0
      invDistMat = sfsmisc::posdefify(invDistMat)

      temporalError <- MASS::mvrnorm(n = 1, mu = rep(0,sampleSize), Sigma = invDistMat)

      linearResponse = linearResponse + temporalAutocorrelation * temporalError #
    }


    if(!(spatialAutocorrelation == 0)) {
      distMat <- as.matrix(dist(cbind(x, y)))

      invDistMat <- 1/distMat * 5000
      diag(invDistMat) <- 0
      invDistMat = sfsmisc::posdefify(invDistMat)

      spatialError <- MASS::mvrnorm(n = 1, mu = rep(0,sampleSize), Sigma = invDistMat)

      linearResponse = linearResponse + spatialAutocorrelation * spatialError
    }


    ########################################################################
    # Link and distribution

    linkResponse = family$linkinv(linearResponse)

    if (family$family == "gaussian") observedResponse = rnorm(n = sampleSize, mean = linkResponse, sd = scale)
    # need checking else if (family$family == "gamma") observedResponse = rgamma(n = sampleSize, shape = linkResponse / scale, scale = scale)
    else if (family$family == "binomial"){
      observedResponse = rbinom(n = sampleSize, binomialTrials, prob = linkResponse)
      if (binomialTrials > 1) observedResponse = cbind(observedResponse1 = observedResponse, observedResponse0 = binomialTrials - observedResponse)
    }
    else if (family$family == "poisson") {
      if(is.null(roundPoissonVariance)) observedResponse = rpois(n = sampleSize, lambda = linkResponse)
      else observedResponse = round(rnorm(n = length(linkResponse), mean = linkResponse, sd = roundPoissonVariance))
    }
    else if (grepl("Negative Binomial",family$family)) {
      theta = as.numeric(gsub("[\\(\\)]", "", regmatches(family$family, gregexpr("\\(.*?\\)", family$family))[[1]]))
      observedResponse = MASS::rnegbin(linkResponse, theta = theta)
    }
    else stop("wrong link argument supplied")

    ########################################################################
    # Zero-inflation

    if(pZeroInflation != 0){
      artificialZeros = rbinom(n = length(observedResponse), size = 1, prob = 1-pZeroInflation)
      observedResponse = observedResponse * artificialZeros
    }


    if(factorResponse) observedResponse = factor(observedResponse)

    # add spatialError?

    out[[i]] <- data.frame(ID = 1:sampleSize, observedResponse, predictors, group = as.factor(group), time, x, y)
  }
  if(length(out) == 1) out = out[[1]]

  if(hasNA) out[1,3] = NA

  return(out)
}


############## TESTING FUNCTIONS IN TERMS OF TYPE I ERROR -----------


#### ORDERED createData ----------
Ntemp.Nre <- c()
for (i in 1:100){
  testData = createData(sampleSize = 100, family = gaussian(), 
                        fixedEffects = 1,  
                        randomEffectVariance = 0, 
                        temporalAutocorrelation = 0) 
  fittedModel <- lm(observedResponse ~ Environment1, data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  Ntemp.Nre[i] <- test$p.value
}

Ntemp.re <- c()
for (i in 1:100){
  testData = createData(sampleSize = 100, family = gaussian(), 
                        fixedEffects = 1,  
                        randomEffectVariance = 0.5, 
                        temporalAutocorrelation = 0) 
  fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group), data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  Ntemp.re[i] <- test$p.value
}

temp.Nre <- c()
for (i in 1:100){
  testData = createData(sampleSize = 100, family = gaussian(), 
                        fixedEffects = 1,  
                        randomEffectVariance = 0, 
                        temporalAutocorrelation = 20) 
  fittedModel <- lm(observedResponse ~ Environment1, data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  temp.Nre[i] <- test$p.value
}

temp.re <- c()
for (i in 1:100){
  testData = createData(sampleSize = 100, family = gaussian(), 
                        fixedEffects = 1,  
                        randomEffectVariance = 0.5, 
                        temporalAutocorrelation = 20) 
  fittedModel <- lme4::lmer(observedResponse ~ Environment1 +(1|group), data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  temp.re[i] <- test$p.value
}

result <- expand.grid(temporal = c("no", "yes"),
                   random.effect = c("no", "yes"))
result$prop.sig = c(sum(Ntemp.Nre<0.05) ,sum(temp.Nre<0.05),
                 sum(Ntemp.re<0.05), sum(temp.re<0.05) )
result


#### SUFFLED TIME createData2 --------

Ntemp.Nre <- c()
for (i in 1:100){
  testData = createData2(sampleSize = 100, family = gaussian(), 
                         fixedEffects = 1,  
                         randomEffectVariance = 0, 
                         temporalAutocorrelation = 0) 
  fittedModel <- lm(observedResponse ~ Environment1, data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  Ntemp.Nre[i] <- test$p.value
}

Ntemp.re <- c()
for (i in 1:100){
  testData = createData2(sampleSize = 100, family = gaussian(), 
                         fixedEffects = 1,  
                         randomEffectVariance = 0.5, 
                         temporalAutocorrelation = 0) 
  fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group), data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  Ntemp.re[i] <- test$p.value
}

temp.Nre <- c()
for (i in 1:100){
  testData = createData2(sampleSize = 100, family = gaussian(), 
                         fixedEffects = 1,  
                         randomEffectVariance = 0, 
                         temporalAutocorrelation = 20) 
  fittedModel <- lm(observedResponse ~ Environment1, data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  temp.Nre[i] <- test$p.value
}


temp.re <- c()
for (i in 1:100){
  testData = createData2(sampleSize = 100, family = gaussian(), 
                         fixedEffects = 1,  
                         randomEffectVariance = 0.5, 
                         temporalAutocorrelation = 20) 
  fittedModel <- lme4::lmer(observedResponse ~ Environment1 +(1|group), data = testData) 
  res = simulateResiduals(fittedModel)
  test <- testTemporalAutocorrelation(res, time =  testData$time)
  temp.re[i] <- test$p.value
}

result2 <- expand.grid(temporal = c("no", "yes"),
                    random.effect = c("no", "yes"))
result2$prop.sig = c(sum(Ntemp.Nre<0.05) ,sum(temp.Nre<0.05),
                  sum(Ntemp.re<0.05), sum(temp.re<0.05) )
result2





































