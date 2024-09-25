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

  time = sample(1:sampleSize, sampleSize) # change here
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
      t = 1:sampleSize             # INCLUDING CODE HERE
      distMat <- as.matrix(dist(t))

      invDistMat <- 1/distMat * 5000
      diag(invDistMat) <- 0
      invDistMat = sfsmisc::posdefify(invDistMat)

      temporalError <- MASS::mvrnorm(n = 1, mu = rep(0,sampleSize), Sigma = invDistMat)

      linearResponse = linearResponse + temporalAutocorrelation * temporalError[time] #
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

##############
set.seed(123)
testData2 <- createData2(sampleSize = 200,
                       overdispersion = 0, randomEffectVariance = 0.5,
                       family = gaussian())
head(testData2)

fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group),
                          data = testData2)
summary(fittedModel)
simulationOutput <- simulateResiduals(fittedModel, n=200)
testTemporalAutocorrelation(simulationOutput = simulationOutput,
                            time = testData2$time)


set.seed(123)
testData2 <- createData2(sampleSize = 100, temporalAutocorrelation = 2,
                       overdispersion = 0, randomEffectVariance = 0.5,
                       family = gaussian())
head(testData2)

fittedModel <- lme4::lmer(observedResponse ~ Environment1 + (1|group),
                          data = testData2)
summary(fittedModel)
simulationOutput <- simulateResiduals(fittedModel, n=200)
testTemporalAutocorrelation(simulationOutput = simulationOutput,
                            time = testData2$time)



## copying test function:

testTemporalAutocorrelation2 <- function(simulationOutput, time, alternative = c("two.sided", "greater", "less"), plot = TRUE){
  
  simulationOutput = ensureDHARMa(simulationOutput, convert = T)
  
  # actually not sure if this is neccessary for dwtest, but seems better to aggregate
  if(any(duplicated(time))) stop("testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use the recalculateResiduals function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testTemporalAutocorrelation.")
  
  alternative <- match.arg(alternative)
  
  if(is.null(time)){
    time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
    message("DHARMa::testTemporalAutocorrelation - no time argument provided, using random times for each data point")
  }
  
  # To avoid Issue #190
  if (length(time) != length(residuals(simulationOutput))) stop("Dimensions of time don't match the dimension of the residuals")
  
  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time, alternative = alternative)
  
  if(plot == T) {
    oldpar <- par(mfrow = c(1,2))
    on.exit(par(oldpar))
    
    plot(simulationOutput$scaledResiduals[order(time)] ~ time[order(time)],
         type = "l", ylab = "Scaled residuals", xlab = "Time", main = "Residuals vs. time", ylim = c(0,1))
    
    abline(h=c(0.5))
    abline(h=c(0,0.25,0.75,1), lty = 2 )
    
    acf(simulationOutput$scaledResiduals[order(time)], main = "Autocorrelation", ylim = c(-1,1))
    legend("topright",
           c(paste(out$method, " p=", round(out$p.value, digits = 5)),
             paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(out$p.value < 0.05, "red", "black" ), bty="n")
    
  }
  
  return(out)
}












