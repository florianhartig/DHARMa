#' Simulate test data
#' @description This function creates synthetic dataset with various problems such as overdispersion, zero-inflation, etc.
#' @param sampleSize sample size of the dataset
#' @param intercept intercept (linear scale)
#' @param fixedEffects vector of fixed effects (linear scale)
#' @param quadraticFixedEffects vector of quadratic fixed effects (linear scale)
#' @param numGroups number of groups for the random effect
#' @param randomEffectVariance variance of the random effect (intercept)
#' @param overdispersion if this is a numeric value, it will be used as the sd of a random normal variate that is added to the linear predictor. Alternatively, a random function can be provided that takes as input the linear predictor.
#' @param family family
#' @param scale scale if the distribution has a scale (e.g. sd for the Gaussian)
#' @param cor correlation between predictors
#' @param roundPoissonVariance if set, this creates a uniform noise on the possion response. The aim of this is to create heteroscedasticity
#' @param pZeroInflation probability to set any data point to zero
#' @param binomialTrials Number of trials for the binomial. Only active if family == binomial
#' @param temporalAutocorrelation strength of temporalAutocorrelation
#' @param spatialAutocorrelation strength of spatial Autocorrelation
#' @param factorResponse should the response be transformed to a factor (inteded to be used for 0/1 data)
#' @param replicates number of datasets to create
#' @param hasNa should an NA be added to the environmental predictor (for test purposes)
#' @export
#' @example /inst/examples/createDataHelp.R
createData <- function(sampleSize = 100, intercept = 0, fixedEffects = 1, quadraticFixedEffects = NULL, numGroups = 10, randomEffectVariance = 1, overdispersion = 0, family = poisson(), scale = 1, cor = 0, roundPoissonVariance = NULL,  pZeroInflation = 0, binomialTrials = 1, temporalAutocorrelation = 0, spatialAutocorrelation =0, factorResponse = F, replicates=1, hasNA = F){

  nPredictors = length(fixedEffects)

  out = list()

  time = 1:sampleSize
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
      distMat <- as.matrix(dist(time))

      invDistMat <- 1/distMat * 5000
      diag(invDistMat) <- 0
      invDistMat = sfsmisc::posdefify(invDistMat)

      temporalError <- MASS::mvrnorm(n=1, mu=rep(0,sampleSize), Sigma=invDistMat)

      linearResponse = linearResponse + temporalAutocorrelation * temporalError
    }


    if(!(spatialAutocorrelation == 0)) {
      distMat <- as.matrix(dist(cbind(x, y)))

      invDistMat <- 1/distMat * 5000
      diag(invDistMat) <- 0
      invDistMat = sfsmisc::posdefify(invDistMat)

      spatialError <- MASS::mvrnorm(n=1, mu=rep(0,sampleSize), Sigma=invDistMat)

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
#createData()
