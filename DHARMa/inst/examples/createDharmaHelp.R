## READING IN HAND-CODED SIMULATIONS

testData = createData(sampleSize = 50, randomEffectVariance = 0)
fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = "poisson")

# in DHARMA, using the simulate.glm function of glm 
sims = simulateResiduals(fittedModel)
plot(sims, quantreg = FALSE)

# Doing the same with a handcode simulate function. 
# of course this code will only work with a 1-par glm model
simulateMyfit <- function(n=10, fittedModel){
  int = coef(fittedModel)[1]
  slo = coef(fittedModel)[2]
  pred = exp(int + slo * testData$Environment1)
  predSim = replicate(n, rpois(length(pred), pred))
  return(predSim)
}

sims = simulateMyfit(250, fittedModel)

dharmaRes <- createDHARMa(simulatedResponse = sims, observedResponse = testData$observedResponse, fittedPredictedResponse = predict(fittedModel, type = "response"), integer = T)
plot(dharmaRes, quantreg = FALSE)

## A BAYESIAN EXAMPLE

\dontrun{
  
  # This example shows how to check the residuals for a 
  # Bayesian fit of a process-based vegetation model, using
  # THe BayesianTools package
  
  library(BayesianTools)
  
  # Create input data for the model
  PAR <- VSEMcreatePAR(1:1000)
  plotTimeSeries(observed = PAR)
  
  # load reference parameter definition (upper, lower prior)
  refPars <- VSEMgetDefaults()
  # this adds one additional parameter for the likelihood standard deviation (see below)
  refPars[12,] <- c(2, 0.1, 4) 
  rownames(refPars)[12] <- "error-sd"
  
  # create some simulated test data 
  # generally recommended to start with simulated data before moving to real data
  referenceData <- VSEM(refPars$best[1:11], PAR) # model predictions with reference parameters  
  referenceData[,1] = 1000 * referenceData[,1] 
  # this adds the error - needs to conform to the error definition in the likelihood
  obs <- referenceData + rnorm(length(referenceData), sd = refPars$best[12])
  
  parSel = c(1:6, 12) # parameters to calibrate
  
  # here is the likelihood 
  likelihood <- function(par, sum = TRUE){
    # set parameters that are not calibrated on default values 
    x = refPars$best
    x[parSel] = par
    predicted <- VSEM(x[1:11], PAR) # replace here VSEM with your model 
    predicted[,1] = 1000 * predicted[,1] # this is just rescaling
    diff <- c(predicted[,1:4] - obs[,1:4]) # difference betweeno observed and predicted
    # univariate normal likelihood. Note that there is a parameter involved here that is fit
    llValues <- dnorm(diff, sd = x[12], log = TRUE)  
    if (sum == FALSE) return(llValues)
    else return(sum(llValues))
  }
  
  # optional, you can also directly provide lower, upper in the createBayesianSetup, see help
  prior <- createUniformPrior(lower = refPars$lower[parSel], 
                              upper = refPars$upper[parSel], best = refPars$best[parSel])
  
  bayesianSetup <- createBayesianSetup(likelihood, prior, names = rownames(refPars)[parSel])
  
  # settings for the sampler, iterations should be increased for real applicatoin
  settings <- list(iterations = 10000, nrChains = 2)
  
  out <- runMCMC(bayesianSetup = bayesianSetup, sampler = "DEzs", settings = settings)
  
  plot(out)
  summary(out)
  gelmanDiagnostics(out) # should be below 1.05 for all parameters to demonstrate convergence 
  
  # Posterior predictive simulations
  
  # Create a function to create posterior predictive simulations
  createPredictions <- function(par){
    # set the parameters that are not calibrated on default values 
    x = refPars$best
    x[parSel] = par
    predicted <- VSEM(x[1:11], PAR) * 1000 
    out = rnorm(length(predicted), mean = predicted, sd = par[7])
    return(out)
  }
  
  posteriorSample = getSample(out, numSamples = 1000)
  posteriorPredictiveSims = apply(posteriorSample, 1, createPredictions)
    
  dim(posteriorPredictiveSims)
  library(DHARMa)
  x = createDHARMa(t(posteriorPredictiveSims))
  plot(x)
}
