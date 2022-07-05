#' Simulated Likelihood Ratio tests
#' 
#' This function uses the DHARMa model wrappers to generate simulated likelihood ratio tests (LRTs) based on a parameteric bootstrap. 
#' 
#' The null hypothesis is that m0 is correct, the tests checks if the increase in likelihood of m1 is higher than expected, using data simulated from m0
#' 
#' @param m0 Null Model
#' @param m1 Alternative Model
#' @param n number of simulations
#' @param seed random seed
#' @param ... additional parameters to pass on to the simulate function of the model object.
#' 
#' @details The function performs a simulated LRT, which works as follows:
#' 
#' 1. H0: Model 1 is correct
#' 2. Our test statistic is the log LRT of M1/M2. Empirical value will always be > 1 because in a nested setting, the more complex model cannot have a worse likelihood. 
#' 3. To generate an expected distribution of the test statistic under H0, we simulate new response data under M0, refit M0 and M1 on this data, and calculate the LRs. 
#' 4. Based on this, calculate p-values etc. in the usual way. 
#' 
#' The motivation for using a simulated LRT rather than a standard ANOVA or AIC for model selection in mixed models is that df for mixed models are not clearly defined, thus standard ANOVA based on Chi2 statistics or AIC are unrealiable, in particular for models with large contributions of REs to the likelihood.  
#' 
#' @note Note that the logic of an LRT assumes that m0 is nested in m1, which guarantees that the L(M1) > L(M0). The function does not explicitly check if models are nested and will work as long as data can be simulated from M0 that can be refit with M) and M1; however, I would strongly advice against using this for non-nested models unless you have a good statsitical reason for doing so. 
#' 
#' @author Florian Hartig
#' 
#' @example inst/examples/simulateLRTHelp.R
#' @export
simulateLRT<-function(m0, m1, n = 250, seed = 123, plot = T, ...){
  n = 250
  
  ######## general assertions and startup calculations ##########
  # identical to simulateResiduals
  
  if (n < 2) stop("error in DHARMa::simulateLRT: n > 1 is required to simulate LRT")
  checkModel(m0)
  checkModel(m1)
  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  ptm <- proc.time()
  
  ####### extract model info ############
  
  out = list()
  
  out$data.name = paste("m0:", deparse(substitute(m0)), "m1:", deparse(substitute(m1)))

  out$m0  = m0
  out$m1  = m1
  
  out$observedLRT = logLik(m1) - logLik(m0)
  
  if(nobs(m0) != nobs(m1)) stop("m0 and m1 seem to have an unequal number of observations. Check for and possibly remove NAs in the data")
  
  out$nObs = nobs(m0)
  out$nSim = n

  out$simulatedResponse = getSimulations(m0, nsim = n, type = "refit", ...)
  
  out$simulatedLR = rep(NA, n)
  
  for (i in 1:n){
    
    simObserved = out$simulatedResponse[[i]]
    
    try({
      
      # for testing
      # if (i==3) stop("x")
      # Note: also set silent = T for production
      
      refittedM0 = getRefit(m0, simObserved)
      refittedM1 = getRefit(m1, simObserved)
      
      out$simulatedLR[i] = logLik(refittedM1) - logLik(refittedM0)

    }, silent = TRUE)
  }
  
  out$statistic = out$observedLRT
  names(out$statistic) = "Log LRT Obs"
  out$method = "DHARMa simulated LRT"
  out$alternative = "greater"
  out$p.value =   getP(out$simulatedLR, out$observedLRT, alternative = "greater", plot = plot)
  
  class(out) = "htest"
  
  return(out)
  
}

