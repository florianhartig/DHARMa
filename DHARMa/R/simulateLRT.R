#' Simulated likelihood ratio tests for (generalized) linear mixed models
#'
#' @description This function uses the DHARMa model wrappers to generate simulated likelihood ratio tests (LRTs) for (generalized) linear mixed models based on a parametric bootstrap. The motivation for using a simulated LRT rather than a standard ANOVA or AIC for model selection in mixed models is that df for mixed models are not clearly defined, thus standard ANOVA based on Chi2 statistics or AIC are unreliable, in particular for models with large contributions of REs to the likelihood.
#'
#' Interpretation of the results as in a normal LRT: the null hypothesis is that m0 is correct, the tests checks if the increase in likelihood of m1 is higher than expected, using data simulated from m0.
#'
#' @param m0 null Model.
#' @param m1 alternative Model.
#' @param n number of simulations.
#' @param seed random seed.
#' @param plot whether null distribution should be plotted.
#' @param suppressWarnings whether to suppress warnings that occur during refitting the models to simulated data. See details for explanations.
#' @param saveModels Whether to save refitted models.
#' @param ... additional parameters to pass on to the simulate function of the model object. See [getSimulations] for details.
#'
#' @details The function performs a simulated LRT, which works as follows:
#'
#' 1. H0: Model 1 is correct.
#' 2. Our test statistic is the log LRT of M1/M2. Empirical value will always be > 1 because in a nested setting, the more complex model cannot have a worse likelihood.
#' 3. To generate an expected distribution of the test statistic under H0, we simulate new response data under M0, refit M0 and M1 on this data, and calculate the LRs.
#' 4. Based on this, calculate p-values etc. in the usual way.
#'
#' About warnings: warnings such as "boundary (singular) fit: see ?isSingular" will likely occur in this function and are not necessarily the sign of a problem. lme4 warns if RE variances are fit to zero. This is desired / likely in this case, however, because we are simulating data with zero RE variances. Therefore, warnings are turned off per default. For diagnostic reasons, you can turn warnings on, and possibly also inspect fitted models via the parameter saveModels to see if there are any other problems in the re-fitted models.
#'
#' Data simulations are performed by [getSimulations], which is a wrapper for the respective model functions. The default for all packages, wherever possible, is to generate marginal simulations (meaning that REs are re-simulated as well). I see no sensible reason to change this, but if you want to and if supported by the respective regression package, you could do so by supplying the necessary arguments via ...
#'
#' @note The logic of an LRT assumes that m0 is nested in m1, which guarantees that the L(M1) > L(M0). The function does not explicitly check if models are nested and will work as long as data can be simulated from M0 that can be refit with M) and M1; however, I would strongly advice against using this for non-nested models unless you have a good statistical reason for doing so.
#'
#' Also, note that LRTs may be unreliable when fit with REML or some other kind of penalized / restricted ML. Therefore, you should fit model with ML for use in this function.
#'
#' @author Florian Hartig
#'
#' @example inst/examples/simulateLRTHelp.R
#' @export
simulateLRT<-function(m0, m1, n = 250, seed = 123, plot = TRUE, suppressWarnings = TRUE, saveModels = FALSE, ...){

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

  if(nobs(m0) != nobs(m1)) stop("m0 and m1 seem to have an unequal number of observations. Check for and possibly remove NAs in the data.")

  out$nObs = nobs(m0)
  out$nSim = n

  out$simulatedResponse = getSimulations(m0, nsim = n, type = "refit", ...)

  out$simulatedLR = rep(NA, n)

  if(saveModels == TRUE) out$saveModels == list()

  for (i in 1:n){

    simObserved = out$simulatedResponse[[i]]

    try({

      # for testing
      # if (i==3) stop("x")
      # Note: also set silent = T for production

      if(suppressWarnings == TRUE){
        invisible(capture.output(suppressWarnings(suppressMessages({
          refittedM0 = getRefit(m0, simObserved)
          refittedM1 = getRefit(m1, simObserved)
        }))))
      }
      else {
        refittedM0 = getRefit(m0, simObserved)
        refittedM1 = getRefit(m1, simObserved)
      }

      if(saveModels == TRUE) out$saveModels[[i]] = list(refittedM0 = refittedM0, refittedM1 = refittedM1)

      out$simulatedLR[i] = logLik(refittedM1) - logLik(refittedM0)

    }, silent = TRUE)
  }

  out$statistic = out$observedLRT
  names(out$statistic) = "LogL(M1/M0)"
  out$method = "DHARMa simulated LRT"
  out$alternative = "M1 describes the data better than M0"
  out$p.value =   getP(out$simulatedLR, out$observedLRT, alternative = "greater", plot = plot, xlab="LogL(M1/M0)")

  class(out) = "htest"

  return(out)

}

