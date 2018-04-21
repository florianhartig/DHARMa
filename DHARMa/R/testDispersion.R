#' DHARMa dispersion tests
#' 
#' This function performs a simulation-based test for over/underdispersion 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param plot whether to plot output
#' @param alternative whether to test for "overdispersion", "underdispersion", or "both" (both reduces power)
#' @details The function implements two tests, depending on whether it is applied on a simulation with refit = F, or refit = T. 
#' 
#' If refit = F (not recommended), the function tests if the IQR of the scaled residuals deviate from the null hypothesis of a uniform distribution. Simulations show that this option is not properly calibrated and much less powerful than the parametric alternative \code{\link{testOverdispersionParametric}} and even the simple \code{\link{testUniformity}}, and therefore it's use is not recommended. A warning will be returned if the function is called. 
#' 
#' If refit = T, the function compares the approximate deviance (via squared pearson residuals) with the same quantity from the models refitted with simulated data. It is much slower than the parametric alternative \code{\link{testOverdispersionParametric}}, but simulations show that it is slightly more powerful than the latter, and more powerful than any other non-parametric test in DHARMa, and it doesn't make any parametric assumptions. However, given the computational cost, I would suggest that most users will be satisfied with the parametric overdispersion test. 
#' 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersionParametric}}
#' @export
testDispersion <- function(simulationOutput, method = c("residual", "var"), alternative = c("greater", "two.sided", "less"), plot = F, ...){
  
  out = list()
  
  if(simulationOutput$refit == F){
    warning("You have called the non-parametric test for overdispersion based on the scaled residuals. Simulations show that this test is less powerful for detecting overdispersion than the default uniform test on the scaled residuals, and a lot less powerful than a parametric overdispersion test, or the non-parametric test on re-simulated residuals. The test you called is only implemented for testing / development purposes, there is no scenario where it would be preferred. See vignette for details.")
    observed = IQR(simulationOutput$scaledResiduals)
    sims = matrix(runif(simulationOutput$nObs * 1000), nrow = 1000)
    ss = apply(sims, 1, IQR)
    out$statistic = c(dispersion = observed / mean(ss))
    out$method = "DHARMa nonparametric overdispersion test via IQR of scaled residuals against IQR expected under uniform"
  } else {
    if(simulationOutput$modelClass == "glmmTMB"){
      message("testOverdispersion with refit = T not implemented for glmmTMB, abort")
      return(NULL)
    }
    observed = sum(residuals(simulationOutput$fittedModel, type = "pearson")^2)
    ss = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
    out$statistic = c(dispersion = observed / mean(ss))
    out$method = "DHARMa nonparametric overdispersion test via comparison to simulation under H0 = fitted model"
  }
  
  #stop("Overdispersion test requires simulated residuals with refit = T") 
  
  p = ecdf(ss)(observed)
  
  if(alternative == "overdispersion") p = 1-p
  if(alternative == "underdispersion") p = p  
  if(alternative == "both") p = min(p, 1-p) * 2     
  
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  
  
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  
  if(method == "residual"){
    if(! class(model)[1] %in% c("glmerMod")){
      return("DHARMa::testOverdispersionParametric currently only works for GLMMs in lme4. For Poisson GLMs, you can use AER::dispersiontest, otherwise use the non-parametric tests of DHARMa to test dispersion.")
    }
    
    ## number of variance parameters in
    ##   an n-by-n variance-covariance matrix
    vpars <- function(m) {
      nrow(m)*(nrow(m)+1)/2
    }
    
    vcov(rd)
    
    model.df <- sum(sapply(lme4::VarCorr(model),vpars))+length(lme4::fixef(model))
    rdf <- nrow(model.frame(model))-model.df
    
    # Residual df
    
    rp <- residuals(model,type="pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq/rdf
    
    # Harrison 2014 seems to do more or less the same 
    
    # Note: blmeco::dispersion_glmer defines the dispersion point estimate different
    
    # computing  estimated scale  ( binomial model) following  D. Bates :
    # That quantity is the square root of the penalized residual sum of
    # squares divided by n, the number of observations, evaluated as:
    # https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015392.html
    
    #n <- length(resid(modelglmer))
    #return(  sqrt( sum(c(resid(modelglmer),modelglmer@u) ^2) / n ) ) 
    #should be between, 0.75 and 1.4 if not under- or overdispersed, respectively
    
    # Hypothesis test 
    
    pval = switch(alternative, 
                  greater = pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE), 
                  two.sided = min(pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE),pchisq(Pearson.chisq, df=rdf, lower.tail=TRUE))* 2, 
                  less = pchisq(Pearson.chisq, df=rdf, lower.tail=TRUE)
    )
    
    out = list(
      statistic=c(dispersion=prat, pearSS = Pearson.chisq, rdf=rdf), 
      method = "Chisq test for overdispersion in GLMMs", 
      data.name = as.character(model@call$family), 
      p.value = pval)
  }
  
  if(method == "var"){
    out = aerDispersion(object = model, alternative = alternative, ...)
    out$method = "Cameron & Trivedi (1990)"
  }
  
  out$alternative = switch(alternative, 
                           greater = "Dispersion > 1 (Overdispersion)", 
                           two.sided = "Dispersion != 1 (Over / underdispersion)", 
                           less = "Dispersion < 1 (Underdispersion)")
  
  class(out) = "htest"
  
  
  if(plot == T) {
    hist(ss, xlim = range(ss, observed, na.rm = T))
    abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}



testOverdispersion <- function(simulationOutput, plot = F){
  message("plotSimulatedResiduals is deprecated, switch your code to using the testDispersion function")
  testDispersion(simulationOutput)
}

testOverdispersionParametric <- function(model, method = c("residual", "var"), alternative = c("greater", "two.sided", "less"), ...){
  message("plotSimulatedResiduals is deprecated, switch your code to using the testDispersion function")
  testDispersion(simulationOutput)
}


#rd <- glm(trips ~ ., data = RecreationDemand, family = poisson)

#testOverdispersionParametric(rd,method = "var")
#testOverdispersionParametric(rd,method = "var")



#' Tests for zero-inflation 
#' 
#' This function compares the observed number of zeros with the zeros expected from simulations. 
#' 
#' @param simulationOutput a DHARMa object with simulated residuals created with \code{\link{simulateResiduals}}
#' @param plot whether to plot output
#' @param alternative whether to test for 'more', 'less', or 'both' more or less zeros in the observed data
#' @details shows the expected distribution of zeros against the observed
#' @seealso \code{\link{testUniformity}}, \code{\link{testSimulatedResiduals}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOverdispersion}}, \code{\link{testOverdispersionParametric}}
#' @export
testZeroInflation <- function(simulationOutput,  plot = T, alternative = "more"){
  
  countZeros <- function(x) sum( x == 0)
  
  zerosObserved = countZeros(simulationOutput$observedResponse)
  
  zerosExpected = apply(simulationOutput$simulatedResponse, 2, countZeros)
  
  
  p = ecdf(zerosExpected)(zerosObserved)
  
  if(alternative == "more") p = 1-p
  if(alternative == "less") p = p  
  if(alternative == "both") p = min(p, 1-p) * 2     
  
  out = list()
  out$statistic = c(ratioObsExp = zerosObserved / mean(zerosExpected))
  out$method = "DHARMa zero-inflation test via comparison to expected zeros with simulation under H0 = fitted model"
  out$alternative = alternative
  out$p.value = p
  out$data.name = deparse(substitute(simulationOutput))
  
  class(out) = "htest"
  
  if(plot == T) {
    hist(zerosExpected, xlim = range(zerosExpected, zerosObserved, na.rm=T ))
    abline(v = zerosObserved, lwd= 2, col = "red")
  }
  return(out)
}


## The following functions are only for validation purpose 

#Function to calculate a point estimate of overdispersion from a mixed model object following Harrison 2014
od.point<-function(modelobject){
  x<-sum(resid(modelobject,type="pearson")^2)
  rdf<-summary(modelobject)$AICtab[5]
  return(x/rdf)
}

#Function to calculate point estimate of overdispersion from blmeco::dispersion_glmer
od.point2<-function(modelglmer){
  n <- length(resid(modelglmer))
  return(sqrt(sum(c(resid(modelglmer), modelglmer@u)^2)/n))
}

# this is the AER dispersion function
aerDispersion <- function (object, trafo = NULL, alternative = c("greater", "two.sided", "less")) 
{
  if (!inherits(object, "glm") || family(object)$family != 
      "poisson") 
    stop("only Poisson GLMs can be tested with method = var")
  alternative <- match.arg(alternative)
  otrafo <- trafo
  if (is.numeric(otrafo)) 
    trafo <- function(x) x^otrafo
  y <- if (is.null(object$y)) 
    model.response(model.frame(object))
  else object$y
  yhat <- fitted(object)
  aux <- ((y - yhat)^2 - y)/yhat
  if (is.null(trafo)) {
    STAT <- sqrt(length(aux)) * mean(aux)/sd(aux)
    NVAL <- c(dispersion = 1)
    EST <- c(dispersion = mean(aux) + 1)
  }
  else {
    auxreg <- lm(aux ~ 0 + I(trafo(yhat)/yhat))
    STAT <- as.vector(summary(auxreg)$coef[1, 3])
    NVAL <- c(alpha = 0)
    EST <- c(alpha = as.vector(coef(auxreg)[1]))
  }
  rval <- list(statistic = c(z = STAT), 
               p.value = switch(alternative, 
                                greater = pnorm(STAT, lower.tail = FALSE), 
                                two.sided = pnorm(abs(STAT), lower.tail = FALSE) * 2, 
                                less = pnorm(STAT)
               ), 
               estimate = EST, 
               null.value = NVAL, 
               data.name = deparse(substitute(object)))
  return(rval)
}  


