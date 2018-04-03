#' Parametric overdispersion test
#' 
#' This function implements a parametric dispersion test based on comparing the residual deviance to the residual degrees of freedom that is commonly used, with the purpose of benchmarking against the nonparametric tests of DHARMA
#' 
#' @param model a fitted model object. See details for possible models
#' @param method which test to use. For alternatives, 
#' @param alternative which direction to test. Options are "greater", "two.sided", "less"
#' @param ... further options for the particular methods
#' @details 
#' 
#' **Method residual** The residual method follows the general GLM wisdom that we can define a dispersion parameter as residual deviance / residual degrees of freedom. For a model with correct dispersion, this parameter
#' 
#' 1. Should be on average 1
#' 2. Be chi2 distributed with df = rdf
#' 
#' For GL(M)Ms, we have to answer three questions
#' 
#' 1. What is the residual deviance
#' 2. What are the rdf
#' 3. Is the distribution still chisq
#' 
#' There are quite a few implementations of this idea, e.g. https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015392.html (implemented in blmeco::dispersion_glmer), http://glmm.wikidot.com/faq, and the code from Harrison, X. A. Using observation-level random effects to model overdispersion in count data in ecology and evolution PeerJ, 2014, 2, e616 
#'  
#' The implementation here follows the suggestion in http://glmm.wikidot.com/faq, which is based on dividing the pearson residuals by the (probably not completely accurate) rdf, and testing this against a chi2 distribution with df = rdf. 
#' 
#' **Method var** corresponds to the overdispersion test by Cameron & Trivedi (1990), implemented in the R package AER. The version here only slightly modifieds the AER function. The idea is that, in a Poisson model, the variance should equal the mean, i.e. we have a null hypothesis H0: E(Y) = Var(Y) = μ. The tests compares this against the alternative assumption Var(Y)=μ+c∗f(μ) where the constant c<0 means underdispersion and c>0 means overdispersion. The coefficient c is estimated with a regression and tested with the corresponding t (or z) statistic. Common specifications of the transformation function trafo are f(μ) = mu^2 or f(μ) = mu. The former corresponds to a negative binomial (NB) model with quadratic variance function (called NB2 by Cameron and Trivedi, 2005), the latter to a NB model with linear variance function (called NB1 by Cameron and Trivedi, 2005) or quasi-Poisson model with dispersion parameter. By default, for trafo = NULL, the latter dispersion formulation is used. Otherwise, trafo can either be specified as a function or an integer corresponding to the function function(x) x^trafo, such that trafo = 1 and trafo = 2 yield the linear and quadratic formulations respectively.
#' 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testOverdispersionParametric <- function(model, method = c("residual", "var"), alternative = c("greater", "two.sided", "less"), ...){
  
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
  

#rd <- glm(trips ~ ., data = RecreationDemand, family = poisson)

#testOverdispersionParametric(rd,method = "var")
#testOverdispersionParametric(rd,method = "var")




