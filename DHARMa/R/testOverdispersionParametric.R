#' Parametric overdipersion test
#' 
#' This function implements a parametric dispersion test based on comparing the residual devialce to the residual degrees of freedom that is commonly used, with the purpose of benchmarking against the nonparametric tests of DHARMA
#' 
#' @param model a fitted model object. See details for possible models
#' @details The general idea of such as test is to copy GLM wisdom that we can define a dispersion parameter as residual deviance / residual degrees of freedom. For a model with correct dispersion, this parameter
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
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
testOverdispersionParametric <- function(model){
  
  if(class(model)[1] %in% c("lm", "glm")){
    return("Parametric overdispersion test not implemented for this model type")
  }
  
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
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
  
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  
  out = list (statistic=c(dispersion=prat, pearSS = Pearson.chisq, rdf=rdf), method = "Chisq test for overdispersion in GLMMs", alternative = "true dispersion greater 1", data.name = model@call$family, p.value = pval)
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

