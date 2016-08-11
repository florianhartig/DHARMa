#' A wrapper for a number of (mostly likelihood-based) parametric overdispersion tests
#' @param model a fitted model object. See details for possible models
#' @param type the test type. See details. 
#' @param ... parameters to pass on to the respective dispersion tests
#' @details This function is a wrapper for a number of parametric overdispersion tests. I am not actively developing these tests for the DHARMa package. The main purpose of providing these functions here is benchmarking agains the simulation-based tests in DHARMa. 
#' 
#' Type 1 is the overdisp_fun from http://glmm.wikidot.com/faq
#' 
#' Type 2 is the function dispersion_glmer in the blmeco package. From the help there: "This function has been posted on the R-helplist. It seems to have been written or motivated by D. Bates. Here is the URL, where we downloaded the function: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015392.html"
#' 
#' Type 3 is from Harrison et al. 2014 (Using observation-level random effects to model overdispersion in count data in ecology and evolution), but also includes a function to test for overdispersion with Boostraping. 
#' 
#' @seealso \code{\link{testSimulatedResiduals}}, \code{\link{testSimulatedResiduals}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}
#' @export
parametricDispersionTest <- function(model, type = 1, ...){
  
  if(type == 1){
    out = overdisp_fun(model, ...)
  }
  if(type == 2){
    out = blmeco::dispersion_glmer(model, ...)
  }
  if(type == 3){
    1
  }
  return(out)
}

#' A wrapper for a number of deviance-based parametric overdispersion tests
#' @param model a fitted model
#' @param type the test type. See details. 
#' @param ... parameters to pass on to the dispersion tests
#' @details This function is a wrapper for a number of parametric overdispersion tests. I am not actively developing these tests for the DHARMa package. The main purpose of providing these functions here is benchmarking agains the simulation-based tests in DHARMa. That being said, feel 
#' 
#' Type 1 is from Harrison et al. 2014 (Using observation-level random effects to model overdispersion in count data in ecology and evolution), but also includes a function to test for overdispersion with Boostraping. 
#' 
#' Type 2 is the function dispersion_glmer in the blmeco package. From the help there: "This function has been posted on the R-helplist. It seems to have been written or motivated by D. Bates. Here is the URL, where we downloaded the function: https://stat.ethz.ch/pipermail/r-sig-mixed-models/2011q1/015392.html"
#' @export
overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


#Function to calculate a point estimate of overdispersion from a mixed model object
od.point<-function(modelobject){
  x<-sum(resid(modelobject,type="pearson")^2)
  rdf<-summary(modelobject)$AICtab[5]
  return(x/rdf)
}

#Function to pass to parametric bootstrap function 'bootMer' that calculates the sum of squared Pearson residuals (required for 'od' function)
FUN <- function(fit) {
  #return(fixef(fit))
  x<-resid(fit,type="pearson")
  return(sum(x^2))
}   

#Function To Calculate Ratio of Model SS to Mean Parametric Bootstrap SS ('bias')
od<-function(bootobject){
  biasvals<-bootobject $t0/bootobject[2]$t
  bias<-mean(biasvals,na.rm=T)
  intervals<-quantile(biasvals,c(0.025,0.975),na.rm=T)
  dat<-c(bias,intervals)
  return(dat)
}

#Parametric bootstrap of the model - requires 'FUN' from above
#library(boot) #required to inspect results

#m1boot<-bootMer(m1,FUN,100)
#m1boot
#Calculate Dispersion Parameter - uses "OD" function above
#od(m1boot)