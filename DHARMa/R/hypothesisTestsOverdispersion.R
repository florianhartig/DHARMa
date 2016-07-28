
#' @export
overdispersionTest <- function(model, type = 1){
  
  if(type == 1){
    out = overdisp_fun(model)
  }
  if(type == 2){
    out = dispersion_glmer(model)
  }
  if(type == 3){
    1
  }
}

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

#



# Harrison et al. 2014 (Using observation-level random effects to model overdispersion in count data in ecology and evolution), but also include the function to test for overdispersion with Boostraping. 


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