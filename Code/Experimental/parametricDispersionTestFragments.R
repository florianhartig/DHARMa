
############# DAS HIER WAR IN DER FUNKTION #####################

if(method == "parametric"){
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



##################### ENDE ###########################




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
