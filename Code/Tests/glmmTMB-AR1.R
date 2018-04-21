testData = createData(sampleSize = 200, family = binomial(), binomialTrials = 20)

fittedModel <- glmer(cbind(observedResponse1,observedResponse0) ~ Environment1 + (1|group) , family = "binomial", data = testData)

temp = model.frame(fittedModel)
temp2 = temp[[1]]
temp[[1]] = NULL
temp = cbind(temp2, temp)

update(fittedModel, data = temp)


library(nlme)
library(lme4)
library(lme4ord)
library(glmmTMB)
library(brms)
library(INLA)


#https://github.com/glmmTMB/glmmTMB/issues/303


#https://bbolker.github.io/mixedmodels-misc/notes/corr_braindump.html

simCor1 <- function(phi=0.8,sdgrp=2,sdres=1,
                    npergrp=20,ngrp=20,
                    seed=NULL,
                    ## set linkinv/simfun for GLMM sims
                    linkinv=identity,
                    simfun=identity) {
  if (!is.null(seed)) set.seed(seed)
  cmat <- sdres*phi^abs(outer(0:(npergrp-1),0:(npergrp-1),"-"))
  errs <- MASS::mvrnorm(ngrp,mu=rep(0,npergrp),Sigma=cmat)
  ranef <- rnorm(ngrp,mean=0,sd=sdgrp)
  d <- data.frame(f=rep(1:ngrp,each=npergrp))
  eta <- ranef[as.numeric(d$f)] + c(t(errs)) ## unpack errors by row
  mu <- linkinv(eta)
  d$y <- simfun(mu)
  d$tt <- factor(rep(1:npergrp,ngrp))
  return(d)
}

d <- simCor1(phi=0.8,sdgrp=2,sdres=1,seed=101)
d$id = 1:400

glmmTMB_simple_fit <- glmmTMB(y~1 + (1|f) + ar1(tt-1|f), data=d,family=gaussian)

summary(glmmTMB_simple_fit)






