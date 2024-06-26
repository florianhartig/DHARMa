




library(DHARMa)
library(lme4)


getP <- function(X){
  dat <- createData(sampleSize = 500, overdispersion = 0)
  m0 = glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = dat)
  a = logLik(m0)
  m1 = glmer.nb(observedResponse ~ Environment1 + (1|group), data = dat)  
  b = logLik(m0)
  x = simulateLRT(m0,m1)
  out = c(a, b, x$p.value)
}

out <- replicate(2, getP())

out = lapply (cl, X = 1:100, getP)


library (parallel)
cl <- makeCluster (4)
res <- parLapply (cl, X = 1:100, getP)
stopCluster (cl)