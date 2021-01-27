library(DHARMa)

## No temporal autocorrelation problem ##

par(mfrow = c(3,2))
runs = 50
pValues = data.frame(normal = rep(NA, runs), refit = rep(NA, runs))

for(j in 1:3){

dat = createData(replicates = 1, sampleSize = 100, intercept = 0,
                 fixedEffects = 1, randomEffectVariance = 0, family = poisson(), temporalAutocorrelation = 0)

for(i in 1:runs){
  fit = glm(observedResponse ~ Environment1 , data = dat, family = poisson)
  res = simulateResiduals(fit, n = 100)
  p1 = testTemporalAutocorrelation(res, time = dat$time,  plot = F)
  res2 = simulateResiduals(fit, n = 100, refit = T)
  p2 = testTemporalAutocorrelation(res2, time = dat$time, plot = F)
  pValues[i,] = c(p1$p.value, p2$p.value)
}
hist(pValues$normal, xlim = c(0,1), breaks = 20, freq = F, main = "p distribution refit = F")
abline(h=1, col = "red")
hist(pValues$refit, xlim = c(0,1), breaks = 20, freq = F, main = "p distribution refit = T")
abline(h=1, col = "red")
}



## Temporal autocorrelation problem ##



par(mfrow = c(3,2))
runs = 50
pValues = data.frame(normal = rep(NA, runs), refit = rep(NA, runs))

for(j in 1:3){
  
  dat = createData(replicates = 1, sampleSize = 100, intercept = 0,
                   fixedEffects = 1, randomEffectVariance = 0, family = poisson(), temporalAutocorrelation = 10)
  
  for(i in 1:runs){
    fit = glm(observedResponse ~ Environment1 , data = dat, family = poisson)
    res = simulateResiduals(fit, n = 100)
    p1 = testTemporalAutocorrelation(res, time = dat$time,  plot = F)
    res2 = simulateResiduals(fit, n = 100, refit = T)
    p2 = testTemporalAutocorrelation(res2, time = dat$time, plot = F)
    pValues[i,] = c(p1$p.value, p2$p.value)
  }
  hist(pValues$normal, xlim = c(0,1), breaks = 20, freq = F, main = "p distribution refit = F")
  abline(h=1, col = "red")
  hist(pValues$refit, xlim = c(0,1), breaks = 20, freq = F, main = "p distribution refit = T")
  abline(h=1, col = "red")
}







runs = 50
pValues = data.frame(normal = rep(NA, runs), refit = rep(NA, runs))

for(i in 1:runs){
  dat = createData(replicates = 1, sampleSize = 100, intercept = 0,
                   fixedEffects = 1, randomEffectVariance = 0, family = poisson(), temporalAutocorrelation = 5)
  fit = glm(observedResponse ~ Environment1 , data = dat, family = poisson)
  res = simulateResiduals(fit)
  p1 = testTemporalAutocorrelation(res, time = dat$time, plot = F)
  res2 = simulateResiduals(fit, refit = T)
  p2 = testTemporalAutocorrelation(res2, time = dat$time, plot = F)
  pValues[i,] = c(p1$p.value, p2$p.value)
}

par(mfrow = c(1,2))
hist(pValues$normal, xlim = c(0,1), breaks = 20, freq = F, main = "p distribution refit = F")
abline(h=1, col = "red")
hist(pValues$refit, xlim = c(0,1), breaks = 20, freq = F, main = "p distribution refit = T")
abline(h=1, col = "red")