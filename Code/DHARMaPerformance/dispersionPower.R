library(DHARMa)

returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 200, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)
plot(out, xlab = "Added dispersion sd", ylab = "Prop significant", main = "n = 200")






returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 40, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out2 = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)


returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 5000, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out3 = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)


returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 10, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out4 = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)


par(mfrow = c(2,2))

plot(out4, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "n = 10", 
     cex.lab = 1.2)
plot(out2, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "n = 40", 
     cex.lab = 1.2)
plot(out, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "n = 200", cex.lab = 1.2)
plot(out3, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "n = 5000", cex.lab = 1.2)



# Comment intercept -------------------------------------------------------


returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 20, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0, intercept = -2)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out1 = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)


returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 20, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0, intercept = 0)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out2 = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)


returnStatistics <- function(control = 1){
  testData = createData(sampleSize = 20, family = poisson(), overdispersion = control, fixedEffects = 0,
                        randomEffectVariance = 0, intercept = 2)
  fittedModel <- glm(observedResponse ~ Environment1, data = testData, family = poisson())
  x = summary(fittedModel)
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out <- c("Type I error GLM slope" = x$coefficients[2,4], "DHARMa testDispersion" = testDispersion(res, plot = FALSE)$p.value)
  return(out)
}

out3 = runBenchmarks(returnStatistics, controlValues = seq(0, 1.5, 0.05), nRep = 500)


par(mfrow = c(1,3))

plot(out4, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "intercept = -2", 
     cex.lab = 1.2)
plot(out2, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "intercept = 0", 
     cex.lab = 1.2)
plot(out3, xlab = "Overdispersion (sd linear pred)", ylab = "Proportion significant", main = "intercept = 2", 
     cex.lab = 1.2)





