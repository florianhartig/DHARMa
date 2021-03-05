library(DHARMa)
library(lme4)

overdispersion = 0.7

testData = createData(sampleSize = 5000, fixedEffects = 1, family = poisson(), randomEffectVariance = 1, overdispersion = overdispersion, numGroups = 100)

# fittedModel <- glm(observedResponse ~ Environment1, family = "poisson", data = testData)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

# res <- simulateResiduals(fittedModel = fittedModel, n = 100)
# testDispersion(res)
# side note: I could find no inflated type I error of any of the tests, even for very high sampleSize, as long as I moved n to 1000 or higher

###### Current default DHARMa tests ######

res <- simulateResiduals(fittedModel = fittedModel, n = 1000)
testDispersion(res)

# Condition on random effects
res <- simulateResiduals(fittedModel = fittedModel, re.form  = NULL)
testDispersion(res)

###### New parametric test (glmmWIKI) ######

# weird, two-sided test is always significant, always tests for underdispersion under H0 ... maybe this is the reason why they all test only for overdispersion in the packages
testDispersion(res, type = "Pearson")

# one-sided test seems to behave OK
testDispersion(res, type = "Pearson", alternative = "greater")


###### NEW OPTIONS ######

# note - could be run with re.form or without
res <- simulateResiduals(fittedModel = fittedModel, n = 1000)

# NEW Option 1: dispersion measured by variance of residuals against uniform 
obs = var(res$scaledResiduals) * 12
sim = replicate(1000, {var(runif(length(res$scaledResiduals)))}) * 12
DHARMa:::getP(simulated = sim, observed = obs, alternative = "two.sided", plot = T)

# NEW Option 2: dispersion measured by variance of the normal, which should be something like a simulation-based deviance residual? 
# issue is how to handle the outliers
obs = var(residuals(res, quantileFunction = qnorm, outlierValues = c(-7,7))) 
sim = replicate(1000, {var(rnorm(length(res$scaledResiduals)))}) # I know we could do this analytical, but I'm lazy
DHARMa:::getP(simulated = sim, observed = obs, alternative = "two.sided", plot = T)

# NEW Option 3 - probably problematic for situations with binomial / possion only 0 / 1 simulations
res <- simulateResiduals(fittedModel = fittedModel, re.form  = NULL)
expectedSD = apply(res$simulatedResponse, 1, sd)
spread <- function(x) sd((x - res$fittedPredictedResponse) / expectedSD) 
testGeneric(res, summary = spread)


##### Tests #####


returnStatistics <- function(control = 0){
  testData = createData(sampleSize = 1000, family = poisson(), overdispersion = control, numGroups = 10)
  fittedModel <- lme4::glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson())
  
  out = list()
  
  res <- simulateResiduals(fittedModel = fittedModel, n = 250)
  out$DHARMaDefault = testDispersion(res, plot = FALSE)$p.value
  
  res2 <- simulateResiduals(fittedModel = fittedModel, n = 250, re.form = NULL)
  out$DHARMaConditional = testDispersion(res2, plot = FALSE)$p.value
  
  out$DHARMaPearson = testDispersion(res, plot = FALSE, type = "Pearson", alternative = "greater")$p.value
  
  # NEW Option 1: dispersion measured by variance of residuals against uniform 
  obs1 = var(res$scaledResiduals) * 12
  sim1 = replicate(1000, {var(runif(length(res$scaledResiduals)))}) * 12
  out$VarResSimuUni = DHARMa:::getP(simulated = sim1, observed = obs1, plot = F, alternative = "two.sided")
  
  # NEW Option 2: dispersion measured by variance of the normal, which should be something like a simulation-based deviance residual? 
  # issue is how to handle the outliers
  obs2 = var(residuals(res, quantileFunction = qnorm, outlierValues = c(-7,7))) 
  sim2 = replicate(1000, {var(rnorm(length(res$scaledResiduals)))}) # I know we could do this analytical, but I'm lazy
  out$VarResSimuNorm = DHARMa:::getP(simulated = sim2, observed = obs2, plot = F, alternative = "two.sided")
  
  # NEW Option 3 - probably problematic for situations with binomial / possion only 0 / 1 simulations
  res <- simulateResiduals(fittedModel = fittedModel, re.form  = NULL)
  expectedSD = apply(res$simulatedResponse, 1, sd)
  spread <- function(x) sd((x - res$fittedPredictedResponse) / max(expectedSD, 0.001)) 
  out$PearsonSimu = testGeneric(res, summary = spread, plot = F)$p.value

  return(unlist(out))
}


outH0 = runBenchmarks(returnStatistics, nRep = 10)  
outPower = runBenchmarks(returnStatistics, nRep = 3, controlValues = seq(0,2,len = 10))
  
omar <- par(mar = c(3,10,3,3), mfrow = c(1,2))
plot(outH0)
plot(outPower)
par(omar)

outH0$controlValues






