# fit <- readRDS("~/Downloads/Pinus.strobus_fit_k10_bsts_selectFALSE.rds")
fit <- readRDS("~/Downloads/Basalarea_fit_Pinus.strobus_tekc(25,50).rds")

summary(fit)

dat = model.frame(fit)

res = simulateResiduals(fit, plot = F)
res2 = recalculateResiduals(res, group = dat$blname)
plot(res, quantreg = F)
testDispersion(res)

# testing if tweedie works in principle, using example of mgcv

library(mgcv)

# checking shape
hist(rTweedie(rep(1,1000),p=1.5,phi=1.3))


f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
  (10 * x)^3 * (1 - x)^10
n <- 3000
x <- runif(n)
mu <- exp(f2(x)/3+.1);x <- x*10 - 4
y <- rTweedie(mu,p=1.5,phi=1.3)

# correct p
b <- gam(y~s(x,k=20),family=Tweedie(p=1.5))

res = simulateResiduals(b, plot = T)
testDispersion(res)
testDispersion(res, type = "PearsonChisq")

# incorrect p
b <- gam(y~s(x,k=20),family=Tweedie(p=1.1))

res = simulateResiduals(b, plot = T)
testDispersion(res) # reacts to the pattern 
testDispersion(res, type = "PearsonChisq") # doesn't react to the pattern

# reason is probably that 
x = residuals(b, type = "scaled.pearson")
sd(x)





# testing variations of the dispersion test 

fit <- readRDS("~/Downloads/Basalarea_fit_Pinus.strobus_tekc(25,50).rds")



# Scaled Pearson residuals are raw residuals divided by the standard deviation of the data according to the model mean variance relationship and estimated scale parameter.
x = residuals(fit, type = "scaled.pearson")
sd(x)


testDispersion(res, type = "PearsonChisq")


res = simulateResiduals(fit, n = 1000, plot = F)

simulatedSD = apply(res$simulatedResponse, 1, sd)

residuals(fit, type = "response") / simulatedSD

  
  sd(res$simulatedResponse)^2
spread <- function(x) var(x - simulationOutput$fittedPredictedResponse) / expectedVar

spread <- function(x) var(x - simulationOutput$fittedPredictedResponse) / var(simulationOutput$fittedPredictedResponse)

out = testGeneric(simulationOutput, summary = spread, alternative = alternative, methodName = "DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated", plot = plot, ...)



out = list()
out$data.name = deparse(substitute(simulationOutput))

simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

alternative <- match.arg(alternative)

observed = summary(simulationOutput$observedResponse)

simulated = apply(simulationOutput$simulatedResponse, 2, summary)

p = getP(simulated = simulated, observed = observed, alternative = alternative)

out$statistic = c(ratioObsSim = observed / mean(simulated))
out$method = methodName
out$alternative = alternative
out$p.value = p










