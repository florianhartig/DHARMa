library(brms)
library(DHARMa)

# example from brms
bprior1 <- prior(student_t(5,0,10), class = b) +
  prior(cauchy(0,2), class = sd)
fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
            data = epilepsy, family = poisson(), prior = bprior1)

# sample from the Posterior Predictive Distribution
preds <- posterior_predict(fit1, nsamples = 250, summary = FALSE)
preds <- t(preds)

res <- createDHARMa(simulatedResponse = preds,
                    fittedPredictedResponse = apply(preds, 1, median),
                    observedResponse = epilepsy$count,
                    integerResponse = "poisson")

plot(res, quantreg = FALSE)



set.seed(123)

dat <- createData(200)

plot(observedResponse ~ Environment1, data = dat)
plot(observedResponse ~ Environment1, data = dat, pch = as.numeric(dat$group))

fit <- glm(observedResponse ~ Environment1, data = dat, family = "poisson")
res <- simulateResiduals(fit, plot = T) # see that there is a model error, add RE

fit <- glmer(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")
res <- simulateResiduals(fit, plot = T) # now it looks fine



# example with
bprior1 <- prior(student_t(5,0,10), class = b) + prior(cauchy(0,2), class = sd)
fit1 <- brm(observedResponse ~ Environment1 + (1|group),
            data = dat, family = poisson(), prior = bprior1)

# sample from the Posterior Predictive Distribution
preds <- posterior_predict(fit1, nsamples = 250, summary = FALSE)

res <- createDHARMa(simulatedResponse = t(preds), 
                    fittedPredictedResponse = apply(t(preds), 1, median), 
                    observedResponse = dat$observedResponse,
                    integerResponse = T)

plot(res, quantreg = FALSE)