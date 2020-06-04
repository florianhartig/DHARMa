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
