#devtools::install_github("mfasiolo/qgam")
#devtools::install_github("florianhartig/DHARMa/DHARMa", ref = "qgam-switch")

library(DHARMa)
library(effects)

# Try out different exponents in logistic regression

# try out with simulated data
n = 10000
x = runif(n, 0, 1000)
#x = rexp(n, rate = 200)
y = rbinom(n, 1, prob = plogis(-3 + 0.8*x^(1/5)))
#y = rbinom(n, 1, prob = plogis(0 + 0.8*x)); 
# y = rbinom(n, 1, prob = plogis(-5 + 0.2025 *x + -0.0002 * x^2))

boxplot(x ~ as.factor(y))


mod1 <- glm(y ~ x, family = "binomial")
mod2 <- glm(y ~ I(x^(1/5)), family = "binomial")
mod3 <- glm(y ~ x + I(x^(1/5)), family = "binomial")

# Try out different exponents in logistic regression

summary(mod1)
summary(mod2)
summary(mod3)

AIC(mod1)
AIC(mod2)
AIC(mod3)

plot(allEffects(mod1), rug = T)
plot(allEffects(mod2), rug = T)
plot(allEffects(mod3), rug = T)



res1 = simulateResiduals(mod1)
plot(res1, quantreg = T, smoothScatter = F, rank = F)
res1b = recalculateResiduals(res1, group = round(res1$fittedPredictedResponse * 300 )) 
plot(res1b)

res2 = simulateResiduals(mod2)
plot(res2, quantreg = T, smoothScatter = F, rank = F)
res2b = recalculateResiduals(res2, group = round(res2$fittedPredictedResponse * 30 )) 
plot(res2b)




res3 = simulateResiduals(mod3)
plot(res3, quantreg = T, smoothScatter = F)
