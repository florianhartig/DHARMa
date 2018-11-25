# https://stats.stackexchange.com/questions/363955/non-normal-residuals-for-tweedie-glm





library(glmmTMB)
library(DHARMa)
library(mgcv)

f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
  (10 * x)^3 * (1 - x)^10
n <- 300
x <- runif(n)
mu <- exp(f2(x)/3+.1);
x <- x*10 - 4
y <- rTweedie(mu,p=1.5,phi=1.3)

fit <- gam(y~s(x,k=20),family=Tweedie(p=1.5))
summary(fit)
res = simulateResiduals(fit)
plot(res)



summary(b)


y <- rgamma(100,shape=5)
x <- 1:100

fit = glmmTMB(y~x,family=tweedie())
res = simulateResiduals(fit)
plot(res)


# GLM doesn't work 

require(statmod)
require(tweedie)

# Fit a poisson generalized linear model with identity link
fit = glm(y~x,family=tweedie(var.power=1,link.power=1))

