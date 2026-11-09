library(mvtnorm)
set.seed(1)
C <- exp(-as.matrix(dist(seq(0,50,by=.1))))
obs <- as.numeric(rmvnorm(1,sigma=C))
par(mfrow = c(1,2))
image(C, main = "Residual covariance")
plot(obs, type = "l")

# TMB's 'one-step-ahead' method (See Thygesen et al. 2017 and TMB::oneStepPredict).
## L * L^T = C
L <- t(chol(C))
r1 <- solve(L, obs)
qqnorm(r1, main="OSA")
abline(0,1)
ks.test(r1, pnorm)

## DHARMA residuals:
x = replicate(100, as.numeric(rmvnorm(1,sigma=C)))

# calculate DHARMa residuals
library(DHARMa)
res <- createDHARMa(x, obs, integerResponse = F)
testUniformity(res)

# calculated rotated DHARMa residuals
x1 = apply(x, 2, function(a) solve(L, a))
res <- createDHARMa(x1, r1, integerResponse = F)
testUniformity(res)

