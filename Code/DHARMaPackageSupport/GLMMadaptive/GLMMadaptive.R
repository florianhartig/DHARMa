devtools::install_github("drizopoulos/GLMMadaptive")
#install.packages("GLMMadaptive")
library(GLMMadaptive)
library(DHARMa)

# simulate some data
set.seed(123L)
n <- 500
K <- 15
t.max <- 25

betas <- c(-2.13, -0.25, 0.24, -0.05)
D <- matrix(0, 2, 2)
D[1:2, 1:2] <- c(0.48, -0.08, -0.08, 0.18)

times <- c(replicate(n, c(0, sort(runif(K-1, 0, t.max)))))
group <- sample(rep(0:1, each = n/2))
DF <- data.frame(year = times, group = factor(rep(group, each = K)))
X <- model.matrix(~ group * year, data = DF)
Z <- model.matrix(~ year, data = DF)

b <- cbind(rnorm(n, sd = sqrt(D[1, 1])), rnorm(n, sd = sqrt(D[2, 2])))
id <- rep(1:n, each = K)
eta.y <- as.vector(X %*% betas + rowSums(Z * b[id, ]))
DF$y <- rbinom(n * K, 1, plogis(eta.y))
DF$id <- factor(id)

################################################

fm1 <- mixed_model(fixed = y ~ year * group, random = ~ 1 | id, data = DF,
                   family = binomial())

predict(fm1)

res = simulateResiduals(fm1)

getSimulations(fm1, 10)

getResiduals(fm1)


predict.MixMod


library(GLMMadaptive)
library(DHARMa)
testData = createData(sampleSize = 200, overdispersion = 0, randomEffectVariance = 0, family = poisson())
fittedModel <- mixed_model(observedResponse ~ Environment1, random =  ~ 1 | group , family = "poisson", data = testData)
nobs(fittedModel)


getSimulations(fittedModel, 10)


class(getSimulations(fittedModel, 10))
dim(getSimulations(fittedModel, 10))
str(getSimulations(fittedModel, 10))

res =  simulateResiduals(fittedModel)
plot(res)


library(lme4)
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group) , family = "poisson", data = testData)

nobs(fittedModel)

class(getSimulations(fittedModel, 10))
dim(getSimulations(fittedModel, 10))
str(getSimulations(fittedModel, 10))

res =  simulateResiduals(fittedModel)
plot(res)



residuals(fittedModel)

predict(fittedModel) - testData$observedResponse


res =  simulateResiduals(fittedModel)




