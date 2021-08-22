# set up the model structure, but don't fit the model yet

library(glmmTMB)

testData = createData(sampleSize = 200, overdispersion = 0, family = binomial())

#  weights = weight
fit = glmmTMB(observedResponse ~ Environment1 + (1|group) + (1|ID), family=binomial(), data=testData, doFit=FALSE)

# set the variance of the random intercept term for stratum to a large, fixed value

fit$parameters$theta[1] = log(1e3)

# adjust the number of parameters

nvarparam <- length(fit$parameters$theta)
fit$mapArg <- list(theta=factor(c(NA, 1:(nvarparam-1))))

# fit the model and summarize the model results

fit2 = glmmTMB:::fitTMB(fit)
summary(fit2)

res <- simulateResiduals(fit2) # error

# reasons seems to be the predict function, which doesn't allow re.form = ~0 in this case
predict(fit2, re.form = ~0)


# solution - specify fixed parameters differently.
# Problem however is that this still creates a pattern, as glmmTMB simulates new REs
# based on the large fixed variance

testData = createData(sampleSize = 200, overdispersion = 0, family = binomial())


fit2B <- glmmTMB(observedResponse ~ Environment1 + (1|group) + (1|ID), family=binomial(), data=testData,
                 start = list(theta = c(log(1e3),0)),
                 map = list(theta = factor(c(NA,1))))
summary(fit2B)

predict(fit2B, re.form = ~0)  

res<- simulateResiduals(fit2B, re.form = )
plot(res)

# estimating the variance instead works fine

fit2C <- glmmTMB(observedResponse ~ Environment1 + (1|group) + (1|ID), family=binomial(), data=testData)
res<- simulateResiduals(fit2C)
plot(res)

