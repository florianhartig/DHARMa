testData <- createData(family = gaussian())

fit <- lm(observedResponse ~  Environment1, data = testData)
summary(fit)
res <- simulateResiduals(fit)
plot(res)

all(predict(fit) == res$fittedPredictedResponse)
all(residuals(fit) == res$fittedResiduals)


testData <- createData(family = gaussian())
fit <- lm(observedResponse ~  Environment1, data = testData)
summary(fit)
res <- simulateResiduals(fit, refit = T)
plot(res)



