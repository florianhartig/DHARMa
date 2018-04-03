library(lme4)
library(DHARMa)

# creating test data according to standard poisson GLMM assumptions
testData = createData(sampleSize = 200, randomEffectVariance = 1.5, family = poisson(), numGroups = 20)

# fitting GLMM
fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

# res ~ pred, with conditional and unconditional predictions
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
par(mfrow = c(2,2))

plotResiduals(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals , main = "Unconditional Residuals\n Unconditional Predictions\n DHARMa standard", rank = T)
plotResiduals(predict(fittedModel), simulationOutput$scaledResiduals, main = "Unconditional Residuals\n Conditional Predictions\n", rank = T)

simulationOutput <- simulateResiduals(fittedModel = fittedModel, re.form = NULL)
plotResiduals(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals , main = "Conditional Residuals\n Unconditional Predictions\n", rank = T)
plotResiduals(predict(fittedModel), simulationOutput$scaledResiduals, main = "Conditional Residuals\n Conditional Predictions\n", rank = T)


# creating test data according to standard poisson GLMM assumptions
testData = createData(sampleSize = 200, randomEffectVariance = 1.5, family = gaussian(), numGroups = 20)

# fitting GLMM
fittedModel <- lmer(observedResponse ~ Environment1 + (1|group), data = testData)

# res ~ pred, with conditional and unconditional predictions
simulationOutput <- simulateResiduals(fittedModel = fittedModel)
par(mfrow = c(2,2))
plotResiduals(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals , main = "Unconditional Residuals\n Unconditional Predictions\n DHARMa standard")
plotResiduals(predict(fittedModel), simulationOutput$scaledResiduals, main = "Unconditional Residuals\n Conditional Predictions\n")

simulationOutput <- simulateResiduals(fittedModel = fittedModel, re.form = NULL)
plotResiduals(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals , main = "Conditional Residuals\n Unconditional Predictions\n")
plotResiduals(predict(fittedModel), simulationOutput$scaledResiduals, main = "Conditional Residuals\n Conditional Predictions\n")


# lme4 lmerMod 
plot(predict(fittedModel), residuals(fittedModel))
cor.test(predict(fittedModel), residuals(fittedModel))
x = residuals(fittedModel)




# lme4 glmerMod fits deviance residuals 

x = residuals(fittedModel, type = response)
hist( x + (predict(fittedModel, type = "response") - testData$observedResponse))

plot(predict(fittedModel), residuals(fittedModel))



# alternative with grouping factor as fixed effect, this removes the pattern with group
fittedModel2 <- glm(observedResponse ~ Environment1 + as.factor(group), family = "poisson", data = testData)
simulationOutput <- simulateResiduals(fittedModel = fittedModel2)
plot(simulationOutput, rank = T)

x = cbind(ranef(fittedModel)$group, coef(fittedModel)$group[,1])
x = as.matrix(x)
barplot(t(x), beside = T)







# https://stats.stackexchange.com/questions/109381/correlation-fitted-residuals-in-mixed-models?rq=1

a <-rnorm(100,0,10)
d<-as.data.frame(rbind(cbind(1,1:100,a),cbind(2,1:100,a)))
names(d)<-c('cond','id','a')
d$y<-d$a + d$cond +rnorm(200,0,4)
mm<-lmer(y~(1|id),data=d[1:100,],REML = F)
summary(mm)

plot(predict(mm),residuals(mm))
cor(predict(mm),residuals(mm))


library(DHARMa)
x = createData(100, numGroups = 20)










