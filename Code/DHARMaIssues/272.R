library(DHARMa)

testData <- createData(100, family = binomial(), randomEffectVariance = 0)

fit <- glm(observedResponse ~ Environment1, family = binomial, data = testData)

res <- simulateResiduals(fittedModel=fit,n=100,refit=F,plot=T)
res <- simulateResiduals(fittedModel=fit,n=100,refit=T,plot=T)
res <- simulateResiduals(fittedModel=fit,n=100,refit=T,plot=F, method = "traditional")

testOutliers(res)


plot(res, quantreg = F)

devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
                         ref = "v0.3.1.0", dependencies = F, build_vignettes = F, force = T)



res$simulatedResponse
res$observedResponse
