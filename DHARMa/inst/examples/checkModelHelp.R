
testData = createData(sampleSize = 200, overdispersion = 0.5, randomEffectVariance = 0.5, family = gaussian(), hasNA = T)
fittedModel <- lm(observedResponse ~ Environment1 , data = testData)
res <- simulateResiduals(fittedModel) # throws NA message

# get the indices of the rows that were used to fit the model
sel = as.numeric(rownames(model.frame(fittedModel)))

# now use the indices when plotting against other variables
plotResiduals(res, form = testData$Environment1[sel])
