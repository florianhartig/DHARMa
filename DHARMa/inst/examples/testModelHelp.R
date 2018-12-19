dat = createData()
fittedModel <- glm(observedResponse ~ Environment1, data = dat)

fittedModel


try(family(fittedModel))
try(class(fittedModel)[1])
try(nobs(fittedModel))
try(getResponse(fittedModel))
try(simulate(fittedModel, nsim = 10))
try(predict(fittedModel))
try(coef(fittedModel))
try(ranef(fittedModel))
try(fixef(fittedModel))
try(refit(fittedModel, newresp = getResponse(fittedModel)))



testModel(fittedModel)

