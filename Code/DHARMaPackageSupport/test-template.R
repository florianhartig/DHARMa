
# Functions needed for integration of a new package 

family(fittedModel)
class(fittedModel)[1]
nobs(fittedModel)
model.frame(fittedModel)[,1] 
simulations = simulate(fittedModel, nsim = n, ...)
predict(fittedModel)
coef(fittedModel)
ranef(fittedModel)
fixef(fittedModel)
refit(fittedModel)