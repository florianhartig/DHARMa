# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2010.00044.x

library(DHARMa)
library(phylolm)

set.seed(123)
tre = rcoal(60)
b0=0; b1=1;
x <- runif(length(tre$tip.label), 0,1)
y <- b0 + b1*x + 
  rTrait(n=1, phy=tre,model="BM",
         parameters=list(ancestral.state=0,sigma2=10))
dat = data.frame(trait=y,pred=x)

fit = lm(trait~pred,data=dat)
res = simulateResiduals(fit, plot = T)

testPhylogeneticAutocorrelation(res, tree = tre)


fit = phylolm(trait~pred,data=dat,phy=tre,model="BM")
summary(fit)

res = DHARMa::simulateResiduals(fit, plot = T)
res = DHARMa::simulateResiduals(fit, plot = T, rotation = "estimated")

