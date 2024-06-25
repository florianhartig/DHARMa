
devtools::install_github("lamho86/phylolm")

library(phylolm)

# GLM

set.seed(123456)
tre = rtree(50)
x = rTrait(n=1,phy=tre)
X = cbind(rep(1,50),x)
y = rbinTrait(n=1,phy=tre, beta=c(-1,0.5), alpha=1 ,X=X)
dat = data.frame(trait01 = y, predictor = x)
fit = phyloglm(trait01~predictor,phy=tre,data=dat,boot=100)

summary(fit)
coef(fit)
vcov(fit)

fit = DHARMa::simulateResiduals(fit, plot = T)



# LM 

set.seed(123456)
tre = rcoal(60)
taxa = sort(tre$tip.label)
b0=0; b1=1;
x <- rTrait(n=1, phy=tre,model="BM",
            parameters=list(ancestral.state=0,sigma2=10))
y <- b0 + b1*x + 
  rTrait(n=1,phy=tre,model="lambda",parameters=list(
    ancestral.state=0,sigma2=1,lambda=0.5))
dat = data.frame(trait=y[taxa],pred=x[taxa])
fit = phylolm(trait~pred,data=dat,phy=tre,model="lambda")
summary(fit)

res = DHARMa::simulateResiduals(fit, plot = T)
testSpatialAutocorrelation(res, )

res = DHARMa::simulateResiduals(fit, plot = T, rotation = "estimated")


