library(nlme)

fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
summary(fm1)
simulate(fm1)
?simulate.lme

# This works, but unclear what it does 

x = simulateResiduals(fm1)
plot(x)


library(mgcv)
 
dat <- gamSim(1,n=400,dist="normal",scale=2)
b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)

x1 = predict(b)
x2 = as.matrix(simulate(b, n = 100000))

# This also works, but also unclear what it does

hist(x2[,1])
qqnorm(x2[,1])

simMean = apply(x2, 1 , mean)
plot(x1, simMean)

# mean is correct

simSD = apply(x2, 1 , sd)
hist(simSD)

plot(x1, simSD)

# but SD seems to scatter a bit too much


## simple comparison of lme and gam
require(mgcv)
require(nlme)
b0 <- lme(travel~1,data=Rail,~1|Rail,method="REML") 

b <- gam(travel~s(Rail,bs="re"),data=Rail,method="REML")

intervals(b0)
gam.vcomp(b)
anova(b)

test <- simulateResiduals(b)
plot(test)


library(gamm4)

set.seed(0) 
dat <- gamSim(1,n=400,scale=2) ## simulate 4 term additive truth
## Now add 20 level random effect `fac'...
dat$fac <- fac <- as.factor(sample(1:20,400,replace=TRUE))
dat$y <- dat$y + model.matrix(~fac-1)%*%rnorm(20)*.5

br <- gamm4(y~s(x0)+x1+s(x2),data=dat,random=~(1|fac))
plot(br$gam,pages=1)

simulate(br$mer, use.u = T)


