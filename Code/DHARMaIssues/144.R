library(mgcv)
#library(mgcViz)
library(DHARMa)


X<-c(1,2,2,3,2,3,2,3,4,5,6,5,7,5,4,5,6,7,4,4,5,6)
Y<-c(2,3,2,3,2,3,2,3,2,3,6,7,6,8,9,7,6,7,8,7,6,7)
data1<-as.data.frame(cbind(X,Y))
mod<-gam(Y~s(X,k=3),data=data1)
plot(mod,resid=TRUE)


simulate(mod)

simout <- simulateResiduals(mod, plot=T)

predict(mod)

x = simulateResiduals(mod)

?mgcViz

