library(spaMM)

## Fit a Poisson GLMM with adjacency (CAR) correlation model
# see ?adjacency for how to fit efficiently such model models 

data("Loaloa")
HLC <- HLCor(cbind(npos,ntot-npos)~Matern(1|longitude+latitude),
             data=Loaloa,family=binomial(),
             ranPars=list(lambda=1,nu=0.5,rho=1/0.7)) 
simulate(HLC,nsim=2)

