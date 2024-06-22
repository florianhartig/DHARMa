library(mgcv)
library(DHARMa)

all <- list()

all$formula <- formula(Type ~ s(Frisian_proficiency) +
                         s(IQ) +
                         te(IQ , by = Measurement) +
                         s(Subject, bs="re") +
                         s(Measurement, bs="re"), data = df)

all$data <- df

f <- replicate(2, update(all$formula, NULL~.))
f[[1]] <- update(f[[1]], Type~.)

model.gam <- gam(f, data=df, family=multinom(2), method="fREML")
summary(model.gam) # works!

simRes <- simulateResiduals(fittedModel = model.gam, plot = F)




library(mgcv)
set.seed(6)
## simulate some data from a three class model
n <- 1000
f1 <- function(x) sin(3*pi*x)*exp(-x)
f2 <- function(x) x^3
f3 <- function(x) .5*exp(-x^2)-.2
f4 <- function(x) 1
x1 <- runif(n);x2 <- runif(n)
eta1 <- 2*(f1(x1) + f2(x2))-.5
eta2 <- 2*(f3(x1) + f4(x2))-1
p <- exp(cbind(0,eta1,eta2))
p <- p/rowSums(p) ## prob. of each category 
cp <- t(apply(p,1,cumsum)) ## cumulative prob.
## simulate multinomial response with these probabilities
## see also ?rmultinom
y <- apply(cp,1,function(x) min(which(x>runif(1))))-1
## plot simulated data...
plot(x1,x2,col=y+3)

## now fit the model...
b <- gam(list(y~s(x1)+s(x2),~s(x1)+s(x2)),family=multinom(K=2))
plot(b,pages=1)
gam.check(b)
summary(b)


simRes <- simulateResiduals(fittedModel = b)
simulate(b) # simulations not implemented 
library(mgcViz)
simulate(b) # mgcViz would allow simulations 


sim_gam <- mgcViz::simulate.gam(b, nsim=100)

sim_res_gam_1 <- createDHARMa(simulatedResponse       = sim_gam,
                              observedResponse        = b$model$y,
                              fittedPredictedResponse = predict(b)[,1],
                              integerResponse         = T)

plot(sim_res_gam_1)

sim_res_gam_2 <- createDHARMa(simulatedResponse       = sim_gam,
                              observedResponse        = b$model$y,
                              fittedPredictedResponse = predict(b)[,2],
                              integerResponse         = T)

plot(sim_res_gam_2)


# testing if missing predictor would show up

b <- gam(list(y~s(x1),~s(x1)),family=multinom(K=2))
plot(b,pages=1)
gam.check(b)
summary(b)


sim_gam <- mgcViz::simulate.gam(b, nsim=100)

sim_res_gam_1 <- createDHARMa(simulatedResponse       = sim_gam,
                              observedResponse        = b$model$y,
                              fittedPredictedResponse = predict(b)[,1],
                              integerResponse         = T)

plot(sim_res_gam_1)
plotResiduals(sim_res_gam_1, form = x2)


# creating plots just for class 2

makeBinary = function(x) as.numeric(x == 2)
sim <- apply(sim_gam,1:2,makeBinary) 

sim_res_gam_1 <- createDHARMa(simulatedResponse       = sim,
                              observedResponse        = makeBinary(b$model$y),
                              fittedPredictedResponse = predict(b)[,2],
                              integerResponse         = T)

plot(sim_res_gam_1)
plotResiduals(sim_res_gam_1, form = x2)

# checking for correct number of predictions in class 2
# as we transformed to 0/1 we can just repurpose the 
# testZeroInflation function 

testZeroInflation(sim_res_gam_1)

