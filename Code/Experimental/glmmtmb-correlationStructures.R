

# mildly long-tailed
plot()

# An ARIMA simulation
x = runif(100)
time = 1:100
id = 1:100

ts.sim <- arima.sim(n = 100, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                    rand.gen = function(n, ...) sqrt(0.1796) * rt(n, df = 5)) + 5*x 
ts.plot(ts.sim)

m1 <- glmmTMB(ts.sim ~ x + ar1(time -1 |id) )
summary(m1)

library(nlme)
library(glmmTMB)
data(sleepstudy,package="lme4")
sleepstudy$row <- factor(1:180)
gt_min <- glmmTMB(Reaction ~ (1|Subject) + ar1(row + 0 | Subject), sleepstudy)
summary(gt_min)
