library(car)

# idea - using box cox transformation on the x data 

par(mfrow = c(1,3))

x = rexp(200)
hist(x)
out = boxCox(lm(x ~ 1))
hist(x^out$x[which.max(out$y)])

# negative numbers don't work at all, but even so, it the distr is right-skewed, it also doesn't really work
x = - rexp(200)
x = x - min(x) + 0.01
hist(x)
out = boxCox(lm(x ~ 1))
hist(x^out$x[which.max(out$y)])

