library(glmmTMB)


##############################

m1 <- glmmTMB(count~ mined + (1|site), 
               zi=~mined, 
               family=poisson, data=Salamanders)
summary(m1)

simulate(m1)
class(m1)

res <- simulateResiduals(m1)
plot(res)


### creating new data based on the fitted models. Residuals should now be perfect

Salamanders$counts2 = simulate(m1)$sim_1

(m2 <- glmmTMB(counts2~spp + mined + (1|site), 
               zi=~spp + mined, 
               family=nbinom2, Salamanders))

res = simulateResiduals(m2)
plot(res)




# problems with ~reform
pred = predict(m1,~reform)
pred = predict(m1)

hist(pred, breaks = 20)

x = fixef(m1)
x$cond[1] + x$cond[2]*as.numeric(Salamanders$mined)

res = simulateResiduals(m1)
plot(res)




##### Test case 1 ##############################

m <- glmmTMB(count~ mined + (1|site), 
              zi=~mined, 
              family=poisson, data=Salamanders)
summary(m)

res = simulateResiduals(m)
plot(res)

### creating new data based on the fitted models. Residuals should now be perfect

Salamanders$count2 = simulate(m)$sim_1

m <- glmmTMB(count2~ mined + (1|site), 
             zi=~mined, 
             family=poisson, data=Salamanders)

res = simulateResiduals(m)
plot(res)






## Zero-inflated negative binomial model
(m2 <- glmmTMB(count~spp + mined + (1|site), 
               zi=~spp + mined, 
               family=nbinom2, Salamanders))

res = simulateResiduals(m2)
plot(res)

# creating a new response based on the fitted response 

Salamanders$count2 = simulate(m2)$sim_1

(m2 <- glmmTMB(count2~spp + mined + (1|site), 
               zi=~spp + mined, 
               family=nbinom2, Salamanders))

res = simulateResiduals(m2)
plot(res)




(m3 <- glmmTMB(count~spp + mined + (1|site), 
               zi=~spp + mined, 
               family=truncated_poisson, Salamanders))
res = simulateResiduals(m3)
plot(res)


## Binomial model
data(cbpp, package="lme4")
(m4 <- glmmTMB(cbind(incidence, size-incidence) ~ period + (1 | herd),
                  data=cbpp, family=binomial))

res = simulateResiduals(m4)
plot(res)



## Dispersion model

sim1=function(nfac=40, nt=100, facsd=.1, tsd=.15, mu=0, residsd=1)
{
  dat=expand.grid(fac=factor(letters[1:nfac]), t= 1:nt)
  n=nrow(dat)
  dat$REfac=rnorm(nfac, sd= facsd)[dat$fac]
  dat$REt=rnorm(nt, sd= tsd)[dat$t]
  dat$x=rnorm(n, mean=mu, sd=residsd) + dat$REfac + dat$REt
  return(dat)
}
set.seed(101)
d1 = sim1(mu=100, residsd =10)
d2 = sim1(mu=200, residsd =5)
d1$sd="ten"
d2$sd="five"
dat = rbind(d1, d2)


m5 = glmmTMB(x~sd+(1|t), dispformula=~sd, dat)

res = simulateResiduals(m5)
plot(res)


fixef(m5)$disp
c(log(5^2), log(10^2)-log(5^2)) #expected dispersion model coefficients



