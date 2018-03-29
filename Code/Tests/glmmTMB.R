library(glmmTMB)


##############################

m1 <- glmmTMB(count~ mined + (1|site), 
               zi=~mined, 
               family=poisson, data=Salamanders)
summary(m1)

simulate(m1)
class(m1)

### creating new data based on the fitted models. Residuals should now be perfect

Salamanders$counts2 = simulate(m2)

(m2 <- glmmTMB(count2~spp + mined + (1|site), 
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

Salamanders$counts2 = simulate(m)

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

Salamanders$counts2 = simulate(m2)

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



