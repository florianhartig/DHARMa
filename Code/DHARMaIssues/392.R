dat = read.csv2("Code/DHARMaIssues/392.csv")

library(glmmTMB)
library(DHARMa)
library(performance)

# model without zip term
m0 = glmmTMB(Cofeeding ~ offset(log(N_sessions)) + Year + family_size + Week + Condition + dyad_identity + family_size:Week + family_size:Condition + family_size:dyad_identity + Week:Condition + Week:dyad_identity + Condition:dyad_identity + (1 |Family_Year) + (1 |Dyad) + (1 |ID1) + (1 |ID2), data = dat, family = nbinom1)

summary(m0)

res <- simulateResiduals(m0)
plot(res)
testZeroInflation(res) # no indication of zero-inflation
check_zeroinflation(m0) # suggests zero-inflation

# creating new data with and without zero-inflation for test later
dat2 = dat3 = dat
dat2$Cofeeding = simulate(m0)[,1] # no zero-inflation
dat3$Cofeeding = simulate(m0)[,1] * sample(c(0,1), nrow(dat), 0.4) # zero-inflation

# model with zip term
m1 = glmmTMB(Cofeeding ~ offset(log(N_sessions)) + Year + family_size + Week + Condition + dyad_identity + family_size:Week + family_size:Condition + family_size:dyad_identity + Week:Condition + Week:dyad_identity + Condition:dyad_identity + (1 |Family_Year) + (1 |Dyad) + (1 |ID1) + (1 |ID2), data = dat, family = nbinom1, ziformula = ~1)

res <- simulateResiduals(m1)
plot(res)
testZeroInflation(res)
check_zeroinflation(m1)

summary(m1) # note ZI estimate is large negative, i.e. there is no zero-inflation fitted, the model basically says that there is no zero-inflation present in this data.

# this is also confirmed by the AIC 
AIC(m0)
AIC(m1)

# -> no sign of zero inflation 

# test using synthetic data 

# dat2 which definitely has no zero-inflation
m0a = glmmTMB(Cofeeding ~ offset(log(N_sessions)) + Year + family_size + Week + Condition + dyad_identity + family_size:Week + family_size:Condition + family_size:dyad_identity + Week:Condition + Week:dyad_identity + Condition:dyad_identity + (1 |Family_Year) + (1 |Dyad) + (1 |ID1) + (1 |ID2), data = dat2, family = nbinom1)

summary(m0a)

testZeroInflation(m0a)
check_zeroinflation(m0a) # shows again zero-inflation

m1a = glmmTMB(Cofeeding ~ offset(log(N_sessions)) + Year + family_size + Week + Condition + dyad_identity + family_size:Week + family_size:Condition + family_size:dyad_identity + Week:Condition + Week:dyad_identity + Condition:dyad_identity + (1 |Family_Year) + (1 |Dyad) + (1 |ID1) + (1 |ID2), data = dat2, family = nbinom1,
              ziformula = ~1)

testZeroInflation(m1a)
check_zeroinflation(m1a) # shows again zero-inflation

AIC(m0a)
AIC(m1a)

# dat3 which definitely has zero-inflation

m0b = glmmTMB(Cofeeding ~ offset(log(N_sessions)) + Year + family_size + Week + Condition + dyad_identity + family_size:Week + family_size:Condition + family_size:dyad_identity + Week:Condition + Week:dyad_identity + Condition:dyad_identity + (1 |Family_Year) + (1 |Dyad) + (1 |ID1) + (1 |ID2), data = dat3, family = nbinom1)

testZeroInflation(m0b)
check_zeroinflation(m0b) # shows again zero-inflation

m1b = glmmTMB(Cofeeding ~ offset(log(N_sessions)) + Year + family_size + Week + Condition + dyad_identity + family_size:Week + family_size:Condition + family_size:dyad_identity + Week:Condition + Week:dyad_identity + Condition:dyad_identity + (1 |Family_Year) + (1 |Dyad) + (1 |ID1) + (1 |ID2), data = dat3, family = nbinom1,
              ziformula = ~1)

testZeroInflation(m1b)
check_zeroinflation(m1b) # shows again zero-inflation



# simulate Data from Poisson distribution

dat <- createData(sampleSize = 1000)
fit <- glmmTMB(observedResponse ~ Environment1 + (1|group), data = dat, family = "poisson")

res <- simulateResiduals(fit, plot = T)
testZeroInflation(res)
check_zeroinflation(fit) # performance package calcualtes correct value

fit <- glmmTMB(observedResponse ~ Environment1 + (1|group), data = dat, family = "nbinom1")

res <- simulateResiduals(fit, plot = T)
testZeroInflation(res)
check_zeroinflation(fit)




