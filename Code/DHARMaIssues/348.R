
dat = read.csv("~/Downloads/Use_Avail.csv",  stringsAsFactors = T)

library(lme4)
library(DHARMa)

M2 <- glmer(cbind(Used, NotUsed) ~ Sex + type + (1 | ID), family = binomial,
            data=dat)
simOut <- simulateResiduals(M2, plot = T)
plotResiduals(simOut, dat$type)
plotResiduals(simOut, dat$Sex)

sessionInfo()


install.packages("DHARMa")
