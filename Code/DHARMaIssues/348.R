M2 <- glmer(cbind(Used, NotUsed) ~ Sex + type + (1 | ID), family = binomial,
            data=Use_Avail)
simOut <- simulateResiduals(M2, plot = T)
plotResiduals(simOut, Use_Avail$type)
plotResiduals(simOut, Use_Avail$Sex)