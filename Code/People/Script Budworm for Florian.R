library(DHARMa)
library(lme4)

load(file="JFdata_late.rdata")
mydata.late = mydata.late[-c(17,18),]
mydata.late$ID = 1:22


mod1 <- glm(cbind(N_parasitized, N_adult) ~ log10(density.attack+1), data = mydata.late, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod1)
plotSimulatedResiduals(simulationOutput = simulationOutput)


mydata.late$ID = 1:22
mod2 <- glmer(cbind(N_parasitized, N_adult) ~ log10(density.attack+1) + (1|ID), data = mydata.late, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod2)
plotSimulatedResiduals(simulationOutput = simulationOutput)


# Overdispersion is jetzt besser aber sieht so aus als fehlt ein quadratischer Effekt 

mod3 <- glmer(cbind(N_parasitized, N_adult) ~ log10(density.attack+1) + I(log10(density.attack+1)^2) + (1|ID), data = mydata.late, family=binomial)
simulationOutput <- simulateResiduals(fittedModel = mod3)
plotSimulatedResiduals(simulationOutput = simulationOutput)


summary(mod3)
