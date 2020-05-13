library(spaMM)

data("freight")

mod <- HLfit(broken ~ transfers, data=freight, family = poisson)
simulate(mod,2)

mod <- fitme(broken ~ transfers, data=freight, family = COMPoisson(nu=2))
simulate(mod,2)

mod <- glm(broken ~ transfers, data=freight, family = COMPoisson(nu=2))
simulate(mod,2)

mod <- HLfit(broken ~ transfers, data=freight, family = COMPoisson(nu=2))
simulate(mod,2)

