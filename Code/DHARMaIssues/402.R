

TooHot = read.csv('https://raw.githubusercontent.com/HugoMH/Stat_M1_BootGLMM/main/TDs/Data/Suicides%20and%20Ambient%20Temperature.csv')
head(TooHot)

TooHot$Temperature2 = TooHot$Temperature^2

Mpoisson = glm(Suicides ~ Temperature + Temperature2 + Country
               ,data = TooHot
               ,family = poisson(link = 'log')) # !!   <°)))><    !!

MpoissonRE = lme4::glmer(Suicides ~ Temperature + Temperature2 + (1|Country) 
                         ,data = TooHot, family = poisson(link = 'log'))

DHARMa::testDispersion(Mpoisson,plot = F)
# dispersion = 11350, p-value < 2.2e-16

DHARMa::testDispersion(MpoissonRE,plot = F) 
summary(MpoissonRE)


# Best to check dispersion with conditional simulations
res2 <- simulateResiduals(MpoissonRE, re.form = NULL)
DHARMa::testDispersion(res2)

plot(res2)


# The analytical test can also be used but it is not generally reliable (biased towards underdispersion)
DHARMa::testDispersion(MpoissonRE, type = "PearsonChisq")


