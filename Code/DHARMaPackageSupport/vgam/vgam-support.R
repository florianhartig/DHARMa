install.packages("svyVGAM")


library(VGAM)

zdata <- data.frame(x2 = runif(nn <- 2000))
zdata <- transform(zdata, pstr0  = logitlink(-0.5 + 1*x2, inverse = TRUE),
                   lambda = loglink(  0.5 + 2*x2, inverse = TRUE))
zdata <- transform(zdata, y = rzipois(nn, lambda, pstr0 = pstr0))
with(zdata, table(y))
fit2 <- vglm(y ~ x2, zipoisson, data = zdata, trace = TRUE)

simulate(fit2)
class(fit2)

library(svyVGAM)

data(nhanes_sxq)
nhdes = svydesign(id=~SDMVPSU,strat=~SDMVSTRA,weights=~WTINT2YR,
                  nest=TRUE, data=nhanes_sxq)

sv1<-svy_vglm(malepartners~RIDAGEYR+factor(RIDRETH1)+DMDEDUC,
              zipoisson(), design=nhdes, crit = "coef")

simulate(sv1)
