library(ordinal)
clmm2

options(contrasts = c("contr.treatment", "contr.poly"))

## More manageable data set:
dat <- subset(soup, as.numeric(as.character(RESP)) <=  24)
dat$RESP <- dat$RESP[drop=TRUE]

m1 <- clm2(SURENESS ~ PROD, data = dat, link="probit")

m2 <- clmm2(SURENESS ~ PROD, random = RESP, data = dat, link="probit",
            Hess = TRUE, method="ucminf", threshold = "symmetric")

predict(m1)
predict(m2)
