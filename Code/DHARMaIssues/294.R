library(lme4)
library(DHARMa)

TUAassocArrival<- read.delim(dec=",","~/Downloads/1-Rolemodel.Choice.TUA_20210715.txt", header = T)
str(TUAassocArrival)

TUAassocArrival$Class_Party<- as.factor(paste(TUAassocArrival$Class_Party))
TUAassocArrival$Class_Party2<- as.factor(paste(TUAassocArrival$Class_Party2))
TUAassocArrival$Class_Male<- as.factor(paste(TUAassocArrival$Class_Male))
TUAassocArrival$Sex_Party<- as.factor(paste(TUAassocArrival$Sex_Party))
TUAassocArrival$Observer_male<- as.factor(paste(TUAassocArrival$Observer_male))
TUAassocArrival$Dyad_ID<- as.factor(paste(TUAassocArrival$Dyad_ID))
TUAassocArrival$Site<- as.factor(paste(TUAassocArrival$Site))

TUAassocArrival$TSA_Month_Total<- as.numeric(TUAassocArrival$TSA_Month_Total)
TUAassocArrival$Present_Month_Area<- as.numeric(TUAassocArrival$Present_Month_Area)
TUAassocArrival$year_in_area<- as.numeric(TUAassocArrival$year_in_area)

TUAassocArrival$Counts_Peeringevents<- as.numeric(TUAassocArrival$Counts_Peeringevents)
TUAassocArrival$total_bouts_in_party<- as.numeric(TUAassocArrival$total_bouts_in_party)

TUAassocArrival$X0m<- as.numeric(TUAassocArrival$X0m)
TUAassocArrival$X.2m<- as.numeric(TUAassocArrival$X.2m)
TUAassocArrival$X.5m<- as.numeric(TUAassocArrival$X.5m)
TUAassocArrival$X.10m<- as.numeric(TUAassocArrival$X.10m)
TUAassocArrival$X.50m<- as.numeric(TUAassocArrival$X.50m)

glmm1b<- glmer(Counts_Peeringevents ~  factor(Class_Party2)  + TSA_Month_Total + offset(log(total_bouts_in_party)) + (1|Dyad_ID), family = poisson, data = TUAassocArrival)

testZeroInflation(glmm1b)
testDispersion(glmm1b)

