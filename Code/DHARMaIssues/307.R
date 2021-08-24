library(DHARMa)
library(lme4)

testData <- createData(family = gaussian())
fit <- lmer(observedResponse ~ Environment1 + (1|group), data = testData)

# pearson residuals
res1 = residuals(fit, type = "pearson")

# DHARMa residuals, 
# n is very large to reduce stochasticity
# re.form = NULL ensures residuals conditional on REs
# quantileFunction = qnorm transforms uniform residuals to normality 
res2 = residuals(simulateResiduals(fit, re.form = NULL, n = 100000), quantileFunction = qnorm)

plot(res1, res2)


Data = read.csv2(file = "https://raw.githubusercontent.com/Flaiba/Momo/master/dataframe.csv")
Data$PropTime = as.numeric(Data$PropTime)
Data$SEX = as.factor(Data$SEX)
Data$LINE = as.factor(Data$LINE)
model=lmer(PropTime~ SEX + (1|LINE:SEX)+(1|LINE), data = Data)

res2 = simulateResiduals(model, re.form = NULL, n = 100000)
plot(res2)

qqnorm(residuals(res2, quantileFunction = qnorm))
plotQQunif(res2)

# shapiro test is significant, probably because it is more 
# powerful than a KS test
shapiro.test(residuals(res2, quantileFunction = qnorm))

qqnorm(rnorm(nrow(Data)))



