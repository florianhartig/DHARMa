dat = read.table("222.txt", header = T, sep = "|")
head(dat)
library(lme4)
mod1=glmer(RT~DISP+(DISP|subj),family=Gamma(link="inverse"),data = dat)

simulationOutput <- simulateResiduals(fittedModel = mod1, plot = T)
# produces infinity error 

x = predict(mod1,  type = "response")
min(x)
max(x)

# unconditional simulations, default in DHARMa
simulate(mod1)
simulate(mod1, re.form = ~0)

# conditional simulations 
simulate(mod1, re.form = NULL)

simulationOutput <- simulateResiduals(fittedModel = mod1, plot = T, re.form = NULL)

