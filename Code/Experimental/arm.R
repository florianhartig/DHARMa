library(lme4)
library(arm)

data(VerbAgg, package = 'lme4')

verb_mod <- glmer(r2 ~ (Anger + Gender + btype + situ)^2 + (1|id) + (1|item), family = binomial, data = VerbAgg, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(verb_mod)

par(mfcol=c(1, 2))
binnedplot(predict(verb_mod, type="response", re.form=NULL), resid(verb_mod, type="response"), nclass=40, main='With random effects')
binnedplot(predict(verb_mod, type="response", re.form=NA), resid(verb_mod, type="response"), nclass=40, main='Without random effects')

plot(predict(verb_mod, type="response", re.form=NULL), predict(verb_mod, type="response", re.form=NA))


library(DHARMa)

dat = createData(sampleSize = 500, family = binomial(), randomEffectVariance = 1, fixedEffects = c(1,1), quadraticFixedEffects = c(5,5))
fit = glmer(observedResponse ~ Environment1 + (1|group), family = binomial, data = dat)
par(mfcol=c(1, 2))
binnedplot(predict(fit, type="response", re.form=NULL), resid(fit, type="response"), nclass=40, main='With random effects')
binnedplot(predict(fit, type="response", re.form=NA), resid(fit, type="response"), nclass=40, main='Without random effects')


res <- simulateResiduals(fit)
plot(res)
plotResiduals(dat$Environment1, res$scaledResiduals)
plotResiduals(dat$Environment2, res$scaledResiduals)


res <- simulateResiduals(verb_mod)
plot(res)

par(mfrow = c(2,2))
plotResiduals(VerbAgg$Anger, res$scaledResiduals, main = "Anger")
plotResiduals(VerbAgg$Gender, res$scaledResiduals, main = "Gender")
plotResiduals(VerbAgg$btype, res$scaledResiduals, main = "btype")
plotResiduals(VerbAgg$situ, res$scaledResiduals, main = "situ")

ranEst = ranef(verb_mod)
plotResiduals(predict(verb_mod, type="response", re.form=NULL), res$scaledResiduals)
plotResiduals(ranEst$id[,1][VerbAgg$id], res$scaledResiduals)



# Create new data based on the fitted model and refit
VerbAgg$newResponse =  simulate(verb_mod)$sim_1
verb_mod <- glmer(newResponse ~ (Anger + Gender + btype + situ)^2 + (1|id) + (1|item), family = binomial, data = VerbAgg)

# Check residuals
res <- simulateResiduals(verb_mod)
ranEst = ranef(verb_mod)
plotResiduals(predict(verb_mod, type="response", re.form=NULL), res$scaledResiduals)
plotResiduals(ranEst$id[,1][VerbAgg$id], res$scaledResiduals)


# ==========================================


#for sim function
library(arm)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.null <- lm(weight ~ 1)

#from rethinking library for numerically stable log sums
log_sum_exp <- function (x) {
  xmax <- max(x)
  xsum <- sum(exp(x - xmax))
  xmax + log(xsum)
}


#function for WAIC from an LM
waic.lm <- function(mod, n.sims=1e3){
  mod_sims <- sim(mod, n.sims=10)
  mod_X <- model.matrix(mod)
  mod_Y <- mod$fitted.values+mod$residuals
  
  #generate distribution of observations
  pred_sims <- apply(mod_sims@coef, 1, function(b) mod_X %*% b )
  # pred_sims_err <- sapply(1:nrow(pred_sims), 
  #                     function(i) rnorm(ncol(pred_sims), pred_sims[i,], mod_sims@sigma[i]))
  
  #I hate nested loops, but it's expedient
  ll <- sapply(1:ncol(pred_sims), 
               function(j){
                 sapply(1:nrow(pred_sims),
                        function(i) dnorm(pred_sims[i,j], mod_Y[i], mod_sims@sigma[i], log=TRUE))
               })
  
  #get the things we'll need...
  lppd <- apply(ll, 1, function(arow) log_sum_exp(arow) - log(length(arow)))
  pWAIC <- apply(ll, 1, var)
  
  -2*sum(lppd) + 2*sum(pWAIC)             
}

waic.lm(lm.D9)
waic.lm(lm.null)















