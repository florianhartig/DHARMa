# ISSUE 38

# Check different seeds for Pearson residuals with refit=T (Pearson Param Boot that doesn't depend on the quantile residuals) and compare it with KS test (that depends on quantile residuals), for increasing number of simulations

library(DHARMa)
library(tidyverse)

# varying parameters
nSims <- c(100,250,1000,10000)

#same data

testData <- createData(overdispersion = 0,
                       sampleSize = 100,
                       intercept = 0,
                       numGroups = 10,
                       randomEffectVariance = 0,
                       family = poisson())
#same model
fittedModel <-glm(observedResponse  ~ 
                    Environment1, data = testData, family = poisson()) 

out <- list()

for (k in 1:100) {

    out.sim <- data.frame(nSims=rep(NA,length(nSims)), 
                          DHA.p.val=rep(NA,length(nSims)),
                          KS.p.val=rep(NA,length(nSims)))
    for (i in 1:length(nSims)) {

      out.sim$nSims[i] = nSims[i]
      # DHARMa refit residuals -> bootstrapped Pearson
      res <- simulateResiduals(fittedModel, refit=F, seed = k, n = nSims[i])
      out.sim$DHA.p.val[i] <- testDispersion(res, plot = F, type = "DHARMa")$p.value
      out.sim$KS.p.val[i] <- testUniformity(res, plot=F)$p.value
    }
    out[[k]] <- out.sim  
}

res <- bind_rows(out, .id="seed") %>%
  pivot_longer(3:4, names_to = "test", values_to = "p.val")

ggplot(res, aes(x=as.factor(nSims), y=p.val, col=as.factor(nSims))) + geom_boxplot()+
  ggbeeswarm::geom_beeswarm(alpha=0.4)  + ylim(0,1) +
  facet_grid(~test)


##############################

# Comparison of the CRAN package assessor (DPIT residuals, Yang 2024,
# doi:10.1080/10618600.2024.2303336; GOF test, Yang, Genest & Neslehova 2026,
# doi:10.1002/cjs.70046) with DHARMa: example plots, type I error and power
# on simulated GLMs, and the dependence of DPIT residuals on the predictors.
# Checked with assessor 1.3.2 and DHARMa 0.5.1.

library(DHARMa)
library(assessor)
library(MASS)
library(pscl)
library(glmmTMB)
library(lme4)

n = 200      # observations per dataset
nRep = 200   # datasets per scenario
B = 100      # bootstrap samples for assessor::gof_disc

# Each scenario simulates data and returns the fitted model, or a list with
# the model for assessor (mA) and an equivalent one for DHARMa (mD) where the
# two packages support different classes
scenarios = list(

  pois_correct = function(){
    x = rnorm(n); y = rpois(n, exp(0.5 + x))
    glm(y ~ x, family = poisson)
  },

  pois_overdispersed = function(){
    x = rnorm(n); y = rnbinom(n, mu = exp(0.5 + x), size = 2)
    glm(y ~ x, family = poisson)
  },

  nb_correct = function(){
    x = rnorm(n); y = rnbinom(n, mu = exp(0.5 + x), size = 2)
    glm.nb(y ~ x)
  },

  pois_missingQuadratic = function(){
    x = runif(n, -1.5, 1.5); y = rpois(n, exp(0.5 + x + 0.5 * x^2))
    glm(y ~ x, family = poisson)
  },

  pois_zeroinflated = function(){
    x = rnorm(n); y = rpois(n, exp(0.5 + x)) * rbinom(n, 1, 0.7)
    glm(y ~ x, family = poisson)
  },

  zip_correct = function(){
    x = rnorm(n); y = rpois(n, exp(0.5 + x)) * rbinom(n, 1, 0.7)
    d = data.frame(x = x, y = y)
    list(mA = zeroinfl(y ~ x | 1, data = d, dist = "poisson"),
         mD = glmmTMB(y ~ x, ziformula = ~1, family = poisson, data = d))
  },

  binary_correct = function(){
    x1 = rnorm(n, 1); x2 = rbinom(n, 1, 0.7)
    y = rbinom(n, 1, plogis(-5 + 2 * x1 + x2 + 3 * x1 * x2))
    glm(y ~ x1 * x2, family = binomial)
  },

  binary_missingInteraction = function(){
    x1 = rnorm(n, 1); x2 = rbinom(n, 1, 0.7)
    y = rbinom(n, 1, plogis(-5 + 2 * x1 + x2 + 3 * x1 * x2))
    glm(y ~ x1 + x2, family = binomial)
  },

  glmm_pois_correct = function(){
    g = gl(20, n / 20); x = rnorm(n)
    y = rpois(n, exp(0.5 + x + rnorm(20, sd = 0.5)[g]))
    glmer(y ~ x + (1|g), family = poisson)
  }
)

# assessor has no method for mixed models; dpit_pois() is given the
# conditional fitted means as a workaround
getDpit = function(m) if(inherits(m, "merMod")) dpit_pois(getME(m, "y"), fitted(m)) else dpit(m)

fitScenario = function(s){
  fit = scenarios[[s]]()
  if(inherits(fit, "list")) fit else list(mA = fit, mD = fit)
}

# seed = FALSE, as recommended in ?simulateResiduals for simulation studies
runOne = function(fit){
  r = residuals(getDpit(fit$mA), scale = "uniform")
  res = simulateResiduals(fit$mD, seed = FALSE)
  c(assessor_KS = suppressWarnings(ks.test(r, "punif")$p.value),
    assessor_gof = if(inherits(fit$mA, "merMod")) NA else gof_disc(fit$mA, B = B)$p.value,
    DHARMa_uniformity = testUniformity(res, plot = FALSE)$p.value,
    DHARMa_dispersion = testDispersion(res, plot = FALSE)$p.value,
    DHARMa_zeroInflation = testZeroInflation(res, plot = FALSE)$p.value,
    DHARMa_quantiles = testQuantiles(res, plot = FALSE)$p.value)
}

# Example plots: one dataset per scenario, DPIT QQ plot (uniform scale)
# next to the DHARMa QQ plot and residual-vs-predicted plot

set.seed(1)
pdf("assessor_vs_DHARMa_examples.pdf", width = 12, height = 4)
for(s in names(scenarios)){
  fit = fitScenario(s)
  res = simulateResiduals(fit$mD)
  par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))
  plot(getDpit(fit$mA), scale = "uniform", main = "assessor DPIT QQ")
  plotQQunif(res)
  plotResiduals(res)
  mtext(s, outer = TRUE)
}
dev.off()

# Type I error and power: proportion of p < 0.05 over nRep datasets

set.seed(123)
results = sapply(names(scenarios), function(s) colMeans(t(replicate(nRep, runOne(fitScenario(s)))) < 0.05))
round(t(results), 2)

# Result (8 Nov 2026):
#                           assessor_KS assessor_gof DHARMa_uniformity DHARMa_dispersion DHARMa_zeroInflation DHARMa_quantiles
# pois_correct                     0.00         0.06              0.00              0.06                 0.01             0.04
# pois_overdispersed               0.79         0.98              0.87              1.00                 0.79             1.00
# nb_correct                       0.00         0.04              0.00              0.01                 0.00             0.06
# pois_missingQuadratic            0.17         0.66              0.03              0.59                 0.49             1.00
# pois_zeroinflated                0.36         1.00              0.73              0.99                 1.00             0.88
# zip_correct                      0.00         0.04              0.00              0.00                 0.00             0.06
# binary_correct                   0.00         0.06              0.02              0.00                 0.00             0.16
# binary_missingInteraction        0.00         0.14              0.04              0.00                 0.00             0.16
# glmm_pois_correct                0.00           NA              0.00              0.00                 0.00             0.06

# DPIT residuals are uniform only marginally, not conditional on the
# predictors: under the correct model they trend with x, and with only
# factor covariates they are not uniform at all

set.seed(5)
out = t(replicate(nRep, {
  x = rnorm(n); f = gl(2, n / 2)
  m = glm(rpois(n, exp(0.5 + x)) ~ x, family = poisson)
  mf = glm(rpois(n, c(2, 5)[f]) ~ f, family = poisson)
  c(DPIT_trend = cor.test(residuals(dpit(m), "uniform"), x)$p.value,
    DHARMa_trend = cor.test(residuals(simulateResiduals(m, seed = FALSE)), x)$p.value,
    DPIT_KS_factorOnly = suppressWarnings(ks.test(residuals(dpit(mf), "uniform"), "punif")$p.value),
    DHARMa_KS_factorOnly = testUniformity(simulateResiduals(mf, seed = FALSE), plot = FALSE)$p.value)
}))
colMeans(out < 0.05)
# Result: DPIT_trend 1, DHARMa_trend 0, DPIT_KS_factorOnly 1, DHARMa_KS_factorOnly 0
