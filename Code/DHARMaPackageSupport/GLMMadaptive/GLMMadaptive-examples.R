devtools::install_github("drizopoulos/GLMMadaptive")
library(GLMMadaptive)
library(DHARMa)

# GLMMadaptive fits mixed effects models by using the adaptive Gauss-Hermite quadrature rule
# see help GLMMadaptive for further information


# simulate some data
set.seed(123L)

data = createData(sampleSize = 500, overdispersion = 0, randomEffectVariance = 0.000, family = binomial())

summary(data)


fm <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data, family = binomial())

# model coefficients
coef(fm) 

# fixed effects
fixef(fm)

# extract conditional modes of random effects
ranef(fm)

# extracts fitted values from objects returned by modeling functions
fitted(fm)

# extracts model residuals
residuals(fm)

#  simulate responses from the distribution corresponding to a fitted model
simulate(fm) 


#
#
## binomial distribution
## a comparison of GLMMadaptive::mixed_model and glm
fittedmodel1 <- mixed_model(fixed = observedResponse ~ Environment1 , random = ~ 1 | ID, data = data, family = binomial())
# detailed output
summary(fittedmodel1)

# controll model glm
controllmodel <- glm(observedResponse ~ Environment1 , data = data, family = binomial())
anova(fittedmodel1, controllmodel)

 

## adding a random effect
fittedmodel2 <- mixed_model(fixed = observedResponse ~ Environment1 , random = ~ time || ID, data = data, family = binomial())
summary(fittedmodel2)

# anova comparison
# anova(fittedmodel1, fittedmodel2)
# anova(fittedmodel2, controllmodel)




## poisson distribution
## a comparison of GLMMadaptive::mixed_model and glm
fittedmodel1 <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data, family = poisson())
summary(fittedmodel1)

# controll model glm
controllmodel <- glm(observedResponse ~ Environment1 * group, data = data, family = poisson())
anova(fittedmodel1, controllmodel)


#
## adding a random effect
fittedmodel2 <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ time || ID, data = data, family = poisson())
summary(fittedmodel2)

# anova comparison
anova(fittedmodel2, controllmodel)
anova(fittedmodel1, fittedmodel2)


## Penalized Mixed Effects Poisson Regression
fittedmodel3 <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data,
                   family = poisson(), penalized = TRUE)

# ajusting penalized degree of freedom
fittedmodel4 <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data,
                            family = poisson(), penalized = list(pen_mu = 0, pen_sigma = 1, pen_df = 200))

#resid_plot function original by GLMMadaptive (see also: https://github.com/drizopoulos/GLMMadaptive/blob/master/vignettes/Goodness_of_Fit.Rmd)
# function supporting to plot the GLMMadaptive models

resids_plot <- function (object, y, nsim = 1000,
                         type = c("subject_specific", "mean_subject"),
                         integerResponse = NULL) {
  if (!inherits(object, "MixMod"))
    stop("this function works for 'MixMod' objects.\n")
  type <- match.arg(type)
  if (is.null(integerResponse)) {
    integer_families <- c("binomial", "poisson", "negative binomial",
                          "zero-inflated poisson", "zero-inflated negative binomial", 
                          "hurdle poisson", "hurdle negative binomial")
    numeric_families <- c("hurdle log-normal", "beta", "hurdle beta", "Gamma")
    if (object$family$family %in% integer_families) {
      integerResponse <- TRUE
    } else if (object$family$family %in% numeric_families) {
      integerResponse <- FALSE
    } else {
      stop("non build-in family object; you need to specify the 'integerResponse',\n",
           "\targument indicating whether the outcome variable is integer or not.\n")
    }
  }
  sims <- simulate(object, nsim = nsim, type = type)
  fits <- fitted(object, type = type)
  dharmaRes <- DHARMa::createDHARMa(simulatedResponse = sims, observedResponse = y, 
                                    fittedPredictedResponse = fits, 
                                    integerResponse = integerResponse)
  DHARMa:::plot.DHARMa(dharmaRes, quantreg = FALSE)
}

# plotting the mixed models
summary(fittedmodel3)
resids_plot(fittedmodel3, data$observedResponse)

# detailed output
summary(fittedmodel4)
resids_plot(fittedmodel4, data$observedResponse)


#
## negative binomial   // in progress
# fittedmodel5 <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data, family  = zi.negative.binomial(), zi_fixed = ~ group, zi_random = ~ 1 | ID )
# summary(fittedmodel5)




#
#
### further examples for GLMMadaptive, see also (https://github.com/drizopoulos/GLMMadaptive)

# simulate data
n <- 100 # number of subjects
K <- 8 # number of measurements per subject
t_max <- 15 # maximum follow-up time


DF <- data.frame(id = rep(seq_len(n), each = K),
                 time = c(replicate(n, c(0, sort(runif(K - 1, 0, t_max))))),
                 sex = rep(gl(2, n/2, labels = c("male", "female")), each = K))

# design matrices for the fixed and random effects
X <- model.matrix(~ sex * time, data = DF)
Z <- model.matrix(~ time, data = DF)

betas <- c(-2.13, -0.25, 0.24, -0.05) # fixed effects coefficients
D11 <- 0.48 # variance of random intercepts
D22 <- 0.1 # variance of random slopes

# we simulate random effects
b <- cbind(rnorm(n, sd = sqrt(D11)), rnorm(n, sd = sqrt(D22)))
# linear predictor
eta_y <- drop(X %*% betas + rowSums(Z * b[DF$id, ]))
# we simulate binary longitudinal data
DF$y <- rbinom(n * K, 1, plogis(eta_y))




### fitting the mixed effect model
fm1 <- mixed_model(fixed = y ~ sex * time, random = ~ 1 | id, data = DF,family = binomial())
summary(fm1)
resids_plot(fm1,DF$y)


fm2 <- mixed_model(fixed = y ~ sex * time, random = ~ time || id, data = DF, family = binomial())

anova(fm1,fm2)
resids_plot(fm2,DF$y)