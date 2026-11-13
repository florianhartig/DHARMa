devtools::install_github("drizopoulos/GLMMadaptive")
library(GLMMadaptive)
library(lme4)
library(DHARMa)

# GLMMadaptive fits mixed effects models by using the adaptive Gauss-Hermite quadrature rule
# see help GLMMadaptive for further information


#' simulate some data 
#' first running BINOMIAL  # Line 27
#' secound running POISSON # Line 171
#' third running  glmmAdaptive exaplme binomial  # Line 348
#' fourth running glmmAdaptive examples with Zero-Inflated Negative Binomial # Line 488
set.seed(123L)

data = createData(sampleSize = 500, overdispersion = 0.0, randomEffectVariance = 0.000, family = binomial())

testmodel_binomial <- mixed_model(fixed = observedResponse ~ Environment1 , random = ~ 1 | ID, data = data, family = binomial())





#'
#'
########## binomial family
summary(testmodel_binomial)

simulate(testmodel_binomial)
class(testmodel_binomial)

res <- simulateResiduals(testmodel_binomial)
plot(res)

##### testing different DHARMa functions  
# testing for getFitted   
getFitted(testmodel_binomial)  
 

# testing for the fixedEffects (extract fixed effects of a model)
getFixedEffects(testmodel_binomial)


# testing for the Observed Response     
DHARMa::getObservedResponse(testmodel_binomial)


# testing for getRefit,           
refit_binomial = getRefit(testmodel_binomial, getObservedResponse(testmodel_binomial))
refit_binomial


# testing for simulations (simulate from a fitted model)
getSimulations(testmodel_binomial, nsim = 2, type = "normal") # type =  if simulations should be prepared for getQuantile or for refit


#####       Testing refit-binomial
getObservedResponse(testmodel_binomial)

# predictions of the model for these points
getFitted(testmodel_binomial)

# extract simulations from the model as matrix
getSimulations(testmodel_binomial, nsim = 2)

# extract simulations from the model for refit (often requires different structure)
x = getSimulations(testmodel_binomial, nsim = 2, type = "refit")


getRefit(testmodel_binomial, x[[1]])        
getRefit(testmodel_binomial, getObservedResponse(testmodel_binomial))


###### testing outliers
# testing for outliers  (reports the outlier of an object)
outliers(testmodel_binomial)


###### testing for simulateResiduals
residuals.DHARMa(testmodel_binomial)  # extracted Residuals "NULL"

simulateResiduals(testmodel_binomial)

residuals.DHARMa( simulateResiduals(testmodel_binomial, arg="mean_subject")  )
residuals.DHARMa( simulateResiduals(testmodel_binomial, arg="mean_subject", method = "traditional")  )


#######generation the simulation Output
simulationOutput_binomial <-  simulateResiduals(fittedModel = testmodel_binomial) 


###### testing plotQQunif  
plotQQunif(  simulationOutput_binomial )


# testing plotResiduals function     
plotResiduals(simulationOutput_binomial , rank = TRUE, quantreg = FALSE)

plotResiduals(simulationOutput_binomial , quantreg = NULL)


# recalculate Residuals of the model
residuals(simulationOutput_binomial)
residuals( recalculateResiduals(simulationOutput_binomial) )  # 100% equal



####### testDispersion  - tests if the simulated dispersion is equal to the observed dispersion
## testing binomial 
DHARMa::testDispersion(simulationOutput_binomial, alternative = "two.sided", plot = T, type = "DHARMa")
#DHARMa::testDispersion(simulationOutput_binomial, alternative = "greater", plot = T, type = "PearsonChisq") # Causing Error; type ?

plot(simulationOutput_binomial)
DHARMa::testDispersion(simulationOutput_binomial, alternative = "greater")
DHARMa::testDispersion(simulationOutput_binomial, alternative = "less")


###### testGeneric  - test if a generic summary statistics (user-defined) deviates from model expectations
## testing binomial
countOnes <- function(x) sum(x == 1)
testGeneric(simulationOutput_binomial, summary = countOnes , alternative = "two.sided")
testGeneric(simulationOutput_binomial, summary = countOnes , alternative = "greater")
testGeneric(simulationOutput_binomial, summary = countOnes , alternative = "less")


# testOutlier - tests if there are more simulation outliers than expected
DHARMa::testOutliers(simulationOutput_binomial)


###### testQuantiles  - fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
DHARMa::testQuantiles(simulationOutput_binomial)


####### testResiduals 
## testing binomial
DHARMa::testResiduals(simulationOutput_binomial)

# testUniformity - tests if the overall distribution conforms to expectations
DHARMa::testUniformity(simulationOutput_binomial)

# testDispersion
DHARMa::testDispersion(simulationOutput_binomial)


######  testSimulatedResiduals 
DHARMa::testSimulatedResiduals(simulationOutput_binomial)


##### testSpatialAutocorrelation 
DHARMa::testSpatialAutocorrelation(testmodel_binomial, x =  data$x, y =  data$y)
DHARMa::testSpatialAutocorrelation(simulationOutput_binomial, x =  data$x, y =  data$y)


##### testSpatialAutocorrelation  - tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function, 
# for example to test for phylogenetic signal in the residuals
DHARMa::testTemporalAutocorrelation(testmodel_binomial, time = data$time)


###### testZeroInflation - tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(simulationOutput_binomial)









########## poisson family
#'
#'
#'
#'
#'
#'
#'
########  # simulate some data
#

testData = createData(sampleSize = 500, family = poisson(), randomEffectVariance = 1, overdispersion = 0.2)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), family = "poisson", data = testData)

testmodel_poisson <- mixed_model(fixed = observedResponse ~ Environment1 * group + group^2, random = ~ 1 | ID, data = testData, family = poisson())


summary(testmodel_poisson)

simulate(testmodel_poisson)
class(testmodel_poisson)

res2 <- simulateResiduals(testmodel_poisson)
plot(res)


##### testing different DHARMa functions  
# testing for get fitted   
getFitted(testmodel_poisson)  


# testing for the fixedEffects (extract fixed effects of a model)
getFixedEffects(testmodel_poisson)


# testing for the Observed Response     
DHARMa::getObservedResponse(testmodel_poisson)


# testing for getRefit,           
refit_poisson = getRefit(testmodel_poisson, getObservedResponse(testmodel_poisson))
refit_poisson


# testing for simulations (simulate from a fitted model)
getSimulations(testmodel_poisson, nsim = 2, type = "normal") # type =  if simulations should be prepared for getQuantile or for refit


#####       Testing refit-poisson
getObservedResponse(testmodel_poisson)

# predictions of the model for these points
getFitted(testmodel_poisson)

# extract simulations from the model as matrix
getSimulations(testmodel_poisson, nsim = 2)

# extract simulations from the model for refit (often requires different structure)
x = getSimulations(testmodel_poisson, nsim = 2, type = "refit")


getRefit(testmodel_poisson, x[[1]])      
getRefit(testmodel_poisson, getObservedResponse(testmodel_poisson))


###### testing outliers
# testing for outliers  (reports the outlier of an object)
outliers(testmodel_poisson)


###### testing for simulateResiduals
residuals.DHARMa(testmodel_poisson)  # extracted Residuals "NULL"

simulateResiduals(testmodel_poisson)

residuals.DHARMa( simulateResiduals(testmodel_poisson, arg="mean_subject")  )
residuals.DHARMa( simulateResiduals(testmodel_poisson, arg="mean_subject", method = "traditional")  )


#######generation the simulation Output
simulationOutput_poisson <-  simulateResiduals(fittedModel = testmodel_poisson) 


###### testing plotQQunif  
plotQQunif(  simulationOutput_poisson )
plotQQunif(  testmodel_poisson )


# testing plotResiduals function     
plotResiduals(simulationOutput_poisson , rank = TRUE, quantreg = FALSE)

plotResiduals(simulationOutput_poisson , quantreg = NULL)


# recalculate Residuals of the model
residuals(testmodel_poisson)
residuals( recalculateResiduals(testmodel_poisson) )  # 100% equal



####### testDispersion  - tests if the simulated dispersion is equal to the observed dispersion
## testing poisson 
DHARMa::testDispersion(simulationOutput_poisson, alternative = "two.sided", plot = T, type = "DHARMa")
#DHARMa::testDispersion(simulationOutput_poisson, alternative = "greater", plot = T, type = "PearsonChisq") # Causing Error; type ?

plot(simulationOutput_poisson)
DHARMa::testDispersion(simulationOutput_poisson, alternative = "greater")
DHARMa::testDispersion(simulationOutput_poisson, alternative = "less")


###### testGeneric  - test if a generic summary statistics (user-defined) deviates from model expectations
## testing poisson
countOnes <- function(x) sum(x == 1)
testGeneric(simulationOutput_poisson, summary = countOnes , alternative = "two.sided")
testGeneric(simulationOutput_poisson, summary = countOnes , alternative = "greater")
testGeneric(simulationOutput_poisson, summary = countOnes , alternative = "less")


# testOutlier - tests if there are more simulation outliers than expected
DHARMa::testOutliers(simulationOutput_poisson)

###### testQuantiles - fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
DHARMa::testQuantiles(simulationOutput_poisson)


####### testResiduals 
## testing poisson
DHARMa::testResiduals(simulationOutput_poisson)

# testUniformity - tests if the overall distribution conforms to expectations
DHARMa::testUniformity(simulationOutput_poisson)

# testDispersion
DHARMa::testDispersion(simulationOutput_poisson)
DHARMa::testDispersion(simulationOutput_poisson)

simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, re.form = NULL)
testDispersion(simulationOutput2)


testDispersion(simulationOutput2, type = "PearsonChisq", alternative = "greater")



######  testSimulatedResiduals 
DHARMa::testSimulatedResiduals(simulationOutput_poisson)


##### testSpatialAutocorrelation  - tests for spatial autocorrelation in the residuals. Can also be used with a generic distance function, 
# for example to test for phylogenetic signal in the residuals
DHARMa::testSpatialAutocorrelation(testmodel_poisson, x =  data$x, y =  data$y)
DHARMa::testSpatialAutocorrelation(simulationOutput_poisson, x =  data$x, y =  data$y)


###### testTemporalAutocorrelation - tests for temporal autocorrelation in the residuals
DHARMa::testTemporalAutocorrelation(testmodel_poisson, time = data$time)


###### testZeroInflation - tests if there are more zeros in the data than expected from the simulations
DHARMa::testZeroInflation(simulationOutput_poisson)








#'
#'
#'
#'
#'
#'
#'
#'
##### Running glmmAdaptive examples
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

DF


### fitting the mixed effect model
fm2 <- mixed_model(fixed = y ~ sex * time, random = ~ 1 | id, data = DF,family = binomial())


### testing with DHARMa
# predictions of the model for these points
getFitted(fm2)

# extract simulations from the model as matrix
getSimulations(fm2, nsim = 2)

# extract simulations from the model for refit (often requires different structure)
x = getSimulations(fm2, nsim = 2, type = "refit")

getRefit(fm2, x[[1]])
getRefit(fm2, getObservedResponse(fm2))




###### testing outliers
# testing for outliers  (reports the outlier of an object)
outliers(fm2)


###### testing for simulateResiduals
# 
residuals.DHARMa(fm2)

simulateResiduals(fm2) 


residuals.DHARMa( simulateResiduals(fm2, arg="mean_subject", method = "PIT")   ) # PIT as dafault
residuals.DHARMa( simulateResiduals(fm2, arg="mean_subject", method = "traditional")  )


#######generation the simulation Output
simulationOutput <-  simulateResiduals(fittedModel = fm2) 


###### testing plotQQunif  
plotQQunif(  simulationOutput )


# testing plotResiduals function     
plotResiduals(simulationOutput , rank = TRUE, quantreg = FALSE)

plotResiduals(simulationOutput , quantreg = NULL)
plotResiduals(simulationOutput , form = NULL, quantreg = NULL, rank = T, asFactor = NULL, smoothScatter = NULL, quantiles = c(0.25, 0.5, 0.75))
plotResiduals(simulationOutput , form = NULL, quantreg = NULL, rank = F,  smoothScatter = TRUE, quantiles = c(0.21, 0.5, 0.8))


# recalculate Residuals of the model
residuals(fm2)
residuals( recalculateResiduals(fm2) )  # 100% equal




####### testDispersion 
DHARMa::testDispersion(simulationOutput, alternative = "two.sided", plot = T, type = "DHARMa")
#DHARMa::testDispersion(simulationOutput, alternative = "greater", plot = T, type = "PearsonChisq") # Causing Error; type ?

plot(simulationOutput)
DHARMa::testDispersion(simulationOutput, alternative = "greater")
DHARMa::testDispersion(simulationOutput, alternative = "less")



###### testGeneric
countOnes <- function(x) sum(x == 1)
testGeneric(simulationOutput, summary = countOnes , alternative = "two.sided")
testGeneric(simulationOutput, summary = countOnes , alternative = "greater")
testGeneric(simulationOutput, summary = countOnes , alternative = "less")


###### testing for outliers 
DHARMa::testOutliers(fm2)

###### testQuantiles 
DHARMa::testQuantiles(fm2)

###### testResiduals 
DHARMa::testResiduals(fm2)

###### testUniformity
DHARMa::testUniformity(fm2)

###### testDispersion
DHARMa::testDispersion(fm2)

###### testOutlier
DHARMa::testOutliers(fm2)

###### testSimulatedResiduals 
DHARMa::testSimulatedResiduals(fm2)

###### testZeroInflation 
DHARMa::testZeroInflation(fm2)






#'
#'
#'
#'
#'
##### Running glmmAdaptive examples with Zero-Inflated Negative Binomial data

n <- 300 # number of subjects
K <- 8 # number of measurements per subject
t_max <- 5 # maximum follow-up time

# we construct a data frame with the design: 
# everyone has a baseline measurement, and then measurements at random follow-up times
DF <- data.frame(id = rep(seq_len(n), each = K),
                 time = c(replicate(n, c(0, sort(runif(K - 1, 0, t_max))))),
                 sex = rep(gl(2, n/2, labels = c("male", "female")), each = K))

# design matrices for the fixed and random effects non-zero part
X <- model.matrix(~ sex * time, data = DF)
Z <- model.matrix(~ 1, data = DF)
# design matrices for the fixed and random effects zero part
X_zi <- model.matrix(~ sex, data = DF)
Z_zi <- model.matrix(~ 1, data = DF)

betas <- c(1.5, 0.05, 0.05, -0.03) # fixed effects coefficients non-zero part
shape <- 2 # shape/size parameter of the negative binomial distribution
gammas <- c(-1.5, 0.5) # fixed effects coefficients zero part
D11 <- 0.5 # variance of random intercepts non-zero part
D22 <- 0.4 # variance of random intercepts zero part

# we simulate random effects
b <- cbind(rnorm(n, sd = sqrt(D11)), rnorm(n, sd = sqrt(D22)))
# linear predictor non-zero part
eta_y <- as.vector(X %*% betas + rowSums(Z * b[DF$id, 1, drop = FALSE]))
# linear predictor zero part
eta_zi <- as.vector(X_zi %*% gammas + rowSums(Z_zi * b[DF$id, 2, drop = FALSE]))
# we simulate negative binomial longitudinal data
DF$y <- rnbinom(n * K, size = shape, mu = exp(eta_y))
# we set the extra zeros
DF$y[as.logical(rbinom(n * K, size = 1, prob = plogis(eta_zi)))] <- 0




#### running DHARMa functions
fm2 <- mixed_model(y ~ sex * time, random = ~ 1 | id, data = DF,
                   family = poisson())



#####
#######generation the simulation Output
simulationOutput4 <-  simulateResiduals(fittedModel = fm2) 


###### testing plotQQunif  
plotQQunif(  simulationOutput4 )


# testing plotResiduals function     
plotResiduals(simulationOutput4 , rank = TRUE, quantreg = FALSE)

plotResiduals(simulationOutput4 , quantreg = NULL)


####### testDispersion 
DHARMa::testDispersion(simulationOutput4)

###### testGeneric
countOnes <- function(x) sum(x == 1)
testGeneric(simulationOutput4, summary = countOnes )



###### testing for outliers 
DHARMa::testOutliers(fm2)

###### testQuantiles 
DHARMa::testQuantiles(fm2)

###### testResiduals 
DHARMa::testResiduals(fm2)

###### testUniformity
DHARMa::testUniformity(fm2)

###### testDispersion
DHARMa::testDispersion(fm2)

###### testOutlier
DHARMa::testOutliers(fm2)

###### testSimulatedResiduals 
DHARMa::testSimulatedResiduals(fm2)

###### testZeroInflation 
DHARMa::testZeroInflation(fm2)


