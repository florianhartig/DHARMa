library(testthat)
library(DHARMa)
library(MASS)
library(lme4)
library(mgcv)
library(glmmTMB)
library(spaMM)
library(GLMMadaptive)


test_that("GLMMadaptive works",
          {
            set.seed(123L)
            skip_on_cran()
            
            data = createData(sampleSize = 500, overdispersion = 0, randomEffectVariance = 0.000, family = binomial())
            
            
            ##############
            ## binomial distribution
            fittedmodel1 <- mixed_model(fixed = observedResponse ~ Environment1 , random = ~ 1 | ID, data = data, family = binomial())
            
            ## adding a random effect
            fittedmodel2 <- mixed_model(fixed = observedResponse ~ Environment1 , random = ~ time || ID, data = data, family = binomial())
             
            
            # anova comparison   // causing warning: " model has not converged" 
            # anova(fittedmodel1, fittedmodel2)
            # anova(fittedmodel2, controllmodel)
            
            
            ##############
            ## poisson distribution
            ## a comparison of GLMMadaptive::mixed_model and glm
            fittedmodel1 <- mixed_model(fixed = observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data, family = poisson())
            
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

            # 
            fittedmodel5 <-  mixed_model(observedResponse ~ Environment1 * group, random = ~ 1 | group, data = data,
                                         family = poisson(), n_phis = 1,  initial_values = list("betas" = gaussian()))
            summary(fittedmodel5)
            resids_plot(fittedmodel5, data$observedResponse)
        
            
            ##############
            ## zero-inflated Poisson Mixed Model   //not working
            # fittedmodel2 <- mixed_model( observedResponse ~ Environment1 * group, random = ~ 1 | ID, data = data,
            #                   family = zi.poisson(),  zi_fixed = ~ Environment1)
            # resids_plot(fittedmodel2, data$observedResponse) 
            
            
            ## Zero-Inflated Poisson Mixed Model Extra Random Effects    //not working
            # fittedmodel3 <- mixed_model( observedResponse ~ Environment1 * group, random = ~ 1 | group, data = data,
            #                            family = zi.poisson(), zi_fixed = ~ Environment1, zi_random = ~ 1 | group)
            # resids_plot(fittedmodel3, data$observedResponse)
            
            
            ##############
            # negative-Binomial                           //not working
            #   fittedmodel <-  mixed_model(observedResponse ~ Environment1 * group, random = ~ 1 | group, data = data, 
            #                    family = negative.binomial(), n_phis = 1, initial_values = list("betas" = poisson()))
            
            
          }
)
