# Tests / Examples using glmmTMB with DHARMa
Florian Hartig  
3/27/2018  




```r
library(DHARMa)
library(glmmTMB)
```

# Testing general package behavior / general problems

## Problems with binomial

This crashes my R (memory allocation issue) on the current CRAN version (27.3.18). Installing the development version on GitHub fixes the problem. sessionInfo() at the end of the document


```r
library(glmmTMB)
data(cbpp, package="lme4")
m <- glmmTMB(cbind(incidence, size-incidence) ~ period + (1 | herd), data=cbpp, family=binomial)
summary(m)
predict(m)
```

## Residual problems

* residual function doesn't work with factor response (bug)
* pearson residuals don't work with zi terms (not implemented)

## with ~reform in pred / simulate

Currently, glmmTMB doesn't support the reform argument. Also, 

* predict() is conditional on all random effects, corresponding to lme4 re.form = NULL
* simulate() is unconditional, i.e. all random effects will be resimulated, corresponding to lme4 re.form = 0

all predictions and simulations are conditional on REs, problematic for some of the residual tests, see https://github.com/florianhartig/DHARMa/issues/43


```r
m1 <- glmmTMB(count~ mined + (1|site), family=poisson, data=Salamanders)

#pred = predict(m1, re.form = 0)
pred = predict(m1)
```


# DHARMa with glmmTMB example cases 

## Test 1 - Simple poisson model


```r
m <- glmmTMB(count~ mined, family=poisson, data=Salamanders)
summary(m)
```

```
##  Family: poisson  ( log )
## Formula:          count ~ mined
## Data: Salamanders
## 
##      AIC      BIC   logLik deviance df.resid 
##   2302.5   2311.4  -1149.2   2298.5      642 
## 
## 
## Conditional model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.2192     0.1048  -11.63   <2e-16 ***
## minedno       2.0368     0.1109   18.36   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, asFactor = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
### creating new data based on the fitted models. Residuals should now be perfect

Salamanders$count2 = simulate(m)$sim_1

m <- glmmTMB(count2~ mined, family=poisson, data=Salamanders)

res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, asFactor = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-4-2.png)<!-- -->


## Test 2 - Poisson + RE


```r
m <- glmmTMB(count~ mined + (1|site), family=poisson, data=Salamanders)
summary(m)
```

```
##  Family: poisson  ( log )
## Formula:          count ~ mined + (1 | site)
## Data: Salamanders
## 
##      AIC      BIC   logLik deviance df.resid 
##   2215.7   2229.1  -1104.8   2209.7      641 
## 
## Random effects:
## 
## Conditional model:
##  Groups Name        Variance Std.Dev.
##  site   (Intercept) 0.3316   0.5759  
## Number of obs: 644, groups:  site, 23
## 
## Conditional model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -1.5053     0.2230  -6.749 1.49e-11 ***
## minedno       2.2644     0.2803   8.080 6.49e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, rank = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
### creating new data based on the fitted models. Residuals should now be perfect, except for the reform pattern

Salamanders$count2 = simulate(m)$sim_1

m <- glmmTMB(count2~ mined + (1|site), family=poisson, data=Salamanders)

res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, rank = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

Fit is still fine, but note the pattern in the residual vs. predicted, which is caused by the reform problem (REs are included in the predictions)

If we calculate unconditional REs by hand, pattern disappears


```r
x = fixef(m)
pred = x$cond[1] + x$cond[2]*as.numeric(Salamanders$mined)
plotResiduals(pred, res$scaledResiduals, asFactor = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```
## NULL
```



## Test 3 - Binomial models

Binomial required a few adjustments, because the simulate function in glmmTMB doesn't behave like glm / glmer, which returns lists of for the n/k case, and a vector instead

standard binomial with DHARMa test data


```r
testData = createData(sampleSize = 100, fixedEffects = 2, family = binomial(), randomEffectVariance = 0)

m <- glmmTMB(observedResponse ~ Environment1 , data=testData, family=binomial)
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res)
```

![](glmmTMB_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

with a missing predictor


```r
testData = createData(sampleSize = 100, fixedEffects = 2, family = binomial(), randomEffectVariance = 0)

m <- glmmTMB(observedResponse ~ 1 , data=testData, family=binomial)
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res)
```

```
## DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T
```

```
## DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

![](glmmTMB_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
plotResiduals(testData$Environment1, res$scaledResiduals)
```

![](glmmTMB_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

n/k example from the help


```r
data(cbpp, package="lme4")

newDat = cbpp
newDat$response = cbind(newDat$incidence, newDat$size-newDat$incidence)
m <- glmmTMB(response ~ period , data=newDat, family=binomial)
m
```

```
## Formula:          response ~ period
## Data: newDat
##      AIC      BIC   logLik df.resid 
## 206.0584 214.1598 -99.0292       52 
## 
## Number of obs: 56
## 
## Fixed Effects:
## 
## Conditional model:
## (Intercept)      period2      period3      period4  
##      -1.269       -1.171       -1.301       -1.782
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, rank = T)
```

```
## DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T
```

```
## DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T
```

![](glmmTMB_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
### creating new data based on the fitted models. Residuals should now be perfect

# R problem - sometimes newDat$newResponse is updated, and sometimes not ... I have no idea why

newDat$newResponse = as.matrix(simulate(m))
newDat$newResponse
```

```
##       sim_1.1 sim_1.2
##  [1,]       1      13
##  [2,]       2      10
##  [3,]       0       9
##  [4,]       0       5
##  [5,]       7      15
##  [6,]       2      16
##  [7,]       0      21
##  [8,]       5      17
##  [9,]       2      14
## [10,]       0      16
## [11,]       0      20
## [12,]       3       7
## [13,]       0      10
## [14,]       1       8
## [15,]       0       6
## [16,]       6      12
## [17,]       0      25
## [18,]       1      23
## [19,]       0       4
## [20,]       4      13
## [21,]       4      13
## [22,]       0      18
## [23,]       3      17
## [24,]       4      12
## [25,]       0      10
## [26,]       1       8
## [27,]       0       5
## [28,]       6      28
## [29,]       2       7
## [30,]       0       6
## [31,]       0       8
## [32,]       0       6
## [33,]       2      20
## [34,]       2      20
## [35,]       2      16
## [36,]       0      22
## [37,]       5      20
## [38,]       3      24
## [39,]       2      20
## [40,]       1      21
## [41,]       2       8
## [42,]       0       8
## [43,]       1       5
## [44,]       0       5
## [45,]       6      15
## [46,]       6      18
## [47,]       0      19
## [48,]       0      23
## [49,]       5      14
## [50,]       0       2
## [51,]       0       3
## [52,]       0       2
## [53,]       4      15
## [54,]       1      14
## [55,]       1      14
## [56,]       1      14
```

```r
m2 <- glmmTMB(newResponse ~ period , data=newDat, family=binomial)
m2
```

```
## Formula:          newResponse ~ period
## Data: newDat
##       AIC       BIC    logLik  df.resid 
## 149.25753 157.35894 -70.62877        52 
## 
## Number of obs: 56
## 
## Fixed Effects:
## 
## Conditional model:
## (Intercept)      period2      period3      period4  
##     -1.2481      -0.9078      -1.7911      -2.1531
```

```r
res = simulateResiduals(m2)
```

```
## Warning in simulateResiduals(m2): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, rank = T)
```

```
## DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T
## DHARMa::plotResiduals - low number of unique predictor values, consider setting asFactor = T
```

![](glmmTMB_files/figure-html/unnamed-chunk-9-2.png)<!-- -->


## Test 4 - Zero-inflated negative binomial models

no RE to test ZI intercept only


```r
m <- glmmTMB(count~1 , zi=~1, family=nbinom1, Salamanders)
summary(m)
```

```
##  Family: nbinom1  ( log )
## Formula:          count ~ 1
## Zero inflation:         ~1
## Data: Salamanders
## 
##      AIC      BIC   logLik deviance df.resid 
##   1914.4   1927.8   -954.2   1908.4      641 
## 
## 
## Overdispersion parameter for nbinom1 family (): 2.86 
## 
## Conditional model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   0.7251     0.1479   4.902  9.5e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.5784     0.3757   -1.54    0.124
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, asFactor = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
### creating new data based on the fitted models. Residuals should now be perfect

Salamanders$count2 = simulate(m)$sim_1
m <- glmmTMB(count2~1 , zi=~1, family=nbinom1, Salamanders)
summary(m)
```

```
##  Family: nbinom1  ( log )
## Formula:          count2 ~ 1
## Zero inflation:          ~1
## Data: Salamanders
## 
##      AIC      BIC   logLik deviance df.resid 
##   1844.2   1857.6   -919.1   1838.2      641 
## 
## 
## Overdispersion parameter for nbinom1 family (): 1.75 
## 
## Conditional model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   0.8133     0.1043   7.795 6.43e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -0.1400     0.2006  -0.698    0.485
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, asFactor = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

Example from the help


```r
m <- glmmTMB(count~spp + mined + (1|site), zi=~spp + mined, family=nbinom2, Salamanders)
summary(m)
```

```
##  Family: nbinom2  ( log )
## Formula:          count ~ spp + mined + (1 | site)
## Zero inflation:         ~spp + mined
## Data: Salamanders
## 
##      AIC      BIC   logLik deviance df.resid 
##   1670.3   1750.7   -817.1   1634.3      626 
## 
## Random effects:
## 
## Conditional model:
##  Groups Name        Variance Std.Dev.
##  site   (Intercept) 0.1443   0.3799  
## Number of obs: 644, groups:  site, 23
## 
## Overdispersion parameter for nbinom2 family (): 1.51 
## 
## Conditional model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -0.6104     0.4052  -1.506  0.13197    
## sppPR        -0.9637     0.6436  -1.497  0.13430    
## sppDM         0.1707     0.2353   0.725  0.46828    
## sppEC-A      -0.3871     0.3424  -1.130  0.25836    
## sppEC-L       0.4879     0.2383   2.047  0.04062 *  
## sppDES-L      0.5895     0.2278   2.588  0.00965 ** 
## sppDF        -0.1133     0.2439  -0.464  0.64232    
## minedno       1.4294     0.3666   3.899 9.66e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   0.9100     0.6279   1.449    0.147    
## sppPR         1.1614     1.3346   0.870    0.384    
## sppDM        -0.9393     0.8005  -1.173    0.241    
## sppEC-A       1.0424     0.7140   1.460    0.144    
## sppEC-L      -0.5623     0.7263  -0.774    0.439    
## sppDES-L     -0.8930     0.7535  -1.185    0.236    
## sppDF        -2.5398     2.1817  -1.164    0.244    
## minedno      -2.5630     0.6045  -4.240 2.24e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, rank = T)

### creating new data based on the fitted models. Residuals should now be perfect

Salamanders$count2 = simulate(m)$sim_1
m <- glmmTMB(count~spp + mined + (1|site), zi=~spp + mined, family=nbinom2, Salamanders)
summary(m)
```

```
##  Family: nbinom2  ( log )
## Formula:          count ~ spp + mined + (1 | site)
## Zero inflation:         ~spp + mined
## Data: Salamanders
## 
##      AIC      BIC   logLik deviance df.resid 
##   1670.3   1750.7   -817.1   1634.3      626 
## 
## Random effects:
## 
## Conditional model:
##  Groups Name        Variance Std.Dev.
##  site   (Intercept) 0.1443   0.3799  
## Number of obs: 644, groups:  site, 23
## 
## Overdispersion parameter for nbinom2 family (): 1.51 
## 
## Conditional model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -0.6104     0.4052  -1.506  0.13197    
## sppPR        -0.9637     0.6436  -1.497  0.13430    
## sppDM         0.1707     0.2353   0.725  0.46828    
## sppEC-A      -0.3871     0.3424  -1.130  0.25836    
## sppEC-L       0.4879     0.2383   2.047  0.04062 *  
## sppDES-L      0.5895     0.2278   2.588  0.00965 ** 
## sppDF        -0.1133     0.2439  -0.464  0.64232    
## minedno       1.4294     0.3666   3.899 9.66e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Zero-inflation model:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   0.9100     0.6279   1.449    0.147    
## sppPR         1.1614     1.3346   0.870    0.384    
## sppDM        -0.9393     0.8005  -1.173    0.241    
## sppEC-A       1.0424     0.7140   1.460    0.144    
## sppEC-L      -0.5623     0.7263  -0.774    0.439    
## sppDES-L     -0.8930     0.7535  -1.185    0.236    
## sppDF        -2.5398     2.1817  -1.164    0.244    
## minedno      -2.5630     0.6045  -4.240 2.24e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
res = simulateResiduals(m)
```

```
## Warning in simulateResiduals(m): Due to limitations in the current
## implementation of glmmTMB, model predictions are calculated conditional
## on the fitted random effects. This can sometimes create assymetries when
## plotting residual vs. predicted, see https://github.com/florianhartig/
## DHARMa/issues/43. If you see assymetries similar to what is shown in this
## issue, but residuals otherwise look fine, you should probably calculate
## a model prediction by hand with fixed effects only, and do the plot for
## those.
```

```r
plot(res, rank = T)
```

![](glmmTMB_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

There is still a slight correlation in both cases - I assume that this is again a problem of https://github.com/florianhartig/DHARMa/issues/43

# Reproducibility info 


```r
sessionInfo()
```

```
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: macOS  10.13.3
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] glmmTMB_0.2.0.9000 DHARMa_0.1.6      
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.16     knitr_1.15.1     TMB_1.7.13       magrittr_1.5    
##  [5] splines_3.3.2    MASS_7.3-45      lattice_0.20-34  foreach_1.4.3   
##  [9] minqa_1.2.4      stringr_1.2.0    tools_3.3.2      grid_3.3.2      
## [13] nlme_3.1-131     htmltools_0.3.5  iterators_1.0.8  yaml_2.1.14     
## [17] lme4_1.1-15      rprojroot_1.2    digest_0.6.12    qrnn_2.0        
## [21] Matrix_1.2-7.1   nloptr_1.0.4     codetools_0.2-15 evaluate_0.10   
## [25] rmarkdown_1.4    gap_1.1-16       stringi_1.1.3    backports_1.0.5
```


