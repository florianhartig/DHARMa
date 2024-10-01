# tests work

    Code
      testUniformity(simulationOutput, plot = FALSE)
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.071007, p-value = 0.2655
      alternative hypothesis: two-sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^- = 0.071007, p-value = 0.1331
      alternative hypothesis: the CDF of x lies below the null hypothesis
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^+ = 0.017586, p-value = 0.8836
      alternative hypothesis: the CDF of x lies above the null hypothesis
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.1461, p-value = 0.248
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.1461, p-value = 0.876
      alternative hypothesis: less
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.1461, p-value = 0.124
      alternative hypothesis: greater
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.1461, p-value = 0.248
      alternative hypothesis: two.sided
      

---

    Code
      testResiduals(simulationOutput, plot = FALSE)
    Output
      $uniformity
      
      	Asymptotic one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.071007, p-value = 0.2655
      alternative hypothesis: two-sided
      
      
      $dispersion
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.1461, p-value = 0.248
      alternative hypothesis: two.sided
      
      
      $outliers
      
      	DHARMa bootstrapped outlier test
      
      data:  simulationOutput
      outliers at both margin(s) = 0, observations = 200, p-value = 1
      alternative hypothesis: two.sided
       percent confidence interval:
       0.00 0.01
      sample estimates:
      outlier frequency (expected: 0.0029 ) 
                                          0 
      
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 1.0501, p-value = 0.616
      alternative hypothesis: two.sided
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = 1.0501, p-value = 0.744
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0037, p-value = 1
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0037, p-value = 0.532
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0098, p-value = 0.904
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = spread, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.068, p-value = 0.304
      alternative hypothesis: two.sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE)
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.12286, p-value = 0.9932
      alternative hypothesis: two-sided
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^- = 0.10586, p-value = 0.7447
      alternative hypothesis: the CDF of x lies below the null hypothesis
      

---

    Code
      testUniformity(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D^+ = 0.12286, p-value = 0.685
      alternative hypothesis: the CDF of x lies above the null hypothesis
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.0862, p-value = 0.8
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.0862, p-value = 0.6
      alternative hypothesis: less
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "greater")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.0862, p-value = 0.4
      alternative hypothesis: greater
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.0862, p-value = 0.8
      alternative hypothesis: two.sided
      

---

    Code
      testResiduals(simulationOutput, plot = FALSE)
    Output
      $uniformity
      
      	Exact one-sample Kolmogorov-Smirnov test
      
      data:  simulationOutput$scaledResiduals
      D = 0.12286, p-value = 0.9932
      alternative hypothesis: two-sided
      
      
      $dispersion
      
      	DHARMa nonparametric dispersion test via sd of residuals fitted vs.
      	simulated
      
      data:  simulationOutput
      dispersion = 1.0862, p-value = 0.8
      alternative hypothesis: two.sided
      
      
      $outliers
      
      	DHARMa bootstrapped outlier test
      
      data:  simulationOutput
      outliers at both margin(s) = 0, observations = 200, p-value = 1
      alternative hypothesis: two.sided
       percent confidence interval:
       0.0 0.1
      sample estimates:
      outlier frequency (expected: 0.004 ) 
                                         0 
      
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: two.sided
      

---

    Code
      testZeroInflation(simulationOutput, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa zero-inflation test via comparison to expected zeros with
      	simulation under H0 = fitted model
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = countOnes, plot = FALSE, alternative = "less")
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = NaN, p-value = 1
      alternative hypothesis: less
      

---

    Code
      testGeneric(simulationOutput, summary = means, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.0098, p-value = 0.904
      alternative hypothesis: two.sided
      

---

    Code
      testGeneric(simulationOutput, summary = spread, plot = FALSE)
    Output
      
      	DHARMa generic simulation test
      
      data:  simulationOutput
      ratioObsSim = 1.1306, p-value = 0.52
      alternative hypothesis: two.sided
      

---

    Code
      testDispersion(simulationOutput, plot = FALSE)
    Output
      
      	DHARMa nonparametric dispersion test via mean deviance residual fitted
      	vs. simulated-refitted
      
      data:  simulationOutput
      dispersion = 1.0869, p-value = 0.344
      alternative hypothesis: two.sided
      

# correlation tests work

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = 0.0082334, expected = -0.0050251, sd = 0.0119231, p-value =
      0.2661
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, x = testData$x, y = testData$y,
      plot = FALSE, alternative = "two.sided")
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = 0.0082334, expected = -0.0050251, sd = 0.0119231, p-value =
      0.2661
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE)
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = 0.0082334, expected = -0.0050251, sd = 0.0119231, p-value =
      0.2661
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE,
        alternative = "two.sided")
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = 0.0082334, expected = -0.0050251, sd = 0.0119231, p-value =
      0.2661
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, plot = FALSE, x = testData$x[1:10],
      y = testData$y[1:9])
    Condition
      Warning in `cbind()`:
      number of rows of result is not a multiple of vector length (arg 2)
      Error in `testSpatialAutocorrelation()`:
      ! Dimensions of x / y coordinates don't match the dimension of the residuals

---

    Code
      testSpatialAutocorrelation(simulationOutput[1:10], plot = FALSE, x = testData$x[
        1:10], y = testData$y[1:10])
    Condition
      Error in `ensureDHARMa()`:
      ! wrong argument to function, simulationOutput must be a DHARMa object or a numeric vector of quantile residuals!

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE, x = testData$
        x)
    Message
      both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = 0.0082334, expected = -0.0050251, sd = 0.0119231, p-value =
      0.2661
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testSpatialAutocorrelation(simulationOutput, distMat = dM, plot = FALSE, y = testData$
        y)
    Message
      both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting
    Output
      
      	DHARMa Moran's I test for distance-based autocorrelation
      
      data:  simulationOutput
      observed = 0.0082334, expected = -0.0050251, sd = 0.0119231, p-value =
      0.2661
      alternative hypothesis: Distance-based autocorrelation
      

---

    Code
      testTemporalAutocorrelation(simulationOutput, plot = FALSE, time = testData$
      time)
    Output
      
      	Durbin-Watson test
      
      data:  simulationOutput$scaledResiduals ~ 1
      DW = 2.0905, p-value = 0.5202
      alternative hypothesis: true autocorrelation is not 0
      

---

    Code
      testTemporalAutocorrelation(simulationOutput, plot = FALSE, time = testData$
      time, alternative = "greater")
    Output
      
      	Durbin-Watson test
      
      data:  simulationOutput$scaledResiduals ~ 1
      DW = 2.0905, p-value = 0.7399
      alternative hypothesis: true autocorrelation is greater than 0
      

# test phylogenetic autocorrelation

    Code
      restest
    Output
      
      	DHARMa Moran's I test for phylogenetic autocorrelation
      
      data:  res
      observed = 0.851667, expected = -0.016949, sd = 0.088733, p-value <
      2.2e-16
      alternative hypothesis: Phylogenetic autocorrelation
      

# testOutliers

    Code
      testOutliers(simulationOutput, plot = F, margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 4, observations = 1000, p-value = 0.8037
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.001090908 0.010209665
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.004 
      

---

    Code
      testOutliers(simulationOutput, plot = F, alternative = "two.sided", margin = "lower")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at lower margin(s) = 4, observations = 1000, p-value = 0.8037
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.001090908 0.010209665
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.004 
      

---

    Code
      testOutliers(simulationOutput, plot = F, margin = "upper")
    Output
      
      	DHARMa outlier test based on exact binomial test with approximate
      	expectations
      
      data:  simulationOutput
      outliers at upper margin(s) = 4, observations = 1000, p-value = 0.8037
      alternative hypothesis: true probability of success is not equal to 0.003984064
      95 percent confidence interval:
       0.001090908 0.010209665
      sample estimates:
      frequency of outliers (expected: 0.00398406374501992 ) 
                                                       0.004 
      

